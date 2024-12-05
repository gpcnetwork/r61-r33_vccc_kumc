/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: denom_pat_demo.sql
# Description: create PAT_TABLE1 and generate summary statistics  
*/

create or replace table PAT_DEMO_LONG (
    PATID varchar(50) NOT NULL,
    BIRTH_DATE date,
    INDEX_DATE date,  
    INDEX_ENC_TYPE varchar(3),
    AGE_AT_INDEX integer, 
    AGEGRP_AT_INDEX varchar(10),
    SEX varchar(3),
    RACE varchar(6),
    HISPANIC varchar(20),
    CENSOR_DATE date,
--     STATUS number,
    INDEX_SRC varchar(20)
);

create or replace procedure get_pat_demo(
    SITES ARRAY,
    DRY_RUN BOOLEAN,
    DRY_RUN_AT STRING
)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a Table 1 for overall GPC cohort
 * @param {array} SITES: an array of site acronyms (matching schema name suffix) - not include CMS
 * @param {boolean} DRY_RUN: dry run indicator. If true, only sql script will be created and stored in dev.sp_out table
 * @param {boolean} DRY_RUN_AT: A temporary location to store the generated sql query for debugging purpose. 
                                When DRY_RUN = True, provide absolute path to the table; when DRY_RUN = False, provide NULL 
**/
if (DRY_RUN) {
    var log_stmt = snowflake.createStatement({
        sqlText: `CREATE OR REPLACE TEMPORARY TABLE `+ DRY_RUN_AT +`(QRY VARCHAR);`});
    log_stmt.execute(); 
}

var i;
for(i=0; i<SITES.length; i++){
    var site = SITES[i].toString();
    var site_cdm = (site === 'CMS') ? 'CMS_PCORNET_CDM' : 'PCORNET_CDM_' + site;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO PAT_DEMO_LONG
            WITH cte_enc_age AS (
                SELECT d.patid,
                    d.birth_date,
                    e.admit_date::date as index_date,
                    e.enc_type as index_enc_type,
                    round(datediff(day,d.birth_date::date,e.admit_date::date)/365.25) AS age_at_index,
                    d.sex, 
                    CASE WHEN d.race IN ('05') THEN 'white' 
                            WHEN d.race IN ('03') THEN 'black'
                            WHEN d.race IN ('NI','UN',NULL) THEN 'NI'
                            ELSE 'ot' END AS race, 
                    CASE WHEN d.hispanic = 'Y' THEN 'hispanic' 
                            WHEN d.hispanic = 'N' THEN 'non-hispanic' 
                            WHEN d.hispanic IN ('NI','UN',NULL) THEN 'NI'
                            ELSE 'ot' END AS hispanic,
                    '`+ site +`' as index_src,
                    max(coalesce(e.discharge_date::date,e.admit_date::date)) over (partition by d.patid) as censor_date,
                  --   max(case when dth.death_date is not null then 1 else 0 end) over (partition by d.patid) as status,
                    row_number() over (partition by e.patid order by coalesce(e.admit_date::date,current_date)) rn
                FROM GROUSE_DB_DEV.`+ site_cdm +`.DEMOGRAPHIC d 
                LEFT JOIN GROUSE_DB_DEV.`+ site_cdm +`.ENCOUNTER e ON d.PATID = e.PATID
            --     LEFT JOIN GROUSE_DB_DEV.`+ site_cdm +`.DEATH dth ON d.PATID = dth.PATID
                )
                SELECT DISTINCT
                     cte.patid
                    ,cte.birth_date
                    ,cte.index_date
                    ,cte.index_enc_type
                    ,cte.age_at_index
                    ,case when cte.age_at_index is null then 'unk'
                          when cte.age_at_index < 19 then 'agegrp1'
                          when cte.age_at_index >= 19 and cte.age_at_index < 24 then 'agegrp2'
                          when cte.age_at_index >= 25 and cte.age_at_index < 85 then 'agegrp' || (floor((cte.age_at_index - 25)/5) + 3)
                          else 'agegrp15' end as agegrp_at_index
                    ,cte.sex
                    ,cte.race
                    ,cte.hispanic
                    ,cte.censor_date
                  --   ,cte.status
                    ,cte.index_src
                FROM cte_enc_age cte
                WHERE cte.rn = 1;
        `;
    
    if (DRY_RUN) {
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); commit_txn.execute();
    }
}
$$
;

/* test */
-- call get_pat_demo(
--     array_construct(
--      'KUMC'
--     ,'UU'
-- ), True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

truncate PAT_DEMO_LONG;
call get_pat_demo(
    array_construct(
       'KUMC'
      ,'UU'
    ), False, NULL
);


create or replace table PAT_TABLE1 as 
with cte_ord as(
    select a.*, 
      --      max(case when b.chart = 'Y' then 1 else 0 end) over (partition by a.patid) as xwalk_ind,
           row_number() over (partition by a.patid order by coalesce(a.index_date,current_date)) as rn
    from PAT_DEMO_LONG a
--     left join GROUSE_DB_DEV.CMS_PCORNET_CDM.V_DEID_ENROLLMENT b on a.patid = b.patid
)
select patid
      ,birth_date
      ,index_date
      ,age_at_index
      ,agegrp_at_index
      ,sex
      ,race
      ,hispanic
      ,index_enc_type
      ,index_src
      ,censor_date
      -- ,xwalk_ind
from cte_ord
where rn = 1
;

select count(distinct patid), count(*) from PAT_TABLE1;
-- 3,290,425


/*collect all patients with at least 1 SBP record and calculate age at measurement*/
create or replace table BP_Cohort as
-- SBP and DBP from VITAL table
with multi_cte as (
    select * from
    (select p.PATID
           ,v.ENCOUNTERID
           ,v.SYSTOLIC
           ,v.DIASTOLIC
           ,v.MEASURE_DATE
           ,round((v.MEASURE_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
           ,'VITAL' as SRC_TABLE
     from identifier($VITAL) v 
     join identifier($DEMOGRAPHIC) p on v.PATID = p.PATID 
     where v.SYSTOLIC is not null
    )
    unpivot (
       VITAL_VAL for VITAL_TYPE in (SYSTOLIC, DIASTOLIC)
    )
    union all
    -- SBP from OBS_CLIN table
    select p.PATID
          ,os.ENCOUNTERID
          ,os.OBSCLIN_START_DATE
          ,round((os.OBSCLIN_START_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
          ,'OBS_CLIN' as SRC_TABLE
          ,'SYSTOLIC' as VITAL_TYPE
          ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
    from identifier($OBS_CLIN) os
    join identifier($DEMOGRAPHIC) p on os.PATID = p.PATID and
         -- os.OBSCLIN_TYPE = 'LC' and 
         os.OBSCLIN_CODE in ( '8460-8' --standing
                             ,'8459-0' --sitting
                             ,'8461-6' --supine
                             ,'8479-8' --palpation
                             ,'8480-6' --general
                            )
    union all
    -- DBP from OBS_CLIN table
    select p.PATID
          ,os.ENCOUNTERID
          ,os.OBSCLIN_START_DATE
          ,round((os.OBSCLIN_START_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
          ,'OBS_CLIN' as SRC_TABLE
          ,'DIASTOLIC' as VITAL_TYPE
          ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
    from identifier($OBS_CLIN) os
    join identifier($DEMOGRAPHIC) p on os.PATID = p.PATID and
         -- os.OBSCLIN_TYPE = 'LC' and 
         os.OBSCLIN_CODE in ( '8454-1' --standing
                             ,'8453-3' --sitting
                             ,'8455-8' --supine
                             ,'8462-4' --general
                            )
)
select distinct 
       PATID,
       ENCOUNTERID,
       AGE_AT_MEASURE,
       MEASURE_DATE,
       "'SYSTOLIC'" as SYSTOLIC,
       "'DIASTOLIC'" as DIASTOLIC
from multi_cte
    pivot (max(VITAL_VAL) for VITAL_TYPE in ('SYSTOLIC','DIASTOLIC'))
        as p
where AGE_AT_MEASURE >= 65
;

