/*
identify study-specific denominator cohort 
*/

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
                    e.facility_type,
                    e.facilityid,
                    e.facility_location,
                    round(datediff(day,d.birth_date::date,e.admit_date::date)/365.25) AS age_at_index,
                    d.sex, 
                    CASE WHEN d.race IN ('05') THEN 'white' 
                         WHEN d.race IN ('03') THEN 'black'
                         WHEN d.race IN ('02') THEN 'asian'
                         WHEN d.race IN ('NI','UN',NULL) THEN 'NI'
                         ELSE 'ot' END AS race, 
                    CASE WHEN d.hispanic = 'Y' THEN 'hispanic' 
                            WHEN d.hispanic = 'N' THEN 'non-hispanic' 
                            WHEN d.hispanic IN ('NI','UN',NULL) THEN 'NI'
                            ELSE 'ot' END AS hispanic,
                    '`+ site +`' as index_src,
                    max(coalesce(e.discharge_date::date,e.admit_date::date)) over (partition by d.patid) as censor_date,
                    max(case when dth.death_date is not null then 1 else 0 end) over (partition by d.patid) as status,
                    row_number() over (partition by e.patid order by coalesce(e.admit_date::date,current_date)) rn
                FROM GROUSE_DB_DEV_CDM.`+ site_cdm +`.DEMOGRAPHIC d 
                LEFT JOIN GROUSE_DB_DEV_CDM.`+ site_cdm +`.ENCOUNTER e ON d.PATID = e.PATID
                LEFT JOIN GROUSE_DB_DEV_CDM.`+ site_cdm +`.DEATH dth ON d.PATID = dth.PATID
                WHERE e.ENC_TYPE in ('AV','OA') and e.ADMIT_DATE between '2022-01-01' and '2024-12-31'
                )
                SELECT DISTINCT
                     cte.patid
                    ,cte.birth_date
                    ,cte.index_date
                    ,cte.index_enc_type
                    -- ,cte.facility_type as index_fac_type
                    ,cte.facilityid as index_fac_id
                    ,cte.facility_location as index_fac_loc 
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
                    ,cte.status
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

create or replace table PAT_DEMO_LONG (
    PATID varchar(50) NOT NULL,
    BIRTH_DATE date,
    INDEX_DATE date,  
    INDEX_ENC_TYPE varchar(3),
    -- INDEX_FAC_TYPE varchar(20), 
    INDEX_FAC_ID varchar(20), 
    INDEX_FAC_LOC varchar(8), 
    AGE_AT_INDEX integer, 
    AGEGRP_AT_INDEX varchar(10),
    SEX varchar(3),
    RACE varchar(6),
    HISPANIC varchar(20),
    CENSOR_DATE date,
    STATUS number,
    INDEX_SRC varchar(20)
);
call get_pat_demo(
    array_construct(
       'KUMC'
      ,'UU'
    ), False, NULL
);

select * from PAT_DEMO_LONG limit 5;
select count(distinct patid) 
from PAT_DEMO_LONG 
-- where trim(index_fac_loc) <> ''
;
-- 1,683,212
-- 1,593,686 (facid)
-- 800,891 (fac_loc)

create or replace table PAT_TABLE1 as 
with cte_ord as(
    select a.*, 
      --      max(case when b.chart = 'Y' then 1 else 0 end) over (partition by a.patid) as xwalk_ind,
           row_number() over (partition by a.patid order by coalesce(a.index_date,current_date)) as rn
    from PAT_DEMO_LONG a
--     left join GROUSE_DB_DEV_CDM.CMS_PCORNET_CDM.V_DEID_ENROLLMENT b on a.patid = b.patid
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
      ,index_fac_id
      ,index_fac_loc
      ,index_src
      ,censor_date
      -- ,xwalk_ind
from cte_ord
where rn = 1 and age_at_index >= 65
;

select count(distinct patid), count(*) from PAT_TABLE1;
-- 361,005
