/*
Copyright (c) 2021-2025 University of Missouri                   
Author: Xing Song, xsm7f@umsystem.edu 
===========================    
Key CDM tables from KUMC and 
1. ENCOUNTER
2. DEMOGRAPHIC
3. VITAL
4. PRESCRIBING
5. PCORNET_TRIAL

Auxillary tables: 
1. ELIG_BP_REDCAP_20241120
2. BASE_BP_REDCAP_20241120
3. Z_MED_RXCUI_REF (ref/med_rxcui_ref.csv)
4. PAT_TABLE1 (denom_cdm.sql)
5. Z_CCI_REF (ref/cci_icd_ref.csv)
6. VCCC_PID_ADDRID
7. VCCC_UNIQUE_ADDR_GEOCODED
=============================
*/ 

select * from ELIG_BP_REDCAP_20241120 limit 5;
select * from BASE_BP_REDCAP_20250123 limit 5;
select * from BASE_PT_REDCAP_20250115 limit 5;
select * from BASELINE_TCOG_20250121 limit 5;
select * from Z_MED_RXCUI_REF limit 5;
select * from PAT_TABLE1 limit 5;
select * from VCCC_PID_ADDRID limit 5;
select * from VCCC_UNIQUE_ADDR_GEOCODED limit 5;
select * from SDOH_DB.AHRQ.AHRQ_CT_2020 limit 5;
select * from SDOH_DB.ADI.ADI_BG_2020 limit 5;
select * from SDOH_DB.RUCA.RUCA_TR_2010 limit 5;

select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PCORNET_TRIAL limit 5;
select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PRESCRIBING limit 5;
select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.ENCOUNTER where trim(drg) <> '' limit 5;

create or replace table ELIG_BP as 
with stk as (
    select study_id, elig_sbp1 as elig_sbp, elig_dbp1 as elig_dbp, elig_sbp1_date as elig_date
    from ELIG_BP_REDCAP_20241120
    union all 
    select study_id, elig_sbp2 as elig_sbp, elig_dbp2 as elig_dbp, elig_sbp2_date as elig_date
    from ELIG_BP_REDCAP_20241120
    union all
    select study_id, elig_sbp3 as elig_sbp, elig_dbp3 as elig_dbp, elig_sbp2_date_2 as elig_date
    from ELIG_BP_REDCAP_20241120
), stk_ord as (
    select stk.*, row_number() over (partition by stk.study_id order by elig_date desc) as rn
    from stk
)
select * exclude(rn,study_id), upper(study_id) as study_id
from stk_ord
where rn = 1 
;

select count(distinct study_id) from ELIG_BP;
-- 1000

create or replace table BASE_BP as 
with cte as (
    select upper(study_id) as study_id, 
           bp_date as base_date,
           measure,
           result
    from (
        select a.*, row_number() over (partition by a.study_id, a.measure order by a.bp_date) rn
        from BASE_BP_REDCAP_20250123 a
    )
    where rn = 1
)
select study_id,
       base_date,
       round(dbp) as base_dbp,
       round(sbp) as base_sbp,
       round(hr) as base_hr 
from cte 
pivot (
    avg(result) for measure in (any order by measure)
) as p(study_id,base_date,DBP,HR,SBP)
;

select * from BASE_BP;

select count(distinct study_id) from base_bp;
-- 1000

select a.* from ELIG_BP a 
where not exists (select 1 from base_bp b where a.study_id = b.study_id);

create or replace table STUDYID_MAPPING as 
select participantid, patid, trial_enroll_date, trial_end_date, trial_withdraw_date, 'KUMC' as site
from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PCORNET_TRIAL where lower(TRIALID) = 'vccc'
union 
select participantid, patid, trial_enroll_date, trial_end_date, trial_withdraw_date, 'UU' as site
from GROUSE_DB_DEV.PCORNET_CDM_UU.PCORNET_TRIAL where lower(TRIALID) = 'vccc'
;

create or replace table VCCC_UNMAPPED as 
select a.*, b.patid, b.site 
from ELIG_BP_REDCAP_20241120 a
left join STUDYID_MAPPING b
on lower(a.study_id) = lower(b.participantid)
where b.patid is null
;

select substr(study_id,1,2), count(distinct study_id) 
from VCCC_UNMAPPED
group by substr(study_id,1,2)
;
-- KU	9


create or replace table OC_TCOG as 
with clean_cte as (
        select  study_id,
                tcog_date,
                'T'||lpad(test_id,2,'0') as testid,
                score,
                normed_score,
                rmse as normed_rmse
    from BASELINE_TCOG_20250127
)
, score_cte as (
    select * from (
        select study_id, tcog_date, testid, score from clean_cte
    )
    pivot (
        avg(score) for testid in (any order by testid)
    ) as p(study_id,tcog_date,T01,T02,T03,T04,T05,T06,T07,T08,T09,T10,T11,T12,T13,T14,T15,T16,T18,T19,T20)
)
, nscore_cte as (
    select * from (
        select study_id, tcog_date, testid, normed_score from clean_cte
        where normed_score is not null
    )
    pivot (
        avg(normed_score) for testid in (any order by testid)
    ) as p(study_id,tcog_date,T08N,T09N,T12N,T13N,T15N,T16N)
)
, nrmse_cte as (
    select * from (
        select study_id, tcog_date, testid, normed_rmse from clean_cte
        where normed_rmse is not null
    )
    pivot (
        avg(normed_rmse) for testid in (any order by testid)
    ) as p(study_id,tcog_date,T08NE,T09NE,T12NE,T13NE,T15NE,T16NE)
)
select score_cte.*, 
       nscore_cte.* exclude(study_id, tcog_date),
       nrmse_cte.* exclude(study_id, tcog_date)
from score_cte
left join nscore_cte on nscore_cte.study_id = score_cte.study_id and nscore_cte.tcog_date = score_cte.tcog_date
left join nrmse_cte on nrmse_cte.study_id = score_cte.study_id and nrmse_cte.tcog_date = score_cte.tcog_date
;

select count(distinct study_id), count(*) from OC_TCOG;
-- 1000	1000
select * from OC_TCOG limit 5;

create or replace table VCCC_BASE_BP_TCOG_SDH as 
select upper(a.study_id) as study_id,
    --    c.patid, 
    --    c.trial_enroll_date, 
    --    c.trial_end_date, 
    --    c.trial_withdraw_date,
    --    c.site,
       p.enroll_date,
       p.disp_date,
       p.disp_status,
       p.site,
       a.elig_sbp,
       a.elig_date,
       b.base_hr,
       b.base_dbp,
       b.base_sbp,
       b.base_sbp - a.elig_sbp as delta_sbp,
       case when b.base_sbp - try_to_number(a.elig_sbp) < 0 and b.base_sbp<130 then 'deltasbp1'
            when b.base_sbp - try_to_number(a.elig_sbp) < 0 and b.base_sbp>=130 and b.base_sbp<140 then 'deltasbp2'
            else 'deltasbp3'
       end as delta_sbp_group,
       datediff(day,b.base_date,a.elig_date) as delta_days,
       case when try_to_number(a.elig_sbp) >=140 and try_to_number(a.elig_sbp) <150 then 'esbp1'
            when try_to_number(a.elig_sbp) >=150 and try_to_number(a.elig_sbp) <160 then 'esbp2'
            when try_to_number(a.elig_sbp) >=160 then 'esbp3'
            else 'esbp0'
       end as esbp_group,
       case when b.base_sbp>=130 and b.base_sbp<140 then 'bsbp1'
            when b.base_sbp>=140 and b.base_sbp<150 then 'bsbp2'
            when b.base_sbp>=150 and b.base_sbp<160 then 'bsbp3'
            when b.base_sbp>=160 then 'bsbp4'
            else 'bsbp0'
       end as bsbp_group,
       datediff('day',p.enroll_date,a.elig_date) as elig_since_index,
       p.sex,
       p.sex_str,
       p.race,
       p.race_str,
       p.ethnicity,
       p.ethn_str,
       p.urm_ind,
       p.age,
       c.patid,
    --    g.g.census_block_group_id_2020,
    --    g.census_tract_id_2020,
       dense_rank() over (order by  g.census_tract_id_2020) as census_track_deid,
       t.* exclude(study_id),
       (t.T08N+t.T09N+t.T12N+t.T13N+t.T15N+t.T16N)/6 as T21N,
       sdh.* exclude(year,tractfips,countyfips,statefips,region,territory),
       coalesce(try_to_number(adi.adi_natrank),33) as adi_natrank,  -- manual mode imputation
       coalesce(try_to_number(adi.adi_staterank),1) as adi_staterank,  -- manual mode imputation
       coalesce(ruca.ruca_primary,99) as ruca_primary,
       case when ruca.ruca_primary in (1,2,3) then 'metro'
            when ruca.ruca_primary in (4,5,6) then 'micro'
            when ruca.ruca_primary in (7,8,9) then 'town'
            when ruca.ruca_primary in (10) then 'rural'
            else 'NI'
       end as ruca_primary_grp,
       case when ruca.ruca_primary in (1,2,3) then 0
            else 1
       end as ruca_primary_nonmetro_ind
from ELIG_BP a 
join BASE_BP b on a.study_id = b.study_id 
join BASE_PT_REDCAP_20250115 p on a.study_id = p.study_id
join VCCC_PID_ADDRID pa on a.study_id = pa.study_id
join VCCC_UNIQUE_ADDR_GEOCODED g on pa.id = g.id
join OC_TCOG t on t.study_id = a.study_id
join SDOH_DB.AHRQ.AHRQ_CT_2020 sdh on lpad(g.census_tract_id_2020,11,'0') = lpad(sdh.tractfips,11,'0')
left join SDOH_DB.ADI.ADI_BG_2020 adi on lpad(g.census_block_group_id_2020,12,'0') = adi.geocodeid
left join SDOH_DB.RUCA.RUCA_TR_2010 ruca on lpad(g.census_tract_id_2020,11,'0') = ruca.geocodeid
left join STUDYID_MAPPING c on a.study_id = c.participantid
order by a.study_id
;

select adi_staterank, count(distinct patid)
from VCCC_BASE_BP_TCOG_SDH
group by adi_staterank
order by count(distinct patid) desc;

select * from VCCC_BASE_BP_TCOG_SDH limit 5;

select count(distinct patid), count(distinct study_id), count(*)
from VCCC_BASE_BP_TCOG_SDH;
-- 1000
select * from SDOH_DB.AHRQ.Z_ALL_SDOH_VARIABLES;
select distinct column_name as VAR, b.domain, b.topic, b.variable_label, b.data_source       
from information_schema.columns a
left join SDOH_DB.AHRQ.Z_ALL_SDOH_VARIABLES b 
on a.column_name = b.variable_name
where table_name = 'VCCC_BASE_BP_TCOG_SDH'
;

create or replace table Z_MED_RXCUI_REF_AH as 
with cte as (
select RXNORM_CUI, 
       ING,
       VA_CLS, 
       VA_CLS_CD, 
       case when VA_CLS_CD in (
        'CV100', -- beta blockers/related
        'CV150', -- alpha blockers/related
        'CV200', -- calcium channel blockers
        'CV400', -- antihypertensive combinations
        'CV490', -- antihypertensive, others
        'CV701', -- thiazides/related diuretics
        'CV702', -- loop diuretics
        'CV703', -- carbonic anhydrase inhibitor diuretics
        'CV704', -- potassium sparing/combinations diretics
        'CV709', -- diuretics, other
        'CV800', -- ACE inhibitors
        'CV805', -- angiotensin II inhibitor 
        'CV806'  -- direct renin inhibitor
       ) then 1 else 0 
       end as ANTIHTN_IND,
       STR,
       UNIT,
       row_number() over (partition by RXNORM_CUI order by ING) as rn
from Z_MED_RXCUI_REF
)
select RXNORM_CUI, 
       ING,
       VA_CLS, 
       VA_CLS_CD,
       ANTIHTN_IND,
       STR as REF_STR,
       UNIT as REF_UNIT
from cte
where rn = 1
;

select * from Z_MED_RXCUI_REF_AH;

create or replace table VCCC_MED_LONG_RAW as 
select  a.patid,
        vccc.study_id,
        vccc.enroll_date,
        vccc.elig_date,
        datediff(day,vccc.enroll_date,vccc.elig_date) as elig_since_index,
        a.prescribingid,
        try_to_number(a.RXNORM_CUI) as RXNORM_CUI, 
        a.raw_rx_med_name,
        a.rx_order_date,
        a.rx_start_date, 
        a.rx_end_date,
        a.rx_dose_ordered,
        a.rx_dose_ordered_unit,
        a.rx_quantity,
        a.rx_dose_form, 
        a.rx_route,
        a.rx_frequency, 
        case when rx_frequency in ('01','05','06','10') then 1 
             when rx_frequency in ('02') then 2 
             when rx_frequency in ('03','07','08') then 3
             when rx_frequency in ('04') then 4
             else 1 
        end as rx_freq_num,
        a.rx_refills,
        coalesce(a.rx_start_date,a.rx_order_date) as rx_start_date_imp,
        m.ING,
        m.VA_CLS,
        m.VA_CLS_CD,
        m.ANTIHTN_IND,
        m.REF_STR,
        m.REF_UNIT,
        coalesce(e.enc_type,'NI') as enc_type,
        e.admit_date::date as admit_date,
        e.raw_enc_type,
        e.admitting_source,
        e.DRG,
        e.payer_type_primary,
        e.raw_payer_name_primary,
        e.discharge_date::date as discharge_date,
        e.discharge_disposition,
        e.discharge_status,
        vccc.enroll_date as index_date,
        datediff(day,vccc.enroll_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index        
from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PRESCRIBING a 
join VCCC_BASE_BP_TCOG_SDH vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV.PCORNET_CDM_KUMC.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on try_to_number(a.RXNORM_CUI) = m.RXNORM_CUI 
where vccc.site = 'KUMC'
union all
select  a.patid,
        vccc.study_id,
        vccc.enroll_date,
        vccc.elig_date,
        datediff(day,vccc.enroll_date,vccc.elig_date) as elig_since_index,
        a.prescribingid,
        try_to_number(a.RXNORM_CUI) as RXNORM_CUI, 
        a.raw_rx_med_name,
        a.rx_order_date,
        a.rx_start_date, 
        a.rx_end_date,
        a.rx_dose_ordered,
        a.rx_dose_ordered_unit,
        a.rx_quantity,
        a.rx_dose_form, 
        a.rx_route,
        a.rx_frequency, 
        case when rx_frequency in ('01','05','06','10') then 1 
             when rx_frequency in ('02') then 2 
             when rx_frequency in ('03','07','08') then 3
             when rx_frequency in ('04') then 4
             else 1 
        end as rx_freq_num,
        a.rx_refills,
        coalesce(a.rx_start_date,a.rx_order_date) as rx_start_date_imp,
        m.ING,
        m.VA_CLS,
        m.VA_CLS_CD,
        m.ANTIHTN_IND,
        m.REF_STR,
        m.REF_UNIT,
        coalesce(e.enc_type,'NI') as enc_type,
        e.admit_date::date as admit_date,
        e.raw_enc_type,
        e.admitting_source,
        e.DRG,
        e.payer_type_primary,
        e.raw_payer_name_primary,
        e.discharge_date::date as discharge_date,
        e.discharge_disposition,
        e.discharge_status,
        vccc.enroll_date as index_date,
        datediff(day,vccc.enroll_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index           
from GROUSE_DB_DEV.PCORNET_CDM_UU.PRESCRIBING a 
join VCCC_BASE_BP_TCOG_SDH vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV.PCORNET_CDM_UU.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on a.RXNORM_CUI = m.RXNORM_CUI 
where vccc.site = 'UU'
;         

create or replace table VCCC_MED_LONG as
with dur_calc as(
    select patid, 
        study_id, 
        elig_since_index,
        prescribingid, 
        rxnorm_cui,
        ING,
        VA_CLS,
        VA_CLS_CD,
        coalesce(ANTIHTN_IND,0) as ANTIHTN_IND,
        coalesce(ING,raw_rx_med_name) as in_or_name,
        substr(coalesce(ING,raw_rx_med_name),1,20) as in_or_name_s,
        rx_start_since_index,
        case when rx_start_since_index<=0 and rx_start_since_index<=elig_since_index then 'bef'
             when rx_start_since_index<=0 and rx_start_since_index>elig_since_index then 'runin'
             else 'aft'
        end as rx_timing,
        rx_start_date_imp,
        rx_end_date,
        rx_refills,
        rx_quantity,
        rx_freq_num,
        rx_dose_ordered_unit,
        ref_str,
        ref_unit,
        datediff(day,rx_start_date_imp,coalesce(rx_end_date,rx_start_date_imp)) as rx_days1,
        round((rx_refills+1) * (rx_quantity/greatest(rx_freq_num,1))) as rx_days2,
        coalesce(rx_dose_ordered/rx_freq_num, ref_str) as rx_str,
        coalesce(rx_dose_ordered,ref_str*rx_freq_num) as rx_dose,
        coalesce(rx_dose_ordered_unit,ref_unit) as rx_unit
    from VCCC_MED_LONG_RAW
)
select distinct
       dur_calc.*, 
       coalesce(dur_calc.rx_days2,dur_calc.rx_days1) as rx_days,
       rx_start_since_index + coalesce(dur_calc.rx_days2,dur_calc.rx_days1) as rx_end_since_index
from dur_calc
;

select * from VCCC_MED_LONG limit 5;

select count(distinct patid) from VCCC_MED_LONG;
-- 723

create or replace procedure get_clinic_bp_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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
    // parameter
    var site = SITES[i].toString();
    var site_cdm = `GROUSE_DB_DEV.PCORNET_CDM_`+ site +``;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
            -- SBP and DBP from VITAL table
            with multi_cte as (
                select * from
                (
                    select   distinct
                             r.PATID
                            ,r.STUDY_ID
                            ,v.SYSTOLIC
                            ,v.DIASTOLIC
                            ,v.MEASURE_DATE
                            ,datediff(day,r.enroll_date,v.measure_date) as DAYS_SINCE_INDEX
                        from `+ TRIAL_REF +` r 
                        join `+ site_cdm +`.VITAL v on r.patid = v.patid
                        where v.SYSTOLIC is not null
                )
                unpivot (
                VITAL_VAL for VITAL_TYPE in (SYSTOLIC, DIASTOLIC)
                )
                union all
                -- SBP from OBS_CLIN table
                select   distinct 
                         r.PATID
                        ,r.STUDY_ID
                        ,os.OBSCLIN_START_DATE
                        ,datediff(day,r.enroll_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
                        ,'SYSTOLIC' as VITAL_TYPE
                        ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
                    from `+ TRIAL_REF +` r
                    join `+ site_cdm +`.OBS_CLIN os on r.patid = os.patid
                    where
                        -- os.OBSCLIN_TYPE = 'LC' and 
                        os.OBSCLIN_CODE in ( '8460-8' --standing
                                            ,'8459-0' --sitting
                                            ,'8461-6' --supine
                                            ,'8479-8' --palpation
                                            ,'8480-6' --general
                                            )
                union all
                -- DBP from OBS_CLIN table
                select   distinct 
                         r.PATID
                        ,r.STUDY_ID
                        ,os.OBSCLIN_START_DATE
                        ,datediff(day,r.enroll_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
                        ,'DIASTOLIC' as VITAL_TYPE
                        ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
                    from `+ TRIAL_REF +` r
                    join `+ site_cdm +`.OBS_CLIN os on r.patid = os.patid
                    where
                        -- os.OBSCLIN_TYPE = 'LC' and 
                        os.OBSCLIN_CODE in ( '8454-1' --standing
                                            ,'8453-3' --sitting
                                            ,'8455-8' --supine
                                            ,'8462-4' --general
                                            )
            )
            select * from multi_cte
            pivot 
            (  
                max(VITAL_VAL) for VITAL_TYPE in ('SYSTOLIC','DIASTOLIC')
            )
            as p(PATID, STUDY_ID, MEASURE_DATE, DAYS_SINCE_INDEX, SBP, DBP)
            ;`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

create or replace table VCCC_CLINIC_BP_LONG (
    PATID varchar(50) NOT NULL,
    STUDY_ID varchar(50) NOT NULL,
    MEASURE_DATE date,     
    DAYS_SINCE_INDEX number,
    SBP integer,
    DBP integer
);

/*test*/
-- call get_clinic_bp_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_CLINIC_BP_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_clinic_bp_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_CLINIC_BP_LONG',
    FALSE, NULL
);

select * from VCCC_CLINIC_BP_LONG limit 5;

select substr(study_id,1,2), count(distinct patid) 
from VCCC_CLINIC_BP_LONG
where days_since_index < 0
group by substr(study_id,1,2)
;
-- 989
-- UT	266
-- KU	723 -> 734

select substr(study_id,1,2),count(distinct patid), 
count(distinct study_id), count(*)
from VCCC_BASE_BP_TCOG_SDH
group by substr(study_id,1,2);


/* healthcare visits */
create or replace procedure get_visits_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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
    // parameter
    var site = SITES[i].toString();
    var site_cdm = `GROUSE_DB_DEV.PCORNET_CDM_`+ site +``;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
           select distinct
                 r.PATID
                ,r.STUDY_ID
                ,enc.ENC_TYPE
                ,enc.ADMIT_DATE
                ,enc.DISCHARGE_DATE
                ,enc.DISCHARGE_STATUS
                ,enc.DISCHARGE_DISPOSITION
                ,enc.DRG
                ,enc.FACILITYID
                ,enc.FACILITY_LOCATION
                ,enc.FACILITY_TYPE
                ,enc.PAYER_TYPE_PRIMARY
                ,enc.RAW_PAYER_TYPE_PRIMARY
                ,enc.RAW_PAYER_ID_PRIMARY   
                ,enc.PROVIDERID
                ,prov.PROVIDER_SPECIALTY_PRIMARY
                ,prov.PROVIDER_NPI
                ,prov.RAW_PROVIDER_SPECIALTY_PRIMARY
                -- ,prov.RAW_PROV_NAME
                -- ,prov.RAW_PROV_TYPE            
                ,datediff(day,r.enroll_date,enc.admit_date) as DAYS_SINCE_INDEX
            from `+ TRIAL_REF +` r 
            join `+ site_cdm +`.ENCOUNTER enc on r.patid = enc.patid
            left join `+ site_cdm +`.PROVIDER prov on enc.providerid = prov.providerid
            ;`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

create or replace table VCCC_VISITS_LONG (
     PATID varchar(50) NOT NULL
    ,STUDY_ID varchar(50) NOT NULL
    ,ENC_TYPE varchar(5)
    ,ADMIT_DATE date
    ,DISCHARGE_DATE date
    ,DISCHARGE_STATUS varchar(50)
    ,DISCHARGE_DISPOSITION varchar(50)
    ,DRG varchar(50)
    ,FACILITYID varchar(50)
    ,FACILITY_LOCATION varchar(50)
    ,FACILITY_TYPE varchar(50)
    ,PAYER_TYPE_PRIMARY varchar(50)
    ,RAW_PAYER_TYPE_PRIMARY varchar(50)
    ,RAW_PAYER_ID_PRIMARY varchar(50)  
    ,PROVIDERID varchar(50)
    ,PROVIDER_SPECIALTY_PRIMARY varchar(50)
    ,PROVIDER_NPI varchar(50)
    ,RAW_PROVIDER_SPECIALTY_PRIMARY varchar(50)
    -- ,RAW_PROV_NAME varchar(50)
    -- ,RAW_PROV_TYPE varchar(50)           
    ,DAYS_SINCE_INDEX number
);

/*test*/
-- call get_visits_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_VISITS_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_visits_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_VISITS_LONG',
    FALSE, NULL
);

select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PROVIDER
where PROVIDER_SPECIALTY_PRIMARY is not null and trim(PROVIDER_SPECIALTY_PRIMARY) <> '';

select substr(STUDY_ID,1,2), 
       count(distinct admit_date) as enc_cnt,
       count(distinct STUDY_ID) as pat_cnt
from VCCC_VISITS_LONG
where enc_type in ('ED','EI') and DAYS_SINCE_INDEX between -730 and -1
group by substr(STUDY_ID,1,2)
;
-- UT	95	60
-- KU	179	124

select substr(STUDY_ID,1,2), 
       count(distinct admit_date) as enc_cnt,
       count(distinct STUDY_ID) as pat_cnt
from VCCC_VISITS_LONG
where enc_type in ('IP','EI') and DAYS_SINCE_INDEX between -730 and -1
group by substr(STUDY_ID,1,2)
;
-- UT	68	37
-- KU	255	142

create or replace procedure get_anthro_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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
    // parameter
    var site = SITES[i].toString();
    var site_cdm = `GROUSE_DB_DEV.PCORNET_CDM_`+ site +``;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +` 
          -- height (m)--
          SELECT r.patid,
                 r.study_id,
                 b.measure_date::date,
                 round(datediff(day,r.enroll_date,b.measure_date::date)/365.25),
                 'HT',b.ht/39.37 -- default at 'in'
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.ht is not null
          UNION
          select r.patid,
                 r.study_id,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.enroll_date,oc.OBSCLIN_START_DATE::date)/365.25),'HT',
                 case when lower(oc.OBSCLIN_RESULT_UNIT) like '%cm%' then oc.OBSCLIN_RESULT_NUM/100
                      else oc.OBSCLIN_RESULT_NUM/39.37 end
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.OBS_CLIN oc 
          ON r.patid = oc.patid AND
             oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '8302-2'
          UNION
          -- weight (kg)--
          SELECT r.patid,
                 r.study_id,
                 b.measure_date::date,
                 round(datediff(day,r.enroll_date,b.measure_date::date)/365.25),
                 'WT',b.wt/2.205 -- default at 'lb'
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.wt is not null
          UNION
          select r.patid,
                 r.study_id,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.enroll_date,oc.OBSCLIN_START_DATE::date)/365.25),'WT',
                 case when lower(oc.OBSCLIN_RESULT_UNIT) like 'g%' then oc.OBSCLIN_RESULT_NUM/1000
                      when lower(oc.OBSCLIN_RESULT_UNIT) like '%kg%' then oc.OBSCLIN_RESULT_NUM
                      else oc.OBSCLIN_RESULT_NUM/2.205 end
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.OBS_CLIN oc 
          ON r.patid = oc.patid AND
             oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '29463-7'
          UNION
          -- bmi (kg/m2)--
          SELECT r.patid,
                 r.study_id,
                 b.measure_date::date,
                 round(datediff(day,r.enroll_date,b.measure_date::date)/365.25),
                 'BMI',b.ORIGINAL_BMI
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.ORIGINAL_BMI is not null
          UNION
          select r.patid,
                 r.study_id,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.enroll_date,oc.OBSCLIN_START_DATE::date)/365.25),
                 'BMI',oc.OBSCLIN_RESULT_NUM 
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.OBS_CLIN oc 
          ON r.patid = oc.patid AND
             oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '39156-5'
          ;
    `;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

create or replace table VCCC_ANTHRO_LONG (
    PATID varchar(50) NOT NULL,
    STUDY_ID varchar(50) NOT NULL,
    MEASURE_DATE date,      -- date of first HT/WT/BMI record
    DAYS_SINCE_INDEX integer,
    MEASURE_TYPE varchar(4),
    MEASURE_NUM double -- ht:m; wt:kg
);

/*test*/
-- call get_anthro_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_ANTHRO_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;

call get_anthro_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_ANTHRO_LONG',
    FALSE, NULL
);

create or replace table VCCC_ANTHRO_TS as
with daily_agg as(
    select patid,study_id,measure_date,HT,WT,days_since_index,
           case when BMI>100 then NULL else BMI end as BMI,
           case when HT = 0 or WT = 0 or round(WT/(HT*HT))>100 then NULL
                else round(WT/(HT*HT)) 
           end as BMI_CALCULATED
    from (
        select patid,
               study_id,
               measure_type, 
               measure_date::date as measure_date, 
               days_since_index, 
               median(measure_num) as measure_num
    from VCCC_ANTHRO_LONG
    group by patid, study_id, measure_type, measure_date::date,days_since_index
    ) 
    pivot(
        median(measure_num) 
        for measure_type in ('HT','WT','BMI')
    ) as p(patid,study_id,measure_date,days_since_index,HT,WT,BMI)
    where (WT is not null and HT is not null and WT>0 and HT>0) or
          (BMI is not null and BMI > 0)
)
select patid,
       study_id,
       measure_date,
       days_since_index,
       round(ht,2) as ht,
       round(wt,2) as wt,
       NVL(bmi_calculated,bmi) as bmi,
       dense_rank() over (partition by patid order by measure_date) as t_discrete,
       row_number() over (partition by patid order by measure_date) as rn
from daily_agg
where NVL(BMI,BMI_CALCULATED) is not null and NVL(BMI,BMI_CALCULATED)>0
;

select * from VCCC_ANTHRO_TS limit 5;

select substr(study_id,1,2), count(distinct study_id)
from VCCC_ANTHRO_TS
where days_since_index = 0
group by substr(study_id,1,2)
;
-- UT	259
-- KU	695

select substr(study_id,1,2), count(distinct study_id)
from VCCC_ANTHRO_TS
where days_since_index between -14 and 0
group by substr(study_id,1,2)
;
-- UT	266
-- KU	723 -> 734
-- study_id sent to KUMC

-- labs
create or replace procedure get_labs_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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
    // parameter
    var site = SITES[i].toString();
    var site_cdm = `GROUSE_DB_DEV.PCORNET_CDM_`+ site +``;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
           select distinct
                 a.PATID
                ,a.STUDY_ID
                ,coalesce(b.specimen_date, b.lab_order_date, b.result_date) as OBS_DATE
                ,datediff(day,a.enroll_date,coalesce(b.specimen_date, b.lab_order_date, b.result_date)) as DAYS_SINCE_INDEX
                ,b.lab_loinc as OBS_CODE
                ,coalesce(c.component,b.raw_lab_name) as OBS_NAME
                ,b.result_num as OBS_NUM
                ,b.result_unit as OBS_UNIT
                ,b.norm_range_low as OBS_REF_LOW
                ,b.norm_range_high as OBS_REF_HIGH
                ,b.result_qual as OBS_QUAL
              from `+ TRIAL_REF +` a
              join `+ site_cdm +`.LAB_RESULT_CM b
              on a.patid = b.patid
              left join ONTOLOGY.LOINC.LOINC_V2_17 c
              on b.lab_loinc = c.loinc_num
            ;`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

create or replace table VCCC_LABS_LONG (
     PATID varchar(50) NOT NULL
    ,STUDY_ID varchar(50) NOT NULL
    ,OBS_DATE date
    ,DAYS_SINCE_INDEX number
    ,OBS_CODE varchar(100)
    ,OBS_NAME varchar(500)
    ,OBS_NUM number 
    ,OBS_UNIT varchar(50)
    ,OBS_REF_LOW varchar(100)
    ,OBS_REF_HIGH varchar(100) 
    ,OBS_QUAL varchar(100)
);

/*test*/
-- call get_labs_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_LABS_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_labs_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_LABS_LONG',
    FALSE, NULL
);


select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%creat%' and days_since_index between -731 and 0
group by substr(study_id,1,2)
;
-- UT	262
-- KU	706

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%urea nitrogen%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	260
-- KU	706

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%potassium%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	261
-- KU	706

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%sodium%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	260
-- KU	706

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%albumin%' and days_since_index <= 0
group by substr(study_id,1,2)
; -- urine
-- UT	260
-- KU	694

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%albumin%creat%' and days_since_index between -731 and 0
group by substr(study_id,1,2)
;
-- UT	49
-- KU	75
-- urine

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%protein%creat%' and days_since_index between -731 and 0
group by substr(study_id,1,2)
;
-- UT	8
-- KU	44
-- urine

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%calcium%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	260
-- KU	706

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like 'protein' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	258
-- KU	693
select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%aspartate%aminotransferase%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	255
-- KU	693
select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%alkaline%phosphatase%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	255
-- KU	694

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%hemoglobin%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	260
-- KU	703

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%mean%corpuscular%volume%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	252
-- KU	695

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%hemoglobin%a1c%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	248
-- KU	496

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%cholesterol%' and days_since_index <= 0 and 
      lower(obs_name) not like '%hdl%' and lower(obs_name) not like '%ldl%'
group by substr(study_id,1,2)
;
-- UT	254
-- KU	685

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%hdl%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	253
-- KU	685

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%ldl%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	251
-- KU	685

select substr(study_id,1,2), count(distinct study_id)
from VCCC_LABS_LONG
where lower(obs_name) like '%triglyceride%' and days_since_index <= 0
group by substr(study_id,1,2)
;
-- UT	253
-- KU	685


create or replace procedure get_sdoh_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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
    // parameter
    var site = SITES[i].toString();
    var site_cdm = `GROUSE_DB_DEV.PCORNET_CDM_`+ site +``;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
            -- SBP and DBP from VITAL table
            with multi_cte as (
                select * from
                (
                    select   distinct
                             r.PATID
                            ,r.STUDY_ID
                            ,v.SMOKING
                            ,v.MEASURE_DATE
                            ,datediff(day,r.enroll_date,v.measure_date) as DAYS_SINCE_INDEX
                        from `+ TRIAL_REF +` r 
                        join `+ site_cdm +`.VITAL v on r.patid = v.patid
                        where v.SYSTOLIC is not null
                )
                unpivot (
                VITAL_VAL for VITAL_TYPE in (SYSTOLIC, DIASTOLIC)
                )
                union all
                -- SBP from OBS_CLIN table
                select   distinct 
                         r.PATID
                        ,r.STUDY_ID
                        ,os.OBSCLIN_START_DATE
                        ,datediff(day,r.enroll_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
                        ,'SYSTOLIC' as VITAL_TYPE
                        ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
                    from `+ TRIAL_REF +` r
                    join `+ site_cdm +`.OBS_CLIN os on r.patid = os.patid
                    where
                        -- os.OBSCLIN_TYPE = 'LC' and 
                        os.OBSCLIN_CODE in ( '8460-8' --standing
                                            ,'8459-0' --sitting
                                            ,'8461-6' --supine
                                            ,'8479-8' --palpation
                                            ,'8480-6' --general
                                            )
                union all
                -- DBP from OBS_CLIN table
                select   distinct 
                         r.PATID
                        ,r.STUDY_ID
                        ,os.OBSCLIN_START_DATE
                        ,datediff(day,r.enroll_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
                        ,'DIASTOLIC' as VITAL_TYPE
                        ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
                    from `+ TRIAL_REF +` r
                    join `+ site_cdm +`.OBS_CLIN os on r.patid = os.patid
                    where
                        -- os.OBSCLIN_TYPE = 'LC' and 
                        os.OBSCLIN_CODE in ( '8454-1' --standing
                                            ,'8453-3' --sitting
                                            ,'8455-8' --supine
                                            ,'8462-4' --general
                                            )
            )
            select * from multi_cte
            pivot 
            (  
                max(VITAL_VAL) for VITAL_TYPE in ('SYSTOLIC','DIASTOLIC')
            )
            as p(PATID, STUDY_ID, MEASURE_DATE, DAYS_SINCE_INDEX, SBP, DBP)
            ;`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

create or replace table VCCC_CLINIC_BP_LONG (
    PATID varchar(50) NOT NULL,
    STUDY_ID varchar(50) NOT NULL,
    MEASURE_DATE date,     
    DAYS_SINCE_INDEX number,
    SBP integer,
    DBP integer
);

/*test*/
-- call get_clinic_bp_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_CLINIC_BP_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_clinic_bp_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_CLINIC_BP_LONG',
    FALSE, NULL
);

select * from VCCC_CLINIC_BP_LONG limit 5;


create or replace procedure get_vccc_cci_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIAL_REF: name of trial participant id list (1 pat/row, key = PATID)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_LONG_TBL: target long table with clinical BP records 
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

       // collect all diagnosis codes
       sqlstmt = `
              INSERT INTO `+ TGT_LONG_TBL +`
              select  distinct
                      a.PATID
                     ,datediff(day,a.enroll_date,NVL(d.DX_DATE::date,d.ADMIT_DATE::date)) as DAYS_SINCE_INDEX
                     ,d.DX
                     ,d.PDX
                     ,d.DX_DATE
                     ,d.ADMIT_DATE
                     ,d.ENC_TYPE
                     ,cci.code_grp
                     ,cci.score
              from `+ TRIAL_REF +` a
              join GROUSE_DB_DEV.`+ site_cdm +`.DIAGNOSIS d 
                     on a.PATID = d.PATID
              join Z_CCI_REF cci
              on d.dx like cci.code||'%' and d.dx_type = cci.code_type
              where NVL(d.DX_DATE::date,d.ADMIT_DATE::date)<=a.enroll_date
              ;`;

       if (DRY_RUN) {
              // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
              var log_stmt = snowflake.createStatement({
                            sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                            binds: [sqlstmt]});
        log_stmt.execute(); 
       } else {
              // run dynamic dml query
              var run_sqlstmt = snowflake.createStatement({sqlText: sqlstmt}); run_sqlstmt.execute();
              var commit_txn = snowflake.createStatement({sqlText: `commit;`}); commit_txn.execute();
       }
}
$$
;

create or replace table CCI_DX_LONG (
    PATID varchar(50) NOT NULL,
    DAYS_SINCE_INDEX number,
    DX varchar(20),
    PDX varchar(20),
    DX_DATE date,
    ADMIT_DATE date,
    ENC_TYPE varchar(20),
    CCI_GRP varchar(11),
    CCI_SCORE integer
);
/*test*/
-- call get_vccc_cci_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'CCI_DX_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_vccc_cci_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'CCI_DX_LONG',
    FALSE, NULL
);

select * from CCI_DX_LONG limit 5;

select count(distinct patid) from CCI_DX_LONG;
-- 781

create or replace table VCCC_BASE_CCI as
with cci_uni as (
    select patid, CCI_GRP, cci_score
    from (
        select a.*, row_number() over (partition by a.patid, a.CCI_GRP order by a.days_since_index desc) rn
        from CCI_DX_LONG a
    )
    where rn = 1
)
, cci_tot as (
    select patid, sum(cci_score) as cci_total
    from cci_uni
    group by patid
)
, cci_ind as (
    select * from (
        select patid, CCI_GRP, 1 as ind from cci_uni
    )
    pivot (
        max(ind) for CCI_GRP in (
            'aids',
            'canc',
            'cevd',
            'chf',
            'cpd',
            'dementia',
            'diab',
            'diabwc',
            'hp',
            'metacanc',
            'mi',
            'mld',
            'msld',
            'pud',
            'pvd',
            'rend',
            'rheumd'
        )
        DEFAULT ON NULL (0)
    )
    as p(patid,aids,canc,cevd,chf,cpd,dementia,diab,diabwc,hp,metacanc,mi,mld,msld,pud,pvd,rend,rheumd)
)
select b.*,
       a.cci_total
from cci_tot a
join cci_ind b
on a.patid = b.patid
; 

select * from VCCC_BASE_CCI limit 5;

select count(distinct patid), count(*) from VCCC_BASE_CCI;