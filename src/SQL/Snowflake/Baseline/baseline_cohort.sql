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
                            ,v.SYSTOLIC
                            ,v.DIASTOLIC
                            ,v.MEASURE_DATE
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
                        ,os.OBSCLIN_START_DATE
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
                        ,os.OBSCLIN_START_DATE
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
            as p(PATID, MEASURE_DATE, SBP, DBP)
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

create or replace table BP_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    MEASURE_DATE date,     
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
--     'BP_EVENT_LONG',
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
    'BP_EVENT_LONG',
    FALSE, NULL
);

select * from BP_EVENT_LONG limit 5;

select count(distinct patid) from BP_EVENT_LONG;