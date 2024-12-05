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
3. Z_MED_RXCUI_REF
4. PAT_TABLE1 (denom_cdm.sql)
=============================
*/ 

select * from ELIG_BP_REDCAP_20241120 limit 5;
select * from BASE_BP_REDCAP_20241120 limit 5;
select * from Z_MED_RXCUI_REF limit 5;
select * from PAT_TABLE1 limit 5;

select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PCORNET_TRIAL limit 5;
select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PRESCRIBING limit 5;
select * from GROUSE_DB_DEV.PCORNET_CDM_KUMC.ENCOUNTER where trim(drg) <> '' limit 5;

create or replace table VCCC_UNMAPPED as 
with trial_stk as (
    select participantid, patid, 'KUMC' as site
    from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PCORNET_TRIAL where TRIALID = 'vccc'
    union 
    select participantid, patid, 'UU' as site
    from GROUSE_DB_DEV.PCORNET_CDM_UU.PCORNET_TRIAL where TRIALID = 'vccc'
)
select a.*, b.patid, b.site 
from ELIG_BP_REDCAP_20241120 a
left join trial_stk b
on a.study_id = b.participantid
where b.patid is null
;

select site, count(distinct study_id) 
from VCCC_UNMAPPED
group by site
;
-- null	82

create or replace table VCCC_BASE_BP as 
with trial_stk as (
    select participantid, patid, trial_enroll_date, trial_end_date, trial_withdraw_date, 'KUMC' as site
    from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PCORNET_TRIAL where TRIALID = 'vccc'
    union 
    select participantid, patid, trial_enroll_date, trial_end_date, trial_withdraw_date, 'UU' as site
    from GROUSE_DB_DEV.PCORNET_CDM_UU.PCORNET_TRIAL where TRIALID = 'vccc'
), base_sbp_only as (
    select study_id, 
           round(result) as base_sbp,
           bp_date as base_sbp_date
    from (
        select a.*, row_number() over (partition by a.study_id, a.measure order by a.bp_date) rn
        from BASE_BP_REDCAP_20241120 a
        where a.measure = 'SBP'
    )
    where rn = 1
)
select a.study_id,
       c.patid, 
       c.trial_enroll_date, 
       c.trial_end_date, 
       c.trial_withdraw_date,
       c.site,
       a.elig_sbp1,
       a.elig_sbp1_date,
       coalesce(a.elig_sbp2,a.elig_sbp3) as elig_sbp2,
       coalesce(a.elig_sbp2_date,a.elig_sbp2_date_2) as elig_sbp2_date,
       b.base_sbp,
       b.base_sbp - a.elig_sbp1 as delta_sbp,
       case when b.base_sbp - a.elig_sbp1 < 0 and b.base_sbp<130 then 'deltasbp1'
            when b.base_sbp - a.elig_sbp1 < 0 and b.base_sbp>=130 and b.base_sbp<140 then 'deltasbp2'
            else 'deltasbp3'
       end as delta_sbp_group,
       datediff('day',b.base_sbp_date,a.elig_sbp1_date) as delta_days,
       case when a.elig_sbp1>=140 and a.elig_sbp1<150 then 'esbp1'
            when a.elig_sbp1>=150 and a.elig_sbp1<160 then 'esbp2'
            when a.elig_sbp1>=160 then 'esbp3'
            else 'esbp0'
       end as esbp_group,
       case when b.base_sbp>=140 and b.base_sbp<150 then 'bsbp1'
            when b.base_sbp>=150 and b.base_sbp<160 then 'bsbp2'
            when b.base_sbp>=160 then 'bsbp3'
            else 'bsbp0'
       end as bsbp_group,
       datediff('day',c.trial_enroll_date,a.elig_sbp1_date) as elig_since_index 
from ELIG_BP_REDCAP_20241120 a 
join base_sbp_only b on a.study_id = b.study_id 
join trial_stk c on a.study_id = c.participantid -- 81 study_id not mapped to patid
where a.elig_sbp1 is not null and trim(a.elig_sbp1)<>'' and 
      b.base_sbp is not null and trim(b.base_sbp)<>''
order by a.study_id
;

select * from VCCC_BASE_BP limit 5;

select count(distinct patid), count(distinct study_id), count(*)
from VCCC_BASE_BP;
-- 899	899

select site, count(distinct patid), count(distinct study_id), count(*)
from VCCC_BASE_BP
group by site
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
        vccc.elig_since_index,
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
        vccc.trial_enroll_date as index_date,
        datediff(day,vccc.trial_enroll_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index        
from GROUSE_DB_DEV.PCORNET_CDM_KUMC.PRESCRIBING a 
join VCCC_BASE_BP vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV.PCORNET_CDM_KUMC.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on try_to_number(a.RXNORM_CUI) = m.RXNORM_CUI 
where vccc.site = 'KUMC'
union all
select  a.patid,
        vccc.study_id,
        vccc.elig_since_index,
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
        vccc.trial_enroll_date as index_date,
        datediff(day,vccc.trial_enroll_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index           
from GROUSE_DB_DEV.PCORNET_CDM_UU.PRESCRIBING a 
join VCCC_BASE_BP vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV.PCORNET_CDM_UU.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on a.RXNORM_CUI = m.RXNORM_CUI 
where vccc.site = 'UU'
;

-- KUMC as missing RXNORM_CUI
--         

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


create or replace procedure get_trial_base_med_long(
    TRIALID string,
    TRIAL_REF string,
    SITES array,
    TGT_TBL string,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TRIALID: designated trial identifier (e.g. vccc)
 * @param {string} TRIAL_REF: name of trial participant id list
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
 * @param {string} TGT_BASE_TBL: target table name for 
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
    
    // dynamic query
    var sqlstmt_par_dx = `
        INSERT INTO `+ TGT_BASE_TBL +`
        SELECT  a.patid,
                'DX' AS event_type,
                NVL(a.dx_date,a.admit_date) AS event_date,
                round(datediff(day,b.birth_date,NVL(a.dx_date,a.admit_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE a.dx LIKE '335.20%' OR a.dx LIKE 'G12.21%';`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par_dx]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par_dx = snowflake.createStatement({sqlText: sqlstmt_par_dx}); run_sqlstmt_par_dx.execute();
        var run_sqlstmt_par_drx = snowflake.createStatement({sqlText: sqlstmt_par_drx}); run_sqlstmt_par_drx.execute();
        var run_sqlstmt_par_px = snowflake.createStatement({sqlText: sqlstmt_par_px}); run_sqlstmt_par_px.execute();
        
        if(site === 'CMS'){
            var run_sqlstmt_par_enr = snowflake.createStatement({sqlText: sqlstmt_par_enr}); run_sqlstmt_par_enr.execute();
        }else{
            var run_sqlstmt_par_prx = snowflake.createStatement({sqlText: sqlstmt_par_prx}); run_sqlstmt_par_prx.execute();
        }
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

/*test*/
-- call get_als_event_long(
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;

create or replace table BP_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    EVENT_TYPE varchar(20),
    EVENT_DATE date,     
    AGE_AT_EVENT integer,
    EVENT_SRC varchar(10)
);
call get_als_event_long(
    array_construct(
         'KUMC'
        ,'UU'
    ), 
    FALSE, NULL
);
