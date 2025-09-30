/*
===========================    
Key CDM tables from KUMC and UU
1. ENCOUNTER
2. DEMOGRAPHIC
3. VITAL
4. PRESCRIBING
5. PCORNET_TRIAL

Auxillary tables: 
1. trial tables: 
  - BYOD_BL1PT_20250115
  - BYOD_BL2EBP_20241120
  - BYOD_BL3BP_20250123
  - BYOD_BL4TCOG_20250127
2. refernce tables: 
  - Z_MED_RXCUI_REF (ref/med_rxcui_ref.csv)
3. CDM tables: 
  - PAT_TABLE1 (denom_pat_cdm.sql)
  - Z_CCI_REF (ref/cci_icd_ref.csv)
4. geocoding tables: 
   - VCCC_PID_ADDRID
   - VCCC_UNIQUE_ADDR_GEOCODED
=============================
*/ 
select * from VCCC_UNIQUE_ADDR_GEOCODED limit 5;
create or replace table ELIG_BP as 
with stk as (
    select study_id, elig_sbp1 as elig_sbp, elig_dbp1 as elig_dbp, elig_sbp1_date as elig_date
    from BYOD_BL2EBP_20241120
    union all 
    select study_id, elig_sbp2 as elig_sbp, elig_dbp2 as elig_dbp, elig_sbp2_date as elig_date
    from BYOD_BL2EBP_20241120
    union all
    select study_id, elig_sbp3 as elig_sbp, elig_dbp3 as elig_dbp, elig_sbp2_date_2 as elig_date
    from BYOD_BL2EBP_20241120
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
           case when redcap_event_name = 'baseline_arm_1' then 1 else 0 end as
           measure,
           result
    from (
        select a.*, row_number() over (partition by a.study_id, a.measure order by a.bp_date) rn
        from BYOD_BL3BP_20250123 a
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
from GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.PCORNET_TRIAL where lower(TRIALID) = 'vccc'
union 
select participantid, patid, trial_enroll_date, trial_end_date, trial_withdraw_date, 'UU' as site
from GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.PCORNET_TRIAL where lower(TRIALID) = 'vccc'
;


create or replace table VCCC_UNMAPPED as 
select a.*, b.patid, b.site 
from BYOD_BL2EBP_20241120 a
left join STUDYID_MAPPING b
on lower(a.study_id) = lower(b.participantid)
where b.patid is null
;

select substr(study_id,1,2), count(distinct study_id) 
from VCCC_UNMAPPED
group by substr(study_id,1,2)
;
-- KU	8

select * from VCCC_UNMAPPED;

create or replace table OC_TCOG as 
with clean_cte as (
    select  study_id,
            tcog_date,
            'T'||lpad(test_id,2,'0') as testid,
            score,
            normed_score,
            rmse as normed_rmse
    from BYOD_BL4TCOG_20250127
)
, score_cte as (
    select * from (
        select study_id, tcog_date, testid, score from clean_cte
    )
    pivot (
        avg(score) for testid in (any order by testid)
    ) as p(study_id,tcog_date,T01,T02,T03,T04,T05,T06,T07,T08,T09,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)
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
with combine as (
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
       try_to_number(a.elig_sbp) as elig_sbp,
       try_to_number(a.elig_dbp) as elig_dbp,
       a.elig_date,
       b.base_hr,
       b.base_dbp,
       b.base_sbp,
       b.base_sbp - try_to_number(a.elig_sbp) as delta_sbp,
       case when b.base_sbp - try_to_number(a.elig_sbp) < 0 and b.base_sbp<130 then 'deltasbp1'
            when b.base_sbp - try_to_number(a.elig_sbp) < 0 and b.base_sbp>=130 and b.base_sbp<140 then 'deltasbp2'
            else 'deltasbp3'
       end as delta_sbp_group,
       case when b.base_sbp - try_to_number(a.elig_sbp) between -10 and 10 then 0 
            when b.base_sbp - try_to_number(a.elig_sbp) < -10 then floor((b.base_sbp - try_to_number(a.elig_sbp))/10)+1
            when b.base_sbp - try_to_number(a.elig_sbp) > 10 then ceil((b.base_sbp - try_to_number(a.elig_sbp))/10)-1
            else 0
       end as delta_sbp_by10,
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
       case when p.race = '1' then 'aian'
            when p.race = '2' then 'asian'
            when p.race = '3' then 'black'
            when p.race = '5' then 'white'
            when p.race = '6' then 'multi'
            when p.race = '0' then 'NI'
            else 'other'
       end as race_str,
       p.ethnicity,
       case when p.ethnicity = 1 then 'hispanic'
            when p.ethnicity = 2 then 'non-hispanic'
            else 'NI'
       end as ethn_str,
       case when p.race = '3' and p.ethnicity <> 1 then 'nh-black'
            when p.race = '5' then 'white'
            when p.ethnicity = 1 then 'hisp'
            else 'other'
       end as race_ethn_str,
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
       end as ruca_primary_nonmetro_ind,
       case when c.patid is null then 1 else 0 end as unmatch_ind, 
       row_number() over (partition by a.study_id order by p.enroll_date) as dedup_rn
from ELIG_BP a 
join BASE_BP b on a.study_id = b.study_id 
join BYOD_BL1PT_20250115 p on a.study_id = p.study_id
join VCCC_PID_ADDRID pa on a.study_id = pa.study_id
join VCCC_UNIQUE_ADDR_GEOCODED g on pa.id = g.id
join OC_TCOG t on t.study_id = a.study_id
join SDOH_DB.AHRQ.AHRQ_CT_2020 sdh on lpad(g.census_tract_id_2020,11,'0') = lpad(sdh.tractfips,11,'0')
left join SDOH_DB.ADI.ADI_BG_2020 adi on lpad(g.census_block_group_id_2020,12,'0') = adi.geocodeid
left join SDOH_DB.RUCA.RUCA_TR_2010 ruca on lpad(g.census_tract_id_2020,11,'0') = ruca.geocodeid
left join STUDYID_MAPPING c on a.study_id = c.participantid
order by a.study_id
)
select combine.* exclude dedup_rn
from combine 
where dedup_rn = 1
;

select distinct ethnicity, ethn_str from VCCC_BASE_BP_TCOG_SDH;


select adi_staterank, count(distinct patid)
from VCCC_BASE_BP_TCOG_SDH
group by adi_staterank
order by count(distinct patid) desc;

select * from VCCC_BASE_BP_TCOG_SDH limit 5;

select count(distinct patid), count(distinct study_id), count(*)
from VCCC_BASE_BP_TCOG_SDH;
-- 1000

select distinct ethn_str from VCCC_BASE_BP_TCOG_SDH;

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
       VA_CLS as CLS, 
       VA_CLS_CD as CLS_CD, 
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
       CLS, 
       CLS_CD,
       ANTIHTN_IND,
       STR as REF_STR,
       UNIT as REF_UNIT
from cte
where rn = 1
;

select * from Z_MED_RXCUI_REF_AH;

create or replace procedure get_meds_long(
    TRIAL_REF string,
    SITES array,
    RX_REF string,
    ADD_RX_GRP string,
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
 * @param {string} RX_REF: name of the rx reference table, at least include columns: RXNORM_CUI, ING, CLS, CLS_CD
 * @param {string} ADD_RX_GRP: column name from RX_REF for additional RX grouping (e.g., indicator of AH)
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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
    var labname_incld = '';
    
    // dynamic query
    var sqlstmt_tmp = `
        CREATE TEMPORARY TABLE RX_STACK as 
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
            from `+ TRIAL_REF +` r 
            join `+ site_cdm +`.PRESCRIBING vccc on a.patid = vccc.patid
            left join GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.ENCOUNTER e on a.patid = e.patid 
            left join Z_MED_RXCUI_REF_AH m on try_to_number(a.RXNORM_CUI) = m.RXNORM_CUI 
            where vccc.site = 'KUMC'
    `
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
           select distinct
                 r.PATID
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
                ,case when enc.PAYER_TYPE_PRIMARY like '1%' then 'medicare'
                      when enc.PAYER_TYPE_PRIMARY like '5%' then 'commercial'
                      when enc.PAYER_TYPE_PRIMARY like '82%' then 'selfpay'
                      when enc.PAYER_TYPE_PRIMARY is NULL or trim(enc.PAYER_TYPE_PRIMARY) = '' or enc.PAYER_TYPE_PRIMARY in ('NI','UN') then 'NI'
                 end as PAYER_TYPE_PRIMARY_GRP
                ,enc.RAW_PAYER_TYPE_PRIMARY
                ,enc.RAW_PAYER_ID_PRIMARY   
                ,enc.PROVIDERID
                ,prov.PROVIDER_SPECIALTY_PRIMARY
                ,prov.PROVIDER_NPI
                ,prov.RAW_PROVIDER_SPECIALTY_PRIMARY
                -- ,prov.RAW_PROV_NAME
                -- ,prov.RAW_PROV_TYPE            
                ,datediff(day,r.index_date,enc.admit_date) as DAYS_SINCE_INDEX
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
from GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.PRESCRIBING a 
join VCCC_BASE_BP_TCOG_SDH vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.ENCOUNTER e on a.patid = e.patid 
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
from GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.PRESCRIBING a 
join VCCC_BASE_BP_TCOG_SDH vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on a.RXNORM_CUI = m.RXNORM_CUI 
where vccc.site = 'Utah'
;

select split_part(raw_rx_med_name,' ', 1) as ingre, count(distinct patid)
from VCCC_MED_LONG_RAW 
where ING is null
group by split_part(raw_rx_med_name,' ', 1)
order by count(distinct patid) desc
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

select * from VCCC_MED_LONG 
where antihtn_ind = 1
limit 5;

select count(distinct patid) from VCCC_MED_LONG;
-- 992


/* 
baseline AHT use features: 
- count: baseline AHT 
- indicator: new AHT during run-in
- indicator: AHT dose increase during run-in
- indicator: AHT maintained during run-in
- indicator: AHT change during run-in 
*/
create or replace table VCCC_BASE_MED as 
with med_1yr as (
    select study_id, 
           count(distinct in_or_name_s) as med_cnt_in,
           count(distinct va_cls_cd) as med_cnt_cls
    from VCCC_MED_LONG
    where antihtn_ind = 1 and rx_start_since_index between -365 and 0
    group by study_id
)
select a.patid, a.study_id,
       coalesce(b.med_cnt_in,0) as med_cnt_in,
       coalesce(b.med_cnt_cls,0) as med_cnt_cls,
       case when b.med_cnt_in between 1 and 5 then 'polyrx_in_grp1'
            when b.med_cnt_in >= 5 then 'polyrx_in_grp2'
            else 'polyrx_in_grp0'
       end as polyrx_in_grp,
from VCCC_BASE_BP_TCOG_SDH a 
left join med_1yr b 
on a.study_id = b.study_id      
;
select * from VCCC_BASE_MED limit 5;

create or replace table VCCC_RUNIN_MED as 
with aht_bef as (
    select distinct 
           patid, study_id, 
           in_or_name, va_cls,
           rx_days, rx_str, rx_dose,
           rx_start_since_index, 
           rx_end_since_index,
           elig_since_index,
           count(distinct ing) over (partition by patid) as in_cnt,
           listagg(distinct ing, '|') within group (order by ing) over (partition by patid) as in_lst
    from VCCC_MED_LONG
    where antihtn_ind = 1 and rx_start_since_index between -730 and elig_since_index-1 and rx_timing = 'bef'
)
, aht_runin as (
    select distinct 
           patid, study_id, 
           in_or_name, va_cls,
           rx_days, rx_str, rx_dose,
           rx_start_since_index, 
           rx_end_since_index,
           elig_since_index,
           count(distinct ing) over (partition by patid) as in_cnt,
           listagg(distinct ing, '|') within group (order by ing) over (partition by patid) as in_lst
    from VCCC_MED_LONG
    where antihtn_ind = 1 and rx_timing = 'runin'
)
, aht_runin_st as (
    select distinct a.patid, a.study_id, 1 as runin_st 
    from aht_runin a 
    where not exists (select 1 from aht_bef b where a.study_id = b.study_id)
)
, aht_maintain as (
    select distinct patid, study_id, runin_mt 
    from (
        select patid, study_id, 1 as runin_mt
        from aht_bef
        where rx_end_since_index >= elig_since_index
        union 
        select a.patid, a.study_id, 1 as runin_mt
        from aht_bef a 
        join aht_runin b 
        on a.study_id = b.study_id and a.in_or_name = b.in_or_name
    )
)
, aht_doseinc as (
    select distinct a.patid, a.study_id, 1 as runin_inc 
    from aht_runin a 
    where exists (
        select 1 from aht_bef b
        where a.study_id = b.study_id and a.in_or_name = b.in_or_name and b.rx_dose > a.rx_dose
    )
)
, aht_runin_add as (
    select distinct a.patid, a.study_id, 1 as runin_add
    from aht_runin a 
    where exists (
        select 1 from aht_bef b 
        where a.study_id = b.study_id and a.in_cnt>b.in_cnt
    )
)
, aht_runin_change as (
    select distinct a.patid, a.study_id, 1 as runin_change
    from aht_runin a 
    where exists (
        select 1 from aht_bef b 
        where a.study_id = b.study_id and a.in_lst <> b.in_lst and b.in_cnt > 0
    )
)
select a.patid, a.study_id, 
       coalesce(st.runin_st,0) as runin_st,
       coalesce(mt.runin_mt,0) as runin_mt,
       coalesce(inc.runin_inc,0) as runin_inc,
       coalesce(add.runin_add,0) as runin_add,
       coalesce(ch.runin_change,0) as runin_change
from VCCC_BASE_BP_TCOG_SDH a 
left join aht_runin_st st on a.study_id = st.study_id
left join aht_maintain mt on a.study_id = mt.study_id
left join aht_doseinc inc on a.study_id = inc.study_id
left join aht_runin_add add on a.study_id = add.study_id 
left join aht_runin_change ch on a.study_id = ch.study_id
;

-- clinical BP
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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
    
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

with per_pat_cnt as (
    select patid, study_id, count(distinct days_since_index) as repeat_bp
    from VCCC_CLINIC_BP_LONG 
    where days_since_index between -365 and 0
    group by patid, study_id
)
select substr(study_id,1,2), count(distinct patid) 
from per_pat_cnt
where repeat_bp > 1
group by substr(study_id,1,2)
;

-- calculate natural 
create or replace table NAT_SBP_VARI as
with sbp_m as (
    select patid, study_id, avg(SBP) as SBP_m, avg(DBP) as DBP_m 
    from VCCC_CLINIC_BP_LONG
    group by patid, study_id
    wher days_since_index between -365 and 0
),  sbp_sd as (
    select patid, study_id, stddev(SBP) as SBP_m, stddev(DBP) as DBP_m 
    from VCCC_CLINIC_BP_LONG
    group by patid, study_id
    wher days_since_index between -365 and 0
),  sbp_cv as (

),  sbp_arv as (

)
;


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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
    var labname_incld = '';
    
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
                ,case when enc.PAYER_TYPE_PRIMARY like '1%' then 'medicare'
                      when enc.PAYER_TYPE_PRIMARY like '5%' then 'commercial'
                      when enc.PAYER_TYPE_PRIMARY like '82%' then 'selfpay'
                      when enc.PAYER_TYPE_PRIMARY is NULL or trim(enc.PAYER_TYPE_PRIMARY) = '' or enc.PAYER_TYPE_PRIMARY in ('NI','UN') then 'NI'
                      else 'other'
                 end as PAYER_TYPE_PRIMARY_GRP
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
    ,PAYER_TYPE_PRIMARY_GRP varchar(50)
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

select * from VCCC_VISITS_LONG limit 5;

select PAYER_TYPE_PRIMARY_GRP, count(distinct patid) from VCCC_VISITS_LONG 
group by PAYER_TYPE_PRIMARY_GRP;

create or replace table VCCC_VISITS_BASE as 
with av as (
    select study_id, count(distinct admit_date) as vis_cnt 
    from VCCC_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE = 'AV'
    group by study_id
), th as (
    select study_id, count(distinct admit_date) as vis_cnt 
    from VCCC_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE = 'TH'
    group by study_id
), ed as (
    select study_id, count(distinct admit_date) as vis_cnt 
    from VCCC_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE in ('ED','EI')
    group by study_id
), ip as (
    select study_id, count(distinct admit_date) as vis_cnt 
    from VCCC_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE in ('IP','EI')
    group by study_id
), payer as (
    select study_id, payer_type_primary_grp from 
    (
        select a.*, row_number() over (partition by a.study_id order by abs(a.days_since_index)) rn
        from VCCC_VISITS_LONG a
        where a.enc_type = 'AV' and payer_type_primary_grp <> 'NI'
    )
    where rn = 1
)
select a.patid, a.study_id,
       coalesce(av.vis_cnt, 0) as av_cnt,
       coalesce(th.vis_cnt, 0) as th_cnt,
       coalesce(ed.vis_cnt, 0) as ed_cnt,
       coalesce(ip.vis_cnt, 0) as ip_cnt,
       coalesce(payer.payer_type_primary_grp,'NI') as payer_type_primary_grp
from VCCC_BASE_BP_TCOG_SDH a 
left join av on a.study_id = av.study_id 
left join th on a.study_id = th.study_id 
left join ed on a.study_id = ed.study_id 
left join ip on a.study_id = ip.study_id 
left join payer on a.study_id = payer.study_id
;

select PAYER_TYPE_PRIMARY_GRP, count(distinct patid) from VCCC_VISITS_BASE 
group by PAYER_TYPE_PRIMARY_GRP;

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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
    
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

create or replace table VCCC_ANTRO_BASE_SEL as 
with lastest as (
    select * from VCCC_ANTHRO_TS
    where rn = 1 
)
select a.patid, a.study_id, 
       b.ht, b.wt, b.bmi
from VCCC_BASE_BP_TCOG_SDH a 
left join lastest b 
on a.study_id = b.study_id 
;

select count(distinct patid), count(distinct study_id), count(*)
from VCCC_ANTRO_BASE_SEL
;


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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
    var labname_incld = '';

    // optinal lab_name column
    if (['KUMC','MCW','UTSW'].includes(site)) {
        labname_incld += `,NULLIF(trim(lower(b.lab_name)), '')`;
    } 
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
           select distinct
                 a.PATID
                ,a.STUDY_ID
                ,coalesce(b.specimen_date, b.lab_order_date, b.result_date) as OBS_DATE
                ,datediff(day,a.enroll_date,coalesce(b.specimen_date, b.lab_order_date, b.result_date)) as DAYS_SINCE_INDEX
                ,b.lab_loinc as OBS_CODE
                ,coalesce(NULLIF(trim(lower(b.raw_lab_name)),'')`+ labname_incld +`,lower(c.component)) as OBS_NAME
                ,coalesce(NULLIF(lower(b.specimen_source),''),lower(c.system)) as OBS_SRC
                ,b.result_num as OBS_NUM
                ,b.result_unit as OBS_UNIT
                ,b.norm_range_low as OBS_REF_LOW
                ,b.norm_range_high as OBS_REF_HIGH
                ,b.result_qual as OBS_QUAL
                ,b.lab_px
                ,b.lab_px_type
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
    ,OBS_SRC varchar(100)
    ,OBS_NUM number 
    ,OBS_UNIT varchar(50)
    ,OBS_REF_LOW varchar(100)
    ,OBS_REF_HIGH varchar(100) 
    ,OBS_QUAL varchar(100)
    ,OBS_PX varchar(20)
    ,OBS_PXTY varchar(10)
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

select * from  VCCC_LABS_LONG limit 5;

select distinct obs_name, obs_code, obs_src
from VCCC_LABS_LONG
where lower(obs_name) like '%acid%' and obs_src like '%urine%'
;

create or replace table VCCC_LAB_BASE_SEL as 
with cr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where a.obs_code in ('2160-0')
        --   and a.obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and a.obs_num is not null
), bun as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('3094-0')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), egfr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('98979-8','48642-3','77147-7','48643-1','33914-3','88294-4','88293-6')
        --   and obs_unit = 'mL/min'
          and a.days_since_index between -731 and 0 and obs_num is not null
), egfr2 as (
    select distinct a.patid, a.study_id, coalesce(egfr.days_since_index,cr.days_since_index) as days_since_index,
           case when a.sex = 1 then coalesce(egfr.obs_num,round(142*power(least(cr.obs_num/0.9,1),-0.302)*power(greatest(cr.obs_num/0.9,1),-1.2)*power(0.9938,a.age)*1))
                else coalesce(egfr.obs_num,round(142*power(least(cr.obs_num/0.7,1),-0.241)*power(greatest(cr.obs_num/0.7,1),-1.2)*power(0.9938,a.age)*1.012))
           end as lab_egfr2,
           row_number() over (partition by a.study_id order by coalesce(egfr.days_since_index,cr.days_since_index) desc) as rn,
           count(distinct coalesce(egfr.days_since_index,cr.days_since_index)) over (partition by a.study_id) as egfr_cnt
    from VCCC_BASE_BP_TCOG_SDH a 
    left join cr on a.study_id = cr.study_id
    left join egfr on a.study_id = egfr.study_id
    where coalesce(egfr.days_since_index,cr.days_since_index) is not null
), pot as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2823-3','6298-4')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), sod as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2951-2','2947-0')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralbcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('9318-7','14959-1','14958-3')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralb as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('14957-5','1754-1','13992-3','29946-1','30003-8','13992-3')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2161-8','20624-3','14683-7','2162-6')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralbcr2 as (
    select a.patid, a.study_id, coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) as days_since_index,
           coalesce(uralbcr.obs_num, round(uralb.obs_num/urcr.obs_num*1000)) as lab_uralbcr2,
           row_number() over (partition by a.study_id order by coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) desc) as rn,
           count(distinct coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index))) over (partition by a.study_id) as uralbcr_cnt
    from VCCC_BASE_BP_TCOG_SDH a 
    left join uralbcr on a.study_id = uralbcr.study_id
    left join uralb on a.study_id = uralb.study_id
    left join urcr on a.study_id = urcr.study_id
    where coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) is not null
), urprotcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('34366-5','13801-6')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urprot as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2889-4','57735-3','2888-6','20454-5')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urprotcr2 as (
    select a.patid, a.study_id, coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) as days_since_index,
           coalesce(urprotcr.obs_num, round(urprot.obs_num/urcr.obs_num*1000)) as lab_urprotcr2,
           row_number() over (partition by a.study_id order by coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) desc) as rn,
           count(distinct coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index))) over (partition by a.study_id) as urprotcr_cnt
    from VCCC_BASE_BP_TCOG_SDH a 
    left join urprotcr on a.study_id = urprotcr.study_id
    left join urprot on a.study_id = urprot.study_id
    left join urcr on a.study_id = urcr.study_id
    where coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) is not null
), cal as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('17861-6')
          and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), alb as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('13980-8','2862-1','61152-5','1751-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), chol as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2093-3')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), ldl as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2089-1','13457-7','2091-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hdl as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2085-9')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), trig as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('2571-8')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), ast as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('1920-8','30239-8')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), alp as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('6768-6')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hem as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('20509-6','718-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hba1c as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('17856-6','4548-4')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), mcv as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('787-2','30428-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uracid as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_LABS_LONG a
    where obs_code in ('3086-6')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
)
select a.patid, a.study_id, 
       cr.obs_num as lab_cr,
       bun.obs_num as lab_bun,
       egfr.obs_num as lab_egfr,
       egfr2.lab_egfr2, 
       coalesce(egfr2.egfr_cnt,0) as egfr_cnt,
       case when egfr2.egfr_cnt > 1 then 1 else 0 end as egfr_multi_ind,
       pot.obs_num as lab_pot,
       sod.obs_num as lab_sod,
       uralb.obs_num as lab_uralb,
       urcr.obs_num as lab_urcr,
       uralbcr.obs_num as lab_uralbcr,
       uralbcr2.lab_uralbcr2, 
       coalesce(uralbcr2.uralbcr_cnt,0) as uralbcr_cnt,
       urprot.obs_num as lab_urprot,
       urprotcr.obs_num as lab_urprotcr,
       urprotcr2.lab_urprotcr2, 
       coalesce(urprotcr2.urprotcr_cnt,0) as urprotcr_cnt,
       case when coalesce(uralbcr2.uralbcr_cnt,0) + coalesce(urprotcr2.urprotcr_cnt,0) > 1 then 1 else 0 end as acrpcr_multi_ind,
       cal.obs_num as lab_cal,
       alb.obs_num as lab_alb,
       chol.obs_num as lab_chol,
       ldl.obs_num as lab_ldl,
       hdl.obs_num as lab_hdl,
       trig.obs_num as lab_trig,
       ast.obs_num as lab_ast,
       alp.obs_num as lab_alp,
       hem.obs_num as lab_hem,
       hba1c.obs_num as lab_hba1c,
       mcv.obs_num as lab_mcv,
       uracid.obs_num as lab_uracid,
       case when coalesce(egfr2.lab_egfr2,uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is null then 1 else 0 end as ckd_ts_no,
       case when coalesce(egfr2.lab_egfr2,uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is not null then 1 else 0 end as ckd_ts_either,
       case when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is not null and egfr2.lab_egfr2 is not null then 1 else 0 end as ckd_ts_both,
       case when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is null and egfr2.lab_egfr2 is not null then 1 else 0 end as ckd_ts_egfr,
       case when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is not null and egfr2.lab_egfr2 is null then 1 else 0 end as ckd_ts_acrpcr,
       case when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is not null and egfr2.lab_egfr2 is not null then 'ckd_ts_both'
            when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is null and egfr2.lab_egfr2 is not null then 'ckd_ts_efgr'
            when coalesce(uralbcr2.lab_uralbcr2,urprotcr2.lab_urprotcr2) is not null and egfr2.lab_egfr2 is null then 'ckd_acrpcr'
            else 'ckd_ts_no'
       end as ckd_ts_grp
from VCCC_BASE_BP_TCOG_SDH a 
left join cr on a.study_id = cr.study_id and cr.rn = 1
left join bun on a.study_id = bun.study_id and bun.rn = 1
left join egfr on a.study_id = egfr.study_id and egfr.rn = 1
left join egfr2 on a.study_id = egfr2.study_id and egfr2.rn = 1
left join pot on a.study_id = pot.study_id and pot.rn = 1
left join sod on a.study_id = sod.study_id and sod.rn = 1
left join uralbcr on a.study_id = uralbcr.study_id and uralbcr.rn = 1
left join uralb on a.study_id = uralb.study_id and uralb.rn = 1
left join urcr on a.study_id = urcr.study_id and urcr.rn = 1
left join uralbcr2 on a.study_id = uralbcr2.study_id and uralbcr2.rn = 1
left join urprotcr on a.study_id = urprotcr.study_id and urprotcr.rn = 1
left join urprot on a.study_id = urprot.study_id and urprot.rn = 1
left join urprotcr2 on a.study_id = urprotcr2.study_id and urprotcr2.rn = 1
left join cal on a.study_id = cal.study_id and cal.rn = 1
left join alb on a.study_id = alb.study_id and alb.rn = 1
left join chol on a.study_id = chol.study_id and chol.rn = 1
left join ldl on a.study_id = ldl.study_id and ldl.rn = 1
left join hdl on a.study_id = hdl.study_id and hdl.rn = 1
left join trig on a.study_id = trig.study_id and trig.rn = 1
left join ast on a.study_id = ast.study_id and ast.rn = 1
left join alp on a.study_id = alp.study_id and alp.rn = 1
left join hem on a.study_id = hem.study_id and hem.rn = 1
left join hba1c on a.study_id = hba1c.study_id and hba1c.rn = 1
left join mcv on a.study_id = mcv.study_id and mcv.rn = 1
left join uracid on a.study_id = uracid.study_id and uracid.rn = 1
;

select * from VCCC_LAB_BASE_SEL
where lab_egfr is null and lab_egfr2 is not null;

select * from VCCC_LAB_BASE_SEL
where lab_egfr2 is not null
order by lab_egfr2 desc
;

create or replace table SDOH_KUMC_FEAS as 
select a.patid, 
       b.obsgen_start_date,
       b.obsgen_result_text,
       ref.SDOH_DOMAIN, 
       ref.SDOH_VAR, 
       ref.SDOH_TEXT, 
       ref.LOINC,
       case when b.obsgen_result_text in ('LA33-6','LA28397-0','LA6729-3') then 1 else 0 end as SDOH_POS_IND
from VCCC_BASE_BP_TCOG_SDH a
join GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.OBS_GEN b on a.patid = b.patid
join Z_SDOH_REF ref on b.OBSGEN_CODE = ref.LOINC 
;
select count(distinct patid) from SDOH_KUMC_FEAS; -- 726
with qcnt as (
    select SDOH_TEXT, count(distinct patid) as pat_cnt, sum(SDOH_POS_IND) as pos_cnt
    from SDOH_KUMC_FEAS
    group by SDOH_TEXT
), acnt as (
    select SDOH_TEXT, count(distinct patid) as pat_cnt
    from SDOH_KUMC_FEAS
    where SDOH_POS_IND = 1
    group by SDOH_TEXT
)
select a.*, b.pat_cnt as pat_pos_cnt
from qcnt a
join acnt b
on a.SDOH_TEXT= b.SDOH_TEXT
order by a.pat_cnt desc
;

create or replace table SDOH_UU_FEAS as 
select a.patid, 
       b.pro_date,
       b.pro_name,
       b.pro_response_text,
       ref.SDOH_DOMAIN, 
       ref.SDOH_VAR, 
       ref.SDOH_TEXT, 
       ref.LOINC,
       case when b.pro_response_text in ('Never','Never true','No','Not at all','Not difficult at all','Declined') or b.pro_response_text is null then 0 else 1 end as SDOH_POS_IND
from VCCC_BASE_BP_TCOG_SDH a
join GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.PRO_CM b on a.patid = b.patid
join Z_SDOH_REF ref on b.PRO_CODE = ref.LOINC 
;
select count(distinct patid) from SDOH_UU_FEAS; -- 264
with qcnt as (
    select SDOH_TEXT, count(distinct patid) as pat_cnt, sum(SDOH_POS_IND) as pos_cnt
    from SDOH_UU_FEAS
    group by SDOH_TEXT
), acnt as (
    select SDOH_TEXT, count(distinct patid) as pat_cnt
    from SDOH_UU_FEAS
    where SDOH_POS_IND = 1
    group by SDOH_TEXT
)
select a.*, b.pat_cnt as pat_pos_cnt
from qcnt a
join acnt b
on a.SDOH_TEXT= b.SDOH_TEXT
order by a.pat_cnt desc
;

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
    var site_cdm = `GROUSE_DB_DEV_CDM.PCORNET_CDM_`+ site +``;
     
    // smoking status
    var sqlstmt_par = `
        INSERT INTO `+ TGT_LONG_TBL +`
            -- SMOKING from VITAL table
            select distinct
                   r.PATID
                  ,r.STUDY_ID
                  ,'SMOKING' as SDOH_VAR
                  ,v.SMOKING as SDOH_VAL
                  ,v.MEASURE_DATE as REPORT_DATE
                  ,datediff(day,r.enroll_date,v.measure_date) as DAYS_SINCE_INDEX
            from `+ TRIAL_REF +` r 
            join `+ site_cdm +`.VITAL v on r.patid = v.patid
            where v.SMOKING in (
                '01', --Current every day smoker  
                '02', --Current some day smoker  
                -- '03', --Former smoker   
                '05', --Smoker, current  status unknown  
                '07', --Heavy tobacco smoker  
                '08' --Light tobacco smoker  
            )
            UNION
            select  distinct 
                    r.PATID
                   ,r.STUDY_ID
                   ,'SMOKING' as SDOH_VAR
                   ,os.obsclin_result_text as SDOH_VAL
                   ,os.OBSCLIN_START_DATE
                   ,datediff(day,r.enroll_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
            from `+ TRIAL_REF +` r
            join `+ site_cdm +`.OBS_CLIN os on r.patid = os.patid
            where
                os.OBSCLIN_TYPE = 'LC' and 
                os.OBSCLIN_CODE in (
                    '8663-7'  -- Cigarettes smoked current (pack per day) - Reported
                )
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

create or replace table VCCC_SDOH_LONG (
    PATID varchar(50) NOT NULL,
    STUDY_ID varchar(50) NOT NULL,
    SDOH_VAR varchar(100),
    SDOH_VAL varchar(100),
    REPORT_DATE date,     
    DAYS_SINCE_INDEX number
);

/*test*/
-- call get_sdoh_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_SDOH_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_sdoh_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_SDOH_LONG',
    FALSE, NULL
);

select * from VCCC_SDOH_LONG limit 5;

create or replace table VCCC_SMOKING_BASE as 
with smok as (
    select distinct study_id, 1 as smoker_ind
from VCCC_SDOH_LONG
where sdoh_var = 'SMOKING' 
      and days_since_index between -1095 and -1
)
select a.patid, a.study_id, 
       coalesce(s.smoker_ind,0) as smoker_ind
from VCCC_BASE_BP_TCOG_SDH a 
left join smok s 
on a.study_id = s.study_id
; 

select count(distinct study_id) from VCCC_SMOKING_BASE;

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
              join GROUSE_DB_DEV_CDM.`+ site_cdm +`.DIAGNOSIS d 
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
select a.patid, a.study_id,
       coalesce(b.cci_total,0) as cci_total,
       case when b.cci_total between 1 and 2 then 'cci_grp1'
            when b.cci_total between 3 and 4 then 'cci_grp2'
            when b.cci_total >= 5 then 'cci_grp3'
            else 'cci_grp0'
       end as cci_total_grp
from VCCC_BASE_BP_TCOG_SDH a 
left join cci_tot b
on a.patid = b.patid
; 

select * from VCCC_BASE_CCI limit 5;

select count(distinct patid), count(*) from VCCC_BASE_CCI;


create or replace procedure get_vccc_efi_long(
    TRIAL_REF string,
    SITES array,
    TGT_LONG_TBL string,
    TIME_WINDOW double,
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
 * @param {integer} TIME_WINDOW: lookback time windows for relevant diagnosis code collection
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
                     ,efi.CODE_GRP
              from `+ TRIAL_REF +` a
              join GROUSE_DB_DEV_CDM.`+ site_cdm +`.DIAGNOSIS d 
              on a.PATID = d.PATID
              join Z_EFI_REF efi
              on d.dx like efi.code||'%' and d.dx_type = efi.code_type
              where datediff(day,NVL(d.DX_DATE::date,d.ADMIT_DATE::date),a.enroll_date) between 0 and `+ TIME_WINDOW +` 
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

create or replace table EFI_DX_LONG (
    PATID varchar(50) NOT NULL,
    DAYS_SINCE_INDEX number,
    DX varchar(20),
    PDX varchar(20),
    DX_DATE date,
    ADMIT_DATE date,
    ENC_TYPE varchar(20),
    EFI_CLS varchar(11)
);
/*test*/
-- call get_vccc_efi_long(
--     'VCCC_BASE_BP_TCOG_SDH',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'EFI_DX_LONG',
--     730,
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_vccc_efi_long(
    'VCCC_BASE_BP_TCOG_SDH',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'EFI_DX_LONG',
    1825,
    FALSE, NULL
);

select * from EFI_DX_LONG limit 5;

select count(distinct patid) from EFI_DX_LONG;
-- 989

create or replace table VCCC_BASE_EFI as
with efi_uni as (
    select patid, EFI_CLS
    from (
        select a.*, row_number() over (partition by a.patid, a.EFI_CLS order by a.days_since_index desc) rn
        from EFI_DX_LONG a
    )
    where rn = 1
), efi_pvt as (
    select * from (
    select patid, EFI_CLS, 1 as ind from efi_uni
)
pivot (
    max(ind) for EFI_CLS in (
        'ANE',
        'ARTH',
        'AFIB',
        'BLID',
        'CANC',
        'CKD',
        'CP',
        'CHF',
        'CAD',
        'DEM',
        'DEP',
        'DIA',
        'DIZ',
        'DYS',
        'FALL',
        'FRA',
        'HEAR',
        'HYPTN',
        'SYNCO',
        'LEUK',
        'MLD',
        'MSLD',
        'MEL',
        'MI',
        'OST',
        'PD',
        'PUD',
        'PVD',
        'PULM',
        'SU',
        'STROKE',
        'TD',
        'UI',
        'USD',
        'VALV',
        'WTLOS',
        'AIDS',
        'CTD',
        'DKD',
        'ADPKD',
        'GN',
        'TIN',
        'URO'
    )
    DEFAULT ON NULL (0)
)
as p(patid,ANE,ARTH,AFIB,BLID,CANC,CKD,CP,CHF,CAD,DEM,DEP,DIA,DIZ,DYS,FALL,FRA,HEAR,HYPTN,SYNCO,LEUK,MLD,MSLD,MEL,MI,OST,PD,PUD,PVD,PULM,SU,STROKE,TD,UI,USD,VALV,WTLOS,AIDS,CTD,DKD,ADPKD,GN,TIN,URO)
)
select  a.patid, a.study_id,
        coalesce(b.ANE,0) as EFI_ANE,
        coalesce(b.ARTH,0) as EFI_ARTH,
        coalesce(b.AFIB,0) as EFI_AFIB,
        coalesce(b.BLID,0) as EFI_BLID,
        coalesce(b.CANC,0) as EFI_CANC,
        coalesce(b.CKD,0) as EFI_CKD,
        coalesce(b.CP,0) as EFI_CP,
        coalesce(b.CHF,0) as EFI_CHF,
        coalesce(b.CAD,0) as EFI_CAD,
        coalesce(b.DEM,0) as EFI_DEM,
        coalesce(b.DEP,0) as EFI_DEP,
        coalesce(b.DIA,0) as EFI_DIA,
        coalesce(b.DIZ,0) as EFI_DIZ,
        coalesce(b.DYS,0) as EFI_DYS,
        coalesce(b.FALL,0) as EFI_FALL,
        coalesce(b.FRA,0) as EFI_FRA,
        coalesce(b.HEAR,0) as EFI_HEAR,
        coalesce(b.HYPTN,0) as EFI_HYPTN,
        coalesce(b.SYNCO,0) as EFI_SYNCO,
        coalesce(b.LEUK,0) as EFI_LEUK,
        coalesce(b.MLD,0) as EFI_MLD,
        coalesce(b.MSLD,0) as EFI_MSLD,
        coalesce(b.MEL,0) as EFI_MEL,
        coalesce(b.MI,0) as EFI_MI,
        coalesce(b.OST,0) as EFI_OST,
        coalesce(b.PD,0) as EFI_PD,
        coalesce(b.PUD,0) as EFI_PUD,
        coalesce(b.PVD,0) as EFI_PVD,
        coalesce(b.PULM,0) as EFI_PULM,
        coalesce(b.SU,0) as EFI_SU,
        coalesce(b.STROKE,0) as EFI_STROKE,
        coalesce(b.TD,0) as EFI_TD,
        coalesce(b.UI,0) as EFI_UI,
        coalesce(b.USD,0) as EFI_USD,
        coalesce(b.VALV,0) as EFI_VALV,
        coalesce(b.WTLOS,0) as EFI_WTLOS,
        coalesce(b.AIDS,0) as EFI_AIDS,
        coalesce(b.CTD,0) as EFI_CTD,
        coalesce(b.DKD,0) as EFI_DKD,
        coalesce(b.ADPKD,0) as EFI_ADPKD,
        coalesce(b.GN,0) as EFI_GN,
        coalesce(b.TIN,0) as EFI_TIN,
        coalesce(b.URO,0) as EFI_URO
from VCCC_BASE_BP_TCOG_SDH a 
left join efi_pvt b 
on a.patid = b.patid
; 

select * from VCCC_BASE_EFI limit 5;

select count(distinct patid), count(*) from VCCC_BASE_EFI;


-- sample a list of study_id who we don't have any anti-hypertensive drugs
select a.study_id
from VCCC_BASELINE_FINAL a 
where a.AV_CNT >= 2 and 
      not exists (
        select 1 from VCCC_MED_LONG m where a.patid = m.patid and m.antihtn_ind = 1)
order by a.study_id
;

--unmatched patid
select distinct study_id, age
from VCCC_BASELINE_FINAL
where patid is null
order by study_id;

-- 
create or replace table VCCC_BASELINE_FINAL as
select  a.*
       ,smk.* exclude (PATID, STUDY_ID)
       ,vis.* exclude (PATID, STUDY_ID)
       ,ant.* exclude (PATID, STUDY_ID)
       ,efi.* exclude (PATID, STUDY_ID)
       ,cci.* exclude (PATID, STUDY_ID)
       ,lab.* exclude (PATID, STUDY_ID)
       ,med.* exclude (PATID, STUDY_ID)
       ,runin.* exclude (PATID, STUDY_ID)
from VCCC_BASE_BP_TCOG_SDH a 
join VCCC_VISITS_BASE vis on a.study_id = vis.study_id
join VCCC_SMOKING_BASE smk on a.study_id = smk.study_id
join VCCC_ANTRO_BASE_SEL ant on a.study_id = ant.study_id
join VCCC_BASE_EFI efi on a.study_id = efi.study_id
join VCCC_BASE_CCI cci on a.study_id = cci.study_id
join VCCC_LAB_BASE_SEL lab on a.study_id = lab.study_id
join VCCC_BASE_MED med on a.study_id = med.study_id
join VCCC_RUNIN_MED runin on a.study_id = runin.study_id
;

select * from VCCC_BASELINE_FINAL 
-- where DELTA_SBP_BY10 <-3
limit 5;

select delta_sbp_by10, count(distinct patid)
from VCCC_BASELINE_FINAL
group by delta_sbp_by10
order by delta_sbp_by10
;

select race_ethn_str, count(distinct patid)
from VCCC_BASELINE_FINAL
group by race_ethn_str
order by race_ethn_str
;

select count(distinct patid), count(distinct study_id), count(*) 
from VCCC_BASELINE_FINAL
;

select ruca_primary_nonmetro_ind,site,
       count(distinct patid), count(distinct study_id), count(*) 
from VCCC_BASELINE_FINAL
group by ruca_primary_nonmetro_ind,site
;

