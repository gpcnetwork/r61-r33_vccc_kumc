/*
===========================    
Key CDM tables from KUMC and 
1. ENCOUNTER
2. DEMOGRAPHIC
3. VITAL
4. PRESCRIBING
5. PCORNET_TRIAL

Auxillary tables: 
1. Prescreening tables
  - BYOD_BL5ENR
  - BYOD_BL5ENR_KUMC
  - BYOD_BL5ENR_UU
2. refernce tables: 
  - Z_MED_RXCUI_REF (ref/med_rxcui_ref.csv)
3. CDM tables: 
  - PAT_TABLE1 (denom_pat_cdm.sql)
  - Z_CCI_REF (ref/cci_icd_ref.csv)
4. Geocoded tables: 
  - VCCC_ENR_UU_GEOCODED
=============================
*/ 
select * from byod_bl5enr limit 5;
select * from byod_bl5enr_kumc limit 5;
select * from byod_bl5enr_uu limit 5;
select * from VCCC_ENR_UU_GEOCODED 
-- where census_block_group_id_2020 <> 'NA'
limit 5;

select substr(RECORD_ID,1,4) as site, 
    --    substr(STUDY_ID,1,2) as site2,
       count(distinct record_id) as pat_cnt 
from byod_bl5enr
group by substr(RECORD_ID,1,4)
    --    ,substr(STUDY_ID,1,2)
;
-- 4422,      6322, KU
-- 4423-4424, 4204  UT

select count(distinct record_id), count(distinct patid)
from byod_bl5enr_kumc 
where patid is not null
;
-- 6265, 5726

create or replace temporary view recordid_dup as
select patid, count(distinct record_id) as record_cnt
from byod_bl5enr_kumc
group by patid
having count(distinct record_id) > 1
order by count(distinct record_id) desc
;

select count(distinct patid)
from recordid_dup
;

select count(distinct record_id), count(distinct patid)
from byod_bl5enr_uu 
where patid is not null
;
-- 4138, 3749

create or replace table VCCC_ENR_ALL as 
with enr_stk as (
    select distinct k.patid, 'KUMC' as site, a.pcp_visit_date,
            case when a.study_id is not null then 'enrol'
                 when k.prescreen_status = '2' then 'declin'
                 when k.prescreen_status = '3' then 'inelig'
                 when k.prescreen_status = '1' then 'unreach'
                 else 'other'
            end as prescreen_status,
            row_number() over (partition by k.patid order by a.pcp_visit_date desc) as rn
        from byod_bl5enr_kumc k 
        join byod_bl5enr a 
        on a.record_id = k.record_id
        union
        select distinct k.patid, 'Utah' as site, a.pcp_visit_date,
            case when a.study_id is not null then 'enrol'
                 when k.prescreen_status = '2' then 'declin'
                 when k.prescreen_status = '3' then 'inelig'
                 when k.prescreen_status = '1' then 'unreach'
                 else 'other'
            end as prescreen_status,
            row_number() over (partition by k.patid order by a.pcp_visit_date desc) as rn
        from byod_bl5enr_uu k 
        join byod_bl5enr a 
        on a.record_id = k.record_id
)
select patid, site, prescreen_status, pcp_visit_date as index_date
from enr_stk 
where patid is not null and rn = 1
union
select a.patid, a.site, 'enrol', max(a.enroll_date) as index_date
from VCCC_BASE_BP_TCOG_SDH a 
where not exists (
    select 1 from enr_stk b 
    where a.patid = b.patid
) and a.patid is not null
group by a.patid, a.site
;

select prescreen_status, count(distinct patid)
from VCCC_ENR_ALL
group by prescreen_status
order by prescreen_status
;
-- declin	3107
-- enrol	1082
-- inelig	3530
-- other	182
-- unreach	1580

select site, count(distinct patid)
from VCCC_ENR_ALL
group by site
;
-- Utah	3749
-- KUMC	5732

select * from VCCC_ENR_ALL 
limit 5;

create or replace table VCCC_UNENR_INDEX as 
with unenrol as (
    select * from VCCC_ENR_ALL
    where prescreen_status <> 'enrol' and patid is not null
), demo_stk as (
    select a.patid, a.birth_date, a.sex, a.race, a.hispanic 
    from GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.DEMOGRAPHIC a 
    join unenrol on a.patid = unenrol.patid 
    union 
    select a.patid, a.birth_date, a.sex, a.race, a.hispanic 
    from GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.DEMOGRAPHIC a 
    join unenrol on a.patid = unenrol.patid  
), combine_dup as (
    select a.patid, 
        a.prescreen_status, 
        a.site,
        b.index_date,
        p.birth_date,
        round(datediff(day,p.birth_date,b.index_date)/365.25) as age,
        p.sex,
        case when p.sex = 'F' then 'Female' else 'Male' end as sex_str,
        p.race,
        CASE WHEN p.race IN ('05') THEN 'white' 
             WHEN p.race IN ('03') THEN 'black'
             WHEN p.race IN ('02') THEN 'asian'
             WHEN p.race IN ('01') THEN 'aian'
             WHEN p.race IN ('04') THEN 'nhopi'
             WHEN p.race IN ('06') THEN 'multi'
             WHEN p.race IN ('OT') THEN 'other'
             ELSE 'NI' 
        END AS race_str, 
        p.hispanic as ethnicity,
        CASE WHEN p.hispanic = 'Y' THEN 'hispanic' 
            WHEN p.hispanic = 'N' THEN 'non-hispanic' 
            ELSE 'NI' 
        END AS ethn_str, 
        case when p.race = '03' and p.hispanic <> 'Y' then 'nh-black'
             when p.race = '05' then 'white'
             when p.hispanic = 'Y' then 'hisp'
            else 'other'
       end as race_ethn_str,
        row_number() over (partition by a.patid order by b.index_date) as rn 
    from VCCC_UNENR a 
    join unenrol b 
    on a.patid = b.patid
    join demo_stk p 
    on a.patid = p.patid
)
select combine_dup.* exclude rn
from combine_dup
where rn = 1

;
select * from VCCC_UNENR_INDEX limit 5;

select count(distinct patid), count(*) 
from VCCC_UNENR_INDEX;
-- 8387	8387

select prescreen_status, count(distinct patid)
from VCCC_UNENR_INDEX
group by prescreen_status
order by prescreen_status
;
-- declin	3105
-- inelig	3522
-- other	182
-- unreach	1578

create or replace procedure get_clinic_bp_long2(
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
            as p(PATID, MEASURE_DATE,SBP, DBP)
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

create or replace table VCCC_UNENR_CLINIC_BP_LONG (
    PATID varchar(50) NOT NULL,
    MEASURE_DATE date,     
    SBP integer,
    DBP integer
);

/*test*/
-- call get_clinic_bp_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_UNENR_CLINIC_BP_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_clinic_bp_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_UNENR_CLINIC_BP_LONG',
    FALSE, NULL
);

select * from VCCC_UNENR_CLINIC_BP_LONG limit 5;

create or replace table VCCC_UNENR_ELIG_BP as 
with bp_bef_idx as (
    select a.patid,
           a.measure_date,
           b.index_date,
           a.sbp as elig_sbp,
           a.dbp as elig_dbp, 
           row_number() over (partition by a.patid order by abs(datediff(day,b.index_date,a.measure_date))) as rn 
    from VCCC_UNENR_CLINIC_BP_LONG a 
    join VCCC_UNENR_INDEX b 
    on a.patid = b.patid
)
select * exclude rn
from bp_bef_idx 
where rn = 1
;

select * from VCCC_UNENR_ELIG_BP limit 5;

select count(distinct patid), count(*) from VCCC_UNENR_ELIG_BP;
-- 8387	8387
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

create or replace table VCCC_UNENR_MED_LONG_RAW as 
select  a.patid,
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
        vccc.index_date,
        datediff(day,vccc.index_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index        
from GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.PRESCRIBING a 
join VCCC_UNENR_INDEX vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV_CDM.PCORNET_CDM_KUMC.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on try_to_number(a.RXNORM_CUI) = m.RXNORM_CUI 
where vccc.site = 'KUMC'
union all
select  a.patid,
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
        vccc.index_date,
        datediff(day,vccc.index_date,coalesce(a.rx_start_date,a.rx_order_date)) as rx_start_since_index           
from GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.PRESCRIBING a 
join VCCC_UNENR_INDEX vccc on a.patid = vccc.patid
left join GROUSE_DB_DEV_CDM.PCORNET_CDM_UU.ENCOUNTER e on a.patid = e.patid 
left join Z_MED_RXCUI_REF_AH m on a.RXNORM_CUI = m.RXNORM_CUI 
where vccc.site = 'Utah'
;         

select * from VCCC_UNENR_MED_LONG_RAW limit 5;

create or replace table VCCC_UNENR_MED_LONG as
with dur_calc as(
    select patid, 
        prescribingid, 
        rxnorm_cui,
        ING,
        VA_CLS,
        VA_CLS_CD,
        coalesce(ANTIHTN_IND,0) as ANTIHTN_IND,
        coalesce(ING,raw_rx_med_name) as in_or_name,
        substr(coalesce(ING,raw_rx_med_name),1,20) as in_or_name_s,
        rx_start_since_index,
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
    from VCCC_UNENR_MED_LONG_RAW
)
select distinct
       dur_calc.*, 
       coalesce(dur_calc.rx_days2,dur_calc.rx_days1) as rx_days,
       rx_start_since_index + coalesce(dur_calc.rx_days2,dur_calc.rx_days1) as rx_end_since_index
from dur_calc
;

select * from VCCC_UNENR_MED_LONG 
where antihtn_ind = 1
limit 5;

select count(distinct patid) from VCCC_UNENR_MED_LONG;
-- 8385

create or replace table VCCC_UNENR_BASE_MED as 
with med_1yr as (
    select patid, 
           count(distinct in_or_name_s) as med_cnt_in,
           count(distinct va_cls_cd) as med_cnt_cls
    from VCCC_UNENR_MED_LONG
    where antihtn_ind = 1 and rx_start_since_index between -365 and 0
    group by patid
)
select distinct a.patid, 
       coalesce(b.med_cnt_in,0) as med_cnt_in,
       coalesce(b.med_cnt_cls,0) as med_cnt_cls,
       case when b.med_cnt_in between 1 and 5 then 'polyrx_in_grp1'
            when b.med_cnt_in >= 5 then 'polyrx_in_grp2'
            else 'polyrx_in_grp0'
       end as polyrx_in_grp,
from VCCC_UNENR_INDEX a 
left join med_1yr b 
on a.patid = b.patid      
;

select * from VCCC_UNENR_BASE_MED limit 5;

select polyrx_in_grp, count(distinct patid), count(*)
from VCCC_UNENR_BASE_MED
group by polyrx_in_grp
;
-- polyrx_in_grp0	3548	3548
-- polyrx_in_grp1	4644	4644
-- polyrx_in_grp2	195	    195


/* healthcare visits */
create or replace procedure get_visits_long2(
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

create or replace table VCCC_UNENR_VISITS_LONG (
     PATID varchar(50) NOT NULL
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
-- call get_visits_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_UNENR_VISITS_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_visits_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_UNENR_VISITS_LONG',
    FALSE, NULL
);

select * from VCCC_UNENR_VISITS_LONG limit 5;

create or replace table VCCC_UNENR_VISITS_BASE as 
with av as (
    select patid, count(distinct admit_date) as vis_cnt 
    from VCCC_UNENR_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE = 'AV'
    group by patid
), th as (
    select patid, count(distinct admit_date) as vis_cnt 
    from VCCC_UNENR_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE = 'TH'
    group by patid
), ed as (
    select patid, count(distinct admit_date) as vis_cnt 
    from VCCC_UNENR_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE in ('ED','EI')
    group by patid
), ip as (
    select patid, count(distinct admit_date) as vis_cnt 
    from VCCC_UNENR_VISITS_LONG
    where days_since_index between -730 and -1 and ENC_TYPE in ('IP','EI')
    group by patid
), payer as (
    select patid, payer_type_primary_grp from 
    (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from VCCC_UNENR_VISITS_LONG a
        where a.enc_type = 'AV' and payer_type_primary_grp <> 'NI'
    )
    where rn = 1
)
select distinct a.patid,
       coalesce(av.vis_cnt, 0) as av_cnt,
       coalesce(th.vis_cnt, 0) as th_cnt,
       coalesce(ed.vis_cnt, 0) as ed_cnt,
       coalesce(ip.vis_cnt, 0) as ip_cnt,
       coalesce(payer.payer_type_primary_grp,'NI') as payer_type_primary_grp
from VCCC_UNENR_INDEX a 
left join av on a.patid = av.patid 
left join th on a.patid = th.patid 
left join ed on a.patid = ed.patid 
left join ip on a.patid = ip.patid 
left join payer on a.patid = payer.patid
;

select count(distinct patid), count(*) 
from VCCC_UNENR_VISITS_BASE;
-- 8387	8387

select payer_type_primary_grp, count(distinct patid)
from VCCC_UNENR_VISITS_BASE
group by payer_type_primary_grp
;

create or replace procedure get_anthro_long2(
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
                 b.measure_date::date,
                 round(datediff(day,r.index_date,b.measure_date::date)/365.25),
                 'HT',b.ht/39.37 -- default at 'in'
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.ht is not null
          UNION
          select r.patid,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.index_date,oc.OBSCLIN_START_DATE::date)/365.25),'HT',
                 case when lower(oc.OBSCLIN_RESULT_UNIT) like '%cm%' then oc.OBSCLIN_RESULT_NUM/100
                      else oc.OBSCLIN_RESULT_NUM/39.37 end
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.OBS_CLIN oc 
          ON r.patid = oc.patid AND
             oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '8302-2'
          UNION
          -- weight (kg)--
          SELECT r.patid,
                 b.measure_date::date,
                 round(datediff(day,r.index_date,b.measure_date::date)/365.25),
                 'WT',b.wt/2.205 -- default at 'lb'
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.wt is not null
          UNION
          select r.patid,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.index_date,oc.OBSCLIN_START_DATE::date)/365.25),'WT',
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
                 b.measure_date::date,
                 round(datediff(day,r.index_date,b.measure_date::date)/365.25),
                 'BMI',b.ORIGINAL_BMI
          FROM `+ TRIAL_REF +` r
          JOIN `+ site_cdm +`.VITAL b 
          ON r.patid = b.patid
          WHERE b.ORIGINAL_BMI is not null
          UNION
          select r.patid,
                 oc.OBSCLIN_START_DATE::date,
                 round(datediff(day,r.index_date,oc.OBSCLIN_START_DATE::date)/365.25),
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

create or replace table VCCC_UNENR_ANTHRO_LONG(
    PATID varchar(50) NOT NULL,
    MEASURE_DATE date,      -- date of first HT/WT/BMI record
    DAYS_SINCE_INDEX integer,
    MEASURE_TYPE varchar(4),
    MEASURE_NUM double -- ht:m; wt:kg
);

/*test*/
-- call get_anthro_long2(
--     'VCCC_UNENR_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_UNENR_ANTHRO_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;

call get_anthro_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_UNENR_ANTHRO_LONG',
    FALSE, NULL
);

create or replace table VCCC_UNENR_ANTHRO_TS as
with daily_agg as(
    select patid,measure_date,HT,WT,days_since_index,
           case when BMI>100 then NULL else BMI end as BMI,
           case when HT = 0 or WT = 0 or round(WT/(HT*HT))>100 then NULL
                else round(WT/(HT*HT)) 
           end as BMI_CALCULATED
    from (
        select patid,
               measure_type, 
               measure_date::date as measure_date, 
               days_since_index, 
               median(measure_num) as measure_num
    from VCCC_UNENR_ANTHRO_LONG
    group by patid, measure_type, measure_date::date,days_since_index
    ) 
    pivot(
        median(measure_num) 
        for measure_type in ('HT','WT','BMI')
    ) as p(patid,measure_date,days_since_index,HT,WT,BMI)
    where (WT is not null and HT is not null and WT>0 and HT>0) or
          (BMI is not null and BMI > 0)
)
select patid,
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

select * from VCCC_UNENR_ANTHRO_TS limit 5;

create or replace table VCCC_UNENR_ANTRO_BASE_SEL as 
with lastest as (
    select * from VCCC_UNENR_ANTHRO_TS
    where rn = 1 
)
select distinct a.patid,
       b.ht, b.wt, b.bmi
from VCCC_UNENR_INDEX a 
left join lastest b 
on a.patid = b.patid 
;

select count(distinct patid), count(distinct patid), count(*)
from VCCC_UNENR_ANTRO_BASE_SEL
;
-- 8387	8387	8387

-- labs
create or replace procedure get_labs_long2(
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
                ,coalesce(b.specimen_date, b.lab_order_date, b.result_date) as OBS_DATE
                ,datediff(day,a.index_date,coalesce(b.specimen_date, b.lab_order_date, b.result_date)) as DAYS_SINCE_INDEX
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

create or replace table VCCC_UNENR_LABS_LONG (
     PATID varchar(50) NOT NULL
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
-- call get_labs_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_UNENR_LABS_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_labs_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_UNENR_LABS_LONG',
    FALSE, NULL
);

select * from  VCCC_UNENR_LABS_LONG limit 5;


create or replace table VCCC_UNENR_LAB_BASE_SEL as 
with cr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where a.obs_code in ('2160-0')
        --   and a.obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and a.obs_num is not null
), bun as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('3094-0')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), egfr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('98979-8','48642-3','77147-7','48643-1','33914-3','88294-4','88293-6')
        --   and obs_unit = 'mL/min'
          and a.days_since_index between -731 and 0 and obs_num is not null
), egfr2 as (
    select distinct a.patid, coalesce(egfr.days_since_index,cr.days_since_index) as days_since_index,
           case when a.sex = 'M' then coalesce(egfr.obs_num,round(142*power(least(cr.obs_num/0.9,1),-0.302)*power(greatest(cr.obs_num/0.9,1),-1.2)*power(0.9938,a.age)*1))
                else coalesce(egfr.obs_num,round(142*power(least(cr.obs_num/0.7,1),-0.241)*power(greatest(cr.obs_num/0.7,1),-1.2)*power(0.9938,a.age)*1.012))
           end as lab_egfr2,
           row_number() over (partition by a.patid order by coalesce(egfr.days_since_index,cr.days_since_index) desc) as rn,
           count(distinct coalesce(egfr.days_since_index,cr.days_since_index)) over (partition by a.patid) as egfr_cnt
    from VCCC_UNENR_INDEX a 
    left join cr on a.patid = cr.patid
    left join egfr on a.patid = egfr.patid
    where coalesce(egfr.days_since_index,cr.days_since_index) is not null
), pot as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2823-3','6298-4')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), sod as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2951-2','2947-0')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralbcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('9318-7','14959-1','14958-3')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralb as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('14957-5','1754-1','13992-3','29946-1','30003-8','13992-3')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2161-8','20624-3','14683-7','2162-6')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uralbcr2 as (
    select a.patid, coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) as days_since_index,
           coalesce(uralbcr.obs_num, round(uralb.obs_num/urcr.obs_num*1000)) as lab_uralbcr2,
           row_number() over (partition by a.patid order by coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) desc) as rn,
           count(distinct coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index))) over (partition by a.patid) as uralbcr_cnt
    from VCCC_UNENR_INDEX a 
    left join uralbcr on a.patid = uralbcr.patid
    left join uralb on a.patid = uralb.patid
    left join urcr on a.patid = urcr.patid
    where coalesce(uralbcr.days_since_index,least(uralb.days_since_index,urcr.days_since_index)) is not null
), urprotcr as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('34366-5','13801-6')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urprot as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2889-4','57735-3','2888-6','20454-5')
        --   and obs_unit = 'mmol/L'
          and a.days_since_index between -731 and 0 and obs_num is not null
), urprotcr2 as (
    select a.patid, coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) as days_since_index,
           coalesce(urprotcr.obs_num, round(urprot.obs_num/urcr.obs_num*1000)) as lab_urprotcr2,
           row_number() over (partition by a.patid order by coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) desc) as rn,
           count(distinct coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index))) over (partition by a.patid) as urprotcr_cnt
    from VCCC_UNENR_INDEX a 
    left join urprotcr on a.patid = urprotcr.patid
    left join urprot on a.patid = urprot.patid
    left join urcr on a.patid = urcr.patid
    where coalesce(urprotcr.days_since_index,least(urprot.days_since_index,urcr.days_since_index)) is not null
), cal as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('17861-6')
          and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), alb as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('13980-8','2862-1','61152-5','1751-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), chol as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2093-3')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), ldl as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2089-1','13457-7','2091-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hdl as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2085-9')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), trig as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('2571-8')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), ast as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('1920-8','30239-8')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), alp as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('6768-6')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hem as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('20509-6','718-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), hba1c as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('17856-6','4548-4')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), mcv as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('787-2','30428-7')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
), uracid as (
    select a.*, row_number() over (partition by a.patid order by a.days_since_index desc) rn 
    from VCCC_UNENR_LABS_LONG a
    where obs_code in ('3086-6')
        --   and obs_unit = 'mg/dL'
          and a.days_since_index between -731 and 0 and obs_num is not null
)
select distinct a.patid,
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
from VCCC_UNENR_INDEX a 
left join cr on a.patid = cr.patid and cr.rn = 1
left join bun on a.patid = bun.patid and bun.rn = 1
left join egfr on a.patid = egfr.patid and egfr.rn = 1
left join egfr2 on a.patid = egfr2.patid and egfr2.rn = 1
left join pot on a.patid = pot.patid and pot.rn = 1
left join sod on a.patid = sod.patid and sod.rn = 1
left join uralbcr on a.patid = uralbcr.patid and uralbcr.rn = 1
left join uralb on a.patid = uralb.patid and uralb.rn = 1
left join urcr on a.patid = urcr.patid and urcr.rn = 1
left join uralbcr2 on a.patid = uralbcr2.patid and uralbcr2.rn = 1
left join urprotcr on a.patid = urprotcr.patid and urprotcr.rn = 1
left join urprot on a.patid = urprot.patid and urprot.rn = 1
left join urprotcr2 on a.patid = urprotcr2.patid and urprotcr2.rn = 1
left join cal on a.patid = cal.patid and cal.rn = 1
left join alb on a.patid = alb.patid and alb.rn = 1
left join chol on a.patid = chol.patid and chol.rn = 1
left join ldl on a.patid = ldl.patid and ldl.rn = 1
left join hdl on a.patid = hdl.patid and hdl.rn = 1
left join trig on a.patid = trig.patid and trig.rn = 1
left join ast on a.patid = ast.patid and ast.rn = 1
left join alp on a.patid = alp.patid and alp.rn = 1
left join hem on a.patid = hem.patid and hem.rn = 1
left join hba1c on a.patid = hba1c.patid and hba1c.rn = 1
left join mcv on a.patid = mcv.patid and mcv.rn = 1
left join uracid on a.patid = uracid.patid and uracid.rn = 1
;

select * from VCCC_UNENR_LAB_BASE_SEL
where lab_egfr is null and lab_egfr2 is not null;

select count(distinct patid), count(*) 
from VCCC_UNENR_LAB_BASE_SEL;
-- 8387	8387

create or replace procedure get_sdoh_long2(
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
            -- SMOKING from VITAL table
            select distinct
                   r.PATID
                  ,'SMOKING' as SDOH_VAR
                  ,v.SMOKING as SDOH_VAL
                  ,v.MEASURE_DATE as REPORT_DATE
                  ,datediff(day,r.index_date,v.measure_date) as DAYS_SINCE_INDEX
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
                   ,'SMOKING' as SDOH_VAR
                   ,os.obsclin_result_text as SDOH_VAL
                   ,os.OBSCLIN_START_DATE
                   ,datediff(day,r.index_date,os.OBSCLIN_START_DATE) as DAYS_SINCE_INDEX
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

create or replace table VCCC_UNENR_SDOH_LONG (
    PATID varchar(50) NOT NULL,
    SDOH_VAR varchar(100),
    SDOH_VAL varchar(100),
    REPORT_DATE date,     
    DAYS_SINCE_INDEX number
);

/*test*/
-- call get_sdoh_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'VCCC_UNENR_SDOH_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_sdoh_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'VCCC_UNENR_SDOH_LONG',
    FALSE, NULL
);

select * from VCCC_UNENR_SDOH_LONG limit 5;

create or replace table VCCC_UNENR_SMOKING_BASE as 
with smok as (
    select distinct patid, 1 as smoker_ind
from VCCC_UNENR_SDOH_LONG
where sdoh_var = 'SMOKING' 
      and days_since_index between -1095 and -1
)
select distinct a.patid, 
       coalesce(s.smoker_ind,0) as smoker_ind
from VCCC_UNENR_INDEX a 
left join smok s 
on a.patid = s.patid
; 

select count(distinct patid), count(*) from VCCC_UNENR_SMOKING_BASE;
-- 8387	8387

create or replace procedure get_vccc_cci_long2(
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
                     ,datediff(day,a.index_date,NVL(d.DX_DATE::date,d.ADMIT_DATE::date)) as DAYS_SINCE_INDEX
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
              where NVL(d.DX_DATE::date,d.ADMIT_DATE::date)<=a.index_date
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

create or replace table CCI_UNENR_DX_LONG (
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
-- call get_vccc_cci_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'CCI_UNENR_DX_LONG',
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_vccc_cci_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'CCI_UNENR_DX_LONG',
    FALSE, NULL
);

select * from CCI_UNENR_DX_LONG limit 5;

select count(distinct patid) from CCI_UNENR_DX_LONG;
-- 6777

create or replace table VCCC_UNENR_BASE_CCI as
with cci_uni as (
    select patid, CCI_GRP, cci_score
    from (
        select a.*, row_number() over (partition by a.patid, a.CCI_GRP order by a.days_since_index desc) rn
        from CCI_UNENR_DX_LONG a
    )
    where rn = 1
)
, cci_tot as (
    select patid, sum(cci_score) as cci_total
    from cci_uni
    group by patid
)
select distinct a.patid,
       coalesce(b.cci_total,0) as cci_total,
       case when b.cci_total between 1 and 2 then 'cci_grp1'
            when b.cci_total between 3 and 4 then 'cci_grp2'
            when b.cci_total >= 5 then 'cci_grp3'
            else 'cci_grp0'
       end as cci_total_grp
from VCCC_UNENR_INDEX a 
left join cci_tot b
on a.patid = b.patid
; 

select * from VCCC_UNENR_BASE_CCI limit 5;

select count(distinct patid), count(*) from VCCC_UNENR_BASE_CCI;
--8387	8387

create or replace procedure get_vccc_efi_long2(
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
                     ,datediff(day,a.index_date,NVL(d.DX_DATE::date,d.ADMIT_DATE::date)) as DAYS_SINCE_INDEX
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
              where datediff(day,NVL(d.DX_DATE::date,d.ADMIT_DATE::date),a.index_date) between 0 and `+ TIME_WINDOW +` 
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

create or replace table EFI_UNENR_DX_LONG (
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
-- call get_vccc_efi_long2(
--     'VCCC_UNENR_INDEX',
--     array_construct(
--          'KUMC'
--         ,'UU'
--     ),
--     'EFI_UNENR_DX_LONG',
--     730,
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;


call get_vccc_efi_long2(
    'VCCC_UNENR_INDEX',
    array_construct(
         'KUMC'
        ,'UU'
    ),
    'EFI_UNENR_DX_LONG',
    1825,
    FALSE, NULL
);

select * from EFI_UNENR_DX_LONG limit 5;

select count(distinct patid) from EFI_UNENR_DX_LONG;
-- 8355

create or replace table VCCC_UNENR_BASE_EFI as
with efi_uni as (
    select patid, EFI_CLS
    from (
        select a.*, row_number() over (partition by a.patid, a.EFI_CLS order by a.days_since_index desc) rn
        from EFI_UNENR_DX_LONG a
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
select  distinct a.patid,
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
from VCCC_UNENR_INDEX a 
left join efi_pvt b 
on a.patid = b.patid
; 

select * from VCCC_UNENR_BASE_EFI limit 5;

select count(distinct patid), count(*) from VCCC_UNENR_BASE_EFI;
-- 8387	8387

 
create or replace table VCCC_UNENR_BASELINE_FINAL as
select  a.*
       ,bp.elig_sbp
       ,bp.elig_dbp
       ,smk.* exclude (PATID)
       ,vis.* exclude (PATID)
       ,ant.* exclude (PATID)
       ,efi.* exclude (PATID)
       ,cci.* exclude (PATID)
       ,lab.* exclude (PATID)
       ,med.* exclude (PATID)
from VCCC_UNENR_INDEX a 
join VCCC_UNENR_ELIG_BP bp on a.patid = bp.patid
join VCCC_UNENR_VISITS_BASE vis on a.patid = vis.patid
join VCCC_UNENR_SMOKING_BASE smk on a.patid = smk.patid
join VCCC_UNENR_ANTRO_BASE_SEL ant on a.patid = ant.patid
join VCCC_UNENR_BASE_EFI efi on a.patid = efi.patid
join VCCC_UNENR_BASE_CCI cci on a.patid = cci.patid
join VCCC_UNENR_LAB_BASE_SEL lab on a.patid = lab.patid
join VCCC_UNENR_BASE_MED med on a.patid = med.patid
;

select * from VCCC_UNENR_BASELINE_FINAL limit 5;

select count(distinct patid), count(*) from VCCC_UNENR_BASELINE_FINAL;
-- 8387	8387

select prescreen_status, count(distinct patid), count(*) from VCCC_UNENR_BASELINE_FINAL
group by prescreen_status;

select payer_type_primary_grp, count(distinct patid), count(*) from VCCC_UNENR_BASELINE_FINAL
group by payer_type_primary_grp;
