/*
Key reference clarity tables needed for generating
- clarity.enroll_info: more detailed enrollment information
- clarity.patient: get demographic information of referred patients
- clarity.patient_race: add race information
- clarity.pat_enc: patient encounter table for more specific information of the referral visit
- clarity.clarity_ser: add provider-level information

Auxillary clarity tables: 
- clarity.zc_patient_race: human-readable labels for race category

External file: 
- import the file "ConceptSet_PCP_NPI.csv" (vccc NPI list) as table "VCCC_NPI"
- make sure the table column names match with the raw csv file, three columns: 
    -- LAST: last name
    -- FIRST: first name
    -- NPI: NPI number
    -- START_DT: start date 
*/ 

select count(distinct NPI) from VCCC_NPI;


--clarity key table refresh date
select max(last_analyzed) from all_tab_columns
where owner = 'CLARITY' and table_name = 'PATIENT';

/*collect demonitor patients: 
 - has a PCP visit with vCCC PCPs
 - the PCP visit happened within study period
 - age at visit >= 65
*/
-- collect VCCC PCPs (name list or PROV_ID is required)
drop table vccc_pcp purge;
create table vccc_pcp as
select vccc_npi.npi
      ,vccc_npi.last
      ,vccc_npi.first
      ,vccc_npi.start_dt
      ,cs.prov_name                -- keep to verify if they are the same provider     
--      ,cs.prov_start_date        -- could add more provide-level information to this table for downstream analysis
--      ,doctors_degree         
from vccc_npi 
join clarity.clarity_ser2 cs2 on vccc_npi.npi = cs2.npi
join clarity.clarity_ser cs on cs2.npi = cs.npi
;

-- collect patients who have at least 1 PCP visit within the enrolling time period
drop table vccc_ref purge;
create table vccc_ref as
select dense_rank() over (order by pe.pat_id) PATID -- de-identification
      ,pcp.prov_name
      ,pcp.npi
--      ,pcp.doctors_degree
--      ,pcp.prov_start_date
      ,round((trunc(pe.EFFECTIVE_DATE_DTTM) - trunc(pt.BIRTH_DATE))/365.25) AGE_AT_VIS
      ,case when pt.SEX_C = 1 then 'F'
            when pt.SEX_C = 2 then 'M'
            else 'NI'
       end as SEX
      ,case when ptr.patient_race_c is NULL then '@'
         when czpr.abbr is null then 'other'
         else lower(czpr.abbr)
        end RACE
      ,pe.BP_SYSTOLIC
      ,pe.EFFECTIVE_DATE_DTTM
from clarity.pat_enc pe 
join vccc_pcp pcp on pcp.prov_id = pe.PCP_PROV_ID
left join clarity.patient pt on pe.PAT_ID = pt.PAT_ID
left join clarity.patient_race ptr on ptr.PAT_ID = pe.PAT_ID
left join clarity.zc_patient_race czpr on ptr.patient_race_c = czpr.patient_race_c
-- increase specifity, given that a PCP_PROV_ID may be documented even for a non-PCP visit
where (pe.DEPARTMENT_NAME like '%FAMILY%CL%' or
       pe.DEPARTMENT_NAME like '%IM%CL%'
      )
-- only patients seen by the vCCC PCP within the study period can be counted towards the denominator
      and pcp.START_DT is not null and pe.EFFECTIVE_DATE_DTTM >= pcp.START_DT
-- age at visit >= 65
      and round((trunc(pe.EFFECTIVE_DATE_DTTM) - trunc(pt.BIRTH_DATE))/365.25) >= 65
-- PCP provider ID is the same as attending physician ID
      and pcp.prov_id = pe.AT_PROV_ID
;

/*collect summary statistics (no cell-size suppression)*/
drop table stats_tbl;
create table stats_tbl (
    cnt_type varchar(40),
    cnt_type_cat varchar(40),
    pat_cnt integer
)
;
-- all 
insert into stats_tbl
select 'all', 'all', count(distinct patient_num) from vccc_ref;

-- breakdown by PCP provider
insert into stats_tbl
select 'by provider', PROV_NAME, count(distinct patient_num) from vccc_ref
group by PROV_NAME;

-- demographic breakdown
insert into stats_tbl
select 'by sex', SEX, count(distinct patient_num) from vccc_ref
group by SEX
union
select 'by race', RACE, count(distinct patient_num) from vccc_ref
group by RACE

-- could add more as needed
;

commit;

/*eyeball the results*/
select * from stats_tbl;