/* Identify vCCC Patient demographic per category 
** Dependencies: denom_epic.sql 
*/
/*collect demographic information and classification*/
drop table denom_dem;
create table denom_dem as
select dem.*, cat.status, vref.prov_name
from PATIENT_CATEGORY cat 
join nightherondata.patient_dimension pd on lpad(pd.mrn,7,'0') = lpad(cat.mrn,7,'0')
join vccc_ref vref on vref.patient_num  = pd.patient_num
join pcornet_cdm.demographic dem on dem.patid = pd.patient_num; 

/*collect summary statistics for each category*/
drop table stats_category;
create table stats_category (
    cnt_type varchar(40),
    cnt_type_cat varchar(40),
    pat_category varchar(40),
    pat_cnt integer
)
;

-- demographic breakdown
insert into stats_category
select 'by sex', SEX, status, count(distinct PATID) from denom_dem
group by SEX, status
union
select 'by race', RACE, status, count(distinct PATID) from denom_dem
group by RACE, status
union 
select 'by provider', PROV_NAME, status , count(distinct PATID) from denom_dem
group by PROV_NAME, status
union 
select 'all', 'all', 'all', count(distinct PATID) from denom_dem
union
select 'by category', status, 'all' , count(distinct PATID) from denom_dem
group by status
union
select 'by hispanic',HISPANIC,  status, count(distinct PATID) from denom_dem
group by HISPANIC, status;

commit;

select dem.sex, dem.race, dem.hispanic, cat.status, vref.prov_name, pat.record_id
from PATIENT_CATEGORY cat 
join PATIENT_SET_4_21_22 pat on lpad(pat.mrn,7,'0')  = lpad(cat.mrn,7,'0')
join nightherondata.patient_dimension pd on lpad(pd.mrn,7,'0') = lpad(cat.mrn,7,'0')
join vccc_ref vref on vref.patient_num  = pd.patient_num
join pcornet_cdm.demographic dem on dem.patid = pd.patient_num;


/*eyeball the results*/
select * from stats_category;