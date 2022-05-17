/* Identify vCCC Patient demographic 
*/
/*collect demographic information*/

drop table pat_dem;
create table pat_dem as
select distinct pat.record_id , czpr.title as race_title ,  czs.title as sex, czeg.abbr as hispanic
from VCCC.patient_set_5_17_22 pat        --patient set from Redcap project
join clarity.patient p on p.pat_mrn_id  = lpad(pat.mrn,7,'0')
join (select cpr1.pat_id, patient_race_c
        from clarity.patient_race cpr1 
        join ( select pat_id,  min(line) as line from clarity.patient_race 
        group by pat_id) cpr2 on cpr1.pat_id = cpr2.pat_id and cpr1.line = cpr2.line ) cpr      --remove mutiple race entries
        on cpr.pat_id = p.pat_id
join CLARITY.zc_patient_race czpr on cpr.patient_race_c = czpr.patient_race_c
join clarity.ZC_SEX czs on czs.RCPT_MEM_SEX_C = p.sex_c
join clarity.ZC_ETHNIC_GROUP czeg on p.ETHNIC_GROUP_C = czeg.ETHNIC_GROUP_C;
