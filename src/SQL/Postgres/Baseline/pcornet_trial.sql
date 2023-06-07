----------------------------------------------------
----------------------- Pcornet Trial --------------
----------------------------------------------------

create table vccc_map_mrn_studyid
as select study_id, max(mrn) as mrn
from vccc_enrollment_status ves 
group by study_id  ; -- vccc_enrollment_status is a redcap report

drop table if exists  vccc_pcornet_trial;

create table vccc_pcornet_trial as select *
from cdm60_deid_dataset.pcornet_trial pt
where 1=0;

insert into vccc_pcornet_trial
select svp.pat_deid_short as patid, 'VCCC' as trialid, mms.study_id ,'D6' as trial_siteid,es1.disp_date::date as trial_enroll_date, 
null as trial_end_date, es2.disp_date::date as trial_withdraw_date , null trial_invite_code
from vccc_map_mrn_studyid mms
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(mms.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id
join (select * from vccc_enrollment_status  where disp_status = 1 and study_id not in 
(select study_id  from vccc_enrollment_status where disp_status >2 )) es1 on es1.study_id = mms.study_id 
left join (select * from vccc_enrollment_status  where disp_status = 2) es2 on es2.study_id = mms.study_id ;

commit;
