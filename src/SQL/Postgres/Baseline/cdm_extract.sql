/* Identify vCCC Patient Cohort and extract CDM data*/

drop table if exists vccc_cohort ;

create table vccc_cohort AS 
select distinct mrn from vccc_cohort_6_23;	--vccc_cohort_6_23 contains a list of vCCC participants;

----------------------------------------------------
----------------- Demographic ----------------------
----------------------------------------------------
drop table if exists  vccc_demographic;

create table vccc_demographic as
select d.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.demographic d on d.patid  = svp.pat_deid_short;

----------------------------------------------------
----------------- ENCOUNTER ----------------------
----------------------------------------------------

drop table if exists  vccc_encounter;

create table vccc_encounter as
select e.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.encounter e on e.patid  = svp.pat_deid_short;

----------------------------------------------------
----------------------- Vital ----------------------
----------------------------------------------------

drop table if exists  vccc_vital;

create table vccc_vital as
select v.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.vital v on v.patid  = svp.pat_deid_short;

----------------------------------------------------
----------------------- Med_admin ------------------
----------------------------------------------------

drop table if exists  vccc_med_admin;

create table vccc_med_admin as
select m.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.med_admin m on m.patid  = svp.pat_deid_short;

----------------------------------------------------
----------------------- Lab_result_cm --------------
----------------------------------------------------

drop table if exists  vccc_lab_result_cm;

create table vccc_lab_result_cm as
select l.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.lab_result_cm l on l.patid  = svp.pat_deid_short;

----------------------------------------------------
----------------------- Diagnosis ------------------
----------------------------------------------------

drop table if exists  vccc_diagnosis;

create table vccc_diagnosis as
select d.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.diagnosis d on d.patid  = svp.pat_deid_short;

----------------------------------------------------
--------------------- Prescribing ------------------
----------------------------------------------------

drop table if exists  vccc_prescribing;

create table vccc_prescribing as
select pr.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.prescribing pr on pr.patid  = svp.pat_deid_short;

----------------------------------------------------
--------------------- Dispensing ------------------
----------------------------------------------------

drop table if exists  vccc_dispensing;

create table vccc_dispensing as
select d.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.dispensing d on d.patid  = svp.pat_deid_short;

----------------------------------------------------
--------------------- OBS_CLIN ------------------
----------------------------------------------------

drop table if exists  vccc_obs_clin;

create table vccc_obs_clin as
select o.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.obs_clin o on o.patid  = svp.pat_deid_short;

----------------------------------------------------
--------------------- PROCEDURES ------------------
----------------------------------------------------

drop table if exists  vccc_procedures;

create table vccc_procedures as
select pr.*
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id 
join cdm60_deid_dataset.procedures pr on pr.patid  = svp.pat_deid_short;

----------------------------------------------------
--------------------- VCCC_mapping ------------------
----------------------------------------------------

drop table if exists  vccc_mapping;

create table vccc_mapping as
select vc.mrn, svp.pat_deid_short
from vccc_cohort vc
join fh_clarity_etl_src.patient p on p.pat_mrn_id  = lpad(vc.mrn::varchar,7,'0') 
join pat_inclusion.static_valid_patients svp on svp.pat_id = p.pat_id;

