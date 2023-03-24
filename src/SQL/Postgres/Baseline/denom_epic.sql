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
- replace fh_clarity_etl_src with your clarity schema
 */
--clarity key table refresh date
SELECT
      max(last_analyze)
FROM
      pg_stat_all_tables
WHERE
      schemaname = 'fh_clarity_etl_src'
      AND relname = 'pat_enc';


/*collect demonitor patients: 
 - has a PCP visit with vCCC PCPs
 - the PCP visit happened within study period
 - age at visit >= 65
 */
-- collect VCCC PCPs (name list or PROV_ID is required)
SELECT
      *
FROM
      npi;

DROP TABLE vccc_pcp;

CREATE TABLE vccc_pcp AS
SELECT
      n.npi,
      n.last,
      n.first,
      n.start_dt,
      cs.prov_name, -- keep to verify if they are the same provider
      cs.PROV_ID
      --      ,cs.prov_start_date        -- could add more provide-level information to this table for downstream analysis
      --      ,doctors_degree
FROM
      npi n
      JOIN fh_clarity_etl_src.clarity_ser_2 cs2 ON n.npi::varchar = cs2.npi
      JOIN fh_clarity_etl_src.clarity_ser cs ON cs2.PROV_ID = cs.PROV_ID;

-- collect patients who have at least 1 PCP visit within the enrolling time period
DROP TABLE vccc_ref;

CREATE TABLE vccc_ref AS
SELECT
      dense_rank() OVER (ORDER BY pe.pat_id) PATID, -- de-identification
      svp.pat_deid_short patient_num, -- CDM patient_num
      pcp.prov_name,
      pcp.npi,
      --      ,pcp.doctors_degree
      --      ,pcp.prov_start_date
      round(DATE_PART('day', pe.CONTACT_DATE - pt.BIRTH_DATE) / 365.25) AGE_AT_VIS,
      CASE WHEN pt.SEX_C = '1' THEN
            'F'
      WHEN pt.SEX_C = '2' THEN
            'M'
      ELSE
            'NI'
      END AS SEX,
      CASE WHEN ptr.patient_race_c IS NULL THEN
            '@'
      WHEN czpr.abbr IS NULL THEN
            'other'
      ELSE
            lower(czpr.abbr)
      END RACE,
      pe.BP_SYSTOLIC,
      pe.CONTACT_DATE,
      enc.disp_enc_type_c
FROM
      fh_clarity_etl_src.pat_enc pe
      JOIN vccc_pcp pcp ON pcp.prov_id = pe.PCP_PROV_ID
      LEFT JOIN fh_clarity_etl_src.patient pt ON pe.PAT_ID = pt.PAT_ID
      JOIN pat_inclusion.static_valid_patients svp ON svp.fh_mrn = pt.pat_mrn_id
      LEFT JOIN fh_clarity_etl_src.patient_race ptr ON ptr.PAT_ID = pe.PAT_ID
      LEFT JOIN fh_clarity_etl_src.zc_patient_race czpr ON ptr.patient_race_c = czpr.patient_race_c
      LEFT JOIN fh_clarity_etl_src.CLARITY_DEP dep ON dep.DEPARTMENT_ID = pe.DEPARTMENT_ID
      LEFT JOIN fh_clarity_etl_src.ZC_DISP_ENC_TYPE enc ON pe.enc_type_c = enc.disp_enc_type_c
      -- increase specifity, given that a PCP_PROV_ID may be documented even for a non-PCP visit
WHERE (dep.DEPARTMENT_NAME LIKE '%FAMILY%'
      OR dep.DEPARTMENT_NAME LIKE '%IM%CL%')
-- only patients seen by the vCCC PCP within the study period can be counted towards the denominator
AND pcp.START_DT IS NOT NULL
AND pe.CONTACT_DATE >= TO_TIMESTAMP( pcp.START_DT,'YYYY-MM-DD')
-- age at visit >= 65
AND round(DATE_PART('day', pe.CONTACT_DATE - pt.BIRTH_DATE) / 365.25) >= 65
-- PCP provider ID is the same as attending physician ID
AND pe.visit_prov_id = pe.PCP_PROV_ID
AND enc.disp_enc_type_c IN ('101', '76') -- only include Office Visit (101) and Telemedicine (76)
;


/*collect summary statistics (no cell-size suppression)*/
DROP TABLE stats_tbl;

CREATE TABLE stats_tbl (
      cnt_type varchar(40),
      cnt_type_cat varchar(40),
      pat_cnt integer
);

-- all
INSERT INTO stats_tbl
SELECT
      'all',
      'all',
      count(DISTINCT PATID)
FROM
      vccc_ref;

-- breakdown by PCP provider
INSERT INTO stats_tbl
SELECT
      'by provider',
      PROV_NAME,
      count(DISTINCT PATID)
FROM
      vccc_ref
GROUP BY
      PROV_NAME;

-- demographic breakdown
INSERT INTO stats_tbl
SELECT
      'by sex',
      SEX,
      count(DISTINCT PATID)
FROM
      vccc_ref
GROUP BY
      SEX
UNION
SELECT
      'by race',
      RACE,
      count(DISTINCT PATID)
FROM
      vccc_ref
GROUP BY
      RACE
      -- could add more as needed
;

COMMIT;


/*eyeball the results*/
SELECT
      *
FROM
      stats_tbl;

