/*
 * Identify clinic locations and find thier stats to identify diverse clinics
 */
DROP TABLE vccc_facility;

CREATE TABLE vccc_facility AS SELECT DISTINCT
    dep.department_id,
    department_name,
    dep.RPT_GRP_EIGHTEEN_C,
    zdr.NAME --, csd.PROV_ID
FROM
    fh_clarity_etl_src.clarity_dep dep
    LEFT JOIN fh_clarity_etl_src.ZC_DEP_RPT_GRP_18 zdr ON zdr.RPT_GRP_EIGHTEEN_C = dep.rpt_grp_eighteen_c
    LEFT JOIN fh_clarity_etl_src.CLARITY_SER_DEPT csd ON csd.DEPARTMENT_ID = dep.DEPARTMENT_ID
    JOIN vccc.vccc_pcp pcp ON pcp.prov_id = csd.prov_id
WHERE
    department_name NOT LIKE 'ZZ%'
    AND department_name NOT LIKE 'ZC%'
    AND department_name NOT LIKE 'ZX%'
    AND dep.RPT_GRP_EIGHTEEN_C IS NOT NULL;


/* find patients who visited these clinics in the last year and are eligible (age>65)*/
DROP TABLE facility_pat;

CREATE TABLE facility_pat AS
SELECT
    d.patid,
    vf.department_id,
    vf.department_name,
    vf.name
FROM
    cdm60_deid_dataset.encounter e
    JOIN vccc.vccc_facility vf ON vf.department_id::varchar = e.facilityid
    JOIN cdm60_deid_dataset.demographic d ON d.patid = e.patid
WHERE
    round(DATE_PART('day', CURRENT_DATE - d.BIRTH_DATE) / 365.25) >= 65 -- consider eligible patient >65
    AND admit_date >= CURRENT_DATE - interval '1 year' -- include patients from last year encounters
GROUP BY
    d.patid,
    vf.department_id,
    vf.department_name,
    vf.name;


/* fing their demographic info (race, age, sex) */
DROP TABLE facility_pat_info;

CREATE TABLE facility_pat_info AS
SELECT
    fp.*,
    d.race,
    d.sex,
    d.hispanic,
    d.birth_date,
    round(DATE_PART('day', CURRENT_DATE - d.BIRTH_DATE) / 365.25) pat_age
FROM
    facility_pat fp
    LEFT JOIN cdm60_deid_dataset.demographic d ON fp.patid = d.patid
    /* find average age for each clinic on patient level */
    SELECT
        department_name,
        min(pat_age),
        max(pat_age),
        avg(pat_age),
        count(*)
    FROM
        fac_pat_info
    GROUP BY
        department_name;


/* Find Hispanic Info per clinic*/
SELECT
    department_name,
    hispanic,
    count(*)
FROM
    facility_stats
GROUP BY
    department_name,
    hispanic;


/* Find sex distribution per clinic */
SELECT
    department_name,
    sex,
    count(*)
FROM
    facility_stats
GROUP BY
    department_name,
    sex;


/* find race stats each clinic on patient level */
DROP TABLE race_percentage;

CREATE TABLE race_percentage AS
SELECT
    f1.department_name,
    f1.race,
    count(*)::float / (f2.counts) * 100 percentage,
    count(*),
    f2.counts
FROM
    fac_pat_info f1
    JOIN (
        SELECT
            department_name,
            count(*) AS counts
        FROM
            fac_pat_info
        GROUP BY
            department_name) f2 ON f1.department_name = f2.department_name
GROUP BY
    f1.department_name,
    f1.race,
    f2.counts
HAVING
    f2.counts >= 10 --excludes clinics with less than 10 patients over the last year
ORDER BY
    f1.department_name,
    race;


/* map race values with description using CDM v60 Data Dictionary*/
CREATE TABLE race_map AS
SELECT
    '01' AS value,
    'American Indian or Alaska Native' AS des
UNION
SELECT
    '02',
    'Asian'
UNION
SELECT
    '03',
    'Black or African American'
UNION
SELECT
    '04',
    'Native Hawaiian or Other Pacific Islander'
UNION
SELECT
    '05',
    'White'
UNION
SELECT
    'OT',
    'Other'
UNION
SELECT
    'NS',
    'Not specified';


/* map race values with description using CDM v60 Data Dictionary*/
CREATE TABLE hispanic_map AS
SELECT
    'Y' AS value,
    'Yes' AS des
UNION
SELECT
    'N',
    'No'
UNION
SELECT
    'R',
    'Refuse to answer'
UNION
SELECT
    'NI',
    'No information'
UNION
SELECT
    'UN',
    'Unknown'
UNION
SELECT
    'OT',
    'Other';


/* Find diverse clinics */
DROP TABLE div_clinics;

CREATE TABLE div_clinics AS
SELECT
    department_name
FROM
    race_percentage
GROUP BY
    department_name,
    race,
    percentage,
    counts
HAVING
    race = '05'
    AND percentage <= 80 -- look up for diverse clinics
    /* Find race distribution for diverse clinics */
    SELECT
        rp.*,
        rm.des
    FROM
        race_percentage rp
    LEFT JOIN race_map rm ON rm.value = rp.race
WHERE
    department_name IN (
        SELECT
            *
        FROM
            div_clinics
        ORDER BY
            percentage ASC)
ORDER BY
    department_name,
    race,
    percentage;


/* Find hispanic distribution for diverse clinics */
SELECT
    department_name,
    hispanic,
    hm.des,
    count(*)
FROM
    facility_stats fs
    LEFT JOIN hispanic_map hm ON hm.value = fs.hispanic
WHERE
    department_name IN (
        SELECT
            *
        FROM
            div_clinics)
GROUP BY
    department_name,
    hispanic,
    hm.des
ORDER BY
    department_name,
    hispanic;

