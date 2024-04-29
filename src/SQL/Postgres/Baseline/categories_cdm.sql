/* Identify vCCC Patient demographic per category 
 ** Dependencies: denom_epic.sql 
 */
/*collect demographic information and classification*/
DROP TABLE denom_dem;

CREATE TABLE denom_dem AS
SELECT
    dem.*,
    coalesce(visit0_status::varchar, cat.prescreen_status::varchar) status,
    vref.prov_name
FROM
    pat_category cat
    JOIN pat_inclusion.static_valid_patients svp ON svp.fh_mrn = cat.mrn
    JOIN vccc_ref vref ON vref.patient_num = svp.pat_deid_short
    JOIN cdm60_deid_dataset.demographic dem ON dem.patid = svp.pat_deid_short;

SELECT
    *
FROM
    pat_category
    /*collect summary statistics for each category*/
    DROP TABLE stats_category;

CREATE TABLE stats_category (
    cnt_type varchar(40),
    cnt_type_cat varchar(40),
    pat_category varchar(40),
    pat_cnt integer
);

-- demographic breakdown
INSERT INTO stats_category
SELECT
    'by sex',
    SEX,
    status,
    count(DISTINCT PATID)
FROM
    denom_dem
GROUP BY
    SEX,
    status
UNION
SELECT
    'by race',
    RACE,
    status,
    count(DISTINCT PATID)
FROM
    denom_dem
GROUP BY
    RACE,
    status
UNION
SELECT
    'by provider',
    PROV_NAME,
    status,
    count(DISTINCT PATID)
FROM
    denom_dem
GROUP BY
    PROV_NAME,
    status
UNION
SELECT
    'all',
    'all',
    'all',
    count(DISTINCT PATID)
FROM
    denom_dem
UNION
SELECT
    'by category',
    status,
    'all',
    count(DISTINCT PATID)
FROM
    denom_dem
GROUP BY
    status
UNION
SELECT
    'by hispanic',
    HISPANIC,
    status,
    count(DISTINCT PATID)
FROM
    denom_dem
GROUP BY
    HISPANIC,
    status;

COMMIT;


/*eyeball the results*/
SELECT
    *
FROM
    stats_category;

