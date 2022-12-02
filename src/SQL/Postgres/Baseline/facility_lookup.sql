/*
 * Identify clinic locations 
 */
DROP TABLE facility_lookup;

CREATE TABLE facility_lookup AS SELECT DISTINCT
    dep.department_id,
    department_name,
    dep.RPT_GRP_EIGHTEEN_C,
    zdr.NAME
FROM
    fh_clarity_etl_src.clarity_dep dep
    LEFT JOIN fh_clarity_etl_src.ZC_DEP_RPT_GRP_18 zdr ON zdr.RPT_GRP_EIGHTEEN_C = dep.rpt_grp_eighteen_c
    LEFT JOIN fh_clarity_etl_src.CLARITY_SER_DEPT csd ON csd.DEPARTMENT_ID = dep.DEPARTMENT_ID
WHERE
    dep.RPT_GRP_EIGHTEEN_C IS NOT NULL;

