----------------------------------------------------
----------------------- Pcornet Trial --------------
----------------------------------------------------
DROP TABLE IF EXISTS vccc_map_mrn_studyid;

CREATE TABLE vccc_map_mrn_studyid AS
SELECT
    study_id,
    max(mrn) AS mrn
FROM
    vccc_enrollment_status ves
GROUP BY
    study_id;

-- vccc_enrollment_status is a redcap report
DROP TABLE IF EXISTS vccc_pcornet_trial;

CREATE TABLE vccc_pcornet_trial AS
SELECT
    *
FROM
    cdm60_deid_dataset.pcornet_trial pt
WHERE
    1 = 0;

INSERT INTO vccc_pcornet_trial
SELECT
    svp.pat_deid_short AS patid,
    'VCCC' AS trialid,
    mms.study_id,
    'D6' AS trial_siteid,
    es1.disp_date::date AS trial_enroll_date,
    NULL AS trial_end_date,
    es2.disp_date::date AS trial_withdraw_date,
    NULL trial_invite_code
FROM
    vccc_map_mrn_studyid mms
    JOIN fh_clarity_etl_src.patient p ON p.pat_mrn_id = lpad(mms.mrn::varchar, 7, '0')
    JOIN pat_inclusion.static_valid_patients svp ON svp.pat_id = p.pat_id
    JOIN (
        SELECT
            *
        FROM
            vccc_enrollment_status
        WHERE
            disp_status = 1
            AND study_id NOT IN (
                SELECT
                    study_id
                FROM
                    vccc_enrollment_status
                WHERE
                    disp_status > 2)) es1 ON es1.study_id = mms.study_id
    LEFT JOIN (
        SELECT
            *
        FROM
            vccc_enrollment_status
        WHERE
            disp_status = 2) es2 ON es2.study_id = mms.study_id
WHERE
    mms.study_id LIKE 'KU-%';

COMMIT;

