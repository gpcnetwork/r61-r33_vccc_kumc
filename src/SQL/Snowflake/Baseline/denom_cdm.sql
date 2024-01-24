/*
Key CDM tables:
1. ENCOUNTER
2. DEMOGRAPHIC
3. PROVIDER

Auxillary tables: 
1. NPPES data and metadata
*/ 

set 


/*collect all patients with at least 1 SBP record and calculate age at measurement*/
create or replace table BP_Cohort as
-- SBP and DBP from VITAL table
with multi_cte as (
    select * from
    (select p.PATID
           ,v.ENCOUNTERID
           ,v.SYSTOLIC
           ,v.DIASTOLIC
           ,v.MEASURE_DATE
           ,round((v.MEASURE_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
           ,'VITAL' as SRC_TABLE
     from identifier($VITAL) v 
     join identifier($DEMOGRAPHIC) p on v.PATID = p.PATID 
     where v.SYSTOLIC is not null
    )
    unpivot (
       VITAL_VAL for VITAL_TYPE in (SYSTOLIC, DIASTOLIC)
    )
    union all
    -- SBP from OBS_CLIN table
    select p.PATID
          ,os.ENCOUNTERID
          ,os.OBSCLIN_START_DATE
          ,round((os.OBSCLIN_START_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
          ,'OBS_CLIN' as SRC_TABLE
          ,'SYSTOLIC' as VITAL_TYPE
          ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
    from identifier($OBS_CLIN) os
    join identifier($DEMOGRAPHIC) p on os.PATID = p.PATID and
         -- os.OBSCLIN_TYPE = 'LC' and 
         os.OBSCLIN_CODE in ( '8460-8' --standing
                             ,'8459-0' --sitting
                             ,'8461-6' --supine
                             ,'8479-8' --palpation
                             ,'8480-6' --general
                            )
    union all
    -- DBP from OBS_CLIN table
    select p.PATID
          ,os.ENCOUNTERID
          ,os.OBSCLIN_START_DATE
          ,round((os.OBSCLIN_START_DATE - p.BIRTH_DATE)/365.25) as AGE_AT_MEASURE
          ,'OBS_CLIN' as SRC_TABLE
          ,'DIASTOLIC' as VITAL_TYPE
          ,os.OBSCLIN_RESULT_NUM as VITAL_VAL
    from identifier($OBS_CLIN) os
    join identifier($DEMOGRAPHIC) p on os.PATID = p.PATID and
         -- os.OBSCLIN_TYPE = 'LC' and 
         os.OBSCLIN_CODE in ( '8454-1' --standing
                             ,'8453-3' --sitting
                             ,'8455-8' --supine
                             ,'8462-4' --general
                            )
)
select distinct 
       PATID,
       ENCOUNTERID,
       AGE_AT_MEASURE,
       MEASURE_DATE,
       "'SYSTOLIC'" as SYSTOLIC,
       "'DIASTOLIC'" as DIASTOLIC
from multi_cte
    pivot (max(VITAL_VAL) for VITAL_TYPE in ('SYSTOLIC','DIASTOLIC'))
        as p
where AGE_AT_MEASURE >= 65
;

