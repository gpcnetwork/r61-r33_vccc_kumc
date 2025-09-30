rm(list=ls()); gc()
setwd("C:/repos/r61-r33_vccc_kumc")

pacman::p_load(
  tidyverse,
  magrittr,
  stringr,
  broom
)

path_to_data<-"C:/repos/r61-r33_vccc_kumc/data"
path_to_res<-"C:/repos/r61-r33_vccc_kumc/res"

#==== enrolled set ====
baseline_aset<-readRDS(file.path(path_to_data,'baseline_aset.rda')) %>%
  # low-freq grouping
  group_by(STATE) %>% mutate(CNT = length(unique(STUDY_ID))) %>% mutate(STATE_GRP = case_when(CNT>100 ~ STATE, TRUE ~ 'OT')) %>%
  group_by(COUNTY) %>% mutate(CNT = length(unique(STUDY_ID))) %>% mutate(COUNTY_GRP = case_when(CNT>20 ~ COUNTY, TRUE ~ 'OT')) %>%
  group_by(DELTA_SBP_BY10) %>% mutate(CNT = length(unique(STUDY_ID))) %>% mutate(DELTA_SBP_BY10_GRP = case_when(CNT>100 ~ DELTA_SBP_BY10, DELTA_SBP_BY10<0 ~ -4, TRUE ~ 1)) %>%
  ungroup %>% dplyr::select(-CNT) %>%
  # add pre-defined features
  mutate(
    delta_sbp_over10 = case_when(
      abs(DELTA_SBP) > 10 ~ 1, 
      TRUE ~ 0
    ),
    delta_sbp_over10_sign = case_when(
      DELTA_SBP > 10 ~ '+1', 
      DELTA_SBP < -10 ~ '-1',
      TRUE ~ '0'
    ),
    delta_sbp_over10by10 = case_when(
      abs(DELTA_SBP) >10 & abs(DELTA_SBP) <= 20 ~ '2',
      abs(DELTA_SBP) >20 ~ '3',
      TRUE ~ '1'
    )
  ) %>%
  mutate(
    RACE_STR = case_when(
      RACE_STR %in% c('white','black','asian','aian','multi') ~ RACE_STR,
      TRUE ~ 'other'
    ),
    RACE_STR = fct_relevel(RACE_STR,'white'),
    ETHN_STR = case_when(
      ETHN_STR == "hispanic" ~ "hispanic",
      TRUE ~ 'non-hispanic'
    ),
    ETHN_STR = fct_relevel(ETHN_STR,'non-hispanic'),
    RACE_ETHN_STR = fct_relevel(RACE_ETHN_STR,'white')
  ) %>%
  mutate(
    LAB_EGFR2 = pmin(LAB_EGFR2,100)
  ) %>%
  mutate(
    AGE_75UP = case_when(
      AGE >= 75 ~ 1,
      TRUE ~ 0
    ),
    AGE_GRP = case_when(
      AGE < 70 & AGE>=65 ~ 'agegrp1',
      AGE < 75 & AGE>=70 ~ 'agegrp2',
      AGE >= 75 ~ 'agegrp3',
      TRUE ~ NA_character_
    ),
    SEX_STR = fct_relevel(SEX_STR, "Male"),
    RACE_STR = case_when(
      RACE_STR %in% c('white','black','asian','aian','multi') ~ RACE_STR,
      TRUE ~ 'other'
    ),
    RACE_STR = fct_relevel(RACE_STR,'white'),
    ETHN_STR = case_when(
      ETHN_STR == "hispanic" ~ "hispanic",
      TRUE ~ 'non-hispanic'
    ),
    ETHN_STR = fct_relevel(ETHN_STR,'non-hispanic'),
    RACE_ETHN_STR = fct_relevel(RACE_ETHN_STR,'white'),
    PAYER_TYPE_PRIMARY_GRP = case_when(PAYER_TYPE_PRIMARY_GRP=="other" ~ "medicare", TRUE ~ PAYER_TYPE_PRIMARY_GRP),
    PAYER_TYPE_PRIMARY_GRP  = fct_relevel(PAYER_TYPE_PRIMARY_GRP,'medicare'),
    BMI_GRP = case_when(
      BMI < 25 ~ 'bmigrp1',
      BMI>=25&BMI<30 ~ 'bmigrp2',
      BMI>=30&BMI<35 ~ 'bmigrp3',
      BMI>=35&BMI<40 ~ 'bmigrp4',
      BMI>=40 ~ 'bmigrp5',
      TRUE ~ 'bmigrp9'
    ),
    ELIG_SBP_GRP = case_when(
      ELIG_SBP>=140&ELIG_SBP<150 ~ 'sbpgrp1',
      ELIG_SBP>=150&ELIG_SBP<160 ~ 'sbpgrp2',
      ELIG_SBP>=160 ~ 'sbpgrp3',
      TRUE ~ NA_character_
    ),
    ELIG_DBP_GRP = case_when(
      ELIG_DBP<80 ~ 'dbpgrp1',
      ELIG_DBP>=80&ELIG_DBP<90 ~ 'dbpgrp2',
      ELIG_DBP>90 ~ 'dbpgrp3',
      TRUE ~ NA_character_
    ),
    MED_CNT_IN_GRP = case_when(
      MED_CNT_IN<3 ~ 'ahtcntgrp1',
      TRUE ~ 'ahtcntgrp2'
    ),
    LAB_EGFR2_GRP = case_when(
      LAB_EGFR2>=60 ~ 'egfrgrp1',
      LAB_EGFR2>=45&LAB_EGFR2<60 ~ 'egfrgrp2',
      LAB_EGFR2>=30&LAB_EGFR2<45 ~ 'egfrgrp3',
      LAB_EGFR2>=15&LAB_EGFR2<30 ~ 'egfrgrp4',
      LAB_EGFR2<15 ~ 'egfrgrp5',
      TRUE ~ 'egfrgrp9'
    ),
    LAB_URALBCR2_GRP = case_when(
      LAB_URALBCR2<30 ~ 'uralbcrgrp1',
      LAB_URALBCR2>=30&LAB_URALBCR2<300 ~ 'uralbcrgrp2',
      LAB_URALBCR2>=300 ~ 'uralbcrgrp3',
      TRUE ~ 'uralbcrgrp9'
    ),
    LAB_URPROTCR2_GRP = case_when(
      LAB_URPROTCR2<150 ~ 'urprotcrgrp1',
      LAB_URPROTCR2>=150&LAB_URPROTCR2<500 ~ 'urprotcrgrp2',
      LAB_URPROTCR2>=500 ~ 'urprotcrgrp3',
      TRUE ~ 'urprotcrgrp9'
    ),
    CKD_EGFR = case_when(
      LAB_EGFR2 < 60 ~ 1,
      TRUE ~ 0
    ),
    PU_URPROTCR = case_when(
      LAB_URPROTCR2 > 150 ~ 1,
      TRUE ~ 0
    ),
    ALBU_URALBCR = case_when(
      LAB_URALBCR2 > 30 ~ 1,
      TRUE ~ 0
    ),
    CKD_TEST_GRP = case_when(
      !is.na(LAB_EGFR2) & (!is.na(LAB_URALBCR2) | !is.na(LAB_URPROTCR2)) ~ 'Both',
      !is.na(LAB_EGFR2) & (is.na(LAB_URALBCR2) & is.na(LAB_URPROTCR2)) ~ 'eGFR',
      is.na(LAB_EGFR2) & (!is.na(LAB_URALBCR2) | !is.na(LAB_URPROTCR2)) ~ 'ACRPCR',
      TRUE ~ 'Neither'
    )
  )

saveRDS(baseline_aset,file=file.path(path_to_data,"baseline_aset_preproc.rda"))


#==== full pre-screened set =====
var_enrol<-c(
  "AGE",
  "AGE_75UP",
  "AGE_GRP",
  "SEX_STR",
  "RACE_STR",
  "ETHN_STR",
  "RACE_ETHN_STR",
  "BMI",
  "BMI_GRP",
  "ELIG_DBP",
  "ELIG_DBP_GRP",
  "ELIG_SBP",
  "ELIG_SBP_GRP",
  "MED_CNT_IN",
  "MED_CNT_IN_GRP",
  "SMOKER_IND",
  "LAB_EGFR2",
  "LAB_EGFR2_GRP",
  "LAB_URALBCR2",
  "LAB_URALBCR2_GRP",
  "LAB_URPROTCR2",
  "LAB_URPROTCR2_GRP",
  "CKD_EGFR",
  "EFI_DIA",
  "LAB_HBA1C",
  "EFI_STROKE",
  "EFI_CKD",
  "CCI_TOTAL",
  "CCI_TOTAL_GRP",
  "PAYER_TYPE_PRIMARY_GRP",
  "EFI_CAD",
  "SITE",
  "PU_URPROTCR",
  "ALBU_URALBCR",
  "CKD_TEST_GRP"
)
full_aset<-readRDS(file.path(path_to_data,"unenrol_aset.rda")) %>%
  mutate(
    LAB_EGFR2 = pmin(LAB_EGFR2,100)
  ) %>%
  mutate(
    AGE_75UP = case_when(
      AGE >= 75 ~ 1,
      TRUE ~ 0
    ),
    AGE_GRP = case_when(
      AGE < 70 & AGE>=65 ~ 'agegrp1',
      AGE < 75 & AGE>=70 ~ 'agegrp2',
      AGE >= 75 ~ 'agegrp3',
      TRUE ~ NA_character_
    ),
    SEX_STR = fct_relevel(SEX_STR, "Male"),
    RACE_STR = case_when(
      RACE_STR %in% c('white','black','asian','aian','multi') ~ RACE_STR,
      TRUE ~ 'other'
    ),
    RACE_STR = fct_relevel(RACE_STR,'white'),
    ETHN_STR = case_when(
      ETHN_STR == "hispanic" ~ "hispanic",
      TRUE ~ 'non-hispanic'
    ),
    ETHN_STR = fct_relevel(ETHN_STR,'non-hispanic'),
    RACE_ETHN_STR = fct_relevel(RACE_ETHN_STR,'white'),
    PAYER_TYPE_PRIMARY_GRP = case_when(PAYER_TYPE_PRIMARY_GRP=="other" ~ "medicare", TRUE ~ PAYER_TYPE_PRIMARY_GRP),
    PAYER_TYPE_PRIMARY_GRP  = fct_relevel(PAYER_TYPE_PRIMARY_GRP,'medicare'),
    BMI_GRP = case_when(
      BMI < 25 ~ 'bmigrp1',
      BMI>=25&BMI<30 ~ 'bmigrp2',
      BMI>=30&BMI<35 ~ 'bmigrp3',
      BMI>=35&BMI<40 ~ 'bmigrp4',
      BMI>=40 ~ 'bmigrp5',
      TRUE ~ 'bmigrp9'
    ),
    ELIG_SBP_GRP = case_when(
      ELIG_SBP>=140&ELIG_SBP<150 ~ 'sbpgrp1',
      ELIG_SBP>=150&ELIG_SBP<160 ~ 'sbpgrp2',
      ELIG_SBP>=160 ~ 'sbpgrp3',
      TRUE ~ NA_character_
    ),
    ELIG_DBP_GRP = case_when(
      ELIG_DBP<80 ~ 'dbpgrp1',
      ELIG_DBP>=80&ELIG_DBP<90 ~ 'dbpgrp2',
      ELIG_DBP>90 ~ 'dbpgrp3',
      TRUE ~ NA_character_
    ),
    MED_CNT_IN_GRP = case_when(
      MED_CNT_IN<3 ~ 'ahtcntgrp1',
      TRUE ~ 'ahtcntgrp2'
    ),
    LAB_EGFR2_GRP = case_when(
      LAB_EGFR2>=60 ~ 'egfrgrp1',
      LAB_EGFR2>=45&LAB_EGFR2<60 ~ 'egfrgrp2',
      LAB_EGFR2>=30&LAB_EGFR2<45 ~ 'egfrgrp3',
      LAB_EGFR2>=15&LAB_EGFR2<30 ~ 'egfrgrp4',
      LAB_EGFR2<15 ~ 'egfrgrp5',
      TRUE ~ 'egfrgrp9'
    ),
    LAB_URALBCR2_GRP = case_when(
      LAB_URALBCR2<30 ~ 'uralbcrgrp1',
      LAB_URALBCR2>=30&LAB_URALBCR2<300 ~ 'uralbcrgrp2',
      LAB_URALBCR2>=300 ~ 'uralbcrgrp3',
      TRUE ~ 'uralbcrgrp9'
    ),
    LAB_URPROTCR2_GRP = case_when(
      LAB_URPROTCR2<150 ~ 'urprotcrgrp1',
      LAB_URPROTCR2>=150&LAB_URPROTCR2<500 ~ 'urprotcrgrp2',
      LAB_URPROTCR2>=500 ~ 'urprotcrgrp3',
      TRUE ~ 'urprotcrgrp9'
    ),
    CKD_EGFR = case_when(
      LAB_EGFR2 < 60 ~ 1,
      TRUE ~ 0
    ),
    PU_URPROTCR = case_when(
      LAB_URPROTCR2 > 150 ~ 1,
      TRUE ~ 0
    ),
    ALBU_URALBCR = case_when(
      LAB_URALBCR2 > 30 ~ 1,
      TRUE ~ 0
    ),
    CKD_TEST_GRP = case_when(
      !is.na(LAB_EGFR2) & (!is.na(LAB_URALBCR2) | !is.na(LAB_URPROTCR2)) ~ 'Both',
      !is.na(LAB_EGFR2) & (is.na(LAB_URALBCR2) & is.na(LAB_URPROTCR2)) ~ 'eGFR',
      is.na(LAB_EGFR2) & (!is.na(LAB_URALBCR2) | !is.na(LAB_URPROTCR2)) ~ 'ACRPCR',
      TRUE ~ 'Neither'
    )
  ) %>%
  dplyr::select(!!c("PATID","PRESCREEN_STATUS",var_enrol)) %>%
  bind_rows(
    readRDS(file.path(path_to_data,"baseline_aset_preproc.rda")) %>% 
      mutate(PRESCREEN_STATUS = "study") %>%
      dplyr::select(!!c("PATID","PRESCREEN_STATUS",var_enrol))
  ) %>%
  mutate(
    ENROL_IND = case_when(
      PRESCREEN_STATUS=="study" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(!is.na(ELIG_SBP)&!is.na(AGE_GRP))

saveRDS(full_aset,file=file.path(path_to_data,"full_aset_preproc.rda"))


# remove invariant metrics
# sdoh_nzv<- nearZeroVar(sdoh_cov, saveMetrics = TRUE)
# sdoh_cov<-sdoh_cov[,row.names(sdoh_nzv)[!sdoh_nzv$zeroVar]]

# # quick imputation
# sdoh_cov_ruca<-sdoh_ruca %>%
#   select(all_of(c("PATID",paste0("RUCA_",1:10)))) %>%
#   left_join(sdoh_cov,by="PATID")
# init<-mice(sdoh_cov_ruca, maxit=0)
# predM<-init$predictorMatrix
# predM[,c("PATID")]=0
# sdoh_cov_imputed<-mice(sdoh_cov_ruca, m=1) # default: pmm
# sdoh_cov_imputed<-complete(sdoh_cov_imputed)

# base_bp<-readRDS(file.path(path_to_data,"tcog_sdh.rda")) %>% unique
# meds<-readRDS(file.path(path_to_data,"meds_long.rda"))
# 
# # wide anti-htn med table
# path_to_file<-file.path(path_to_data,"antihtn_t.rda")
# if(!file.exists(path_to_file)){
#   antihtn<-meds %>% 
#     filter(ANTIHTN_IND==1) %>%
#     filter(RX_START_SINCE_INDEX<=0) %>%
#     mutate(
#       runin_cont = case_when(
#         RX_START_SINCE_INDEX<=ELIG_SINCE_INDEX&RX_END_SINCE_INDEX<=ELIG_SINCE_INDEX ~ 1,
#         TRUE ~ 0
#       )
#     )
#   
#   antihtn_t_dose<-antihtn %>%
#     group_by(STUDY_ID,IN_OR_NAME,RX_TIMING) %>%
#     summarise(dose_mean = mean(RX_DOSE,na.rm=T),.groups = 'drop') %>%
#     filter(!(dose_mean==-Inf)) %>%
#     group_by(STUDY_ID,IN_OR_NAME) %>%
#     reframe(dose_inc = case_when(
#       dose_mean[RX_TIMING=='runin']>dose_mean[RX_TIMING=='bef'] ~ 1
#     )) %>%
#     filter(!is.na(dose_inc)) %>%
#     pivot_wider(
#       names_from = 'IN_OR_NAME',
#       values_from = 'dose_inc',
#       values_fill = 0,
#       names_glue = "{IN_OR_NAME}_does_inc"
#     ) %>%
#     mutate(antihtn_dose_inc = 1)
#   
#   antihtn_t_stk<-antihtn %>%
#     select(STUDY_ID,IN_OR_NAME,RX_TIMING) %>%
#     unique %>% mutate(ind=1) %>%
#     rename(name=IN_OR_NAME) %>%
#     bind_rows(
#       antihtn %>%
#         select(STUDY_ID,VA_CLS_CD,RX_TIMING,runin_cont) %>%
#         unique %>% mutate(ind=1) %>%
#         rename(name=VA_CLS_CD)
#     ) %>%
#     pivot_wider(
#       names_from = 'RX_TIMING',
#       values_from = 'ind',
#       values_fill = 0
#     ) %>%
#     mutate(
#       use_cat = case_when(
#         bef==1&runin==1 ~ 'maintain',
#         bef==1&runin_cont==1 ~ 'maintain',
#         bef==1&(runin==0|runin_cont==0) ~ 'runin_stop',
#         bef==0&runin==1 ~ 'runin_start'
#       ),
#       ind=1
#     ) %>%
#     unite('name_use',c("name","use_cat"),sep="_") %>%
#     select(STUDY_ID,name_use,ind) %>% unique %>%
#     pivot_wider(
#       names_from = 'name_use',
#       values_from = 'ind',
#       values_fill = 0
#     )
#   
#   antihtn_t_add_med<-base_bp %>%
#     select(STUDY_ID) %>% unique %>%
#     left_join(
#       antihtn %>%
#         filter(RX_START_SINCE_INDEX-ELIG_SINCE_INDEX >= -90) %>%
#         select(STUDY_ID,RX_TIMING,IN_OR_NAME,runin_cont) %>%
#         unique %>% 
#         group_by(STUDY_ID,RX_TIMING) %>%
#         summarise(
#           in_cnt = length(unique(IN_OR_NAME)),
#           runin_cont_cnt = length(unique(IN_OR_NAME[runin_cont==1])),
#           .groups = "drop"
#         ) %>%
#         pivot_wider(
#           names_from = 'RX_TIMING',
#           values_from = 'in_cnt',
#           values_fill = 0
#         ) %>%
#         left_join(
#           antihtn_t_dose %>%
#             select(STUDY_ID) %>%
#             mutate(
#               antihtn_doseup = 1
#             ) %>% unique,
#           by = "STUDY_ID"
#         ) %>%
#         mutate(
#           antihtn_delta_med = case_when(
#             bef == 0 & runin == 0 ~ 'untreated',
#             bef == 0 & runin > 0 ~ 'antihtn_runin',
#             (runin_cont_cnt == 0 & runin > 0) | antihtn_doseup == 1 ~ 'antihtn_add',
#             runin > 0 ~ 'antihtn_change',
#             bef - runin_cont_cnt > 0 & runin == 0 ~ 'antihtn_rm',
#             TRUE ~ 'antihtn_mt'
#           )
#         ),
#       by="STUDY_ID"
#     ) %>%
#     replace_na(list(antihtn_delta_med='untreated')) %>%
#     select(STUDY_ID,antihtn_delta_med) %>% 
#     unique %>% 
#     mutate(antihtn_delta_med2 = antihtn_delta_med, ind = 1) %>%
#     pivot_wider(
#       names_from = 'antihtn_delta_med2',
#       values_from = 'ind',
#       values_fill = 0
#     )
#     
#    antihtn_t<-base_bp %>% 
#      select(STUDY_ID) %>% unique %>%
#      left_join(antihtn_t_add_med,by="STUDY_ID") %>%
#      left_join(antihtn_t_stk,by="STUDY_ID") %>%
#      left_join(antihtn_t_dose,by="STUDY_ID") %>%
#      replace(is.na(. ), 0)
#    
#   # intermediate table    
#   saveRDS(antihtn_t,file=path_to_file)
# }else{
#   antihtn_t<-readRDS(path_to_file)
# }
# 
# # baseline aset
# path_to_file<-file.path(path_to_data,'baseline_aset.rda')
# if(!file.exists(path_to_file)){
#   antihtn_t<-readRDS(file.path(path_to_data,"antihtn_t.rda"))
#   baseline_aset<-base_bp %>%
#     mutate(INDEX_DATE = ENROLL_DATE) %>%
#     select(
#       STUDY_ID,SEX,SEX_STR,RACE,RACE_STR,AGE,ETHNICITY,ETHN_STR,INDEX_DATE,SITE,
#       BASE_SBP,DELTA_SBP,ELIG_SINCE_INDEX,DELTA_SBP_GROUP,ESBP_GROUP,BSBP_GROUP
#     ) %>%
#     unique %>%
#     mutate(decr_ind = case_when(DELTA_SBP<0 ~ 1, TRUE ~ 0)) %>%
#     mutate(
#       sig_decr_ind1 = as.numeric(DELTA_SBP_GROUP %in% c('deltasbp1')),
#       sig_decr_ind2 = as.numeric(DELTA_SBP_GROUP %in% c('deltasbp1','deltasbp2'))
#     ) %>%
#     inner_join(antihtn_t,by = "STUDY_ID") %>%
#     filter(!is.na(SEX)&!is.na(AGE))
#   
#   # save intermediate table
#   saveRDS(baseline_aset,file=path_to_file)
# }else{
#   baseline_aset<-readRDS(path_to_file)
# }
# 
