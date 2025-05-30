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

# remove invariant metrics
sdoh_nzv<- nearZeroVar(sdoh_cov, saveMetrics = TRUE)
sdoh_cov<-sdoh_cov[,row.names(sdoh_nzv)[!sdoh_nzv$zeroVar]]

# # quick imputation
# sdoh_cov_ruca<-sdoh_ruca %>%
#   select(all_of(c("PATID",paste0("RUCA_",1:10)))) %>%
#   left_join(sdoh_cov,by="PATID")
# init<-mice(sdoh_cov_ruca, maxit=0)
# predM<-init$predictorMatrix
# predM[,c("PATID")]=0
# sdoh_cov_imputed<-mice(sdoh_cov_ruca, m=1) # default: pmm
# sdoh_cov_imputed<-complete(sdoh_cov_imputed)

# 
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
