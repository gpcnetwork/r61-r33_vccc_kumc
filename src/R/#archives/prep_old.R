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

# wide BP table
path_to_file<-file.path(root_dir,"data","bp.rda")
if(!file.exists(path_to_file)){
  enc_excld<-read.csv(file.path(root_dir,"private","CDM","encounter_limited.csv"), stringsAsFactors = F) %>%
    filter(enc_type %in% c("IP","EI","ED","IS"))
  
  bp<-read.csv(file.path(root_dir,"private","REDCap","BloodPressure.csv"), stringsAsFactors = F) %>%
    select(study_id,redcap_event_name,avg_calculated_sbp,avg_calculated_dbp,visit_bp_datetime_m1) %>%
    filter(!is.na(avg_calculated_sbp)) %>%
    rename(
      event_name = redcap_event_name,
      sbp = avg_calculated_sbp, 
      dbp = avg_calculated_dbp,
      date = visit_bp_datetime_m1
    ) %>%
    bind_rows(
      read.csv(file.path(root_dir,"private","REDCap","qardio_data_2024-02-12.csv"),stringsAsFactors = F) %>% 
        select(study_id,redcap_event_name,bp_date,sys,dia) %>%
        rename(
          event_name = redcap_event_name,
          sbp = sys, 
          dbp = dia,
          date = bp_date
        )
    ) %>%
    bind_rows(
      read.csv(file.path(root_dir,"private","REDCap","ClinicBloodPressure.csv"), stringsAsFactors = F) %>%
        select(study_id,elig_sbp1,elig_dbp1,elig_sbp1_date) %>%
        mutate(event_name = 'elig_2') %>%
        rename(
          sbp = elig_sbp1, 
          dbp = elig_dbp1,
          date = elig_sbp1_date
        )
    ) %>%
    bind_rows(
      read.csv(file.path(root_dir,"private","REDCap","ClinicBloodPressure.csv"), stringsAsFactors = F) %>%
        select(study_id,elig_sbp2,elig_dbp1,elig_sbp2_date) %>%
        mutate(event_name = 'elig_1') %>%
        rename(
          sbp = elig_sbp2,
          dbp = elig_dbp1,
          date = elig_sbp2_date
        )
    ) %>%
    mutate(date = as.Date(date)) %>%
    arrange(study_id, date) %>%
    filter(!is.na(sbp)) %>%
    separate(event_name, into = c("type","idx"),extra="drop") %>%
    mutate(idx = case_when(idx=="arm" ~ "0",
                           TRUE ~ idx)) %>%
    mutate(idx = as.integer(idx)) %>%
    bind_rows(
      read.csv(file.path(root_dir,"private","CDM","vital_limited.csv"), stringsAsFactors = F) %>%
        anti_join(enc_excld, by=c("patid","encounterid")) %>%
        select(participantid,systolic,diastolic,measure_date) %>%
        rename(
          study_id = participantid,
          sbp = systolic,
          dbp = diastolic,
          date = measure_date
        ) %>%
        mutate(
          date = as.Date(date),
          type = "clinic"
        ) %>%
        group_by(study_id) %>%
        mutate(idx = rank(date)) %>% 
        ungroup
    ) %>%
    group_by(study_id) %>%
    filter(any(type=='baseline')) %>%
    mutate(index_date = date[type=='baseline']) %>%
    ungroup %>%
    mutate(days_since_index = as.numeric(date - index_date)) %>%
    group_by(study_id,type) %>%
    mutate(idx = rank(date)) %>% 
    ungroup %>%
    arrange(study_id,date)
  
  saveRDS(bp,file=path_to_file)
}else{
  bp<-readRDS(path_to_file)
}

# long BP table 
path_to_file<-file.path(root_dir,"data","bp_long.rda")
if(!file.exists(path_to_file)){
  bp_long<-bp %>%
    pivot_longer(
      cols = c("sbp","dbp"),
      names_to = "bp_type",
      values_to = "bp_val"
    ) %>%
    filter(days_since_index>=-720)
}else{
  bp_long<-readRDS(path_to_file)
}

# baseline table
path_to_file<-file.path(root_dir,'data','baseline_gap.rda')
if(!file.exists(path_to_file)){
  baseline_gap<-bp_long %>% 
    filter(grepl("^(KU)+",study_id)) %>%
    filter(type %in% c("elig","baseline")) %>%
    filter(bp_type == 'sbp') %>%
    group_by(study_id) %>%
    mutate(
      esbp_median = median(bp_val[type=="elig"]),
      bsbp = bp_val[type=="baseline"],
      gap = bsbp -esbp_median,
      elig_since_index = max(days_since_index[type=="elig"],na.rm=T)
    ) %>%
    mutate(gap_sign = sign(gap)) %>%
    mutate(gap_sign = case_when(
      gap_sign==-1&bp_val[type=="baseline"]<130 ~ 'deltasbp1',
      gap_sign==-1&bp_val[type=="baseline"]>=130&bp_val[type=="baseline"]<140 ~ 'deltasbp2',
      TRUE ~ 'deltasbp3')
    ) %>%
    mutate(
      gap_sign_label = recode(
        gap_sign,
        'deltasbp1' = '1.decrease-to-normal',
        'deltasbp2' = '2.decrease-to-uneligible',
        'deltasbp3' = '3.decrease-but-eligible-or-same'
      )
    ) %>%
    mutate(
      esbp_group = case_when(
        esbp_median>=140&esbp_median<150 ~ 'esbp1',
        esbp_median>=150&esbp_median<160 ~ 'esbp2',
        esbp_median>=160 ~ 'esbp3',
        TRUE ~ 'esbp0'
      ),
      bsbp_group = case_when(
        bsbp>=140&bsbp<150 ~ 'bsbp1',
        bsbp>=150&bsbp<160 ~ 'bsbp2',
        bsbp>=160 ~ 'bsbp3',
        TRUE ~ 'bsbp0'
      )
    ) %>%
    ungroup %>%
    filter(!is.na(gap) & elig_since_index<=0)
  # save intermediate table
  saveRDS(baseline_gap,file=path_to_file)
}else{
  baseline_gap<-readRDS(path_to_file)
}

# long MED table
path_to_file<-file.path(root_dir,"data","med_long.rda")
if(!file.exists(path_to_file)){
  med_sel<-read.csv(file.path(root_dir,"private","CDM","prescribing_limited.csv")) %>%
    # filter(!enc_type %in% c("IP","ED","EI","IS")) %>%
    select(participantid,patid,prescribingid,medication_class) %>%
    rename(study_id = participantid)
  
  med_ref<-read.csv(file.path(root_dir,"ref","med_rxcui_ref.csv"), stringsAsFactors = F) %>%
    mutate(AntiHTN_ind = case_when(VA_CLS_CD %in% c(
      'CV100', # beta blockers/related
      'CV150', # alpha blockers/related
      'CV200', # calcium channel blockers
      'CV400', # antihypertensive combinations
      'CV490', # antihypertensive, others
      'CV701', # thiazides/related diuretics
      'CV702', # loop diuretics
      'CV703', # carbonic anhydrase inhibitor diuretics
      'CV704', # potassium sparing/combinations diretics
      'CV709', # diuretics, other
      'CV800', # ACE inhibitors
      'CV805', # angiotensin II inhibitor 
      'CV806'  # direct renin inhibitor
    ) ~ 1, TRUE ~ 0)) %>%
    group_by(RXNORM_CUI) %>%
    arrange(IN,VA_CLS) %>%
    summarise(
      IN = paste0(unique(IN),collapse = '_'),
      STR = STR[1],
      VA_CLS = paste0(unique(VA_CLS),collapse = '_'),
      VA_CLS_CD = paste0(unique(VA_CLS_CD),collapse = '_'),
      AntiHTN_ind = max(AntiHTN_ind),
      .groups = 'drop'
    )
  
  med<-read.csv(file.path(root_dir,"private","CDM","prescribing.csv"),stringsAsFactors = F) %>%
    # filter(!enc_type %in% c("IP","ED","EI","IS")) %>%
    select(patid,participantid,prescribingid,rxnorm_cui,raw_rx_med_name,rx_order_date,rx_start_date,rx_end_date,rx_dose_ordered,rx_dose_ordered_unit,rx_quantity,rx_dose_form,rx_route,rx_frequency,rx_refills) %>%
    mutate(
      rx_order_date = as.Date(rx_order_date),
      rx_start_date = as.Date(rx_start_date),
      rx_end_date = as.Date(rx_end_date)
    ) %>%
    tidyr::replace_na(list(rx_refills=0)) %>%
    mutate(rx_freq_num = case_when(
      rx_frequency %in% c('01','05','06','10') ~ 1,
      rx_frequency == '02' ~ 2,
      rx_frequency %in% c('03','07','08') ~ 3,
      rx_frequency == '04' ~ 4,
      TRUE ~ 1
    ))%>%
    rename(study_id = participantid) %>%
    arrange(study_id,rx_order_date) %>%
    mutate(rx_start_date_imp = coalesce(rx_start_date,rx_order_date)) %>%
    mutate(
      rx_days1 = as.numeric(coalesce(rx_end_date,rx_start_date_imp) - rx_start_date_imp),
      rx_days2 = round((rx_refills+1) * (rx_quantity/pmax(rx_freq_num,1))),
      rx_days = coalesce(rx_days2,rx_days1)
    ) %>%
    inner_join(
      bp %>% filter(type=="baseline") %>%
        select(study_id,index_date) %>% unique,
      by="study_id"
    ) %>%
    mutate(
      rx_start_since_index = as.numeric(as.Date(rx_start_date_imp) - index_date)
    ) %>% 
    filter(rx_start_since_index>=-365) %>%
    left_join(med_ref,by=c("rxnorm_cui" = "RXNORM_CUI")) %>%
    left_join(med_sel,by=c("study_id","patid","prescribingid"),multiple = "all") %>%
    mutate(
      AntiHTN_ind = coalesce(AntiHTN_ind,as.numeric(!is.na(medication_class))),
      in_or_name = coalesce(IN,raw_rx_med_name),
      rx_str = coalesce(rx_dose_ordered,STR),
      rx_end_since_index = rx_start_since_index + rx_days,
      in_or_name_s = substr(in_or_name,1,20)
    ) %>%
    filter(!is.na(IN))
  saveRDS(med,file=path_to_file)
}else{
  med<-readRDS(path_to_file)
}

# wide anti-htn med table
path_to_file<-file.path(root_dir,"data","antihtn_t.rda")
if(!file.exists(path_to_file)){
  antihtn<-med %>% 
    filter(AntiHTN_ind==1) %>%
    inner_join(
      bp %>% filter(type=="elig") %>%
        group_by(study_id) %>%
        summarise(elig_since_index=max(days_since_index),.groups="drop"),
      by="study_id"
    ) %>%
    filter(rx_start_since_index<=0) %>%
    mutate(
      rx_timing = case_when(
        rx_start_since_index<=elig_since_index ~ 'bef',
        rx_start_since_index>elig_since_index ~ 'runin'
      ),
      runin_cont = case_when(
        rx_start_since_index<=elig_since_index&rx_end_since_index<=elig_since_index ~ 1,
        TRUE ~ 0
      )
    )
  
  antihtn_t_dose<-antihtn %>%
    group_by(study_id,in_or_name,rx_timing) %>%
    summarise(dose_mean = mean(rx_dose_ordered,na.rm=T),.groups = 'drop') %>%
    filter(!(dose_mean==-Inf)) %>%
    group_by(study_id,in_or_name) %>%
    reframe(dose_inc = case_when(
      dose_mean[rx_timing=='runin']>dose_mean[rx_timing=='bef'] ~ 1
    )) %>%
    filter(!is.na(dose_inc)) %>%
    pivot_wider(
      names_from = 'in_or_name',
      values_from = 'dose_inc',
      values_fill = 0,
      names_glue = "{in_or_name}_does_inc"
    ) %>%
    mutate(antihtn_dose_inc = 1)
  
  antihtn_t_stk<-antihtn %>%
    select(study_id,in_or_name,rx_timing) %>%
    unique %>% mutate(ind=1) %>%
    rename(name=in_or_name) %>%
    bind_rows(
      antihtn %>%
        select(study_id,VA_CLS_CD,rx_timing,runin_cont) %>%
        unique %>% mutate(ind=1) %>%
        rename(name=VA_CLS_CD)
    ) %>%
    pivot_wider(
      names_from = 'rx_timing',
      values_from = 'ind',
      values_fill = 0
    ) %>%
    mutate(
      use_cat = case_when(
        bef==1&runin==1 ~ 'maintain',
        bef==1&runin_cont==1 ~ 'maintain',
        bef==1&(runin==0|runin_cont==0) ~ 'runin_stop',
        bef==0&runin==1 ~ 'runin_start'
      ),
      ind=1
    ) %>%
    unite('name_use',c("name","use_cat"),sep="_") %>%
    select(study_id,name_use,ind) %>% unique %>%
    pivot_wider(
      names_from = 'name_use',
      values_from = 'ind',
      values_fill = 0
    )
  
  antihtn_t_add_med<-bp %>%
    select(study_id) %>% unique %>%
    left_join(
      antihtn %>%
        filter(rx_start_since_index-elig_since_index >= -90) %>%
        select(study_id,rx_timing,in_or_name,runin_cont) %>%
        unique %>% 
        group_by(study_id,rx_timing) %>%
        summarise(
          in_cnt = length(unique(in_or_name)),
          runin_cont_cnt = length(unique(in_or_name[runin_cont==1])),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = 'rx_timing',
          values_from = 'in_cnt',
          values_fill = 0
        ) %>%
        mutate(
          antihtn_delta_med = case_when(
            bef == 0 & runin == 0 ~ 'untreated',
            bef == 0 & runin > 0 ~ 'antihtn_runin',
            runin_cont_cnt == 0 & runin > 0 ~ 'antihtn_add',
            bef - runin_cont_cnt > 0 & runin < bef ~ 'antihtn_rm',
            TRUE ~ 'antihtn_mt'
          )
        ),
      by="study_id"
    ) %>%
    replace_na(list(antihtn_delta_med='untreated')) %>%
    select(study_id,antihtn_delta_med) %>% 
    unique %>% 
    mutate(antihtn_delta_med2 = antihtn_delta_med, ind = 1) %>%
    pivot_wider(
      names_from = 'antihtn_delta_med2',
      values_from = 'ind',
      values_fill = 0
    )
  
  antihtn_t<-bp %>% select(study_id) %>% unique %>%
    left_join(antihtn_t_add_med,by="study_id") %>%
    left_join(antihtn_t_stk,by="study_id") %>%
    left_join(antihtn_t_dose,by="study_id") %>%
    replace(is.na(. ), 0)
  
  # intermediate table    
  saveRDS(antihtn_t,file=path_to_file)
}else{
  antihtn_t<-readRDS(path_to_file)
}

# baseline aset
path_to_file<-file.path(root_dir,'data','baseline_aset.rda')
if(!file.exists(path_to_file)){
  med<-readRDS(file.path(root_dir,"data","med_long.rda"))
  antihtn_t<-readRDS(file.path(root_dir,"data","antihtn_t.rda"))
  baseline_aset<-readRDS(file.path(root_dir,'data','baseline_gap.rda')) %>%
    select(study_id,index_date,esbp_median,bsbp,gap,elig_since_index,gap_sign,gap_sign_label,esbp_group,bsbp_group) %>%
    unique %>%
    mutate(decr_ind = case_when(gap<0 ~ 1, TRUE ~ 0)) %>%
    mutate(
      sig_decr_ind1 = as.numeric(gap_sign %in% c('deltasbp1')),
      sig_decr_ind2 = as.numeric(gap_sign %in% c('deltasbp1','deltasbp2'))
    ) %>%
    inner_join(antihtn_t,by = "study_id") %>%
    inner_join(
      demo<-read.csv(file.path(root_dir,"private","CDM","demog_2024-02-12.csv"),stringsAsFactors = F) %>%
        select(study_id,sex,race,ethnicity,age) %>%
        mutate(
          sex = relevel(as.factor(sex),ref="1"),
          race = relevel(as.factor(race),ref="5"),
          ethnicity = relevel(as.factor(ethnicity),ref="2")
        ),
      by="study_id"
    ) %>%
    filter(!is.na(sex)&!is.na(age))
  # clean up column names
  colnm<-colnames(baseline_aset)
  colnm<-gsub(" ","_",colnm)
  colnm<-gsub("/","_",colnm)
  colnm<-gsub("-","_",colnm)
  colnm<-gsub(",","_",colnm)
  colnames(baseline_aset)<-colnm
  # save intermediate table
  saveRDS(baseline_aset,file=path_to_file)
}else{
  baseline_aset<-readRDS(path_to_file)
}

