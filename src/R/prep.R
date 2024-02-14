rm(list=ls())

pacman::p_load(
  tidyverse,
  magrittr,
  stringr,
  broom
)
root_dir<-file.path(
  'C:',"repos","r61-r33-vccc-kumc"
)

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

bp_long<-bp %>%
  pivot_longer(
    cols = c("sbp","dbp"),
    names_to = "bp_type",
    values_to = "bp_val"
  ) %>%
  filter(days_since_index>=-365)

saveRDS(bp_long,file=file.path(root_dir,"private","bp_long.rda"))

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
    IN = paste0(unique(IN),collapse = ';'),
    STR = STR[1],
    VA_CLS = paste0(unique(VA_CLS),collapse = ';'),
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
  filter(rx_start_since_index>=-180) %>%
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

saveRDS(med,file=file.path(root_dir,"private","med_long.rda"))
