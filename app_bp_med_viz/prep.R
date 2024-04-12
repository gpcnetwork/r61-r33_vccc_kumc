pacman::p_load(
  tidyverse,
  magrittr
)
root_dir<-file.path(
  'C:',"repos","r61-r33-vccc-kumc"
)

n<-200
sample_df<-readRDS(file.path(root_dir,"private","bp_long.rda")) %>%
  select(study_id) %>% 
  unique %>% 
  sample_n(n) %>% 
  mutate(id = row_number())

bp_long_sample<-readRDS(file.path(root_dir,"private","bp_long.rda")) %>%
  inner_join(sample_df,by="study_id")
  
saveRDS(
  bp_long_sample,
  file.path(root_dir,"app_bp_med_viz","data","bp_long_sample.rda")
)

med_long_sample<-readRDS(file.path(root_dir,"private","med_long.rda")) %>%
  inner_join(sample_df,by="study_id")

saveRDS(
  med_long_sample,
  file.path(root_dir,"app_bp_med_viz","data","med_long_sample.rda")
)
