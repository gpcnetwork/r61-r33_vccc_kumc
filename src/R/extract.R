rm(list=ls()); gc()
setwd("C:/repos/r61-r33_vccc_kumc")

# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr
)

# make db connection
sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = Sys.getenv("ODBC_DSN_NAME"),
  uid = Sys.getenv("SNOWFLAKE_USER"),
  pwd = Sys.getenv("SNOWFLAKE_PWD")
)

path_to_data<-"C:/repos/r61-r33_vccc_kumc/data"

# collect patient table1
base_bp<-tbl(sf_conn,sql("select * from SX_VCCC.VCCC_BASE_BP")) %>% collect()
saveRDS(base_bp,file=file.path(path_to_data,"base_bp.rda"))

# collect meds
meds<-tbl(sf_conn,sql("select * from SX_VCCC.VCCC_MED_LONG")) %>% collect()
saveRDS(meds,file=file.path(path_to_data,"meds_long.rda"))

# collect tcog-sdh cohor
tcog_sdh<-tbl(sf_conn,sql("select * from SX_VCCC.VCCC_BASE_BP_TCOG_SDH")) %>% collect() %>%
  # low-freq grouping
  group_by(STATE) %>% mutate(CNT = length(unique(STUDY_ID))) %>% mutate(STATE_GROUP = case_when(CNT>100 ~ STATE, TRUE ~ 'OT')) %>%
  group_by(COUNTY) %>% mutate(CNT = length(unique(STUDY_ID))) %>% mutate(COUNTY_GROUP = case_when(CNT>20 ~ COUNTY, TRUE ~ 'OT')) %>%
  ungroup %>% select(-CNT) %>%
  arrange(ENROLL_DATE)
saveRDS(tcog_sdh,file=file.path(path_to_data,"tcog_sdh.rda"))
