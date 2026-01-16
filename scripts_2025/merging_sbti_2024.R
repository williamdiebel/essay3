# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# The purpose of this script is to take the sbti merging table and identify
# matches to gvkey values in the existing panel data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preamble and data loading ####
setwd("~/Dropbox/NetZero WD Shared Folder/Data")
library(tidyverse)
library(stringdist)
library(fuzzyjoin)
# Merging table
sbti_2024_merging_table <- readRDS("sbti_2024_merging_table.rds")
# Company IDs from panel
rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled <- readRDS(
  "rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds")
gvkeys_and_company_names <- rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled
gvkeys_and_company_names <- gvkeys_and_company_names %>%
  select(gvkey, factset_id, primary_isin, all_isins, reprisk_id, conm,
         name)
gvkeys_and_company_names <- gvkeys_and_company_names[
  -which(duplicated(gvkeys_and_company_names))
  , ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Isolating prior sbti matches as a reference point ####
prior_matches <- rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled %>%
  filter(sbti_committed_3 == 1)
prior_matches <- prior_matches %>%
  select(gvkey, factset_id, primary_isin, all_isins, reprisk_id, conm,
         name, SBTi_Company_Name)
prior_matches <- prior_matches[
  -which(duplicated(prior_matches))
  , ] # 1484 unique (matched) firms
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Matching on sbti_2024 ####

  ## By isin ####

gvkeys_and_company_names_by_isin <- merge(gvkeys_and_company_names,
                                  sbti_2024_merging_table %>%
                                    filter(!is.na(isin)) %>%
                                    mutate(primary_isin = isin) %>%
                                    select(sbti_id, primary_isin))

gvkeys_and_company_names_no_isin <- gvkeys_and_company_names %>%
  filter(!gvkey %in% gvkeys_and_company_names_by_isin$gvkey)

  ## By name ####

  ### Exact conm ####

gvkeys_and_company_names_by_conm_exact <- merge(
  gvkeys_and_company_names_no_isin %>% mutate(firm_name = tolower(conm)),
  sbti_2024_merging_table %>% mutate(firm_name = tolower(company_name)) %>%
    select(firm_name, sbti_id))
gvkeys_and_company_names_no_isin_conm_exact <- gvkeys_and_company_names %>%
  filter(!gvkey %in% gvkeys_and_company_names_by_conm_exact$gvkey 
         & !gvkey %in% gvkeys_and_company_names_by_isin$gvkey)

  ### Exact name ####

gvkeys_and_company_names_by_name_exact <- merge(
  gvkeys_and_company_names_no_isin_conm_exact %>%
    mutate(firm_name = tolower(name)),
  sbti_2024_merging_table %>%
    mutate(firm_name = tolower(company_name)) %>%
    select(firm_name, sbti_id)
)
gvkeys_and_company_names_no_isin_conm_exact_name_exact <- 
  gvkeys_and_company_names %>% 
  filter(!gvkey %in% gvkeys_and_company_names_by_conm_exact$gvkey 
         & !gvkey %in% gvkeys_and_company_names_by_isin$gvkey
         & !gvkey %in% gvkeys_and_company_names_by_name_exact$gvkey)

  ### Clean exact conm ####
# Applying the following standard steps to remove common pre-/post-fixes from
# company names.
# 1. creating a cleaned version of the name variable in the panel data
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-gvkeys_and_company_names_no_isin_conm_exact_name_exact$conm

# 2. cleaning name vbls
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LIMITED","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LTD","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LT…E","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LT…","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," INCORPORATED","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," INC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\/INC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CORPORATION","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CORP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," COMPANY","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CO","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CO.","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," INDUSTRIES","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," THE "," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," THE","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\/","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"THE ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CHIMIE","CHEMICAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," ULC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," HOLDINGS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," D/B/A","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," OPERATIONS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\.","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\,","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\-"," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\;","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\'","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CO ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," I ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," II ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," III ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," 1 ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," 2 ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LP"," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," OF "," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"1","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"2","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"3","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"4","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"5","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"6","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"7","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"8","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"9","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," PLC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," HLDGS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," HLDG","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," ADR","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LLC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CL A","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," PWR"," POWER")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," MFG"," MANUFACTURING")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," CP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\/DE","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," TECH"," TECHNOLOGY")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," SVCS"," SERVICES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," INTL"," INTERNATIONAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," ENTRPRS"," ENTERPRISES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," HOLDCO","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," TRANSPORTATION"," TRANS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," TRANSPORT"," TRANS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," LABS"," LABORATORIES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," GRP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," GROUP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," INTL"," INTERNATIONAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\-OLD","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"U S ","US ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," U S"," US")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"PRTNRS","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\&","AND")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"INFORMATION","INFO")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"FINANCL","FINANCIAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"PRTNR","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"PTNR","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean," FINL"," FINANCIAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\(","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean,"\\)","")

# Remove anything following slash and slash itself
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean, "\\/.*", "")

# Remove anything between parantheses and parentheses
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean, " \\s*\\([^\\)]+\\)", "")

# Remove leading and trailing blanks
gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean<-str_trim(gvkeys_and_company_names_no_isin_conm_exact_name_exact$name_clean)

# Doing same for sbti_2024_merging_table names
# 1. creating a cleaned version of the name variable in the panel data
sbti_2024_merging_table$name_clean<-sbti_2024_merging_table$company_name
sbti_2024_merging_table$name_clean <- toupper(sbti_2024_merging_table$name_clean)

# 2. cleaning name vbls
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LIMITED","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LTD","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LT…E","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LT…","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," INCORPORATED","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," INC","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\/INC","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CORPORATION","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CORP","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," COMPANY","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CO","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CO.","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," INDUSTRIES","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," THE "," ")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," THE","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\/","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"THE ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CHIMIE","CHEMICAL")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," ULC","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," HOLDINGS","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," D/B/A","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," OPERATIONS","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\.","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\,","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\-"," ")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\;","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\'","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CO ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," I ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," II ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," III ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," 1 ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," 2 ","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LP"," ")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," OF "," ")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"1","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"2","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"3","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"4","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"5","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"6","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"7","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"8","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"9","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," PLC","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," HLDGS","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," HLDG","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," ADR","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LLC","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CL A","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," PWR"," POWER")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," MFG"," MANUFACTURING")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," CP","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\/DE","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," TECH"," TECHNOLOGY")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," SVCS"," SERVICES")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," INTL"," INTERNATIONAL")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," ENTRPRS"," ENTERPRISES")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," HOLDCO","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," TRANSPORTATION"," TRANS")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," TRANSPORT"," TRANS")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," LABS"," LABORATORIES")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," GRP","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," GROUP","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," INTL"," INTERNATIONAL")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\-OLD","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"U S ","US ")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," U S"," US")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"PRTNRS","PARTNERS")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\&","AND")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"INFORMATION","INFO")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"FINANCL","FINANCIAL")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"PRTNR","PARTNERS")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"PTNR","PARTNERS")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean," FINL"," FINANCIAL")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\(","")
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean,"\\)","")

# Remove anything following slash and slash itself
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean, "\\/.*", "")

# Remove anything between parantheses and parentheses
sbti_2024_merging_table$name_clean<-str_replace_all(sbti_2024_merging_table$name_clean, " \\s*\\([^\\)]+\\)", "")

# Remove leading and trailing blanks
sbti_2024_merging_table$name_clean<-str_trim(sbti_2024_merging_table$name_clean)

# Matching
gvkeys_and_company_names_by_conm_clean <- merge(
  gvkeys_and_company_names_no_isin_conm_exact_name_exact,
  sbti_2024_merging_table %>%
    select(name_clean, sbti_id)
  )
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean <-
  gvkeys_and_company_names %>%
  filter(!gvkey %in% gvkeys_and_company_names_by_conm_exact$gvkey 
         & !gvkey %in% gvkeys_and_company_names_by_isin$gvkey
         & !gvkey %in% gvkeys_and_company_names_by_name_exact$gvkey
         & !gvkey %in% gvkeys_and_company_names_by_conm_clean$gvkey)

  ### Clean exact name ####
# Doing same for sbti_2024_merging_table names
# 1. creating a cleaned version of the name variable in the panel data
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean <- toupper(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean)

# 2. cleaning name vbls
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LIMITED","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LTD","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LT…E","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LT…","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," INCORPORATED","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," INC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\/INC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CORPORATION","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CORP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," COMPANY","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CO","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CO.","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," INDUSTRIES","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," THE "," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," THE","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\/","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"THE ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CHIMIE","CHEMICAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," ULC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," HOLDINGS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," D/B/A","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," OPERATIONS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\.","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\,","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\-"," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\;","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\'","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CO ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," I ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," II ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," III ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," 1 ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," 2 ","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LP"," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," OF "," ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"1","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"2","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"3","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"4","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"5","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"6","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"7","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"8","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"9","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," PLC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," HLDGS","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," HLDG","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," ADR","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LLC","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CL A","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," PWR"," POWER")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," MFG"," MANUFACTURING")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," CP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\/DE","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," TECH"," TECHNOLOGY")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," SVCS"," SERVICES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," INTL"," INTERNATIONAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," ENTRPRS"," ENTERPRISES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," HOLDCO","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," TRANSPORTATION"," TRANS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," TRANSPORT"," TRANS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," LABS"," LABORATORIES")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," GRP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," GROUP","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," INTL"," INTERNATIONAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\-OLD","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"U S ","US ")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," U S"," US")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"PRTNRS","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\&","AND")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"INFORMATION","INFO")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"FINANCL","FINANCIAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"PRTNR","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"PTNR","PARTNERS")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean," FINL"," FINANCIAL")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\(","")
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean,"\\)","")

# Remove anything following slash and slash itself
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean, "\\/.*", "")

# Remove anything between parantheses and parentheses
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_replace_all(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean, " \\s*\\([^\\)]+\\)", "")

# Remove leading and trailing blanks
gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean<-str_trim(gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean$name_clean)

# Matching
gvkeys_and_company_names_by_name_clean <- merge(
  gvkeys_and_company_names_no_isin_conm_exact_name_exact_conm_clean,
  sbti_2024_merging_table %>%
    select(name_clean, sbti_id)
)

## Merging all the matches ####
all_gvkey_sbti_matches <- rbind(
  gvkeys_and_company_names_by_isin %>% select(gvkey, sbti_id),
  gvkeys_and_company_names_by_conm_exact %>% select(gvkey, sbti_id),
  gvkeys_and_company_names_by_name_exact %>% select(gvkey, sbti_id),
  gvkeys_and_company_names_by_conm_clean %>% select(gvkey, sbti_id),
  gvkeys_and_company_names_by_name_clean %>% select(gvkey, sbti_id)
  )
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Matching based on lei values in the rrpanel and sbti_2024 data ####

# Creating a tibble w gvkeys of interest
gvkey_lei_sbti_id <- prior_matches$gvkey[
  which(!prior_matches$gvkey %in% all_gvkey_sbti_matches$gvkey)
] %>% 
  tibble(gvkey = .)
# merging the lei data from rrpanel
gvkey_lei_sbti_id <- merge(gvkey_lei_sbti_id,
                           rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled %>%
                             select(gvkey, SBTi_LEI))
# removing duplicates caused by the panel structure
gvkey_lei_sbti_id <- gvkey_lei_sbti_id[
  -which(
    duplicated(gvkey_lei_sbti_id)
  )
  ,
]
# adding sbti_ids
gvkey_lei_sbti_id <- merge(gvkey_lei_sbti_id %>%
                             filter(!is.na(SBTi_LEI)) %>% 
                             mutate(lei = SBTi_LEI),
                           sbti_2024_merging_table %>%
                             select(lei, sbti_id),
                           all.x = TRUE)
gvkey_lei_sbti_id <- gvkey_lei_sbti_id %>% select(gvkey, lei, sbti_id)
gvkey_lei_sbti_id <- gvkey_lei_sbti_id[
  -which(is.na(gvkey_lei_sbti_id$sbti_id))
  ,
]
lei_matches <- gvkey_lei_sbti_id %>% select(gvkey, sbti_id)

# adding lei matches to the merging key
all_gvkey_sbti_matches_lei <- rbind(all_gvkey_sbti_matches, lei_matches)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Comparing gvkeys matched in rrpanel and with sbti_2024 ####
which(prior_matches$gvkey %in% all_gvkey_sbti_matches$gvkey) %>% 
  length() # [1] 1430
which(!prior_matches$gvkey %in% all_gvkey_sbti_matches$gvkey) %>%
  length() # [1] 54
# In summary, 96% of firms previously matched across sbti and compustat data
# were successfully matched using the above approach. Next, I'll take a quick
# look through the unidentified prior matches and see if I can manually add
# these.

unmatched_prior_matches <- prior_matches[
  which(
    !
      prior_matches$gvkey %in% 
      all_gvkey_sbti_matches$gvkey
    )
  ,
]
unmatched_prior_matches$SBTi_Company_Name <- toupper(
  unmatched_prior_matches$SBTi_Company_Name
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Fuzzy matching #### 
## Unmatched firms from prior_matches ####
sbti_2024_merging_table_fuzzy <- sbti_2024_merging_table %>% 
  filter(!sbti_id %in% all_gvkey_sbti_matches_lei$sbti_id)
sbti_2024_merging_table_fuzzy$company_name <- toupper(
  sbti_2024_merging_table_fuzzy$company_name)

# 1. Compute *all* JW distances (no threshold)
all_matches <- stringdist_inner_join(
  unmatched_prior_matches %>% mutate(.row = row_number()),    # tag rows
  sbti_2024_merging_table_fuzzy %>% select(company_name, sbti_id),
  by = c("SBTi_Company_Name" = "company_name"),
  method = "jw",
  max_dist = Inf,
  distance_col = "dist"
)

# 2. For each original row, keep only the 1 with smallest dist
closest_matches <- all_matches %>%
  group_by(.row) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-.row)

# `closest_matches` now has, for each `conm`, only the single closest 
# `company_name` & `sbti_id`

closest_matches %>% select(SBTi_Company_Name, conm, company_name, sbti_id) %>% view()

# Manual inspection reveals a 28% success rate
gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id <- rep(NA, nrow(
  gvkeys_and_company_names_no_prior_to_fuzzy
))

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "FERROVIAL")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "FERROVIAL")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "NOMURA REAL ESTATE")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "NOMURA REAL ESTATE")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "SARAWAK ENERGY")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "SARAWAK ENERGY")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "DSV A")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "DSV A")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "VOLKERWESSELS")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "VOLKERWESSELS")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "DP WORLD")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "DP WORLD")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "AMERICANAS S A")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "AMERICANAS S.A.")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "BOUYGUES SA")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "BOUYGUES CONSTRUCTION")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "REGINA MIRACLE INTL")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "REGINA MIRACLE")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "BOLLORE SE")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "BOLLORÉ LOGISTICS")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "CHAROEN POKPHAND EN")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "CHAROEN POKPHAND GROUP")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "PHARMA MAR")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "PHARMA MAR")
  ]

gvkeys_and_company_names_no_prior_to_fuzzy$sbti_id[
  str_which(gvkeys_and_company_names_no_prior_to_fuzzy$conm, "PIRAEUS FINANCIAL")
] <- sbti_2024_merging_table$sbti_id[
  str_which(sbti_2024_merging_table$company_name, "PIRAEUS FINANCIAL")
  ]

gvkeys_and_company_names_by_prior_matches <- gvkeys_and_company_names_no_prior_to_fuzzy %>%
  filter(!is.na(sbti_id)) %>% 
  select(gvkey, sbti_id)

all_gvkey_sbti_matches_lei_prior <- rbind(
  all_gvkey_sbti_matches_lei,
  gvkeys_and_company_names_by_prior_matches)

# Finally, checking for and removing duplicates
  # Adding back SBTi data demographics
all_gvkey_sbti_matches_lei_prior_v2 <- merge(all_gvkey_sbti_matches_lei_prior,
                                          sbti_2024_merging_table,
                                          all.x = TRUE)
  # Adding back rrpanel data demographics
all_gvkey_sbti_matches_lei_prior_v3 <- merge(all_gvkey_sbti_matches_lei_prior_v2,
                                          gvkeys_and_company_names,
                                          all.x = TRUE)
  # Examining gvkey duplicates
all_gvkey_sbti_matches_lei_prior_v3 %>% 
  filter(
    duplicated(gvkey) | duplicated(gvkey, fromLast = TRUE)
  ) %>% 
  select(gvkey, sbti_id, conm, company_name) %>%
  view()
    # Creating a list of false positive matches that created duplicates
names_to_exclude <- all_gvkey_sbti_matches_lei_prior_v3 %>% 
  filter(
    duplicated(gvkey) | duplicated(gvkey, fromLast = TRUE)
  ) %>%
  select(company_name)
all_gvkey_sbti_matches_lei_prior_v4 <- all_gvkey_sbti_matches_lei_prior_v3 %>%
  filter(!company_name %in% names_to_exclude$company_name[-4])

  # Examining sbti_id duplicates
all_gvkey_sbti_matches_lei_prior_v4 %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(gvkey, sbti_id, conm, company_name) %>%
  arrange(sbti_id) %>%
  view()

  # From the previous line, I manually inspected each case to determine the
  # following gvkeys for exclusion based on false positive conm to company
  # name matches.
gvkey_to_exclude <- c("249638",
                     "22402",
                     "139662",
                     "15704",
                     "275457",
                     "222153",
                     "239498",
                     "248015",
                     "10795",
                     "224893",
                     "316464",
                     "1045",
                     "207230",
                     "203455",
                     "205235",
                     "274143",
                     "15645",
                     "20747",
                     "102285",
                     "211453",
                     "209143",
                     "179132",
                     "234977",
                     "104600",
                     "211514",
                     "13440",
                     "65142",
                     "35458")
all_gvkey_sbti_matches_lei_prior_v5 <- all_gvkey_sbti_matches_lei_prior_v4 %>%
  filter(!gvkey %in% gvkey_to_exclude)

saveRDS(all_gvkey_sbti_matches_lei_prior_v5, "all_gvkey_sbti_matches_lei_prior.rds")
