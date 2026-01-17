# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# The purpose of this script is to pull commitment-level data (only) from each
# of the three sbti datasets downloaded in different periods (2022 from rob;
# 2023 from myself, and 2025 from myself)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preamble and data loading ####
setwd("~/Dropbox/NetZero WD Shared Folder/Data")
library(tidyverse)
sbti_2022 <- readxl::read_excel("sbti_data_04_25/SBTi data Oct 2022_WD.xlsx")
sbti_2023 <- readxl::read_excel(
  "companies-taking-action as of 2023.04.08_WD.xlsx")
all_sbti_targets <- readRDS("all_sbti_targets.rds")
all_sbti_targets_and_commitments <- readRDS(
  "all_sbti_targets_and_commitments.rds")
all_gvkey_sbti_matches_lei_prior <- readRDS(
  "all_gvkey_sbti_matches_lei_prior.rds")
sbti_2024_merging_table <- readRDS("sbti_2024_merging_table.rds")
rrpanel <- readRDS("rrpanel_comp_fs_sbti_bloombergsubset_cdp_filled.rds")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Removing duplicates ####
# # Some orgs in the sbti 2022 and sbti 2023 dataset are duplicated across
# # observations at the isin and lei levels. The easiest way to deal with this is
# # now since I'll be merging sbti 2022 and 2023 with the sbti_id values from the
# # sbti_2024_merging_table based on isin and lei. This will prevent duplicates
# # from being introduced later.
# 
#   ## by isin ####
#     ### 2022 ####
# duplicated_isins_2022 <- unique(
#   sbti_2022$isin[
#     which(duplicated(sbti_2022$isin))
#     ]
#   )
# duplicated_isins_2022
#     # Above reveals several formatting issues
# sbti_2022$isin[which(sbti_2022$isin=="NA")] <- NA
# sbti_2022$isin[which(sbti_2022$isin=="N/A")] <- NA
# sbti_2022$isin[which(sbti_2022$isin=="n/a")] <- NA
# sbti_2022$isin[which(sbti_2022$isin=="Not applicable")] <- NA
# sbti_2022$isin[which(sbti_2022$isin=="not available")] <- NA
# duplicated_isins_2022 <- unique(
#   sbti_2022$isin[
#     which(duplicated(sbti_2022$isin))
#   ]
# )
# duplicated_isins_2022
# 
#     ### 2023 ####
#   ## by lei ####
#     ### 2022 ####
#     ### 2023 ####
#   ## by name ####
#     ### 2022 ####
#     ### 2023 ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Identifying firms in 2022 ####

  ## Assigning sbti_id ####

  # First, I want to assign sbti_id values to firms in 2022 and 2023, which will
  # enable me to use the all_gvkey_sbti_matches_lei_prior table to assign 
  # gvkeys.
  # I can assign sbti_id values to firms in the 2022 data based on isin, lei, 
  # and firm name as appears in the sbti_2024_merging_table.

    ### 2022 ####
      #### by isin ####
colnames(sbti_2022)[1] <- "company_name_2022"
sbti_2022_by_isin <- merge(sbti_2022 %>%
                             filter(!is.na(isin)), 
                           sbti_2024_merging_table %>%
                             filter(!is.na(isin)) %>%
                             select(isin, sbti_id, company_name),
                           by = "isin")
    # Here, I need to inspect and address the issue of duplicate isin values in
    # the sbti data. For example, Universal Corporation and Universal Music 
    # Group are listed separately in SBTi, but both share the same reported  
    # isin. Therefore, I should screen and select the observations by 
    # taking the earliest commitment date.
sbti_2022_by_isin %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(sbti_id, company_name_2022, company_name, isin, date) %>%
  arrange(sbti_id) %>%
  view()
sbti_2022_by_isin$sbti_id %>% n_distinct() # [1] 1628 unique sbti_id values

    # The result of the data cleaning (taking earliest date) should result in
    # 1628 obs, one for each unique firm.
sbti_2022_by_isin <- sbti_2022_by_isin %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()
nrow(sbti_2022_by_isin) # [1] 1628
sbti_2022_by_isin$sbti_id %>% n_distinct() # [1] 1628

      #### by lei ####
sbti_2022_by_lei <- merge(sbti_2022 %>%
                            filter(!is.na(lei) &
                                     (is.na(isin) | !isin %in% 
                                        sbti_2022_by_isin$isin)), 
                          sbti_2024_merging_table %>%
                            filter(!is.na(lei)) %>%
                            select(lei, sbti_id, company_name),
                          by = "lei")
    # Like above, I need to inspect and address the issue of duplicate lei 
    # values in the sbti data. Taking the earliest commitment date.
sbti_2022_by_lei %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(sbti_id, company_name_2022, company_name, lei, date) %>%
  arrange(sbti_id) %>%
  view()
sbti_2022_by_lei$sbti_id %>% n_distinct() # [1] 683 unique sbti_id values

sbti_2022_by_lei <- sbti_2022_by_lei %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()
nrow(sbti_2022_by_lei) # [1] 683
sbti_2022_by_lei$sbti_id %>% n_distinct() # [1] 683

sbti_2022_by_name <- merge(x = sbti_2022 %>%
                             filter(!is.na(company_name_2022) &
                                      (is.na(lei) | !(lei %in% 
                                                        sbti_2022_by_isin$lei)) &
                                      (is.na(isin) | !(isin %in% 
                                                         sbti_2022_by_isin$isin))), 
                          y =  sbti_2024_merging_table %>%
                             filter(!is.na(company_name)) %>%
                             select(sbti_id, company_name),
                           by.x = "company_name_2022", by.y = "company_name"
                          )
sbti_2022_by_name %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(sbti_id, company_name_2022, date) %>%
  arrange(sbti_id) %>%
  view()
sbti_2022_by_name <- sbti_2022_by_name %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))
sbti_2022_by_name$company_name <- sbti_2022_by_name$company_name_2022

  # aggregating
sbti_2022_sbti_id_added <- rbind(sbti_2022_by_isin,
                                 sbti_2022_by_lei,
                                 sbti_2022_by_name)

  # removing duplicated sbti_id values (taking the earliest date)
sbti_2022_sbti_id_added <- sbti_2022_sbti_id_added %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

    ### 2023 ####

sbti_2023_by_isin <- merge(sbti_2023 %>%
                             filter(!is.na(isin)),
                           sbti_2024_merging_table %>%
                             filter(!is.na(isin)) %>%
                             select(isin, sbti_id),
                           by = "isin")
  # checking for duplicates
sbti_2023_by_isin %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(sbti_id, company_name, isin, date) %>%
  arrange(sbti_id) %>%
  view()
sbti_2023_by_isin$sbti_id %>% n_distinct() # [1] 1865 unique sbti_id values
  # addressing duplicates
sbti_2023_by_isin <- sbti_2023_by_isin %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

sbti_2023_by_lei <- merge(sbti_2023 %>%
                            filter(!is.na(lei) &
                                     (is.na(isin) | 
                                        !(isin %in% sbti_2023_by_isin$isin))), 
                          sbti_2024_merging_table %>%
                            filter(!is.na(lei)) %>%
                            select(lei, sbti_id),
                          by = "lei")
sbti_2023_by_lei %>% 
  filter(
    duplicated(sbti_id) | duplicated(sbti_id, fromLast = TRUE)
  ) %>% 
  select(sbti_id, company_name, lei, date) %>%
  arrange(sbti_id) %>%
  view()
sbti_2023_by_lei <- sbti_2023_by_lei %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

sbti_2023_by_name <- merge(sbti_2023 %>%
                             filter(!is.na(company_name) &
                                      (is.na(lei) | !(lei %in% 
                                         sbti_2023_by_isin$lei)) &
                                      (is.na(isin) | !(isin %in% 
                                         sbti_2023_by_isin$isin))), 
                           sbti_2024_merging_table %>%
                             filter(!is.na(company_name)) %>%
                             select(company_name, sbti_id),
                           by = "company_name")
sbti_2023_by_name <- sbti_2023_by_name %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

  # aggregating
sbti_2023_sbti_id_added <- rbind(sbti_2023_by_isin,
                                 sbti_2023_by_lei,
                                 sbti_2023_by_name)
  # removing duplicated sbti_id values (taking the earliest date)

sbti_2023_sbti_id_added <- sbti_2023_sbti_id_added %>%
  group_by(sbti_id) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # for each sbti_id, take the single row with the minimal date_published
  slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

  ## Assigning gvkey ####
    ### 2022 ####
sbti_2022_sbti_id_gvkey_added <- merge(sbti_2022_sbti_id_added,
                                       all_gvkey_sbti_matches_lei_prior %>%
                                         select(sbti_id, gvkey))
# N = 1318 gvkeys identified -- this roughly equates to prior_matches (N = 1484)
    ### 2023 ####
sbti_2023_sbti_id_gvkey_added <- merge(sbti_2023_sbti_id_added,
                                       all_gvkey_sbti_matches_lei_prior %>%
                                         select(gvkey, sbti_id))
# N = 1481 gvkeys identified -- this is a slightly lower matching rate than last 
# time (1484)

  # In summary, I've added gvkey values to the raw data from sbti 2022 and 2023:
    # sbti_2022_sbti_id_gvkey_added -> intersection of 
    # all_gvkey_sbti_matches_lei_prior and sbti 2022.
        # N = 1318
    # sbti_2023_sbti_id_gvkey_added -> intersection of 
    # all_gvkey_sbti_matches_lei_prior and sbti 2023.
        # N = 1481
    # all_gvkey_sbti_matches_lei_prior -> intersection of rrpanel and sbti 2024
        # N = 1840

  # Finally, since there are 81 firms in the rrpanel with commitments that are 
  # not captured in the sbti_2023_sbti_id_gvkey_added, I want to manually screen
  # and add the appropriate sbti_id for all.
      # The two lines below produce the firm names for which I'm missing sbti_id
gvkeys_sbti <- rrpanel %>% filter(sbti_committed_3==1)
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique()
      # Go through each name with the following protocol:
          # 1. cross-ref and use the name in the 2023 data to ID observation
          # 2. cross-ref the firm in the 2025 data
          # 3. manually assign the sbti_id from the 2025 data if above steps
          #    are successful. If not (e.g., the company does not have an 
          #    sbti_id), remove the firm from the rrpanel

# 1
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[1]
# WESTPAC BANKING
# in 2023 data; no sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel %>% filter(!conm=="WESTPAC BANKING")

# 2
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[2]
# AUCKLAND INTL AIRPORT LTD
# in 2023 data; no sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="AUCKLAND INTL AIRPORT LTD")

# 3
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[3]
# LEVEL 3 COMMUNICATIONS INC
# not in 2023 data -- false positive name match, no action

# 4
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[4]
# CTT CORREIOS DE PORTUGAL SA
# in 2023 data; valid sbti_id 40015742
sbti_2023$sbti_id <- rep(NA, nrow(sbti_2023))
sbti_2023$gvkey <- rep(NA, nrow(sbti_2023))
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="CTT - Correios de Portugal SA"
)] <- "40015742"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="CTT - Correios de Portugal SA"
)] <- "316694"

# 5
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[5]
# STANDARD CHARTERED PLC
# in 2023 data; valid sbti_id 40009253
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Standard Chartered Bank"
)] <- "40009253"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Standard Chartered Bank"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="STANDARD CHARTERED PLC")])

# 6
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[6]
# CAESARS ENTERTAINMENT CORP
# in 2023 data; no valid sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="CAESARS ENTERTAINMENT CORP")

# 7
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[7]
# HAZAMA ANDO CORP
# in 2023 data; valid sbti_id 40004341
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="HAZAMA ANDO CORPORATION"
)] <- "40004341"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="HAZAMA ANDO CORPORATION"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="HAZAMA ANDO CORP")])

# 8
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[8]
# ADANI GREEN ENERGY
# in 2023 data Adani Green Energy Ltd.; no valid sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="ADANI GREEN ENERGY")

# 9
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[9]
# THOMSON-REUTERS CORP
# in 2023 data Thomson Reuters; valid sbti_id 40001574
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Thomson Reuters"
)] <- "40001574"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Thomson Reuters"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="THOMSON-REUTERS CORP")])

# 10
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[10]
# VMWARE INC -CL A
# in 2023 data VMware, Inc; no valid sbti_id 
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="VMWARE INC -CL A")

# 11
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[11]
# CANADIAN PACIFIC RAILWAY LTD
# in 2023 data Canadian Pacific Railway Company; no valid sbti_id 
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="CANADIAN PACIFIC RAILWAY LTD")

# 12
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[12]
# KANSAS CITY SOUTHERN
# in 2023 data; no valid sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="KANSAS CITY SOUTHERN")

# 13
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[13]
# NEXT PLC
# in 2023 data Next plc; valid sbti_id 40007599
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Next plc"
)] <- "40007599"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Next plc"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="NEXT PLC")])

# 14
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[14]
# EBAY INC
# in 2023 data as "eBay Inc."; valid sbti_id "40003430"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="eBay Inc."
)] <- "40003430"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="eBay Inc."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="EBAY INC")])

# 15
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[15]
gvkeys_sbti$SBTi_Company_Name[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[15]
# "AIR PRODUCTS & CHEMICALS INC"
#  not in 2023 data -- false positive name match, no action

# 16
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[16]
# "EQT AB"
# in 2023 data as "EQT AB"; valid sbti_id "40013497"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="EQT AB"
)] <- "40013497"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="EQT AB"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="EQT AB")])

# 17
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[17]
# "UNITED AIRLINES HOLDINGS INC"
# in 2023 data as "United Airlines, Inc."; valid sbti_id "40005583"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="United Airlines, Inc."
)] <- "40005583"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="United Airlines, Inc."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="UNITED AIRLINES HOLDINGS INC")])

# 18
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[18]
# "DP WORLD PLC"
# in 2023 data as "DP World"; valid sbti_id "40005448"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="DP World"
)] <- "40005448"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="DP World"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="DP WORLD PLC")])

# 19
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[19]
# "PHAROL SGPS SA"
# false positive -- no action

# 20
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[20]
# "KEURIG DR PEPPER INC"
# in 2023 data as "Keurig Dr Pepper"; valid sbti_id "40000587"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Keurig Dr Pepper"
)] <- "40000587"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Keurig Dr Pepper"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="KEURIG DR PEPPER INC")])

# 21
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[21]
# "VEIDEKKE A/S"
# in 2023 data as "Veidekke ASA"; valid sbti_id "40003018"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Veidekke ASA"
)] <- "40003018"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Veidekke ASA"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="VEIDEKKE A/S")])

# 22
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[22]
# "RAIFFEISEN INTL BANK HLDG AG"
# in 2023 data as "Raiffeisen Bank International AG"; no valid sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="RAIFFEISEN INTL BANK HLDG AG")

# 23
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[23]
# "REXEL SA"
# in 2023 data as "Rexel"; valid sbti_id "40009442"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Rexel"
)] <- "40009442"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Rexel"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="REXEL SA")])

# 24
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[24]
# "AIR NEW ZEALAND LTD"
# in 2023 data as "Air New Zealand"; no valid sbti_id 
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="AIR NEW ZEALAND LTD")

# 25
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[25]
# "PUBLIC POWER CORP SA"
# in 2023 data as "Public Power Corporations"; no valid sbti_id
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="PUBLIC POWER CORP SA")

# 26
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[26]
# "AMERICAN AIRLINES GROUP INC"
# in 2023 data as "American Airlines"; valid sbti_id "40005666"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="American Airlines"
)] <- "40005666"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="American Airlines"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="AMERICAN AIRLINES GROUP INC")])

# 27
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[27]
# "ZENDESK INC"
# in 2023 data as "Zendesk"; valid sbti_id "40014706"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Zendesk"
)] <- "40014706"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Zendesk"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="ZENDESK INC")])

# 28
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[28]
# "ROYAL MAIL HOLDINGS"
# in 2023 data as "Royal Mail Group UK"; valid sbti_id "40007261"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Royal Mail Group UK"
)] <- "40007261"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Royal Mail Group UK"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="ROYAL MAIL HOLDINGS")])

# 29
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[29]
# "HANKOOK & COMPANY CO LTD"
# in 2023 data as "Hankook Tire & Technology Co., Ltd."; valid sbti_id "40004239"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Hankook Tire & Technology Co., Ltd."
)] <- "40004239"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Hankook Tire & Technology Co., Ltd."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="HANKOOK & COMPANY CO LTD")])

# 30
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[30]
# "ZAYO GROUP HOLDINGS INC"
# in 2023 data as "ZAYO GROUP LLC"; valid sbti_id "40011306"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="ZAYO GROUP LLC"
)] <- "40011306"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="ZAYO GROUP LLC"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="ZAYO GROUP HOLDINGS INC")])

# 31
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[31]
# "PRIMAX ELECTRONICS LTD"
# in 2023 data as "Primax Electronics (KS) Corp.,Ltd."; valid sbti_id "40007471"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Primax Electronics (KS) Corp.,Ltd."
)] <- "40007471"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Primax Electronics (KS) Corp.,Ltd."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="PRIMAX ELECTRONICS LTD")])

# 32
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[32]
# "HAYLEYS PLC"
# in 2023 data as "Hayleys Fabric PLC"; valid sbti_id "40014760"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Hayleys Fabric PLC"
)] <- "40014760"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Hayleys Fabric PLC"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="HAYLEYS PLC")])

# 33
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[33]
# "QUESS CORP LTD"
# in 2023 data as "Quess Corp Limited"; valid sbti_id "40008899"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Quess Corp Limited"
)] <- "40008899"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Quess Corp Limited"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="QUESS CORP LTD")])

# 34
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[34]
# "REGINA MIRACLE INTL (HLDGS)"
# in 2023 data as "Regina Miracle International ( Group ) Limited"; valid sbti_id "40016305"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Regina Miracle International ( Group ) Limited"
)] <- "40016305"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Regina Miracle International ( Group ) Limited"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="REGINA MIRACLE INTL (HLDGS)")])

# 35
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[35]
# "KOMATSU LTD"
# in 2023 data as "Komatsu Ltd."; valid sbti_id "40012097"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Komatsu Ltd."
)] <- "40012097"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Komatsu Ltd."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="KOMATSU LTD")])

# 36
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[36]
# "BNK FINANCIAL GROUP INC"
# in 2023 data as "BNK Financial Group Inc."; valid sbti_id "40007398"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="BNK Financial Group Inc."
)] <- "40007398"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="BNK Financial Group Inc."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="BNK FINANCIAL GROUP INC")])

# 37
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[37]
# "OTSUKA SHOKAI CO LTD"
# in 2023 data as "OTSUKA CORPORATION"; valid sbti_id "40004530"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="OTSUKA CORPORATION"
)] <- "40004530"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="OTSUKA CORPORATION"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="OTSUKA SHOKAI CO LTD")])

# 38
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[38]
# "WORTHINGTON INDUSTRIES"
# in 2023 data as "Worthington Industries Sustainable Energy"; valid sbti_id "40009209"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Worthington Industries Sustainable Energy"
)] <- "40009209"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Worthington Industries Sustainable Energy"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="WORTHINGTON INDUSTRIES")])

# 39
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[39]
# "HOMESERVE PLC"
# in 2023 data as "HomeServe plc"; no valid sbti_id 
rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024 %>% 
  filter(!conm=="HOMESERVE PLC")

# 40
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[40]
# "SL GREEN REALTY CORP"
# in 2023 data as "SL Green Realty Corp."; valid sbti_id "40014275"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="SL Green Realty Corp."
)] <- "40014275"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="SL Green Realty Corp."
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="SL GREEN REALTY CORP")])

# 41
gvkeys_sbti$conm[which(!gvkeys_sbti$gvkey 
                       %in% sbti_2023_sbti_id_gvkey_added$gvkey)] %>% 
  unique() %>% .[41]
# "BHARAT FORGE LTD"
# in 2023 data as "Bharat Forge"; valid sbti_id "40012160"
sbti_2023$sbti_id[which(
  sbti_2023$company_name=="Bharat Forge"
)] <- "40012160"
sbti_2023$gvkey[which(
  sbti_2023$company_name=="Bharat Forge"
)] <- unique(rrpanel$gvkey[which(rrpanel$conm=="BHARAT FORGE LTD")])

manually_identified_commitments_and_targets <- sbti_2023 %>% 
  filter(!is.na(sbti_id))
# All the above are firms that I matched previously (in 2023)... 
# and didn't come up when i tried to match firms in sbti_id and gvkey 
# previously. See note in merging_sbti_2024.R (OneNote page).
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Isolating commitments from targets in 2022, 2023, and 2024 ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # In the 2022 dataset, near_term_target_status is the only indicator
    # that differentiates between "committed" and "targets". Though there are
    # long term targets, these represent a subset of the near term target 
    # setters. There are no long term target "committed" values.
sbti_2022_commitments <- sbti_2022_sbti_id_gvkey_added %>% 
  filter(near_term_target_status == "Committed")
sbti_2022_targets <- sbti_2022_sbti_id_gvkey_added %>% 
  filter(near_term_target_status == "Targets Set")

sbti_2023_commitments <- sbti_2023_sbti_id_gvkey_added %>%
  filter(near_term_target_status == "Committed")
sbti_2023_targets <- sbti_2023_sbti_id_gvkey_added %>%
  filter(near_term_target_status == "Targets Set")

manually_identified_commitments <- 
  manually_identified_commitments_and_targets %>% 
  filter(near_term_target_status == "Committed")
manually_identified_targets <- manually_identified_commitments_and_targets %>%
  filter(near_term_target_status == "Targets Set")

sbti_2024_commitments <- all_sbti_targets_and_commitments %>%
  filter(!sbti_id %in% all_sbti_targets$sbti_id)
sbti_2024_targets <- all_sbti_targets

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Aggregating commitments and creating a list of firms to exclude ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  ## 2022 ####
excluded_target_setters_2022 <- sbti_2022_targets
included_commitments_2022 <- sbti_2022_commitments
    # N = 641

  ## 2023 ####
included_commitments_2023 <- sbti_2023_commitments %>%
  filter(!(sbti_id %in% sbti_2022_commitments$sbti_id))
    # N = 141

  ## manually identified ####
included_manually_identified_commitments <- manually_identified_commitments %>%
  filter(!(sbti_id %in% sbti_2022_commitments$sbti_id) &
           !(sbti_id %in% sbti_2023_commitments$sbti_id))
    # N = 10

  ## 2024 ####
included_commitments_2024 <- sbti_2024_commitments %>%
  filter(!(sbti_id %in% sbti_2022_commitments$sbti_id) &
           !(sbti_id %in% manually_identified_commitments$sbti_id) &
           !(sbti_id %in% sbti_2023_commitments$sbti_id))
  # Since the 2024 data can have multiple observations per firm, I want to
  # retain the single observation that indicates the initial commitment date
included_commitments_2024 <- included_commitments_2024 %>%
  mutate(date_published = as.Date(date_published)) %>%
  group_by(sbti_id) %>%
  # for each gvkey, take the single row with the minimal date_published
  slice_min(order_by = date_published, n = 1, with_ties = FALSE) %>%
  ungroup()
  # N = 2307

  # Creating year variables prior to merging the commitments
    # First just need to update the date formatting for the manual set
included_manually_identified_commitments$date <-
  dmy(included_manually_identified_commitments$date)

included_commitments_2022 <- included_commitments_2022 %>%
  # Extract the 4-digit year as numeric
  mutate(year = as.numeric(format(date, "%Y")))

included_commitments_2023 <- included_commitments_2023 %>%
  # Extract the 4-digit year as numeric
  mutate(year = as.numeric(format(date, "%Y")))

included_manually_identified_commitments <- 
  included_manually_identified_commitments %>%
  # Extract the 4-digit year as numeric
  mutate(year = as.numeric(format(date, "%Y")))

included_commitments_2024$year <- year(included_commitments_2024$date_published)

commitments_all <- rbind(included_commitments_2022 %>%
                           select(sbti_id, year),
                         included_commitments_2023 %>%
                           select(sbti_id, year),
                         included_commitments_2024 %>%
                           select(sbti_id, year))
  # some date values are blank -- I need to make sure to drop these observations
  # while adding the associated gvkey values to the exclude list
excluded_missing_commitment_dates <- commitments_all %>%
  filter(is.na(year))

commitments_all <- commitments_all %>%
  filter(!is.na(year))
    # N = 3083
commitments_all$year %>% min()
commitments_all$sbti_id %>% duplicated() %>% which()
commitments_all$gvkey %>% duplicated() %>% which()

  # naming the variable accordingly
colnames(commitments_all)[2] <- "initial_commitment_year"
commitments_all %>% group_by(initial_commitment_year) %>% count()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Adding initial_commitment_year to rrpanel ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rrpanel_sbti_commitments_2024_1 <- rrpanel_sbti_commitments_2024
  ## First, adding sbti_id to rrpanel
  rrpanel_sbti_commitments_2024_2 <- merge(rrpanel_sbti_commitments_2024_1,
                                         all_gvkey_sbti_matches_lei_prior %>%
                                           select(gvkey, sbti_id),
                                         all.x = TRUE)
  
  ## Second, excluding all the firms that set targets prior to 2022 or had
  ## missing date values from their commitments.
  rrpanel_sbti_commitments_2024_3 <- rrpanel_sbti_commitments_2024_2 %>%
    filter(
    !(sbti_id %in% excluded_target_setters_2022$sbti_id) &
      !(sbti_id %in% excluded_missing_commitment_dates$sbti_id)
    )
    # Consolidating all the target setters to be exluded for future reference
  excluded_missing_commitment_dates_obs <- sbti_2022_commitments %>%
    filter(sbti_id %in% excluded_missing_commitment_dates$sbti_id)
  exclude_all <- rbind(excluded_target_setters_2022,
                       excluded_missing_commitment_dates_obs)
  saveRDS(exclude_all, "exclude_all.rds")
  length(which(unique(rrpanel$gvkey) %in% exclude_all$gvkey))
  # 680
  
  ## Third, adding initial_commitment_year to rrpanel
  rrpanel_sbti_commitments_2024_4 <- merge(rrpanel_sbti_commitments_2024_3,
                                            commitments_all %>%
                                              select(sbti_id, initial_commitment_year),
                                            all.x = TRUE)
  rrpanel_sbti_commitments_2024 <- rrpanel_sbti_commitments_2024_4
  
  # Saving rrpanel_sbti_commitments_2024.rds ####
  saveRDS(rrpanel_sbti_commitments_2024, "rrpanel_sbti_commitments_2024.rds")
  
  rrpanel_sbti_commitments_2024 %>% filter(!is.na(initial_commitment_year)) %>%
    select(gvkey) %>% n_distinct()
  # 973 firms with commitments
  
  rrpanel_sbti_commitments_2024 %>% filter(!is.na(initial_commitment_year)) %>% arrange(sbti_id, year) %>% view()
  