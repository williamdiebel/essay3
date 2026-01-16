setwd("~/Dropbox/NetZero WD Shared Folder/Data")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preamble ####
# The purpose of this script is to build the panel extension (2021 - 2023) with
# several stages:
#   1. import and build framework with compustat data 
#   2. add cdp scp membership data
#   3. add reprisk data
#   4. add bloomberg esg data
#   5. add initial_commitment_year to comp panel
#   6. merge with extant rrpanel (imported as rrpanel_2024 below)
#   7. add sbti committed variable data
#   8. checking and revising all variables to be used in the analysis
#   9. save and export updated panel (panel_extended.rds)
rrpanel_2024 <- readRDS("rrpanel_sbti_commitments_cdp_scp_members_2024.rds") 
library(tidyverse)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   1. import and build framework with compustat data ####
gvkey_2021_2023 <- readRDS("gvkey_2021_2023.rds")
comp_na <- read_csv("compustat_21_24/comp_na_2021_2024.csv")
comp_global <- read_csv("compustat_21_24/comp_global_2021_2024.csv")

  # First step is to work with the NA and global datasets to create roa vbl.
  # NOTE: the version to be used is computed based on winsorized base measures
  # and is created later in the present script, and named "roa_oibdp_w1_at_w1"

  comp_na <- comp_na %>%
    mutate(roa = oibdp / at)
  
  comp_global <- comp_global %>%
    mutate(roa = oibdp / at)
  
  # Second step is to combine NA and global datasets. As I want to avoid
  # introducing duplicates, I'll just pull firms from the global dataset that
  # aren't already represented in the NA dataset.
  
  comp_joined <- bind_rows(
    comp_na, 
    comp_global %>% filter(!(gvkey %in% comp_na$gvkey))
    )
  
  # Third, restricting observations to fiscal year 2021 - 2023
  
  comp_joined <- comp_joined %>%
    filter(fyear %in% 2021:2023)
  # 31617 observations
  
  # Identifying and removing firm-year duplicated
  
    # Identifying firm-year duplcates
  dupes <- comp_joined %>%
    group_by(gvkey, fyear) %>%
    count() %>%
    filter(n == 2) # 36 observations will be removed
  
    # extracting all firm-year duplicated associated obs
  comp_filtered <- comp_joined %>%
    semi_join(dupes %>% distinct(gvkey, fyear),
              by = c("gvkey", "fyear"))
  
    # keeping only values with non-infinite roa values and higher roa values of
    # the two (no two ties are included in such cases)
  comp_filtered_2 <- comp_filtered %>%
    filter(!is.infinite(roa)) %>%
    group_by(gvkey, fyear) %>%
    slice_max(roa, n = 1, with_ties = FALSE)
  
  # drop rows that are in comp_filtered but *not* in comp_filtered_2 ----
  # first, identify the rows I don’t want (difference between the two tables)
  rows_to_drop <- anti_join(
    comp_filtered,
    comp_filtered_2,
    by = c("gvkey", "fyear", "roa")   # using the columns that define a row
  )
  
  # second, removing rows_to_drop from comp_joined
  comp_joined <- comp_joined %>%
    anti_join(rows_to_drop, by = c("gvkey", "fyear", "roa"))
  
  # Checking to make sure there are no more duplicated
  comp_joined %>%
    group_by(gvkey, fyear) %>%
    count() %>%
    filter(n == 2)
  # Checks out
  # And the number of observations preserved is 36 less than I started with,
  # which also is correct (31617 - 36 = 31581).
  
  # Adding in the demographic characteristics and firm id values from the panel
  
    # isolating all the demographic/id vbls from rrpanel
  demographics_and_ids <- rrpanel_2024 %>% 
    select(gvkey,
           primary_isin,
           conm, 
           reprisk_id, 
           factset_id, 
           sbti_id, 
           FourDigitName, 
           headquarter_country,
           country_industry) %>% 
    distinct()
    # adding demographic/id vbls to comp_joined
  comp_joined_ids <- merge(comp_joined,
        demographics_and_ids)
  
  # Up to this point, I have a panel with all needed compustat data from 2021 -
  # 2023. There are no remaining firm-year duplicates and each row contains 
  # the appropriate ids to merge with all the other needed data from sbti, 
  # reprisk, bloomberg, cdp_scp, etc.
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   2. add cdp scp membership data ####

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    ## 2a. preparing the cdp scp membership data ####
  
    # panel of cdp scp members in 2021 & 2022 w matched gvkeys from 
    # updating_cdp_.R
    cdp_scp_members_gvkey <- readRDS("cdp_scp_members_gvkey.rds")
  
    # creating the cdp_sc_member variable (for continuity with extant rrpanel)
    cdp_scp_members_gvkey$cdp_sc_member <- 1

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    ## 2b. preparing the comp panel framework for merging
    
    # editing the year vbl name for consistency (changing from fyear)
    colnames(comp_joined_ids)[3] <- "year"
    comp_joined_ids$gvkey <- as.character(comp_joined_ids$gvkey)
    
# merging the datasets to add the cdp_sc_member vbl
comp_cdpscp <- merge(comp_joined_ids,
                     cdp_scp_members_gvkey,
                     all.x = TRUE)
comp_cdpscp %>% filter(cdp_sc_member == 1) %>% select(gvkey) %>% n_distinct()
comp_cdpscp %>% filter(cdp_sc_member == 1) %>% nrow()

# how many new cdp sc members are in the merged data compared to rrpanel?
length(
  which(!
          (
            unique(comp_cdpscp$gvkey[which(comp_cdpscp$cdp_sc_member == 1)]) 
            %in% 
              unique(rrpanel_2024$gvkey[which(rrpanel_2024$cdp_sc_member == 1)])
            )
        )
  ) # [1] 42

# how many total cdp sc members are in the extended panel? 
unique(
  c(
    comp_cdpscp$gvkey[which(comp_cdpscp$cdp_sc_member == 1)],
    rrpanel_2024$gvkey[which(rrpanel_2024$cdp_sc_member == 1)]
  )
) %>% length() # [1] 131

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   3. add reprisk data ####
incidents <- read_csv(
  "~/Dropbox/Mac (2)/Documents/Data/RepRisk Datafeed/Incidents.csv")
incidents$year <- year(incidents$incident_date)

###           - e_incidents ####
e_incidents <- incidents %>% filter(environment == TRUE)
e_incidents <- e_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(e_incidents = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp, e_incidents, all.x = TRUE)
###           - climate_incidents ####
climate_incidents <- incidents %>% filter(climate_ghg_pollution == TRUE 
                                          | 
                                            greenhouse_gas_emissions == TRUE)
climate_incidents <- climate_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_incidents = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, climate_incidents, all.x = TRUE)
###           - sc_incidents ####
sc_incidents <- incidents %>% filter(supply_chain_issues == TRUE)
sc_incidents <- sc_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(sc_incidents = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, sc_incidents, all.x = TRUE)
###           - esc_incidents ####
esc_incidents <- incidents %>% filter(environment == TRUE 
                                      & 
                                        supply_chain_issues == TRUE)
esc_incidents <- esc_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(esc_incidents = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, esc_incidents, all.x = TRUE)
###           - climate_sc_incidents ####
climate_sc_incidents <- incidents %>% 
  filter(supply_chain_issues == TRUE 
         & (climate_ghg_pollution == TRUE | greenhouse_gas_emissions == TRUE))
climate_sc_incidents <- climate_sc_incidents %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_sc_incidents = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, climate_sc_incidents, all.x = TRUE)
###           - e_incidents_highsev ####
e_incidents_highsev <- incidents %>% filter(environment == TRUE & severity == 3)
e_incidents_highsev <- e_incidents_highsev %>%
  group_by(reprisk_id, year) %>%
  summarise(e_incidents_highsev = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, e_incidents_highsev, all.x = TRUE)
###           - climate_incidents_highsev ####
climate_incidents_highsev <- incidents %>% 
  filter(severity == 3 & (climate_ghg_pollution == TRUE 
                          | greenhouse_gas_emissions == TRUE))
climate_incidents_highsev <- climate_incidents_highsev %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_incidents_highsev = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, climate_incidents_highsev, all.x = TRUE)
###           - sc_incidents_highsev ####
sc_incidents_highsev <- incidents %>% 
  filter(severity == 3 & supply_chain_issues == TRUE)
sc_incidents_highsev <- sc_incidents_highsev %>%
  group_by(reprisk_id, year) %>%
  summarise(sc_incidents_highsev = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, sc_incidents_highsev, all.x = TRUE)
###           - esc_incidents_highsev ####
esc_incidents_highsev <- incidents %>% 
  filter(severity == 3 & environment == TRUE & supply_chain_issues == TRUE)
esc_incidents_highsev <- esc_incidents_highsev %>%
  group_by(reprisk_id, year) %>%
  summarise(esc_incidents_highsev = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, esc_incidents_highsev, all.x = TRUE)
###           - climate_sc_incidents_highsev ####
climate_sc_incidents_highsev <- incidents %>% 
  filter(severity == 3 
         & supply_chain_issues == TRUE 
         & (climate_ghg_pollution == TRUE | greenhouse_gas_emissions == TRUE))
climate_sc_incidents_highsev <- climate_sc_incidents_highsev %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_sc_incidents_highsev = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, 
                        climate_sc_incidents_highsev, all.x = TRUE)
###           - e_incidents_highreach ####
e_incidents_highreach <- incidents %>% filter(environment == TRUE & reach == 3)
e_incidents_highreach <- e_incidents_highreach %>%
  group_by(reprisk_id, year) %>%
  summarise(e_incidents_highreach = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, e_incidents_highreach, all.x = TRUE)
###           - climate_incidents_highreach ####
climate_incidents_highreach <- incidents %>% 
  filter(reach == 3 
         & (climate_ghg_pollution == TRUE | greenhouse_gas_emissions == TRUE))
climate_incidents_highreach <- climate_incidents_highreach %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_incidents_highreach = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, 
                        climate_incidents_highreach, all.x = TRUE)
###           - sc_incidents_highreach ####
sc_incidents_highreach <- incidents %>%
  filter(reach == 3 & supply_chain_issues == TRUE)
sc_incidents_highreach <- sc_incidents_highreach %>%
  group_by(reprisk_id, year) %>%
  summarise(sc_incidents_highreach = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, sc_incidents_highreach, all.x = TRUE)
###           - esc_incidents_highreach ####
esc_incidents_highreach <- incidents %>% 
  filter(reach == 3 & environment == TRUE & supply_chain_issues == TRUE)
esc_incidents_highreach <- esc_incidents_highreach %>%
  group_by(reprisk_id, year) %>%
  summarise(esc_incidents_highreach = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, esc_incidents_highreach, all.x = TRUE)
###           - climate_sc_incidents_highreach ####
climate_sc_incidents_highreach <- incidents %>% 
  filter(reach == 3 
         & supply_chain_issues == TRUE 
         & (climate_ghg_pollution == TRUE | greenhouse_gas_emissions == TRUE))
climate_sc_incidents_highreach <- climate_sc_incidents_highreach %>%
  group_by(reprisk_id, year) %>%
  summarise(climate_sc_incidents_highreach = n()) %>%
  ungroup()
comp_cdpscp_rr <- merge(comp_cdpscp_rr, 
                        climate_sc_incidents_highreach, all.x = TRUE)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   4. add bloomberg esg data ####
bb_panel <- readRDS("bb_panel.rds")

# merging bb with extended panel data
comp_cdpscp_rr_bb <- merge(comp_cdpscp_rr,
                           bb_panel,
                           by = c("primary_isin", "year"),
                           all.x = TRUE)
# adding the correct nomenclature to ensure continuity
comp_cdpscp_rr_bb$e_disc_coalesced_filled <- comp_cdpscp_rr_bb$e_disc_2025

comp_cdpscp_rr_bb$e_disc_coalesced_zeros <- ifelse(
  is.na(comp_cdpscp_rr_bb$e_disc_coalesced_filled), 
  0,
  comp_cdpscp_rr_bb$e_disc_coalesced_filled
  )

comp_cdpscp_rr_bb$e_disc_missing <- ifelse(
  is.na(comp_cdpscp_rr_bb$e_disc_coalesced_filled),
        1,
        0
  )

comp_cdpscp_rr_bb$scope1_coalesced_filled <- comp_cdpscp_rr_bb$scope1_2025

comp_cdpscp_rr_bb$scope1_zeros <- ifelse(
  is.na(comp_cdpscp_rr_bb$scope1_coalesced_filled), 
  0,
  comp_cdpscp_rr_bb$scope1_coalesced_filled
)

comp_cdpscp_rr_bb$scope1_missing <- ifelse(
  is.na(comp_cdpscp_rr_bb$scope1_coalesced_filled),
  1,
  0
)

# how many observations have e_disc during 2021 - 2023?
length(which(!(is.na(comp_cdpscp_rr_bb$e_disc_coalesced_filled))))
  # [1] 13986
  13986/31581
    # 44% of observations
# how many firms have e_disc during 2021 - 2023?
comp_cdpscp_rr_bb %>% filter(!is.na(e_disc_coalesced_filled)) %>%
  select(gvkey) %>% n_distinct()
  # [1] 6608
  6608/10871
    # 61% of firms

# how many observations have scope1 during 2021 - 2023?
length(which(!(is.na(comp_cdpscp_rr_bb$scope1_coalesced_filled))))
  # [1] 10326
  10326/31581
    # 33% of observations
# how many firms have scope1 during 2021 - 2023?
comp_cdpscp_rr_bb %>% filter(!is.na(scope1_coalesced_filled)) %>%
  select(gvkey) %>% n_distinct()
  # [1] 4530
  4530/10871
    # 42% of firms

# what is the average disclosure score for firms?
  
  # excluding non-rated firms
  mean(comp_cdpscp_rr_bb$e_disc_coalesced_filled, na.rm = TRUE)
    # [1] 30.0
  
  # applying 0 for non-rated firms
  mean(comp_cdpscp_rr_bb$e_disc_coalesced_zeros)
    # [1] 13.3
  
# what is the average disclosure score for firms that have scope 1 versus those
# that do not?
  
  # not reporting scope 1
  comp_cdpscp_rr_bb %>% filter(is.na(scope1_coalesced_filled)) %>%
    summarise(avg_disc = mean(e_disc_coalesced_filled, na.rm = TRUE))
    # 11.5 if excluding non rated disclosures  
  comp_cdpscp_rr_bb %>% filter(is.na(scope1_coalesced_filled)) %>%
    summarise(avg_disc = mean(e_disc_coalesced_zeros))
    # 2.8 if applying 0 for non rated disclosures
  
  # reporting scope 1
  comp_cdpscp_rr_bb %>% filter(!is.na(scope1_coalesced_filled)) %>%
    summarise(avg_disc = mean(e_disc_coalesced_filled, na.rm = TRUE))
  # 40.9 if excluding non rated disclosures  
  comp_cdpscp_rr_bb %>% filter(!is.na(scope1_coalesced_filled)) %>%
    summarise(avg_disc = mean(e_disc_coalesced_zeros))
  # 34.9 if applying 0 for non rated disclosures
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # aligning datadate types
  rrpanel_2024$datadate <- ymd(rrpanel_2024$datadate)
  comp_cdpscp_rr_bb$datadate <- mdy(comp_cdpscp_rr_bb$datadate)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   5. add initial_commitment_year vbl to comp panel ####
  
  commitments <- rrpanel_2024 %>% 
    filter(!is.na(initial_commitment_year)) %>%
    select(gvkey, initial_commitment_year) %>% distinct()
  
  comp_cdpscp_rr_bb_sbti <- merge(comp_cdpscp_rr_bb,
                                  commitments,
                                  all.x = T)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   6. merge extended comp panel with extant rrpanel ####
  
  # ensuring datadate values are formatted consistently
rrpanel_extended <- bind_rows(rrpanel_2024,
                              comp_cdpscp_rr_bb_sbti)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   7. add sbti_committed variable ####

  # first, i need to ensure that the initial commitment year vbl is properly
  # extended across all observations for sbti firms
  rrpanel_extended$conm[which(
    !is.na(rrpanel_extended$initial_commitment_year))] %>% unique() %>% 
    .[c(1:5)]
  
  rrpanel_extended %>% 
    filter(conm == "WH SMITH PLC") %>% select(year, initial_commitment_year)
    # looks good
  rrpanel_extended %>% 
    filter(conm == "SMITHS GROUP PLC") %>% select(year, initial_commitment_year)
    # looks good
  rrpanel_extended %>% 
    filter(conm == "COSTAIN GROUP PLC") %>% select(year, initial_commitment_year)
    # looks good
  rrpanel_extended %>% 
    filter(conm == "BUNZL PLC") %>% select(year, initial_commitment_year)
    # looks good
  rrpanel_extended %>% 
    filter(conm == "BILFINGER SE") %>% select(year, initial_commitment_year)
    # looks good

  # new vbl will be called "sbti_commitment_2024"
rrpanel_extended$sbti_commitment_2024 <- ifelse(
  is.na(rrpanel_extended$initial_commitment_year),
  NA,
  ifelse(rrpanel_extended$year >= rrpanel_extended$initial_commitment_year,
         1,
         0)
)

    rrpanel_extended %>% 
      filter(conm == "WH SMITH PLC") %>% select(year, 
                                                initial_commitment_year,
                                                sbti_commitment_2024) %>%
      arrange(year)
    # looks good
rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                     year,
                                                     initial_commitment_year,
                                                     sbti_commitment_2024)
# filling in the NAs with zeros
rrpanel_extended$sbti_commitment_2024 <- ifelse(
  is.na(rrpanel_extended$sbti_commitment_2024),
        0,
        rrpanel_extended$sbti_commitment_2024
  )

    rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                         year,
                                                         initial_commitment_year,
                                                         sbti_commitment_2024)
    # looks good

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   8. checking and revising all variables to be used in the analysis ####

## DV ####
rrpanel_extended$sbti_commitment_2024
    # already verified the variable is consistent across all periods in the
    # previous section

## Key IV 1 ####
rrpanel_extended$esc_incidents
    rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                         year,
                                                         total_incident_count,
                                                         esc_incidents)
    # values are accurate and correspond exactly to the old version of the vbl
    # total_incident_count, just need to replace NAs with zero.
rrpanel_extended$esc_incidents <- ifelse(
  is.na(rrpanel_extended$esc_incidents),
  0,
  rrpanel_extended$esc_incidents)
    # double checking i filled in correctly
    rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                         year,
                                                         total_incident_count,
                                                         esc_incidents)
    # checks out

    # Note: I should edit all other RepRisk measures in the same way, i.e., all
    # variables should code NA with 0
    
    rrpanel_extended$e_incidents <- ifelse(
      is.na(rrpanel_extended$e_incidents),
      0,
      rrpanel_extended$e_incidents)

    rrpanel_extended$climate_incidents <- ifelse(
      is.na(rrpanel_extended$climate_incidents),
      0,
      rrpanel_extended$climate_incidents)
    
    rrpanel_extended$sc_incidents <- ifelse(
      is.na(rrpanel_extended$sc_incidents),
      0,
      rrpanel_extended$sc_incidents)
    
    rrpanel_extended$climate_sc_incidents <- ifelse(
      is.na(rrpanel_extended$climate_sc_incidents),
      0,
      rrpanel_extended$climate_sc_incidents)
    
    rrpanel_extended$e_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$e_incidents_highsev),
      0,
      rrpanel_extended$e_incidents_highsev)
    
    rrpanel_extended$e_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$e_incidents_highsev),
      0,
      rrpanel_extended$e_incidents_highsev)
    
    rrpanel_extended$climate_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$climate_incidents_highsev),
      0,
      rrpanel_extended$climate_incidents_highsev)
    
    rrpanel_extended$sc_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$sc_incidents_highsev),
      0,
      rrpanel_extended$sc_incidents_highsev)
    
    rrpanel_extended$esc_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$esc_incidents_highsev),
      0,
      rrpanel_extended$esc_incidents_highsev)
    
    rrpanel_extended$climate_sc_incidents_highsev <- ifelse(
      is.na(rrpanel_extended$climate_sc_incidents_highsev),
      0,
      rrpanel_extended$climate_sc_incidents_highsev)
    
    rrpanel_extended$e_incidents_highreach <- ifelse(
      is.na(rrpanel_extended$e_incidents_highreach),
      0,
      rrpanel_extended$e_incidents_highreach)
    
    rrpanel_extended$climate_incidents_highreach <- ifelse(
      is.na(rrpanel_extended$climate_incidents_highreach),
      0,
      rrpanel_extended$climate_incidents_highreach)
    
    rrpanel_extended$sc_incidents_highreach <- ifelse(
      is.na(rrpanel_extended$sc_incidents_highreach),
      0,
      rrpanel_extended$sc_incidents_highreach)
    
    rrpanel_extended$esc_incidents_highreach <- ifelse(
      is.na(rrpanel_extended$esc_incidents_highreach),
      0,
      rrpanel_extended$esc_incidents_highreach)
    
    rrpanel_extended$climate_incidents_highreach <- ifelse(
      is.na(rrpanel_extended$climate_incidents_highreach),
      0,
      rrpanel_extended$climate_incidents_highreach)
    
## Key IV 2 ####
rrpanel_extended$cdp_sc_member
rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                     year,
                                                     cdp_sc_member)
  # contains NAs for non-members in 2021 - 2023
  # first... just double checking that members have appropriate values in 2021 -
  # 2023
    rrpanel_extended %>% filter(year > 2020 & cdp_sc_member == 1) %>%
      select(conm,
             year,
             cdp_sc_member)
    # looks good
  
rrpanel_extended$cdp_sc_member <- ifelse(
  is.na(rrpanel_extended$cdp_sc_member),
  0,
  rrpanel_extended$cdp_sc_member
)
  # qc check
  rrpanel_extended %>% arrange(gvkey, year) %>% select(conm,
                                                     year,
                                                     cdp_sc_member)
  # all good

## Controls ####
  # importing currency conversion tables
rrpanel_extended$datadate %>% min() # [1] "2007-06-30"
rrpanel_extended$datadate %>% max() # [1] "2024-05-31"
      # define the cap once
      cap_date <- as.Date("2023-12-31")
      # option 1: pmin — simplest
      rrpanel_extended$datadate <- pmin(rrpanel_extended$datadate, cap_date)
currency_07_15 <- read_csv("compustat_21_24/daily_exchange_rates_2007_2015.csv")
currency_16_24 <- read_csv("compustat_21_24/daily_exchange_rates_2016_2024.csv")
currency_to_gbp <- rbind(currency_07_15, currency_16_24)
currency_to_usd <- rbind(currency_07_15, currency_16_24)

  # creating a "to gbp" table from all currencies
    # isolating vbls of interest
  currency_to_gbp <- currency_to_gbp %>% select(datadate, tocurd, exratd)
    # creating a "to gbp" factor from the tocurd
  currency_to_gbp$to_gbp <- 1/currency_to_gbp$exratd
  colnames(currency_to_gbp)[2] <- "curcd"
  currency_to_gbp <- currency_to_gbp %>% select(-exratd)
  currency_to_gbp
  
  # creating a "to usd" table from gbp
  # isolating vbls of interest
  currency_to_usd <- currency_to_usd %>% 
    filter(tocurd == "USD" & fromcurd == "GBP")
  currency_to_usd <- currency_to_usd %>%
    select(datadate, exratd)
  colnames(currency_to_usd)[2] <- "gbp_to_usd"
  currency_to_usd
  
    # adding to_gbp
  rrpanel_extended <- merge(rrpanel_extended,
                            currency_to_gbp,
                            all.x = TRUE)
  rrpanel_extended %>% filter(is.na(to_gbp)) %>% view()
  currency_to_gbp %>% filter(curcd == "ZMK") %>% arrange(datadate) %>% view()
  # correcting values for the two obs that did not pick up to_gbp due to missing
  # currency exchange dates for the exact datadates. Subbing in the values for
  # prior days.
  rrpanel_extended$datadate[which(
    rrpanel_extended$curcd == "ZMK" &
      rrpanel_extended$datadate == "2007-06-30"
  )] <- as.Date("2007-06-29")
  rrpanel_extended$datadate[which(
    rrpanel_extended$curcd == "ZMK" &
      rrpanel_extended$datadate == "2007-09-30"
  )] <- as.Date("2007-09-28")
    # remerging
  rrpanel_extended <- rrpanel_extended %>%
    select(-to_gbp)
  rrpanel_extended <- merge(rrpanel_extended,
                            currency_to_gbp,
                            all.x = TRUE)
  rrpanel_extended %>% filter(is.na(to_gbp)) %>% view()
  
    # adding gbp_to_usd
  rrpanel_extended <- merge(rrpanel_extended,
                            currency_to_usd,
                            all.x = TRUE)
  rrpanel_extended %>% filter(is.na(gbp_to_usd)) %>% view()
  
  # note: all financial measures must first be multiplied by to_gbp, then by
  # gbp_to_usd to obtain figures in USD

  # at_usd_winsorized_1_log
  
    # currency conversion to USD
    rrpanel_extended$at_usd <- rrpanel_extended$at * 
      rrpanel_extended$to_gbp * rrpanel_extended$gbp_to_usd
  
    # winsorization
     # compute the cut-offs, ignoring NAs
      cuts <- quantile(rrpanel_extended$at_usd,
                       probs = c(0.01, 0.99),
                       na.rm = TRUE)
      
    # winsorize and create the new column
    rrpanel_extended$at_usd_winsorized_1 <- pmin(
      pmax(rrpanel_extended$at_usd, cuts[1]), cuts[2])
      # manually inspecting values to ensure things look correct
      rrpanel_extended %>% filter(is.na(at)) %>% view()
      rrpanel_extended <- rrpanel_extended %>% filter(!is.na(at))
      rrpanel_extended %>% filter(at == 0) %>% view()
      rrpanel_extended %>% filter(is.na(at_usd)) %>% 
        select(conm,
               datadate,
               year,
               curcd,
               to_gbp,
               gbp_to_usd) %>% 
        view() 
      
      min(rrpanel_extended$at_usd)
      min(rrpanel_extended$at_usd_winsorized_1)
      ggplot(rrpanel_extended, aes(at_usd)) + geom_boxplot()
      ggplot(rrpanel_extended, aes(at_usd_winsorized_1)) + geom_boxplot()
    # log transform
      rrpanel_extended$at_usd_winsorized_1_log <- log(
        rrpanel_extended$at_usd_winsorized_1
      )
  # oibdp_usd_winsorized_1
    # currency conversion to USD
      rrpanel_extended$oibdp_usd <- rrpanel_extended$oibdp * 
        rrpanel_extended$to_gbp * rrpanel_extended$gbp_to_usd
    # winsorization
      # compute the cut-offs, ignoring NAs
      cuts <- quantile(rrpanel_extended$oibdp_usd,
                       probs = c(0.01, 0.99),
                       na.rm = TRUE)
      
      # winsorize and create the new column
      rrpanel_extended$oibdp_usd_winsorized_1 <- pmin(
        pmax(rrpanel_extended$oibdp_usd, cuts[1]), cuts[2])
    # look at extreme values and remove <= 0 if necessary
      min(rrpanel_extended$oibdp_usd, na.rm = TRUE)
      min(rrpanel_extended$oibdp_usd_winsorized_1, na.rm = TRUE)
      ggplot(rrpanel_extended %>% filter(!is.na(oibdp_usd)), aes(oibdp_usd)) + 
        geom_boxplot()
      ggplot(rrpanel_extended %>% filter(!is.na(oibdp_usd)), 
             aes(oibdp_usd_winsorized_1)) + geom_boxplot()
  # roa_oibdp_w1_at_w1
    # calculate as oibdp_usd_winsorized_1/at_usd_winsorized_1
      rrpanel_extended <- rrpanel_extended %>% mutate(
        roa_oibdp_w1_at_w1 = oibdp_usd_winsorized_1/at_usd_winsorized_1
      )
  # lt_usd_winsorized_1
    # currency conversion to USD
      rrpanel_extended$lt_usd <- rrpanel_extended$lt *
        rrpanel_extended$to_gbp * rrpanel_extended$gbp_to_usd
    # winsorization
      # compute the cut-offs, ignoring NAs
      cuts <- quantile(rrpanel_extended$lt_usd,
                       probs = c(0.01, 0.99),
                       na.rm = TRUE)
      
      # winsorize and create the new column
      rrpanel_extended$lt_usd_winsorized_1 <- pmin(
        pmax(rrpanel_extended$lt_usd, cuts[1]), cuts[2])
      ggplot(rrpanel_extended %>% filter(!is.na(lt_usd)), aes(lt_usd)) +
        geom_boxplot()
      ggplot(rrpanel_extended %>% filter(!is.na(lt_usd)), 
             aes(lt_usd_winsorized_1)) +
        geom_boxplot()
  # tll_lt_w1_at_w1
    # calculate as lt_usd_winsorized_1/at_usd_winsorized_1
      rrpanel_extended <- rrpanel_extended %>% mutate(
        tll_lt_w1_at_w1 = lt_usd_winsorized_1/at_usd_winsorized_1
      )
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #   9. save and export ####
  saveRDS(rrpanel_extended, "panel_extended.rds")
  
  
  