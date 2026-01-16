# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# The purpose of this script is to take the sbti target level data (downloaded 
# in April 2025) and turn it into a series of measures that capture targets, 
# commitments, and their specificity.
setwd("~/Dropbox/NetZero WD Shared Folder/Data")
library(tidyverse)
sbti_2025 <- readxl::read_excel("sbti_data_04_25/ByTargetsDownload.xlsx")
sbti_2024 <- sbti_2025 %>%
  filter(year(date_published)<2025)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Preliminary data cleaning ####
# For two of the important variables (target and status), missing data is
# inconsistently coded. Therefore, I want to standardize missing entries before
# proceeding.
sbti_2024$target %>% unique()
sbti_2024$target[which(sbti_2024$target=="NA")] <- NA
sbti_2024$status %>% unique()
sbti_2024$status[which(sbti_2024$status=="NA")] <- NA
sbti_2024$scope[which(sbti_2024$scope=="NA")] <- NA
sbti_2024$commitment_type[which(sbti_2024$commitment_type=="NA")] <- NA
sbti_2024$lei[which(sbti_2024$lei=="NA")] <- NA
sbti_2024$isin[which(sbti_2024$isin=="NA")] <- NA
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Read me ####
#   "ByTargetsDownload.xlsx" (sbti_2025) contain three types of mutually 
#   exclusive observations: 
#       I: Targets - 19017
#      II: Commitments - 3786
#     III: Removed Commitments - 1400 observations
#
#   ## Identifying a target observation #### 
#
#     Two mutually exclusive formats:
#
#     1.target %in% c("Net-zero", "Long-term", "Long-term pre-CNZS", 
#                     "Near-term")
#       - these observations have missing data (NA) for $status
#
#     2.status == "Target set"
#       - these observations have missing data (NA) for $target
#
#   ## Identifying a commitment observation ####
#
#     1. status %in% c("Active", "Extended")
# 
#   ## Identifying a removed commitment observation ####
#     
#     1. status == "Removed"
#
#     The next line shows each reason for a commitment being removed. 
sbti_2024 %>%
  filter(status == "Removed") %>%
  group_by(reason_for_commitment_extension_or_removal) %>%
  count() %>%
  arrange(desc(n))
      # # A tibble: 5 × 2
      # # Groups:   reason_for_commitment_extension_or_removal [5]
      #   reason_for_commitment_extension_or_removal     n
      #   <chr>                                      <int>
      # 1 Expired commitment                          1313
      # 2 Withdrawn commitment                          46
      # 3 Company change                                28
      # 4 Target set at parent company level            12
      # 5 NA                                             1
#
#     The most common reason (1313/1400 = 94%) is that the commitment expired 
#     (firms have 24 months from making their commitment to achieving sbti's 
#     approval for setting the formal target).
#     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Counting the number of observations corresponding to targets, commitments, and
# removed types.
sbti_2024 %>%
  filter(
    target %in% c("Net-zero", "Long-term", "Long-term pre-CNZS", "Near-term")
    |
      status == "Target set" 
  ) %>%
  nrow() # [1] 19017
sbti_2024 %>%
  filter(
    status %in% c("Active", "Extended") 
  ) %>%
  nrow() # [1] 3786
sbti_2024 %>%
  filter(
    status == "Removed" 
  ) %>%
  nrow() # [1] 1400
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Dividing dataset into target/commitment hierarchy ####

      ## Net-zero ####

        ### Targets ####

# Identifying all net zero targets
net_zero_sbti_targets <- sbti_2024 %>%
  filter(
    (
      status == "Target set"
      & commitment_type %in% c("Net-zero", "BA1.5 Option 1", "BA1.5 Option 2")
    ) |
      target == "Net-zero"
  ) # 3072 net zero targets
# Looking at the number of unique companies with a net zero target
net_zero_sbti_targets$sbti_id %>% n_distinct() # [1] 2012 unique firms
saveRDS(net_zero_sbti_targets, "net_zero_sbti_targets.rds")

        ### Targets and commitments ####

# Identifying all net zero targets and commitments
net_zero_sbti_targets_and_commitments <- sbti_2024 %>%
  filter(
    (
      status %in% c("Target set", "Active", "Extended")
    & commitment_type %in% c("Net-zero", "BA1.5 Option 1", "BA1.5 Option 2")
    ) |
      target == "Net-zero"
  ) # 4483 net zero targets and commitments
# Looking at the number of unique companies with a net zero target or commitment
net_zero_sbti_targets_and_commitments$sbti_id %>% n_distinct() # [1] 3372 unique
                                                               # firms
saveRDS(net_zero_sbti_targets_and_commitments, 
        "net_zero_sbti_targets_and_commitments.rds")
      ## Scope 3 ####

          ### Long term targets ####
scope3_sbti_lt_targets <- sbti_2024 %>%
  filter(
    (
      status == "Target set"
      & commitment_type %in% c("Net-zero", "BA1.5 Option 1", "BA1.5 Option 2")
      )
    | target == "Net-zero"
    | (
      target == "Long-term" 
      & scope %in% c("1+2+3", "1+3", "3.0")
      )
    ) # 4612 long-term scope 3 targets
scope3_sbti_lt_targets$sbti_id %>% n_distinct() # 2012
saveRDS(scope3_sbti_lt_targets, "scope3_sbti_lt_targets.rds")

          ### Long term and near term targets ####
scope_3_sbti_targets <- sbti_2024 %>%
  filter(
    (
      status == "Target set"
      & commitment_type %in% c("Net-zero", "BA1.5 Option 1", "BA1.5 Option 2")
    )
    | target == "Net-zero"
    | (
      target %in% c("Long-term", "Near-term") 
      & scope %in% c("1+2+3", "1+3", "3.0")
    )
  ) # 8934 scope 3 targets
scope_3_sbti_targets$sbti_id %>% n_distinct() # [1] 3669 unique firms
saveRDS(scope_3_sbti_targets, "scope_3_sbti_targets.rds")

      ## All targets ####
all_sbti_targets <- sbti_2024 %>%
  filter(
    target %in% c("Net-zero", "Long-term", "Long-term pre-CNZS", "Near-term")
    |
      status == "Target set" 
  ) # 19017 targets
all_sbti_targets$sbti_id %>% n_distinct() # 7326 firms
saveRDS(all_sbti_targets, "all_sbti_targets.rds")

      ## All targets and commitments ####
all_sbti_targets_and_commitments <- sbti_2024 %>%
  filter(is.na(status) | status != "Removed") # 22803 targets and commitments
all_sbti_targets_and_commitments$sbti_id %>% n_distinct() # [1] 9717 unique 
                                                          # firms
saveRDS(all_sbti_targets_and_commitments, 
        "all_sbti_targets_and_commitments.rds")

      ## Removed ####
sbti_removed_commitments <- sbti_2024 %>%
  filter(status == "Removed") # 1400 removed commitments
sbti_removed_commitments$sbti_id %>% n_distinct() # [1] 988 unique firms
saveRDS(sbti_removed_commitments, "sbti_removed_commitments.rds")

      # Having a quick look to see if any companies in the removed dataset are
      # also associated with set targets
removed_ids <- sbti_removed_commitments %>%
  filter(sbti_id %in% all_sbti_targets_and_commitments$sbti_id) %>%
  select(sbti_id)
removed_ids <- removed_ids$sbti_id %>% unique() # [1] 300 unique firms
      # Creating a table for export to examine firms that removed and set
      # targets
removed_and_set <- sbti_2024 %>%
  filter(sbti_id %in% removed_ids) %>% 
  arrange(company_name, date_published, status)
saveRDS(removed_and_set, "removed_and_set.rds")
writexl::write_xlsx(removed_and_set, "removed_and_set.xlsx")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Merging ####

# For the puposes of merging, I want to create a simplified list of firms
sbti_2024_merging_table <- sbti_2024 %>%
  select(sbti_id, isin, lei, company_name, location, region, sector, 
         organization_type)
sbti_2024_merging_table <- sbti_2024_merging_table[
  -which(duplicated(sbti_2024_merging_table)), 
  ] # 10411 unique firms

sbti_2024_merging_table$sbti_id %>% n_distinct() # [1] 10405 unique firms 

duplicated_sbti_ids <- sbti_2024_merging_table$sbti_id[
  which(duplicated(sbti_2024_merging_table$sbti_id))] # several sbti_ids are
                                                      # indeed duplicated
sbti_2024_merging_table %>% 
  filter(sbti_id %in% duplicated_sbti_ids) %>%
  arrange(sbti_id) # from inspection, I can see that duplicated sbti_ids
                   # include 1 ob w lei==NA and 1 ob w valid lei
sbti_2024_merging_table <- sbti_2024_merging_table[
  -which(
    sbti_2024_merging_table$sbti_id %in% duplicated_sbti_ids 
    & is.na(sbti_2024_merging_table$lei))
  ,
]
saveRDS(sbti_2024_merging_table, "sbti_2024_merging_table.rds")
writexl::write_xlsx(sbti_2024_merging_table, "sbti_2024_merging_table.xlsx")
