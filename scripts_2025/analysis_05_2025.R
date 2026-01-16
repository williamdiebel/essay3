# Preamble ####
# The purpose of this script is to take the cleaned, extended panel
# (panel_extended.rds) and conduct the main descriptive statistics and analyses
# that will go into the manuscript.
setwd("~/Dropbox/NetZero WD Shared Folder/Data")
library(tidyverse)
library(fixest)
library(logistf)
library(survival)
library(geepack)
library(brglm2)
library(lme4)
panel_extended <- readRDS("panel_extended.rds")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Entire panel prior to removing missing roa data
                                           # Number of observations
panel_extended %>% nrow()                  # 193554
                                           # Number of unique firms
panel_extended$gvkey %>% n_distinct()      # 13947
                                           # Firms with SBTi commitments
panel_extended %>%                         # 819
  summarise(
    n_distinct(gvkey[sbti_commitment_2024 == 1])
  )
                                           # CDP SCP member firms
panel_extended %>%                         # 131
  summarise(
    n_distinct(gvkey[cdp_sc_member == 1])
  )

# Complete panel after removing missing roa data
complete_data <- panel_extended %>%
  filter(!is.na(roa_oibdp_w1_at_w1) &
           !is.na(tll_lt_w1_at_w1) &
           !is.na(at_usd_winsorized_1_log))

                                          # Number of observations
complete_data %>% nrow()                  # 164185
                                          # Number of unique firms
complete_data$gvkey %>% n_distinct()      # 12993
                                          # Firms with SBTi commitments
complete_data %>%                         # 811
  summarise(
    n_distinct(gvkey[sbti_commitment_2024 == 1])
    )
                                          # CDP SCP member firms
complete_data %>%                         # 108
  summarise(
    n_distinct(gvkey[cdp_sc_member == 1])
  )

# Add the lagged version of the variable
complete_data <- complete_data %>%
  group_by(gvkey) %>%
  arrange(year) %>%
  mutate(
    sbti_commitment_lead1   = lead(sbti_commitment_2024, 1),
  )

complete_data %>% 
  arrange(conm, year) %>%
  select(conm, year, sbti_commitment_2024, sbti_commitment_lead1) %>%
  view()
  # looks good -- just need to replace the newly created NAs with 0 or 1
  # depending upon whether the firm makes an sbti commitment or not. Coalescing
  # the sbti_commitment_2024 and sbti_commitment_lead1 vbls will do the trick
complete_data <- complete_data %>%
  mutate(
    sbti_commitment_lead1 = coalesce(
      sbti_commitment_lead1, sbti_commitment_2024
    )
  )

# Complete panel after removing 2023 period
complete_data_2022 <- complete_data %>% filter(year<2023)
                                          # Number of observations
complete_data_2022 %>% nrow()             # 154062
                                          # Number of unique firms
complete_data_2022$gvkey %>% n_distinct() # 12993
                                          # Firms with SBTi commitments
complete_data_2022 %>%                    # 811
  filter(sbti_commitment_lead1 == 1) %>%
  select(gvkey) %>% n_distinct()
                                          # CDP SCP member firms
complete_data_2022 %>%                    # 108
  filter(cdp_sc_member == 1) %>%
  select(gvkey) %>% n_distinct()

saveRDS(complete_data_2022, "complete_data_2022.rds")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Descriptive stats
complete_data_2022 %>% select(
  roa_oibdp_w1_at_w1
  ) %>% summary()
complete_data_2022 %>% select(
  at_usd_winsorized_1_log
  ) %>% summary()
complete_data_2022 %>% select(
  tll_lt_w1_at_w1
  ) %>% summary()
complete_data_2022 %>% select(
  e_disc_coalesced_zeros
  ) %>% summary()
complete_data_2022 %>% select(
  scope1_zeros
  ) %>% summary()

# Main fixed effects LPM model specifications ####
## Country fixed effects ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | headquarter_country + year,
    data = complete_data_2022,
    cluster = ~gvkey
    )
  )
## Adds industry fixed effects ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | headquarter_country + FourDigitName + year,
    data = complete_data_2022,
    cluster = ~gvkey
    )
  )
## Adds firm fixed effects ####

summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents_highreach + 
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | gvkey + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)

# CDP SCP endogeneity robustness ####
## Instrumented models ####
### IV ####
#### Creating the IV ####
complete_data_2022 <- 

#### IV Models ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents*esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | gvkey + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)

### Control function ####
summary(
  feols(
    sbti_commitment_lead1 ~ 
      cdp_sc_member + esc_incidents*esc_incidents_highreach +
      e_disc_coalesced_zeros + e_disc_missing +
      log(scope1_zeros + 1) + scope1_missing +
      roa_oibdp_w1_at_w1 + at_usd_winsorized_1_log + 
      tll_lt_w1_at_w1 
    | gvkey + year,
    data = complete_data_2022,
    cluster = ~gvkey
  )
)

## Other robustness models ####
### GLMM ####
### MLM ####
### Tobit ####
### Hazard ####



