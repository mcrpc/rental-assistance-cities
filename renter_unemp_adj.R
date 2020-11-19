library(tidyverse)
library(fs)
library(srvyr)
library(ipumsr)


# Load function ind_to_bls() to recode IPUMS ind codes to BLS ones we're using
source(path("R", "ind_to_bls.R"))


# Read in IPUMS USA ACS microdata, standardize the column names
ipums_raw <- read_ipums_ddi("data/usa_00002.xml") %>%
  read_ipums_micro() %>%
  rename_with(str_to_lower) %>%
  filter(sample == 201803, statefip == 17)

# it is necessary to run R's garbage collection after loading the IPUMS data
gc()

# Add column with BLS industry categories, filter to relevant universe, set up
# survey weights
unemp_data <- ipums_raw %>% 
  mutate(ind_group_bls = ind_to_bls(ind)) %>% 
  filter(
    empstat %in% 1:2, 
    !is.na(ind_group_bls),
    gq %in% 1:2,
    ownershp %in% 1:2
  ) %>% 
  as_survey_design(weights = "perwt")

# Get unemployment rate by industry for renters only
renter_unemp <- unemp_data %>%
  filter(ownershp == 2) %>% 
  group_by(ind_group_bls) %>% 
  summarise(
    renter_unemp_rate = survey_mean(empstat == 2, vartype = NULL)
  )

# Get overall unemployment rate by industry 
all_unemp <- unemp_data %>% 
  group_by(ind_group_bls) %>% 
  summarise(
    all_unemp_rate = survey_mean(empstat == 2, vartype = NULL)
  )

# Create an adjustment factor to account for higher unemployment rates for
# renters, even within industry
unemp_adj_data <- left_join(renter_unemp, all_unemp, by = "ind_group_bls") %>% 
  transmute(
    ind_group_bls,
    unemp_renter_adj = renter_unemp_rate/all_unemp_rate
  )

unemp_adj_data %>% 
  write_csv(path("data", "unemp_renter_adj.csv")) %>% 
  write_rds(path("data", "unemp_renter_adj.rds"))
