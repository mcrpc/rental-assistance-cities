library(sf)
library(tidyverse)
library(tigris)
library(scales) # number formatting
library(srvyr) # survey functions
library(gt) # making tables
library(furrr) # parallel processing

pumas <- pumas(17, cb = TRUE, 2019) %>%
  transmute("name" = NAME10, "puma" = as.numeric(PUMACE10))
ipums_clean <- read_rds("data/ipums_clean.rds")

survey_total_ci90 <- partial(survey_total, vartype = "ci", level = 0.90)
survey_mean_ci90 <- partial(survey_mean, vartype = "ci", level = 0.90)

# Number of iterations to run when compiling results
ITERATIONS <- 2

# We manage the random number generator seed directly, so we can ignore warnings from the package that handles the parallelization 
options(future.rng.onMisuse = "ignore")

# No scientific notation in outputs
options(scipen = 999)

# To deal with the random assignment of job loss and UI takeup for individuals,
# we need to run generate the results multiple times with different random seeds
# and then average the results. To speed this up we use {furrr} to run this in
# parallel across multiple cores.
plan(multiprocess)

estimate_household_need <- function(.data) {
  .data %>%
    mutate(
      risk_group = if_else(is.na(inc_wages), NA, risk_group),
      risk_wages = if_else(risk_group, inc_wages, NA_real_)
    ) %>%
    group_by(serial) %>%
    mutate(
      hh_risk_members_num = sum(risk_group, na.rm = TRUE),
      hh_wage_earners_num = sum(inc_wages > 0, na.rm = TRUE),
      hh_any_risk = case_when(
        all(is.na(risk_group)) ~ NA, # no wage earners
        any(risk_group, na.rm = TRUE) ~ TRUE, # any wage earners are at risk
        all(!risk_group, na.rm = TRUE) ~ FALSE # all wage earners are at NOT at risk
      ),
      hh_risk_wages = sum(risk_wages, na.rm = TRUE),
      hh_risk_wages_pct = sum(risk_wages, na.rm = TRUE) / na_if(hh_inc_nom, 0),
    ) %>%
    ungroup() %>%
    mutate(
      risk_burden = gross_rent_nom / ((hh_inc_nom - hh_risk_wages) / 12),
      risk_rent_need = ((gross_rent_nom/risk_burden) - (gross_rent_nom/target_burden)),
    ) %>%
    # Make some adjustments to all the rent_need columns
    mutate_at(
      vars(matches("risk_.*rent_need.*")),
      ~{
        # note that rent need is expressed as a negative number at first, after this it is positive
        case_when(
          is.na(.) ~ 0, # missing change to zero (missing if no cash rent, etc.)
          . > 0    ~ 0, # positive value for need means they don't have need, set to zero
          -. > gross_rent_nom ~ gross_rent_nom, # cap needs at the full rent value
          TRUE ~ -. # change the negative numbers to positive
        )
      }
    )
}

household_summarize_stats <- function(.data) {
  .data %>%
    filter(
      pernum == 1,
      hh_any_risk
    ) %>%
    as_survey_design(weights = hhwt) %>% 
    group_by(puma) %>%
    summarise(
      renter_households = survey_total_ci90(1),
      lost_wages_agg_monthly = survey_total_ci90(hh_risk_wages/12),
      rent_monthly_tot = survey_total_ci90(gross_rent_nom, na.rm = TRUE),
      rent_monthly_avg = survey_mean_ci90(gross_rent_nom, na.rm = TRUE)
    ) %>%
    ungroup()
}

prep_household_data <- function(seed, .data) {
  set.seed(seed)
  
  .data %>%
    mutate(risk_group = runif(n()) < job_loss_pct) %>%
    estimate_household_need() %>%
    household_summarize_stats()
}

puma_household_data <- seq_len(ITERATIONS) %>%
  future_map_dfr(
    .f = prep_household_data,
    .data = filter(ipums_clean, is_renter)
  )
  
  

puma_pop <- ipums_clean %>% 
  as_survey_design(weights = perwt) %>% 
  group_by(puma) %>% 
  summarize(
    pop_num = survey_total_ci90(1),
    pop_renter_num = survey_total_ci90(is_renter)
  )

puma_renter_pct <- ipums_clean %>%
  filter(pernum == 1) %>%
  as_survey_design(weights = hhwt) %>%
  group_by(puma) %>%
  summarize(
    hh_renter_num = survey_total_ci90(is_renter),
    hh_renter_pct = survey_mean_ci90(is_renter),
  )
  
puma_renter_stats <- ipums_clean %>% 
  filter(pernum == 1, is_renter) %>% 
  as_survey_design(weights = hhwt) %>% 
  group_by(puma) %>% 
  summarise(
    hh_rent_burden_sev_num = survey_total_ci90(is_rent_burdened_sev, na.rm = TRUE),
    hh_rent_burden_sev_pct = survey_mean_ci90(is_rent_burdened_sev, na.rm = TRUE),
    hh_renter_lt80ami_num = survey_total_ci90(hh_inc_nom < hud_inc_lim80),
    hh_renter_lt80ami_pct = survey_mean_ci90(hh_inc_nom < hud_inc_lim80)
  )

puma_data <- list(
  puma_pop, puma_household_data, puma_renter_pct, puma_renter_stats
) %>% 
  reduce(left_join, by = "puma") %>%
  select(!ends_with(c("_low", "_upp")))

pumas <- left_join(pumas, puma_data) %>%
  write_sf("data/pumas_il_2019.geojson")
