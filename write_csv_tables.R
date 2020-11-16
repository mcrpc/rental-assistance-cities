library(tidyverse) # general data manipulation and graphing
library(scales) # number formatting
library(srvyr) # survey functions
library(gt) # making tables
library(furrr) # parallel processing

ipums_clean <- read_rds("data/ipums_clean.rds")

UI_TAKEUP_RATE <- 0.64

# No scientific notation in outputs
options(scipen = 999)

# To deal with the random assignment of job loss and UI takeup for individuals,
# we need to run generate the results multiple times with different random seeds
# and then average the results. To speed this up we use {furrr} to run this in
# parallel across multiple cores.
plan(multicore)

# We manage the random number generator seed directly, so we can ignore warnings from the package that handles the parallelization 
options(future.rng.onMisuse = "ignore")

ITERATIONS <- 2

source("R/add-ui-and-need-vars.R")

survey_total_ci90 <- partial(survey_total, vartype = "ci", level = 0.90)
survey_mean_ci90 <- partial(survey_mean, vartype = "ci", level = 0.90)

# data prep functions -----------------------------------------------------

# When using the {srvyr} package we get separate columns for the lower and
# upper bounds of the confidence interval. This little helper function creates
# a single margin of error column.
add_moe <- function(.data) {
  .data %>%
    pivot_longer(where(is.numeric)) %>% 
    mutate(
      value_type = case_when(
        str_detect(name, "_low") ~ "low",
        str_detect(name, "_upp") ~ "upp",
        TRUE ~ "est"
      ),
      name = str_remove(name, "(_low|_upp)")
    ) %>% 
    pivot_wider(names_from = value_type, values_from = value) %>% 
    mutate(moe_90 = upp - est) %>% 
    select(-low, -upp) %>% 
    pivot_longer(where(is.numeric), names_to = "value_type") %>% 
    unite(name_type, name, value_type) %>% 
    pivot_wider(names_from = name_type, values_from = value)
}

jobslost_summarise_stats <- function(.data) {
  .data %>%  
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>%
    summarise(
      renter_households = survey_total_ci90(1),
      lost_wages_agg_monthly = survey_total_ci90(hh_risk_wages/12),
      ui_benefits_reg_agg_monthly = survey_total_ci90(hh_ui_benefits_month_reg),
      ui_benefits_extra300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_extra300),
      ui_benefits_all300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_all300),
      rent_monthly_tot = survey_total_ci90(gross_rent_nom, na.rm = TRUE),
      rent_monthly_avg = survey_mean_ci90(gross_rent_nom, na.rm = TRUE)
    ) %>% 
    ungroup()
}

ui_percent_stats <- function(.data) {
  .data %>% 
    filter(
      pernum == 1, # keep only one row per household
      # Keep all renter households, not just ones with income loss
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      renter_hh_ui_pct = survey_mean_ci90(hh_any_risk & hh_ui_benefits_month_reg > 0),
      renter_hh_noui_pct = survey_mean_ci90(!is.na(hh_any_risk) & hh_any_risk & hh_ui_benefits_month_reg == 0)
    )
}

prep_jobslost_data <- function(seed, .data) {
  set.seed(seed)
  
  data_w_need <- .data %>% 
    mutate(risk_group = runif(n()) < job_loss_pct) %>% 
    add_ui_takeup(UI_TAKEUP_RATE) %>% 
    add_need_vars()
  
  data_all <- data_w_need %>%
    jobslost_summarise_stats() %>% 
    mutate(type = "all")
  
  data_all_ui_pct <- data_w_need %>%
    ui_percent_stats() %>% 
    mutate(type = "all_ui_pct")
  
  data_sevburden <- data_w_need %>%
    # Include only those who were severely rent burdened pre-covid
    filter(is_rent_burdened_sev) %>%
    jobslost_summarise_stats() %>% 
    mutate(type = "sevburden")
  
  data_sevburden_pct <- data_w_need %>%
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      sevburden_pct = survey_mean_ci90(is_rent_burdened_sev)
    ) %>% 
    mutate(type = "sevburden_pct")
  
  data_all_noui <- data_w_need %>%
    # Include only those who don't get UI benefits
    filter(hh_ui_benefits_month_reg == 0) %>%
    jobslost_summarise_stats() %>% 
    mutate(type = "all_noui")
  
  data_all_ui <- data_w_need %>%
    # Include only those who did get UI benefits
    filter(hh_ui_benefits_month_reg > 0) %>%
    jobslost_summarise_stats() %>% 
    mutate(type = "all_ui")
  
  data_lt80ami_pct <- data_w_need %>%
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      lt80ami_pct = survey_mean_ci90(hh_inc_nom > hud_inc_lim80)
    ) %>% 
    mutate(type = "lt80ami_pct")
  
  data_lt80ami <- data_w_need %>%
    # Include only households with income below 80% AMI
    filter(hh_inc_nom > hud_inc_lim80) %>% 
    jobslost_summarise_stats() %>% 
    mutate(type = "lt80ami")
  
  bind_rows(
    data_all,
    data_all_ui_pct,
    data_sevburden,
    data_sevburden_pct,
    data_all_noui,
    data_all_ui,
    data_lt80ami_pct,
    data_lt80ami
  ) %>% 
    mutate(seed = seed)
  
}

prep_household_data <- function(.data) {
  # total renter population in IL state
  ils_renter_pop_num <- .data %>% 
    as_survey_design(weights = perwt) %>% 
    summarise(
      pop_renter_num = survey_total_ci90(1)
    ) %>% 
    pull(pop_renter_num)
  
  household_pop <- .data %>% 
    filter(!is.na(county_name)) %>% 
    as_survey_design(weights = perwt) %>% 
    group_by(county_name) %>% 
    summarise(
      pop_num = survey_total_ci90(1),
      pop_renter_num = survey_total_ci90(is_renter)
    ) %>% 
    mutate(
      pop_renter_ils_pct = pop_renter_num / ils_renter_pop_num
    )
  
  household_renter_pct <- .data %>% 
    filter(!is.na(county_name), pernum == 1) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      hh_renter_num = survey_total_ci90(is_renter),
      hh_renter_pct = survey_mean_ci90(is_renter),
    )
  
  household_renter_stats <- .data %>% 
    filter(!is.na(county_name), pernum == 1, is_renter) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      hh_rent_burden_sev_num = survey_total_ci90(is_rent_burdened_sev, na.rm = TRUE),
      hh_rent_burden_sev_pct = survey_mean_ci90(is_rent_burdened_sev, na.rm = TRUE),
      hh_renter_lt80ami_num = survey_total_ci90(hh_inc_nom < hud_inc_lim80),
      hh_renter_lt80ami_pct = survey_mean_ci90(hh_inc_nom < hud_inc_lim80)
    )
  
  household_data <- list(
    household_pop, household_renter_pct, household_renter_stats
  ) %>% 
    reduce(left_join, by = "county_name")
}

person_results <- seq_len(ITERATIONS) %>%
  future_map_dfr(
    .f = function(seed, .data) {
      set.seed(seed)
      data_w_vars <- .data %>% 
        mutate(risk_group = runif(n()) < job_loss_pct) %>% 
        add_ui_takeup(UI_TAKEUP_RATE) %>% 
        add_need_vars() %>% 
        as_survey_design(weights = perwt)
      
      county_data <- data_w_vars %>% 
        group_by(county_name) %>% 
        summarise(
          # Renters only:
          
          pop_num = survey_total_ci90(1),
          
          # total number of wage earners
          wage_earners_num = survey_total_ci90(inc_wages > 0, na.rm = TRUE),
          
          # total number of wage earners estiamted to lose jobs
          risk_earners_num = survey_total_ci90(risk_group, na.rm = TRUE),
          
          # share of wage earners estimated to lose jobs
          risk_earners_pct = survey_mean_ci90(risk_group, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        mutate(seed = seed)
      
    },
    .data = filter(ipums_clean, !is.na(county_name), is_renter)
  )

need_summarise_stats <- function(.data) {
  .data %>%  
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>%
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>% 
    summarise(
      rent_need_tot_monthly = survey_total_ci90(risk_rent_need),
      rent_need_ui_reg_tot_monthly = survey_total_ci90(risk_rent_need_ui_reg),
      rent_need_ui_all300_tot_monthly = survey_total_ci90(risk_rent_need_ui_all300),
    ) %>% 
    ungroup()
}

prep_need_data <- function(seed, .data) {
  set.seed(seed)
  
  
  data_w_vars <- .data %>% 
    mutate(risk_group = runif(n()) < job_loss_pct) %>% 
    add_ui_takeup(UI_TAKEUP_RATE)
  
  
  data_all_default <- data_w_vars %>% 
    add_need_vars() %>%
    need_summarise_stats()
  
  data_all_30cutoff <- data_w_vars %>% 
    mutate(target_burden = 0.30) %>%
    add_need_vars() %>%
    need_summarise_stats()
  
  
  data_lt80ami_default <- data_w_vars %>% 
    # Include only households with income below 80% AMI
    filter(hh_inc_nom > hud_inc_lim80) %>% 
    add_need_vars() %>%
    need_summarise_stats()
  
  
  bind_rows(
    data_all_default %>% mutate(type = "all_default"),
    data_all_30cutoff %>% mutate(type = "all_30cutoff"),
    data_lt80ami_default %>% mutate(type = "lt80ami_default"),
  ) %>%
    mutate(seed = seed)
  
  
}


# run data prep -----------------------------------------------------------

jobslost_data <- seq_len(ITERATIONS) %>%
  future_map_dfr(
    .f = prep_jobslost_data,
    .data = filter(ipums_clean, !is.na(county_name), is_renter)
  ) %>%
  select(-seed) %>%
  group_by(county_name, type) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  add_moe() %>%
  arrange(county_name, desc(type)) %>% 
  filter(county_name == "McLean") %>%
  write_csv("data/mclean_jobslost_data.csv")

household_data <- prep_household_data(ipums_clean) %>% 
  add_moe() %>% 
  filter(county_name == "McLean") %>%
  # in final tables we leave some variables out, but keep them here commented
  # out since they are useful to have for reference in the text
  # select(-starts_with("pop_renter_num"), -starts_with("pop_renter_ils_pct")) %>%
  write_csv("data/mclean_hh_data.csv")

person_data <- person_results %>% 
  select(-seed) %>% 
  group_by(county_name) %>% 
  summarise_all(mean) %>%
  add_moe() %>% 
  filter(county_name == "McLean") %>%
  write_csv("data/mclean_person_data.csv")

need_data <- seq_len(ITERATIONS) %>%
  future_map_dfr(
    .f = prep_need_data,
    .data = ipums_clean %>% filter(!is.na(county_name), is_renter)
  ) %>%
  select(-seed) %>%
  group_by(county_name, type) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  add_moe() %>%
  arrange(county_name, desc(type)) %>%
  filter(county_name == "McLean") %>%
  write_csv("data/mclean_need_data.csv")

