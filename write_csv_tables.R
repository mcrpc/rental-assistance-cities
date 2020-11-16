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

hh_summarise_stats <- function(.data) {
  .data %>%  
    filter(
      pernum == 1 # keep only one row per household
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>%
    summarise(
      households = survey_total_ci90(1),
      lost_wages_agg_monthly = survey_total_ci90(hh_risk_wages/12),
      ui_benefits_reg_agg_monthly = survey_total_ci90(hh_ui_benefits_month_reg),
      ui_benefits_extra300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_extra300),
      ui_benefits_all300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_all300),
      rent_monthly_tot = survey_total_ci90(gross_rent_nom, na.rm = TRUE),
      rent_monthly_avg = survey_mean_ci90(gross_rent_nom, na.rm = TRUE)
    ) %>% 
    ungroup()
}

vuln_summarise_stats <- function(.data) {
  .data %>%  
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    as_survey_design(weights = hhwt) %>% 
    group_by(county_name) %>%
    summarise(
      households = survey_total_ci90(1),
      lost_wages_agg_monthly = survey_total_ci90(hh_risk_wages/12),
      ui_benefits_reg_agg_monthly = survey_total_ci90(hh_ui_benefits_month_reg),
      ui_benefits_extra300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_extra300),
      ui_benefits_all300_agg_monthly = survey_total_ci90(hh_ui_benefits_month_all300),
      rent_monthly_tot = survey_total_ci90(gross_rent_nom, na.rm = TRUE),
      rent_monthly_avg = survey_mean_ci90(gross_rent_nom, na.rm = TRUE),
      rent_need_tot_monthly = survey_total_ci90(risk_rent_need),
      rent_need_ui_reg_tot_monthly = survey_total_ci90(risk_rent_need_ui_reg),
      rent_need_ui_all300_tot_monthly = survey_total_ci90(risk_rent_need_ui_all300)
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

prep_hh_data <- function(seed, .data) {
  set.seed(seed)
  
  data_w_need <- .data %>% 
    mutate(risk_group = runif(n()) < job_loss_pct) %>% 
    add_ui_takeup(UI_TAKEUP_RATE) %>% 
    add_need_vars()
  
  data_renter_vuln <- data_w_need %>%
    filter(is_renter) %>%
    vuln_summarise_stats() %>% 
    mutate(
      group = "renter",
      status = "vulnerable"
    )
  
  data_renter_vuln_sevburden <- data_w_need %>%
    # Include only those who were severely rent burdened pre-covid
    filter(is_rent_burdened_sev, is_renter) %>%
    vuln_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "sevburden",
      status = "vulnerable"
    )
  
  data_renter_vuln_noui <- data_w_need %>%
    # Include only those who don't get UI benefits
    filter(hh_ui_benefits_month_reg == 0, is_renter) %>%
    vuln_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "noui",
      status = "vulnerable"
    )
  
  data_renter_vuln_ui <- data_w_need %>%
    # Include only those who did get UI benefits
    filter(hh_ui_benefits_month_reg > 0, is_renter) %>%
    vuln_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "ui",
      status = "vulnerable"
    )
  
  data_renter_vuln_lt80ami <- data_w_need %>%
    # Include only households with income below 80% AMI
    filter(hh_inc_nom > hud_inc_lim80, is_renter) %>% 
    vuln_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "lt80ami",
      status = "vulnerable"
    )
  
  data_renter_sevburden <- data_w_need %>%
    filter(is_rent_burdened_sev, is_renter) %>%
    hh_summarise_stats() %>%
    mutate(
      group = "renter"
    )
  
  data_renter_all <- data_w_need %>%
    filter(is_renter) %>%
    hh_summarise_stats() %>%
    mutate(
      group = "renter"
    )
  
  data_renter_all_sevburden <- data_w_need %>%
    # Include only those who were severely rent burdened pre-covid
    filter(is_rent_burdened_sev, is_renter) %>%
    hh_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "sevburden"
    )
  
  data_renter_all_lt80ami <- data_w_need %>%
    # Include only households with income below 80% AMI
    filter(hh_inc_nom > hud_inc_lim80, is_renter) %>% 
    hh_summarise_stats() %>% 
    mutate(
      group = "renter",
      type = "lt80ami"
    )
  
  bind_rows(
    data_renter_vuln,
    data_renter_vuln_sevburden,
    data_renter_vuln_noui,
    data_renter_vuln_ui,
    data_renter_vuln_lt80ami,
    data_renter_all,
    data_renter_all_sevburden,
    data_renter_all_lt80ami
  ) %>% 
    mutate(seed = seed)
  
}


# may exclude these -------------------------------------------------------

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
          
          # total number of wage earners estimated to lose jobs
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

hh_data <- seq_len(ITERATIONS) %>%
  future_map_dfr(
    .f = prep_hh_data,
    .data = filter(ipums_clean, !is.na(county_name))
  ) %>%
  select(-seed) %>%
  group_by(county_name, group, type, status) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  add_moe() %>%
  arrange(county_name, desc(type)) %>% 
  filter(county_name == "McLean") %>%
  write_csv("data/mclean_hh_data.csv")

# person_data <- person_results %>% 
#   select(-seed) %>% 
#   group_by(county_name) %>% 
#   summarise_all(mean) %>%
#   add_moe() %>% 
#   filter(county_name == "McLean") %>%
#   write_csv("data/mclean_person_data.csv")

# need_data <- seq_len(ITERATIONS) %>%
#   future_map_dfr(
#     .f = prep_need_data,
#     .data = ipums_clean %>% filter(!is.na(county_name), is_renter)
#   ) %>%
#   select(-seed) %>%
#   group_by(county_name, type) %>%
#   summarise_all(mean) %>%
#   ungroup() %>%
#   add_moe() %>%
#   arrange(county_name, desc(type)) %>%
#   filter(county_name == "McLean") %>%
#   write_csv("data/mclean_need_data.csv")

