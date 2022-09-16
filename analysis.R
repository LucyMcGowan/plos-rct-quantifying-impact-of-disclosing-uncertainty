library(MASS)
library(tidyverse)
library(gtsummary)
library(rms)

load("data/dat.rds")

## Create Table 1 ----

d %>%
  dplyr::select(age, gender, race, ethnicity, edu_attain, avg_income, baseline_sanitize, trt) %>%
  tbl_summary(by = "trt",
              label = list(
                "age" ~ "Age",
                "gender" ~ "Gender",
                "race" ~ "Race",
                "ethnicity" ~ "Ethnicity",
                "edu_attain" ~ "Educational Attainment",
                "avg_income" ~ "Average Household Income",
                "baseline_sanitize" ~ "(baseline) How likely are you to sanitize your phone?"
              ),
              statistic = list(all_continuous() ~ "{median} ({min}, {p25}, {p75}, {max})")) %>%
  modify_caption("Baseline Characteristics")

## Fit Primary Models ----

o <- polr(agree_statement2 ~ trt * baseline_agree, data = dat, Hess = TRUE) 
o_agree <- dat %>% 
  filter(baseline_agree == "yes") %>%
  polr(agree_statement2 ~ trt, data = ., Hess = TRUE)

o_disagree <- dat %>% 
  filter(baseline_agree == "no") %>%
  polr(agree_statement2 ~ trt, data = ., Hess = TRUE)

## Create Table 2 ----

o %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment",
                 "baseline_agree" ~ "Baseline Agreement")) -> o1

o_agree %>% 
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment")) -> o2

o_disagree %>% 
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment")) -> o3
tbl_stack(list(o1, o2, o3), 
          group_header = c("Model fit on the whole sample with interaction", 
                           "Model among baseline agreers", 
                           "Model among baseline disagreers")) %>%
  modify_caption("Proportional odds model examining the impact of seeing uncertainty in the initial recommendation on accepting a future recommendation.")

## Create Table 3 ----

dat %>%
  nest_by(baseline_sanitize) %>%
  mutate(mod = list(polr(agree_statement2 ~ trt, data = data, Hess = TRUE))) %>% 
  summarise(l = list(tbl_regression(mod, exponentiate = TRUE,
                                    label = list("trt" ~ "Treatment"))),
            .groups = "drop") %>%
  pull(l) %>% 
  tbl_stack(c("Baseline likelihood to sanitize mobile phone = 1: Not at all likely",
              "Baseline likelihood to sanitize mobile phone = 2",
              "Baseline likelihood to sanitize mobile phone = 3",
              "Baseline likelihood to sanitize mobile phone = 4",
              "Baseline likelihood to sanitize mobile phone = 5: Very likely")) %>%
  modify_caption("Sensitivity analysis: Proportional odds model fit stratified by baseline likelihood to sanitize mobile phone (with 5 levels).") 

## Create Table 4 ----

o <- orm(agree_statement1 ~ trt * baseline_agree, data = dat)
o1 <- polr(agree_statement1 ~ trt * baseline_agree, data = dat, Hess = TRUE)
o2 <- polr(agree_statement1 ~ trt, data = dat, Hess = TRUE)
brant::brant(o2)
tbl_regression(o1, exponentiate = TRUE, 
               label = list("trt" ~ "Treatment",
                            "baseline_agree" ~ "Baseline Agreement")) -> o1
tbl_regression(o2, exponentiate = TRUE, 
               label = list("trt" ~ "Treatment")) -> o2

tbl_stack(list(o1, o2), 
          group_header = c("Model fit with interaction", 
                           "Model fit without interaction")) %>%
  modify_caption("Proportional odds model examining the impact of seeing uncertainty in the initial recommendation on accepting that recommendation.")