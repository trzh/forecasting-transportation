#######################################
## Load libraries and functions

library(tidyverse)
library(here)
library(mlogit)
library(knitr)
library(caret)

here("code",
     "mlogit_helpers.R") |>
  source()

'%!in%' <- function(x,y)!('%in%'(x,y))

#######################################
## Load data

hh_data <- here("data",
                "NHTS",
                "hhpub.csv") |>
  read_csv(show_col_types = FALSE)

person_data <- here("data",
                    "NHTS",
                    "perpub.csv") |>
  read_csv(show_col_types = FALSE)

hh_data <- hh_data |>
  select(WRKCOUNT,
         DRVRCNT,
         HHVEHCNT,
         HHSIZE,
         NUMADLT,
         HHFAMINC,
         HBPPOPDN,
         HOUSEID,
         BIKE2SAVE,
         PLACE,
         PTRANS,
         WALK2SAVE,
         PRICE)

person_data <- person_data |>
  select(HOUSEID,
         R_AGE,
         WORKER,
         DRIVER,
         W_CANE,
         W_CHAIR,
         W_CRUTCH,
         W_DOG,
         W_MTRCHR,
         W_SCOOTR,
         W_WHCANE,
         W_WLKR,
         CONDNIGH,
         CONDPUB,
         CONDRIDE,
         CONDRIVE,
         CONDSPEC,
         CONDTAX,
         CONDTRAV)

#######################################
## Construct variables from original model

hh_data <- hh_data |>
  mutate(veh_avail = case_when(HHVEHCNT == 0 ~ "Zero",
                               DRVRCNT > HHVEHCNT ~ "Insuff.",
                               TRUE ~ "Suff."))

hh_data <- hh_data |>
  mutate(veh_avail = case_when(HHVEHCNT == 0 ~ "Zero",
                               DRVRCNT > HHVEHCNT ~ "Insuff.",
                               TRUE ~ "Suff."))

hh_data <- hh_data |>
  mutate(n_child = HHSIZE - NUMADLT)

n_seniors <- person_data |>
  mutate(is_senior = R_AGE > 64) |>
  group_by(HOUSEID) |>
  summarise(n_seniors = sum(is_senior))

hh_data <- hh_data |>
  left_join(n_seniors)

hh_data <- hh_data |>
  mutate(three_drivers = DRVRCNT > 2)

hh_data <- hh_data |>
  mutate(n_extra_drivers = ifelse(three_drivers, DRVRCNT - 2, 0))

hh_data <- hh_data |>
  mutate(HHFAMINC = as.numeric(HHFAMINC)) |>
  filter(HHFAMINC > 0) |>
  mutate(income = case_when(HHFAMINC < 4 ~ "low",
                            HHFAMINC < 5 & HHSIZE > 1 ~ "low",
                            HHFAMINC < 6 & HHSIZE > 3 ~ "low",
                            HHFAMINC < 7 & HHSIZE > 5 ~ "low",
                            HHFAMINC < 8 & HHSIZE > 7 ~ "low",
                            HHFAMINC > 8 ~ "high",
                            TRUE ~ "medium")) |>
  mutate(income = factor(income, levels = c("medium", "low", "high")))

non_work_driver <- person_data |>
  mutate(non_work_driver = WORKER == "02" & DRIVER == "01") |>
  group_by(HOUSEID) |>
  summarise(non_work_driver = max(non_work_driver))

hh_data <- hh_data |>
  left_join(non_work_driver)

hh_data <- hh_data |>
  filter(HBPPOPDN > 0) |>
  mutate(density = case_when(HBPPOPDN < 7000 ~ "Low",
                             HBPPOPDN < 10000 ~ "High",
                             TRUE ~ "Medium"))

#######################################
## Construct medical device variable

person_data <- person_data |>
  mutate(use_med_device = case_when((as.numeric(W_CANE) > 0) ~ TRUE,
                                    (as.numeric(W_CHAIR) > 0) ~ TRUE,
                                    (as.numeric(W_CRUTCH) > 0) ~ TRUE,
                                    (as.numeric(W_DOG) > 0) ~ TRUE,
                                    (as.numeric(W_MTRCHR) > 0) ~ TRUE,
                                    (as.numeric(W_SCOOTR) > 0) ~ TRUE,
                                    (as.numeric(W_WHCANE) > 0) ~ TRUE,
                                    (as.numeric(W_WLKR) > 0)~ TRUE,
                                    TRUE ~ FALSE))

med_device <- person_data |>
  group_by(HOUSEID) |>
  summarise(use_med_device = max(use_med_device))

hh_data <- hh_data |>
  left_join(med_device)

#######################################
## construct medical condition impact variable

person_data <- person_data |>
  mutate(med_cond_impact = case_when((as.numeric(CONDNIGH) == 01) ~ TRUE,
                                    (as.numeric(CONDPUB) == 01) ~ TRUE,
                                    (as.numeric(CONDRIDE) == 01) ~ TRUE,
                                    (as.numeric(CONDRIVE) == 01) ~ TRUE,
                                    (as.numeric(CONDTAX) == 01) ~ TRUE,
                                    (as.numeric(CONDTRAV) == 01) ~ TRUE,
                                    TRUE ~ FALSE))

med_cond <- person_data |>
  group_by(HOUSEID) |>
  summarise(med_cond_impact = max(med_cond_impact))

hh_data <- hh_data |>
  left_join(med_cond)

#######################################
## construct burden reduction variable

hh_data <- hh_data |>
  mutate(reduce_burden_action = case_when((as.numeric(BIKE2SAVE) == 01 |
                                             as.numeric(BIKE2SAVE) == 02) ~ TRUE,
                                    (as.numeric(PTRANS) == 01 |
                                       as.numeric(PTRANS) == 02) ~ TRUE,
                                    (as.numeric(WALK2SAVE) == 01 |
                                       as.numeric(WALK2SAVE) == 02) ~ TRUE,
                                    TRUE ~ FALSE))

reduce_burden <- hh_data |>
  group_by(HOUSEID) |>
  summarise(reduce_burden_action = max(reduce_burden_action))

#######################################
## train data

hh_data <- hh_data |>
  select(HOUSEID,
         veh_avail,
         WRKCOUNT,
         n_child,
         n_seniors,
         n_extra_drivers,
         three_drivers,
         non_work_driver,
         income,
         density,
         PLACE,
         PRICE,
         reduce_burden_action,
         use_med_device,
         med_cond_impact)

set.seed(6676077)

hh_data_train_ids <- sample(hh_data$HOUSEID, 
                            size = ceiling(nrow(hh_data)/2))

hh_data_train <- hh_data |>
  filter(HOUSEID %in% hh_data_train_ids)

hh_data_test <- hh_data |>
  filter(HOUSEID %!in% hh_data_train_ids)

veh_dfidx_train <- fn_make_dfidx(hh_data_train,
                                 "HOUSEID",
                                 "veh_avail")

veh_dfidx_test <- fn_make_dfidx(hh_data_test,
                                "HOUSEID",
                                "veh_avail")

#######################################
## run model

alt_model <- mlogit(choice ~ 0 | 
                      WRKCOUNT +
                      n_child +
                      n_seniors +
                      n_extra_drivers +
                      three_drivers + 
                      non_work_driver +
                      income +
                      PLACE +
                      PRICE +
                      use_med_device +
                      reduce_burden_action +
                      med_cond_impact +
                      density | 0,
                    veh_dfidx_train,
                    reflevel = "Suff.")

summary(alt_model)

#######################################
## calculate AIC

AIC(alt_model)