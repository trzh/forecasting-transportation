# Load libraries and functions

library(tidyverse)
library(here)
library(knitr)
library(srvyr)
library(mlogit)
library(caret)
library(pscl)

here("code",
     "mlogit_helpers.R") |>
  source()

'%!in%' <- function(x,y)!('%in%'(x,y))

# Load datasets

hh_data <- here("data",
                "NHTS",
                "hhpub.csv") |>
  read_csv(show_col_types = FALSE)

person_data <- here("data",
                    "NHTS",
                    "perpub.csv") |>
  read_csv(show_col_types = FALSE)

trip_data <- here("data",
                  "NHTS",
                  "trippub.csv") |>
  read_csv(show_col_types = FALSE)  |>
  select(HOUSEID,
         PERSONID,
         TDTRPNUM,
         TRIPPURP,
         WHYFROM,
         WHYTO,
         TRPTRANS,
         WTTRDFIN)

# Find total number of trips made TO rel_com
trip_data <- trip_data |>
  mutate(purpose = (WHYTO == "19")) |>
  group_by(HOUSEID, PERSONID)

# Find number of people who made certain number of trips to rel_com

rel_com_count <- trip_data |>
  mutate(is_rel_com = (WHYTO == "19")) |>
  summarise(n_rel_com = sum(is_rel_com),
            n_trips = n()) |>
  mutate(n_non_rel_com = n_trips - n_rel_com)

# Summarize trip purposes

rel_com_count |>
  group_by(n_rel_com) |>
  summarise(`Number of people who made trips (unweighted)` = n()) |>
  mutate(`Percent of people who made trips (unweighted)` = 
           100 * `Number of people who made trips (unweighted)`/
           sum(`Number of people who made trips (unweighted)`)) |>
  arrange(desc(`Percent of people who made trips (unweighted)`)) |>
  kable(format.args = list(big.mark = ","), digits = 0)

trip_data |>
  group_by(purpose) |>
  summarise(`Number of total trips made (unweighted)` = n()) |>
  mutate(`Percent of total trips made (unweighted)` = 
           100 * `Number of total trips made (unweighted)`/
           sum(`Number of total trips made (unweighted)`)) |>
  arrange(desc(`Percent of total trips made (unweighted)`)) |>
  kable(format.args = list(big.mark = ","), digits = 0)

# Construct person-level predictor variables

person <- person_data |>
  select(HOUSEID, PERSONID, R_RACE, R_AGE, BORNINUS, EDUC, TRAVDAY) |>
  mutate(BORNINUS = as.numeric(BORNINUS)) |>
  filter(BORNINUS > 0) |>
  mutate(immigrant = BORNINUS == 2) |>
  filter(R_RACE != "-7" & R_RACE != "-8") |>
  mutate(R_AGE = as.numeric(R_AGE)) |>
  filter(R_AGE > 0) |>
  filter(EDUC != "-1" & EDUC != "-7" & EDUC != "-8") |>
  mutate(higher_ed = (EDUC == "04" | EDUC == "05")) |>
  mutate(TRAVDAY = as.numeric(TRAVDAY)) |>
  mutate(weekend = (TRAVDAY == 1 | TRAVDAY == 7))

# Join only religious & community trips to person data

only_rel_com_trips <- rel_com_count |>
  filter(n_rel_com != 0) |>
  group_by(HOUSEID, PERSONID) |>
  select(HOUSEID, PERSONID, n_rel_com)

rel_com_trips <- trip_data |>
  filter(purpose == "TRUE") |>
  group_by(HOUSEID, PERSONID) |>
  summarise(rel_com_trips = n())

person <- person |>
  left_join(rel_com_trips) |>
  replace_na(list(rel_com_trips = 0))

# Plot values

ggplot(person) +
  geom_histogram(aes(x = rel_com_trips),
                 binwidth = 1,
                 color = "purple",
                 fill = "lightpink") +
  scale_x_continuous(name = "Numbers of religious and community trips") +
  scale_y_continuous(name = "Number of people in sample") +
  theme_minimal()

# Find mean and standard deviation

person |>
  summarise(`Average count of RELCON trips` = mean(rel_com_trips),
            `Standard deviation` = sd(rel_com_trips)) |>
  kable(digits = 3)

# Run zero-inflated Poisson regression

rel_com_model_zp <- zeroinfl(rel_com_trips ~
                               R_RACE +
                               R_AGE +
                               immigrant +
                               higher_ed +
                               weekend,
                             data = person,
                             dist = "poisson")

summary(rel_com_model_zp)

AIC(rel_com_model_zp)

# Visualize variation in zero-inflated Poisson regression

rel_com_check_zp <- tibble(observed = rel_com_model_zp$model$rel_com_trips, 
                           predicted = rel_com_model_zp$fitted.values)

ggplot(rel_com_check_zp) +   
  geom_jitter(aes(x = observed,                  
                  y = predicted),               
              color = "lightpink",               
              alpha = 0.1,               
              size = 0.1) +   
  scale_x_continuous(name = "Number of observed trips per person") +   
  scale_y_continuous(name = "Number of predicted trips per person") +
  theme_minimal()

# Run alternative models: linear regression

rel_com_model_linear <- lm(rel_com_trips ~
                             R_RACE +
                             R_AGE +
                             immigrant +
                             higher_ed +
                             weekend,
                           data = person)

summary(rel_com_model_linear)

AIC(rel_com_model_linear)

# Visualize variation in linear regression

rel_com_check_linear <- tibble(observed = rel_com_model_linear$model$rel_com_trips, 
                           predicted = rel_com_model_linear$fitted.values)

ggplot(rel_com_check_linear) +   
  geom_jitter(aes(x = observed,                  
                  y = predicted),               
              color = "lightpink",               
              alpha = 0.1,               
              size = 0.1) +   
  scale_x_continuous(name = "Number of observed trips per person") +   
  scale_y_continuous(name = "Number of predicted trips per person",
                     limits = c(0, 0.35)) +
  theme_minimal()

# Run alternative models: Poisson regression

rel_com_model_poisson <- glm(rel_com_trips ~
                               R_RACE +
                               R_AGE +
                               immigrant +
                               higher_ed +
                               weekend,
                             data = person,
                             family = "poisson")

summary(rel_com_model_poisson)

AIC(rel_com_model_poisson)

# Visualize variation in Poisson regression

rel_com_check_poisson <- tibble(observed = rel_com_model_poisson$model$rel_com_trips, 
                           predicted = rel_com_model_poisson$fitted.values)

ggplot(rel_com_check_poisson) +   
  geom_jitter(aes(x = observed,                  
                  y = predicted),               
              color = "lightpink",               
              alpha = 0.1,               
              size = 0.1) +   
  scale_x_continuous(name = "Number of observed trips per person") +   
  scale_y_continuous(name = "Number of predicted trips per person") +
  theme_minimal()