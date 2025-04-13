###
# Load libraries and read in data

library(tidyverse)
library(here)
library(knitr)
library(srvyr)
library(tidycensus)
library(jtools)

trips <- here("data",
              "NHTS",
              "trippub.csv") |>
  read_csv(show_col_types = FALSE)

people <- here("data",
               "NHTS",
               "perpub.csv") |>
  read_csv(show_col_types = FALSE)

###
# Filter health trips

health_trips <- trips |>
  filter(WHYTO == "18" |
           WHYFROM == "18")

###
# Filter seniors

sr_health_trips <- health_trips |>
  filter(R_AGE > 64)

###
# Filter trip distance

short_sr_health_trips <- sr_health_trips |>
  filter(TRPMILES < 1.5)

###
# Generate outcome variable

short_sr_health_trips <- short_sr_health_trips |>
  mutate(walk = TRPTRANS == "01")

###
# See what share of trips are walk trips

short_sr_health_trips |>
  mutate(Mode = factor(ifelse(walk, "Walk", "Other mode"),
                       levels = c("Walk", "Other mode"))) |>
  group_by(Mode) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

###
# See what share of total senior health trips take place by walking

short_sr_health_trips |>
  as_survey_design(weights = WTTRDFIN) |>
  mutate(Mode = factor(ifelse(walk, "Walk", "Other mode"),
                       levels = c("Walk", "Other mode"))) |>
  group_by(Mode) |>
  survey_tally(vartype = "ci") |>
  mutate(`Estimated percent of trips` = 
           paste0(round(100*n/sum(n)),"%"),
         `Lower estimate (95% confidence)` = 
           paste0(round(100*n_low/sum(n)),"%"),
         `Upper estimate (95% confidence)` = 
           paste0(round(100*n_upp/sum(n)),"%")) |>
  select(Mode,
         `Estimated percent of trips`,
         `Lower estimate (95% confidence)`,
         `Upper estimate (95% confidence)`) |>
  kable()

###
# Construct predictor variables

# Distance

sample_trips <- short_sr_health_trips |>
  filter(TRPMILES >=0)

# Medical condition status

medcond_data <- people |>
  select(HOUSEID, PERSONID, MEDCOND)

sample_trips <- sample_trips |>
  left_join(medcond_data) |>
  mutate(medcond = MEDCOND == "01")

sample_trips |>
  group_by(medcond) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

# Suburb indicator

sample_trips <- sample_trips |>
  filter(OBHUR != "-9")

sample_trips <- sample_trips |>
  mutate(suburb = OBHUR == "S")

sample_trips |>
  group_by(suburb) |>
  summarize(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

# Immigrant indicator

immigrant_data <- people |>
  select(HOUSEID, PERSONID, BORNINUS)

sample_trips <- sample_trips |>
  left_join(immigrant_data) |>
  mutate(immigrant = BORNINUS == "01")

sample_trips |>
  group_by(immigrant) |>
  summarize(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

###
# Estimate model

model <- glm(walk ~ 
               TRPMILES +
               R_AGE +
               medcond +
               suburb +
               immigrant,
             data = sample_trips,
             family = "binomial")

coeff_labels <- c("Trip distance (miles)" = "TRPMILES",
                  "Age (years)" = "R_AGE",
                  "Medical condition" = "medcondTRUE",
                  "Trip origin in suburban block group" = "suburbTRUE",
                  "Born in US" = "immigrantTRUE")

export_summs(model, 
             robust = "HC3", 
             coefs = coeff_labels,
             error_format = "(p = {p.value})",
             error_pos = "right")

######
# Visualizations

# Distance

ggplot(sample_trips) +
  geom_histogram(aes(x = TRPMILES),
                 color = "cornflowerblue",
                 fill = "lavender",
                 binwidth = 0.1) +
  scale_x_continuous(name = "Trip distance (miles)",
                     breaks = seq(0, 1.5, by=0.1)) +
  scale_y_continuous(name = "Number of trips in sample") +
  theme_minimal()

# Age

ggplot(sample_trips) +
  geom_histogram(aes(x = R_AGE),
                 color = "cornflowerblue",
                 fill = "lavender",
                 binwidth = 1) +
  scale_x_continuous(name = "Traveler's age (years)",
                     breaks = seq(0, 1.5, by=0.1)) +
  scale_y_continuous(name = "Number of trips in sample") +
  theme_minimal()

###
# Plot predicted probabilities of walking

effect_plot(model,
            pred = "TRPMILES",
            colors = "cornflowerblue",
            interval = TRUE) +
  scale_x_continuous(name = "Trip distance (miles)",
                     breaks = seq(0, 1.5, by=0.1)) +
  scale_y_continuous(name = "Probabilitity of walking",
                     breaks = breaks <- seq(0, 0.8, by = 0.1),
                     labels = paste0(breaks*100, "%"))

###
# Plot categorical predictors

effect_plot(model = model,
            pred = "medcond",
            colors = "cornflowerblue",
            interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0, 100, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Does this person have a disability or\n",
                                 "medical condition that makes it difficult\n",
                                 "to travel outside the home?"),
                   labels = c("No", "Yes"))

effect_plot(model = model,
            pred = "suburb",
            colors = "cornflowerblue",
            interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0, 100, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Did this person take a trip that started\n",
                                 "in a suburban block group?"),
                   labels = c("No", "Yes"))

effect_plot(model = model,
            pred = "immigrant",
            colors = "cornflowerblue",
            interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0, 100, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Was this person born in the U.S.?"),
                   labels = c("No", "Yes"))
