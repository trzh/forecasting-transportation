###
# Load libraries

options(java.parameters = '-Xmx4G')

library(tidyverse)
library(here)
library(knitr)
library(tigris)
library(stringr)
library(maptiles)
library(tidyterra)
library(r5r)
library(sf)
library(leaflet)
library(rJava)

'%!in%' <- function(x,y)!('%in%'(x,y))

here("code",
     "grvty_balancing.R") |>
  source()

###
# Select study area

all_cbsas <- core_based_statistical_areas(progress_bar = FALSE,
                                          year = 2024) |>
  select(NAMELSAD) |>
  mutate(type = ifelse(!is.na(str_match(NAMELSAD, "Metro")), "Metro", "Micro")) |>
  mutate(type = as.character(type))

cambridge <- all_cbsas |>
  filter(NAMELSAD == "Cambridge, MD Micro Area") |>
  st_transform("WGS84")

base_map <- get_tiles(cambridge,
                      provider = "CartoDB.Positron",
                      zoom = 9,
                      crop = TRUE)

ggplot(cambridge) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(fill = NA,
          color = "cornflowerblue") +
  theme_void()

###
# Load job data

state <- "md"
year <- "2021"

cambridge_counties_5_digit <- c("24019")
cambridge_counties_3_digit <- substr(cambridge_counties_5_digit, 3, 5)

url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/",
              state,
              "/od/",
              state,
              "_od_main_JT00_",
              year,
              ".csv.gz")

pa_data <- read_csv(url) |>
  mutate(w_county = substr(w_geocode, 1, 5),
         h_county = substr(h_geocode, 1, 5)) |>
  filter(h_county %in% cambridge_counties_5_digit &
           w_county %in% cambridge_counties_5_digit) |>
  mutate(w_geocode = as.character(w_geocode),
         h_geocode = as.character(h_geocode))

###
# Aggregate data to zone totals

total_prod <- pa_data |>
  group_by(h_geocode) |>
  summarise(goods_p = sum(SI01),
            trade_p = sum(SI02),
            serve_p = sum(SI03),
            total_p = sum(S000)) |>
  rename(geocode = h_geocode)

total_attr <- pa_data |>
  group_by(w_geocode) |>
  summarize(goods_a = sum(SI01),
            trade_a = sum(SI02),
            serve_a = sum(SI03),
            total_a = sum(S000)) |>
  rename(geocode = w_geocode)

trip_gen <- full_join(total_prod,
                      total_attr) |>
  replace_na(list(goods_p = 0, 
                  goods_a = 0,
                  trade_p = 0,
                  trade_a = 0,
                  serve_p = 0,
                  serve_a = 0,
                  total_p = 0,
                  total_a = 0))

###
# Load spatial data

msa_blocks <- blocks(state = "MD",
                     county = cambridge_counties_3_digit,
                     progress_bar = FALSE)

ggplot(msa_blocks) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(fill = NA,
          color = "cornflowerblue") +
  theme_void()

trip_gen_locs <- msa_blocks |>
  rename(geocode = GEOID20) |>
  right_join(trip_gen) |>
  select(geocode, 
         goods_p, 
         trade_p, 
         serve_p,
         total_p,
         goods_a, 
         trade_a,
         serve_a,
         total_a) |>
  st_transform("WGS84")

leaflet(trip_gen_locs) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "cornflowerblue",
              fillColor = "cornflowerblue",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = trip_gen_locs$geocode)

###
# Load the network

cambridge_network <- here("P4",
                      "network") |>
  setup_r5()

street_vis <- street_network_to_sf(cambridge_network)

street_lines <- street_vis$edges
street_pts <- street_vis$vertices

st_write(street_lines,
         here("data",
              "street-lines_1.shp"))

st_write(street_pts,
         here("data",
              "street-pts_1.shp"))

stop_r5()

street_lines <- here("data",
                     "street-lines_1.shp") |>
  st_read()

street_pts <- here("data",
                   "street-pts_1.shp") |>
  st_read()

base_map <- get_tiles(street_lines,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot() +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = trip_gen_locs,
          color = "darkseagreen",
          fill = "darkseagreen1") +
  geom_sf(data = street_lines,
          color =  "cornflowerblue") +
  theme_void()

###
# Skim the network

trip_gen_loc_ids <- trip_gen_locs |>
  st_point_on_surface() |>
  st_nearest_feature(street_pts)

trip_gen_pts <- street_pts[trip_gen_loc_ids,] |>
  mutate(id = trip_gen_locs$geocode) |>
  select(id)

cambridge_network <- here("P4",
                      "network") |>
  setup_r5()

skim <- travel_time_matrix(cambridge_network,
                           origins = trip_gen_pts,
                           destinations = trip_gen_pts,
                           mode = "CAR",
                           max_trip_duration = 180)

stop_r5()

write_csv(skim, file = here("data",
                            "cambridge-skim.csv"))

skim <- read_csv(here("data",
                      "cambridge-skim.csv"),
                 col_types = "ccn")

head(skim) |>   
  kable()

###
# Find missing zones from skim

missing_zones <- trip_gen |>
  mutate(missing = trip_gen$geocode %!in% skim$from_id &
           trip_gen$geocode %!in% skim$to_id)

###
# Apply a gravity model

flow_tt <- pa_data |>
  rename(from_id = h_geocode,
         to_id = w_geocode) |>
  right_join(skim) |>
  rename(flow_total = S000,
         flow_goods = SI01,
         flow_trade = SI02,
         flow_serve = SI03) |>
  replace_na(list(flow_total = 0,
                  flow_goods = 0,
                  flow_trade = 0,
                  flow_serve = 0))

avg_tts <- tibble(`Worker sector` = c("Goods", "Trade", "Service", "Total"),
                  `Average travel time (observed)` = c(
                    sum(flow_tt$flow_goods * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_goods),
                    sum(flow_tt$flow_trade * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_trade),
                    sum(flow_tt$flow_serve * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_serve),
                    sum(flow_tt$flow_total * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_total)))

kable(avg_tts, digits = 1)

betas <- 1/avg_tts$`Average travel time (observed)`
names(betas) <- c("Goods", "Trade", "Service", "Total")

initial_betas <- tibble(`Worker sector` = names(betas),
                        `Initial β value` = betas)

kable(initial_betas, digits = 3)

kable(betas_table, digits = 3)


###
# Visualize travel time sensitivity

friction <- tibble(`Travel time (min)` = seq(0, 30, by=1)) |>
  mutate(Goods = exp(-1 * betas["Goods"] * `Travel time (min)`),
         Trade = exp(-1 * betas["Trade"] * `Travel time (min)`),
         Service = exp(-1 * betas["Service"] * `Travel time (min)`),
         `All industries` = exp(-1 * betas["Total"] * `Travel time (min)`)) |>
  pivot_longer(cols = -`Travel time (min)`,
               names_to = "Industry") |>
  rename(`Destination attractiveness` = value)

ggplot(friction) +
  geom_line(aes(x = `Travel time (min)`,
                y = `Destination attractiveness`,
                linetype = Industry)) +
  scale_x_continuous(breaks = seq(0, 30, by=5)) +
  scale_y_continuous(breaks = seq(0, 1.1, by=0.1)) +
  theme_minimal()

###
# Calculate friction factors

flow_tt <- flow_tt |>
  mutate(friction_goods = exp(-1 * betas["Goods"] * travel_time_p50),
         friction_trade = exp(-1 * betas["Trade"] * travel_time_p50),
         friction_serve = exp(-1 * betas["Service"] * travel_time_p50),
         friction_total = exp(-1 * betas["Total"] * travel_time_p50))

###
# Estimate initial trip matrix

flow_goods_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "goods_p",
                                  zone_d = "goods_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_goods",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_trade_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "trade_p",
                                  zone_d = "trade_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_trade",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_serve_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "serve_p",
                                  zone_d = "serve_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_serve",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_total_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "total_p",
                                  zone_d = "total_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_total",
                                  tolerance = 0.001,
                                  max_iter = 100)

write_csv(flow_goods_est$flows,
          file = here("P4",
                      "data",
                      "init-goods-flow.csv"))

write_csv(flow_trade_est$flows,
          file = here("P4",
                      "data",
                      "init-trade-flow.csv"))

write_csv(flow_serve_est$flows,
          file = here("P4",
                      "data",
                      "init-serve-flow.csv"))

write_csv(flow_total_est$flows,
          file = here("P4",
                      "data",
                      "init-total-flow.csv"))

###
# Evaluate model fit

flow_goods <- here("P4",
                   "data",
                   "init-goods-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         goods_flow_est = flow)

flow_trade <- here("P4",
                   "data",
                   "init-trade-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         trade_flow_est = flow)

flow_serve <- here("P4",
                   "data",
                   "init-serve-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         serve_flow_est = flow)

flow_total <- here("P4",
                   "data",
                   "init-total-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         total_flow_est = flow)

flow_tt <- flow_tt |>
  left_join(flow_goods) |>
  left_join(flow_trade) |>
  left_join(flow_serve) |> 
  left_join(flow_total)

avg_tts <- avg_tts |>
  mutate(`Average travel time (estimated)` = c(
    sum(flow_tt$goods_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$goods_flow_est),
    sum(flow_tt$trade_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$trade_flow_est),
    sum(flow_tt$serve_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$serve_flow_est),
    sum(flow_tt$total_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$total_flow_est)))

avg_tts |>
  kable(digits = 1)

avg_tts <- avg_tts |>
  mutate(rmse = c((mean((flow_tt$flow_goods - flow_tt$goods_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_trade - flow_tt$trade_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_serve - flow_tt$serve_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_total - flow_tt$total_flow_est)^2))^0.5))

kable(avg_tts, digits = 2)

###
# Visualize comparison between expected and actual

plot_flows <- function(flow_df,
                       obs_col_name,
                       est_col_name) {
  
  summary <- flow_df |>
    rename(obs = all_of(obs_col_name),
           est = all_of(est_col_name)) |>
    group_by(obs, est) |>
    summarize(n = n()) 
  
  max_scale <- max(summary$obs, summary$est)
  my_interval <- ceiling(max_scale / 10)
  dot_size <- floor(70 / max_scale)
  
  max_n_exp = round(log10(max(summary$n)))
  
  ggplot(summary) +
    geom_point(aes(x = obs,
                   y = est,
                   color = n),
               size = dot_size) +
    scale_x_continuous(name = "Observed flow", 
                       limits = c(0, max_scale),
                       breaks = seq(0, max_scale, by=my_interval)) +
    scale_y_continuous(name = "Estimated flow", 
                       limits = c(0, max_scale),
                       breaks = seq(0, max_scale, by=my_interval)) +
    scale_color_viridis_c(transform = "log",
                          breaks = my_breaks <- c(10^seq(-1, 
                                                         max_n_exp, 
                                                         by=1)),
                          labels = formatC(my_breaks, format = "d", 
                                           big.mark = ","),
                          direction = -1,
                          name = "Number of P-A pairs") +
    theme_minimal()
  
  
}

plot_flows(flow_tt, 
           obs_col_name = "flow_goods",
           est_col_name = "goods_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_trade",
           est_col_name = "trade_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_serve",
           est_col_name = "serve_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_total",
           est_col_name = "total_flow_est")

###
# Calibrate the gravity model

flow_tt <- flow_tt |>
  select(-goods_flow_est,
         -trade_flow_est,
         -serve_flow_est,
         -total_flow_est)

## Calibrate goods beta
calibrated_flows_goods <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_goods",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_goods <- calibrated_flows_goods$beta

goods_flow_est <- calibrated_flows_goods$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         goods_flow_est = flow_est) |>
  select(from_id, to_id, goods_flow_est)

flow_tt <- flow_tt |>
  left_join(goods_flow_est)

## Calibrate trade beta

calibrated_flows_trade <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_trade",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_trade <- calibrated_flows_trade$beta

trade_flow_est <- calibrated_flows_trade$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         trade_flow_est = flow_est) |>
  select(from_id, to_id, trade_flow_est)

flow_tt <- flow_tt |>
  left_join(trade_flow_est)

## calibrate service beta
calibrated_flows_serve <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_serve",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_serve <- calibrated_flows_serve$beta

serve_flow_est <- calibrated_flows_serve$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         serve_flow_est = flow_est) |>
  select(from_id, to_id, serve_flow_est)

flow_tt <- flow_tt |>
  left_join(serve_flow_est)

## calibrate total beta
calibrated_flows_total <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_total",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_total <- calibrated_flows_total$beta

total_flow_est <- calibrated_flows_total$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         total_flow_est = flow_est) |>
  select(from_id, to_id, total_flow_est)

flow_tt <- flow_tt |>
  left_join(total_flow_est)

betas_table <- tibble(Industry = c("Goods", 
                                   "Trade",
                                   "Service",
                                   "Total"),
                      beta_initial = betas,
                      beta_calibrated = c(beta_goods,
                                          beta_trade,
                                          beta_serve,
                                          beta_total))
write_csv(flow_tt,
          here("P4",
               "data",
               "calib-flows.csv"))

write_csv(betas_table,
          here("P4",
               "data",
               "calib-betas.csv"))

###
# Evaluate model fit again

flow_tt <- here("P4",
                "data",
                "calib-flows.csv") |>
  read_csv()

avg_tts <- avg_tts |>
  select(-rmse) |>
  mutate(`Average travel time (estimated)` = c(
    sum(flow_tt$goods_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$goods_flow_est),
    sum(flow_tt$trade_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$trade_flow_est),
    sum(flow_tt$serve_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$serve_flow_est),
    sum(flow_tt$total_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$total_flow_est)))

avg_tts |>
  kable(digits = 1)

avg_tts <- avg_tts |>
  mutate(rmse = c((mean((flow_tt$flow_goods - flow_tt$goods_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_trade - flow_tt$trade_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_serve - flow_tt$serve_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_total - flow_tt$total_flow_est)^2))^0.5))

kable(avg_tts, digits = 2)

plot_flows(flow_tt,
           obs_col_name = "flow_goods",
           est_col_name = "goods_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_trade",
           est_col_name = "trade_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_serve",
           est_col_name = "serve_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_total",
           est_col_name = "total_flow_est")

###
#  Interpret calibrated parameters

betas_table <- here("P4",
                    "data",
                    "calib-betas.csv") |> 
  read_csv()

friction <- tibble(`Travel time (min)` = seq(1, 60, by=1)) |>
  mutate(Goods = exp(-1 * betas_table$beta_calibrated[1] * `Travel time (min)`),
         Trade = exp(-1 * betas_table$beta_calibrated[2] * `Travel time (min)`),
         Service = exp(-1 * betas_table$beta_calibrated[3] * `Travel time (min)`),
         `All industries` = 
           exp(-1 * betas_table$beta_calibrated[4] * `Travel time (min)`)) |>
  pivot_longer(cols = -`Travel time (min)`,
               names_to = "Sector") |>
  rename(`Destination attractiveness` = value) |>
  filter(`Destination attractiveness` < 2)

ggplot(friction) +
  geom_line(aes(x = `Travel time (min)`,
                y = `Destination attractiveness`,
                linetype = Sector)) +
  scale_x_continuous(breaks = seq(0, 60, by=5)) +
  scale_y_continuous(breaks = seq(0, 2, by=0.1),
                     limits = c(0, 1.5)) +
  theme_minimal()
