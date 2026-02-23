library(tidyverse)
library(janitor)
library(sf)

# Portland ----------------------------------------------------------------

portland_boundaries <-
  read_sf("data-raw/City_Boundaries.geojson") |>
  clean_names() |>
  filter(cityname == "Portland")

portland_boundaries

portland_boundaries |>
  ggplot() +
  geom_sf()

traffic_signals <-
  read_sf("data-raw/Traffic_Signals.geojson") |>
  clean_names()

traffic_signals

traffic_signals |>
  ggplot() +
  geom_sf(size = 0.25)

snow_and_ice_routes <-
  read_sf("data-raw/Snow_and_Ice_Routes.geojson") |>
  clean_names()

snow_and_ice_routes

snow_and_ice_routes |>
  ggplot() +
  geom_sf(aes(color = priority))

ggplot() +
  geom_sf(
    data = portland_boundaries,
    fill = "gray80",
    alpha = 0.5
  ) +
  geom_sf(
    data = snow_and_ice_routes,
    alpha = 0.5
  ) +
  geom_sf(
    data = traffic_signals,
    aes(color = software_type),
    alpha = 0.5,
    size = 1
  ) +
  theme_dk(hide_gridlines = TRUE, hide_legend = TRUE)


# Tigris ------------------------------------------------------------------

library(tigris)

us_states <- states()

us_states

us_states |>
  shift_geometry() |>
  ggplot() +
  geom_sf()

kentucky_counties <- counties(state = "Kentucky")

kentucky_counties

kentucky_counties |>
  ggplot() +
  geom_sf()

# Median Income -----------------------------------------------------------

library(tidycensus)
library(scales)

median_income <-
  get_acs(
    state = "Illinois",
    geography = "county",
    variables = "B19013_001",
    geometry = TRUE
  )

median_income

median_income |>
  ggplot(aes(fill = estimate)) +
  geom_sf()


# International Data ------------------------------------------------------

library(rnaturalearth)

?rnaturalearth::ne_countries()


ne_countries(
  country = c("Ukraine"),
  scale = "large",
  returnclass = "sf"
) |>
  select(sovereignt) |>
  ggplot() +
  geom_sf()


# Mapview -----------------------------------------------------------------

library(mapview)

mapview(us_states)


# Interactive -------------------------------------------------------------

library(ggiraph)

median_income_interactive_plot <-
  median_income |>
  ggplot(aes(
    fill = estimate,
    tooltip = estimate |> dollar()
  )) +
  geom_sf_interactive()

girafe(ggobj = median_income_interactive_plot)
