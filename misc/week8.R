library(tidyverse)

# as.numeric() vs parse_number() -----------------------------------------

names_and_ages <-
  tribble(
    ~name,
    ~age,
    "David",
    "45",
    "Rachel",
    "46",
    "Leila",
    "9",
    "Elias",
    "9 years old (born in 2016)",
    "Diego",
    "No longer alive"
  )

names_and_ages

names_and_ages |>
  mutate(age_as_numeric = as.numeric(age)) |>
  mutate(age_parse_number = parse_number(age))


# case_match() ------------------------------------------------------------

countries <-
  tribble(
    ~country_name,
    "USA",
    "US",
    "United States of America",
    "Canada"
  )

countries

countries |>
  mutate(
    country_name_v2 = case_match(
      country_name,
      "USA" ~ "USA",
      "US" ~ "USA",
      "United States of America" ~ "USA",
      "Canada" ~ "Canada"
    )
  ) |>
  count(country_name_v2)

# case_match() vs case_when() ---------------------------------------------

library(palmerpenguins)

data(penguins)

penguins

penguins |>
  select(species) |>
  mutate(
    species_v2 = case_match(
      species,
      "Adelie" ~ "Island 1",
      "Chinstrap" ~ "Island 2",
      "Gentoo" ~ "Island 3"
    )
  )

penguins |>
  select(species) |>
  mutate(
    species_v2 = case_when(
      species == "Adelie" ~ "Island 1",
      species == "Chinstrap" ~ "Island 2",
      species == "Gentoo" ~ "Island 3"
    )
  )

penguins |>
  select(species, bill_length_mm) |>
  mutate(
    species_and_length = case_when(
      species == "Adelie" & bill_length_mm > 35 ~ "Big Adelie Penguin",
      species == "Adelie" & bill_length_mm <= 35 ~ "Small Adelie Penguin",
      .default = "Other"
    )
  ) |>
  group_by(species_and_length) |>
  summarize(avg_bill_length = mean(bill_length_mm, na.rm = TRUE))

# Joins with mismatched variable types ------------------------------------

fruits <-
  tibble(
    id = c(1, 2, 3, 4),
    name = c("apple", "banana", "cherry", "date")
  )

prices <-
  tibble(
    id = c("1", "2", "3", "4"),
    price = c(0.99, 1.50, 2.00, 2.50)
  ) |>
  mutate(id = parse_number(id))

prices

left_join(
  fruits,
  prices,
  join_by(id)
)

# Many-to-many joins ------------------------------------------------------

orders <-
  tibble(
    order_date = c("2024-01-01", "2024-01-01", "2024-01-02"),
    product = c("apple", "apple", "banana"),
    quantity = c(5, 3, 2),
    location = "Store A"
  )

inventory <-
  tibble(
    product = c("apple", "apple", "banana"),
    location = c("Store A", "Store B", "Store A"),
    stock = c(100, 150, 75)
  )

left_join(
  orders,
  inventory,
  join_by(product)
) |>
  view()

left_join(
  orders,
  inventory,
  join_by(product, location)
)

# Iteration --------------------------------------------------------------

# Import multiple years of data

library(readxl)
library(janitor)

total_population_2019 <-
  read_excel(
    "data-raw/2019-obtn-by-county.xlsx",
    sheet = "Total Population"
  ) |>
  clean_names() |>
  mutate(year = 2019)

total_population_2020 <-
  read_excel(
    "data-raw/2020-obtn-by-county.xlsx",
    sheet = "Total Population"
  ) |>
  clean_names() |>
  mutate(year = 2020)

total_population <-
  bind_rows(
    total_population_2019,
    total_population_2020
  )

import_single_year_data <- function(year) {
  read_excel(
    path = str_glue("data-raw/{year}-obtn-by-county.xlsx"),
    sheet = "Total Population"
  ) |>
    clean_names() |>
    mutate(data_year = year)
}

import_single_year_data(year = 2021)

obtn_years <- 2019:2023

total_population <-
  map(
    obtn_years,
    import_single_year_data
  ) |>
  bind_rows()

total_population |>
  group_by(data_year) |>
  summarize(avg_population = mean(population))

# Make multiple plots

make_total_population_plot <- function(county_name) {
  total_population |>
    filter(geography == county_name) |>
    ggplot(
      aes(
        x = data_year,
        y = population
      )
    ) +
    geom_line()

  ggsave(
    filename = str_glue("plots/{county_name}.png")
  )
}

make_total_population_plot("Multnomah")

oregon_counties <-
  total_population |>
  distinct(geography) |>
  pull(geography)

walk(
  oregon_counties,
  make_total_population_plot
)


# walk2 -------------------------------------------------------------------

make_total_population_plot_single_year <- function(county_to_highlight, year) {
  total_population |>
    filter(geography != "Urban") |>
    filter(geography != "Rural") |>
    filter(geography != "Oregon") |>
    mutate(highlight_county = if_else(geography == county_to_highlight, "Y", "N")) |>
    filter(data_year == year) |>
    ggplot(
      aes(
        x = population,
        y = geography,
        fill = highlight_county
      )
    ) +
    geom_col() +
    scale_fill_manual(
      values = c(
        "Y" = "red",
        "N" = "gray"
      )
    )

  ggsave(
    filename = str_glue("plots/{county_to_highlight}-{year}.png")
  )
}

make_total_population_plot_single_year(
  county_to_highlight = "Baker",
  year = 2022
)

all_counties_all_years <-
  total_population |>
  distinct(geography, data_year) |>
  filter(geography != "Urban") |>
  filter(geography != "Rural") |>
  filter(geography != "Oregon")

counties <-
  all_counties_all_years |>
  pull(geography)

years <-
  all_counties_all_years |>
  pull(data_year)

walk2(
  counties,
  years,
  make_total_population_plot_single_year
)
