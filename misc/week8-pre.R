library(tidyverse)

# as.numeric() vs parse_number() -----------------------------------------

names_and_ages <-
  tribble(
    ~name                        ,
    ~age                         ,
    "David"                      ,
    "46"                         ,
    "Rachel"                     ,
    "46"                         ,
    "Leila"                      ,
    "9"                          ,
    "Elias"                      ,
    "9 years old (born in 2016)" ,
    "Diego"                      ,
    "No longer alive"
  )

names_and_ages

names_and_ages |>
  mutate(age_as_numeric = as.numeric(age))

names_and_ages |>
  mutate(age_parse_number = parse_number(age))


# Why save as RDS vs CSV? ------------------------------------------------

names_and_ages_clean <-
  names_and_ages |>
  mutate(
    age = parse_number(age),
    age_group = factor(
      if_else(age < 18, "child", "adult"),
      levels = c("child", "adult")
    )
  )

names_and_ages_clean

names_and_ages_clean |>
  count(age_group) |>
  ggplot(aes(x = age_group, y = n)) +
  geom_col()

write_csv(names_and_ages_clean, "data/names_and_ages.csv")

names_and_ages_clean_csv <-
  read_csv("data/names_and_ages.csv")

names_and_ages_clean_csv |>
  count(age_group) |>
  ggplot(aes(x = age_group, y = n)) +
  geom_col()

write_rds(names_and_ages_clean, "data/names_and_ages.rds")

names_and_ages_clean_rds <-
  read_rds("data/names_and_ages.rds")

names_and_ages_clean_rds |>
  count(age_group) |>
  ggplot(aes(x = age_group, y = n)) +
  geom_col()


# Joins with mismatched variable types ------------------------------------

fruits <-
  tibble(
    product_id = c(1, 2, 3, 4),
    name = c("apple", "banana", "cherry", "date")
  )

prices <-
  tibble(
    product_id = c("1", "2", "3", "4"),
    price = c(0.99, 1.50, 2.00, 2.50)
  )

prices

left_join(
  fruits,
  prices,
  join_by(product_id)
)

# Many-to-many joins ------------------------------------------------------

students <-
  tribble(
    ~student , ~course   , ~section_id ,
    "Alice"  , "Math"    ,           1 ,
    "Alice"  , "English" ,           1 ,
    "Bob"    , "Math"    ,           2
  )

courses <-
  tribble(
    ~course   , ~section_id , ~professor  ,
    "Math"    ,           1 , "Dr. Smith" ,
    "Math"    ,           2 , "Dr. Jones" ,
    "English" ,           1 , "Dr. Brown"
  )

left_join(
  students,
  courses,
  join_by(course)
)

left_join(
  students,
  courses,
  join_by(course, section_id)
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
