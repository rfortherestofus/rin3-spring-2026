# Install if you don't have this package
# install.packages("tidyverse")

library(tidyverse)
library(readxl)

penguins <- read_csv("data-raw/penguins.csv")

read_csv("data-raw/2022-obtn-by-county.xlsx")

read_excel("data-raw/2019-obtn-by-county.xlsx",
           sheet = "Total Population")

read_csv("data-raw/penguins.csv")


# Exploration -------------------------------------------------------------

penguins |> 
  filter(year == 2007)

penguins %>% 
  filter(year == 2007)


adelie_mean_bill_length <- 
penguins |> 
  filter(species == "Adelie") |> 
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))
  
# adelie_mean_bill_length_2007 <- 
penguins |> 
  filter(year == 2007) |> 
  filter(species == "Adelie") |> 
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))


# The .by argument ------------------------------------------------------

penguins |>
  summarize(
    mean_bill_length = mean(
      bill_length_mm,
      na.rm = TRUE
    ),
    .by = c(island, species)
  ) |> 
  slice_max(order_by = mean_bill_length,
            n = 1)

penguins |>
  group_by(island, species) |> 
  summarize(
    mean_bill_length = mean(
      bill_length_mm,
      na.rm = TRUE
    )
  ) |> 
  ungroup() |> 
  slice_max(order_by = mean_bill_length,
            n = 1)

# Working directories -----------------------------------------------------

penguins_mean_bill_length <-
  penguins |>
  filter(island == "Biscoe") |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

# Native pipe vs tidyverse pipe -------------------------------------------

penguins %>%
  count(species, sex)

# Parentheses -------------------------------------------------------------

penguins |>
  select(-species)

penguins |>
  select(-(bill_length_mm:body_mass_g))

read_csv("data-raw/penguins_data.csv", na = c("-999", "-999.0"))

# select() issues ---------------------------------------------------------

penguins |>
  select(-island:year)

penguins |>
  select(c(-1, island:bill_length_mm))

# Does not remove the "species" variable but this does:

penguins |>
  select(island:year)

# NA values ---------------------------------------------------------------

read_csv("data-raw/penguins_data.csv", na = c("-999")) |>
  view()


# NA values ---------------------------------------------------------------

penguins <- read_csv(file = "data-raw/penguins.csv")

penguins |>
  mutate(not_actually_na = "NA") |>
  mutate(actually_na = na_if(not_actually_na, "NA")) |>
  mutate(really_not_na = replace_na(actually_na, "NA"))


# Rounding ----------------------------------------------------------------

penguins |>
  filter(island == "Biscoe") |>
  drop_na(body_mass_g, sex) |>
  group_by(sex) |>
  summarize(mean_body_mass = mean(body_mass_g)) |>
  mutate(mean_body_mass = round(mean_body_mass, digits = 1)) |> 
  view()

# Viewing your dataset ----------------------------------------------------

penguins |> 
  view()

penguins |>
  print(n = 100)
