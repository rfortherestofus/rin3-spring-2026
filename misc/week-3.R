# Load Packages -----------------------------------------------------------

library(tidyverse)

# Import Data -------------------------------------------------------------

penguins <- read_csv("data-raw/penguins.csv")

penguins_bill_length_by_island <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

penguins_by_species <-
  penguins |>
  count(species)


# `==` and lowercase `x` and `y` ------------------------------------------

ggplot(
  data = penguins,
  # why can't I use == within this function?
  mapping = aes(
    x = flipper_length_mm,
    # must be lowercase x and y
    y = body_mass_g
  )
) +
  geom_point()



# Color vs Fill ----------------------------------------------------------

ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = island
  )
) +
  geom_point()


ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()

ggplot(
  penguins,
  aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = island
  )
) +
  geom_point(
    shape = 21,
    fill = "orange"
  )


# `geom_bar()` vs `geom_col()` -------------------------------------------

penguins_by_species <-
  penguins |>
  count(species)

ggplot(
  data = penguins,
  mapping = aes(x = species)
) +
  geom_bar()

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n
  )
) +
  geom_col()

# Applying values specific color/fill properties -------------------------

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Adelie" = "green",
      "Chinstrap" = "red",
      "Gentoo" = "orange"
    )
  )

# Dropping Points in Scatterplots ----------------------------------------

ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point() +
  scale_x_continuous(limits = c(170, 200))

penguins_filtered <-
  penguins |>
  drop_na(flipper_length_mm, body_mass_g) |>
  filter(flipper_length_mm < 200)

ggplot(
  data = penguins_filtered,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point()

# Bar Chart Width ---------------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    label = mean_bill_length,
    color = island
  )
) +
  geom_col(width = 0.5) +
  theme_minimal()

ggplot(
  data = penguins,
  aes(x = bill_length_mm)
) +
  geom_histogram(binwidth = 1)

# Reordering Bar Charts ---------------------------------------------------

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = reorder(species, n, decreasing = TRUE),
    y = n,
    fill = species
  )
) +
  geom_col()

penguins_by_species_reordered <-
  penguins_by_species |>
  arrange(n)
# mutate(species = fct(species, levels = c("Adelie", "Chinstrap", "Gentoo")))
# mutate(species = fct_reorder(species, n)) |>
# mutate(species = fct_rev(species))

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()


# Wrapping Long Text ------------------------------------------------------

library(gapminder)

data("gapminder")

ggplot(
  data = gapminder,
  aes(
    x = year,
    y = lifeExp,
    group = country
  )
) +
  geom_line() +
  facet_wrap(vars(country))

gapminder_wrapped <-
  gapminder |>
  mutate(country_wrapped = str_wrap(country, width = 10))

ggplot(
  data = gapminder_wrapped,
  aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line() +
  facet_wrap(vars(country_wrapped))

gapminder |>
  mutate(country_wrapped = str_wrap(country, width = 10)) |>
  ggplot(
    aes(
      x = year,
      y = lifeExp
    )
  ) +
  geom_line() +
  facet_wrap(vars(country_wrapped)) +
  theme(axis.text.x = element_blank())


ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = flipper_length_mm,
    size = flipper_length_mm
  )
) +
  geom_point() 

gapminder |> 
  filter(country == "Afghanistan") |>  
  mutate(year = as.character(year)) |> 
ggplot(
  aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col()

penguins |> 
  count(species) |> 
  mutate(pct = n / sum(n)) |> 
  ggplot(
    aes(
      y = pct,
      x = 1,
      fill = species,
      label = pct
    )
  ) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5))
