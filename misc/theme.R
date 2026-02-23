library(tidyverse)
library(palmerpenguins)

# Theme -------------------------------------------------------------------

theme_dk <- function(hide_gridlines = FALSE, hide_legend = FALSE) {
  my_theme <- theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(
        color = "grey60",
        size = 18
      )
    )

  if (hide_gridlines == TRUE) {
    my_theme <- my_theme +
      theme(panel.grid = element_blank())
  }

  if (hide_legend == TRUE) {
    my_theme <- my_theme +
      theme(legend.position = "none")
  }

  my_theme
}

# Plots -------------------------------------------------------------------

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point()

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = island
  )
) +
  geom_point() +
  theme_dk(hide_legend = TRUE)

penguins |>
  count(island) |>
  ggplot(
    aes(
      x = island,
      y = n
    )
  ) +
  geom_col() +
  theme_dkk(hide_gridlines = TRUE)
