#  Recreating this: https://show.rfor.us/cditiy

library(tidyverse)
library(ggchicklet)
library(patchwork)
library(scales)

cbem_bar_chart <- function(state_to_filter, age_group_to_filter) {
  cbem <-
    read_csv("https://rin3fall2025.rfortherestofus.com/data-raw/cbem.csv") |>
    mutate(percent_formatted = percent(percent, accuracy = 0.1))

  max_y_value <-
    cbem |>
    filter(location == state_to_filter) |>
    filter(group != "All Persons") |>
    slice_max(
      order_by = percent,
      n = 1
    ) |>
    mutate(percent = percent + 0.02) |>
    pull(percent)

  state_average <-
    cbem |>
    filter(location == state_to_filter) |>
    filter(age_group == age_group_to_filter) |>
    filter(group == "All Persons") |>
    pull(percent)

  state_average_formatted <-
    state_average |>
    percent(accuracy = 0.1)

  state_average_text <- str_glue("CBEM State Rate\n{state_average_formatted}")

  cbem |>
    filter(location == state_to_filter) |>
    filter(age_group == age_group_to_filter) |>
    filter(group != "All Persons") |>
    mutate(group = fct(
      group,
      levels =
        c(
          "American Indian or Alaska Native",
          "Asian or Pacific Islander",
          "Black or African American",
          "White",
          "Hispanic or Latino"
        )
    )) |>
    mutate(x_position = row_number()) |>
    mutate(x_position = case_when(
      group == "Hispanic or Latino" ~ 5.25,
      .default = x_position
    )) |>
    ggplot(
      aes(
        x = x_position,
        y = percent,
        fill = group,
        label = percent_formatted
      )
    ) +
    annotate(
      geom = "text",
      x = 5.25,
      y = state_average,
      vjust = -1,
      color = "grey60",
      lineheight = 1,
      label = state_average_text
    ) +
    geom_hline(
      yintercept = state_average,
      linetype = "dashed",
      color = "grey60"
    ) +
    geom_chicklet() +
    geom_text(
      vjust = 1.75,
      color = "white",
      family = "Montserrat"
    ) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#9CC892",
        "Asian or Pacific Islander" = "#0066cc",
        "Black or African American" = "#477A3E",
        "White" = "#6CC5E9",
        "Hispanic or Latino" = "#ff7400"
      )
    ) +
    scale_y_continuous(limits = c(0, max_y_value)) +
    theme_void() +
    theme(legend.position = "none")
}

cbem_bar_chart_two_age_groups <- function(state_to_chart) {
  cbem_bar_chart(state_to_filter = state_to_chart, age_group_to_filter = "Under 18") +
    cbem_bar_chart(state_to_filter = state_to_chart, age_group_to_filter = "Under 25")
}

cbem_bar_chart_two_age_groups(state_to_chart = "Oregon")
