#  Recreating this: https://show.rfor.us/cditiy

library(tidyverse)
library(ggchicklet)
library(patchwork)
library(scales)

cbem <-
  read_csv("https://rin3spring2026.rfortherestofus.com/data-raw/cbem.csv")
