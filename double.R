#!/usr/bin/Rscript

suppressMessages({
  library(tidyverse)
})

# Globals ----

ASSOCIATIONS <- "results/long_rules.tsv"


# Helpers ----

# Load Data ----

associations <- read_tsv(ASSOCIATIONS, col_types = cols(rID = "i", count = "i"))

# Main ----

associations <- associations |>
  filter(!exclude)

upsidedown <- vector(mode = "integer", length = nrow(associations))

CURR <- 1

associations |>
  group_by(rID)

for (seq_len(nrow(associations))) {
  
}

# Output ----

long_rules |>
  write_tsv("results/long_rules.tsv")
