#!/usr/bin/Rscript

suppressMessages({
  library(tidyverse)
  library(arules)
})

# Globals ----

HITS <- "data/regions.tsv"

CONFIDENCE <- 0.64 # RHS frequency, given LHS
SUPPORT <- 0.01 # LHS frequency

OUT_TSV <- "results/rawrules.tsv" # RHS frequency, given LHS

# Helpers ----

split_domains <- function(x, pattern = "\\|") {
  map(x, \(i) if (is.na(i)) NA else str_split_1(i, pattern = pattern)) |>
    unlist()
}

# Load Data ----

hits <- read_tsv(HITS)

# Apriori ----

transactions <- hits |>
  select(neID, ARCH)

transactions <- transactions |>
  group_by(neID) |>
  reframe(domain = unique(split_domains(ARCH))) |>
  mutate(presence = TRUE) |>
  pivot_wider(
    names_from = domain,
    values_from = presence,
    values_fill = FALSE,
    names_sort = TRUE
  )

rownames_transactions <- transactions$neID

transactions <- transactions |>
  select(-neID)

transactions <- as(transactions, "transactions")
rules <- apriori(transactions, parameter = list(support = SUPPORT, confidence = CONFIDENCE))

rules_df <- as(rules, "data.frame")
rules_tb <- as_tibble(rules_df)

# Output ----

rules_tb |>
  write_tsv(OUT_TSV)
