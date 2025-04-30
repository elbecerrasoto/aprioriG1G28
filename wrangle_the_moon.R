#!/usr/bin/Rscript

suppressMessages({
  library(tidyverse)
})

# Globals ----

RAW <- "results/rawrules.tsv"

# Helpers ----

get_sides <- function(rules) {
  n <- length(rules)
  lhs <- vector(mode = "character", length = n)
  rhs <- vector(mode = "character", length = n)

  out <- list(lhs = lhs, rhs = rhs)

  for (i in seq_along(rules)) {
    irule <- str_split_1(rules[[i]], pattern = "=>") |>
      str_replace_all(",", ";") |>
      str_remove_all("[{}\\s]") |>
      as.list() |>
      set_names("lhs", "rhs")

    out$lhs[[i]] <- irule$lhs
    out$rhs[[i]] <- irule$rhs
  }
  out
}


arch_length <- function(arch, pattern = ";") {
  f <- function(iarch) {
    ifelse(iarch == "", 0L,
      length(str_split_1(iarch, pattern))
    )
  }

  map_int(arch, f)
}

# Load Data ----

raw <- read_tsv(RAW, col_types = cols(count = col_integer()))

# Main ----

raw <- raw |>
  arrange(desc(count), desc(lift)) |>
  mutate(rID = str_c("R", 1:nrow(raw))) |>
  relocate(rID)


wide_rules <- raw |>
  mutate(
    lhs = get_sides(rules)$lhs,
    rhs = get_sides(rules)$rhs,
    lhslen = arch_length(lhs),
    rhslen = arch_length(rhs),
    rulen = lhslen + rhslen
  ) |>
  relocate(rID, lhs, rhs, rulen, lhslen, rhslen) |>
  select(-rules)



# Output ----
