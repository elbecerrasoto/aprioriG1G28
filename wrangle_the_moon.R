#!/usr/bin/Rscript

suppressMessages({
  library(tidyverse)
})

# Globals ----

ENTRIES <- "data/entries.tsv"
RAW <- "results/rawrules.tsv"

# PTTG domain
EXCLUDE <- c("IPR027797")

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

break_arch <- function(arch, pattern = ";") {
  f <- function(iarch) {
    str_split_1(iarch, pattern)
  }
  map(arch, f) |> unlist()
}

# Load Data ----

raw <- read_tsv(RAW, col_types = cols(count = col_integer()))
entries <- read_tsv(ENTRIES, show_col_types = FALSE)

# Main ----

raw <- raw |>
  arrange(desc(count), desc(lift)) |>
  mutate(rID = 1:nrow(raw)) |>
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

lhs_tb <- wide_rules |>
  group_by(rID) |>
  reframe(
    domain = break_arch(lhs),
    side = "lhs"
  )

rhs_tb <- wide_rules |>
  group_by(rID) |>
  reframe(
    domain = break_arch(rhs),
    side = "rhs"
  )

long_rules <- bind_rows(lhs_tb, rhs_tb) |>
  arrange(rID)

long_rules <- left_join(long_rules, wide_rules, join_by(rID)) |>
  select(-lhs, -rhs)

long_rules <- left_join(long_rules, entries, join_by(domain == ENTRY_AC))

long_rules <- long_rules |>
  rename(
    entry = ENTRY_NAME,
    type = ENTRY_TYPE
  ) |>
  relocate(entry, type, .after = side)

exclude_tb <- long_rules |>
  group_by(rID) |>
  summarize(exclude = any(EXCLUDE %in% domain))

long_rules <- long_rules |>
  left_join(exclude_tb, join_by(rID)) |>
  relocate(exclude, .after = rID)

# Output ----

long_rules |>
  write_tsv("results/long_rules.tsv")
