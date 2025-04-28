library(tidyverse)
library(arules)

HITS <- "ARCH.tsv"
hits <- read_tsv(HITS)


# the transaction format

split_domains <- function(x, pattern = "\\|") {
  map(x, \(i) if (is.na(i)) NA else str_split_1(i, pattern = pattern)) |>
    unlist()
}

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
rules <- apriori(transactions, parameter = list(support = 0.12, confidence = 0.72))
