library(dplyr)
library(stringr)
library(openxlsx2)

# Archivos con la descripción de las variables
cb_cycle1 <- "data-raw/International Codebook_PIAAC Public-use File (PUF) Variables and Values_Feb2023.xlsx"
cb_cycle2 <- "data-raw/piaac-cy2-international-codebook.xlsx"

# Dominios de variables a ignorar
ignore_domains <-
  c("Literacy", "Numeracy", "Problem", "Reading", "Adaptive", "Observation") |>
  paste(collapse = "|")

# Lee las descripciones de las variables
cycle1 <- read_xlsx(cb_cycle1, skip_empty_cols = TRUE) |>
  filter(str_starts(Domain, ignore_domains, negate = TRUE)) |>
  filter(str_starts(Name, "SPFWT|PV", negate = TRUE))
cycle2 <- read_xlsx(cb_cycle2) |>
  filter(str_starts(Domain, ignore_domains, negate = TRUE)) |>
  filter(str_starts(Variable, "SPFWT|PV", negate = TRUE))

common_names <- intersect(cycle1$Name, cycle2$Variable)

comon1 <- cycle1 |> filter(Name %in% common_names)
comon2 <- cycle2 |> filter(Variable %in% common_names)


ed1 <- cycle1 |>
  filter(str_detect(Label, "ducation"))

ed2 <- cycle2 |>
  filter(str_detect(Label, "ducation"))



cycle1_domains <- unique(cycle1$Domain) |> sort()
cycle2_domains <- unique(cycle2$Domain) |> sort()
common_domains <- intersect(cycle1_domains, cycle2_domains) |> sort()
