library(dplyr)
library(readr)
library(glue)
library(stringr)

get_col_types <- function(cycle) {
  fname <- glue("data-raw/col_codes{cycle}.csv")
  tab <- read_csv(fname, col_types = "icc")
  tab$type |> as.list() |> setNames(tab$name)
}

# Lee los datos del primer ciclo
cols1 <- get_col_types(1)
esp1 <- read_csv("data-raw/prgespp1.csv.gz", col_types = cols1)

# Lee los datos del primer ciclo
cols2 <- get_col_types(2)
esp2 <- read_delim("data-raw/prgespp2.csv.gz", col_types = cols2,
                   delim = ";", na = ".")


common_vars <- c("SEQID", "GENDER_R", "AGE_R", "PVLIT1", "PVNUM1")
vars1 <- c("EARNHR", "EDCAT7", "C_Q07_T", "C_D05",
           "C_Q09",
           common_vars)
db1 <- esp1 |>
  select(all_of(vars1)) |>
  mutate(EARNHR = parse_double(EARNHR, na = "V"),
         EDCAT7 = parse_integer(EDCAT7, na = "N"),
         C_Q07_T = parse_integer(C_Q07_T, na = c("N", "M")),
         C_D05 = parse_integer(C_D05, na = "N"),
         C_Q09 = parse_integer(C_Q09, na = c("N", "V", "D", "R")),
  )

vars2 <- c("EARNHRC2", "EDCAT7_TC1", "C2_Q07_T", "C2_D05",
           "C2_Q10",
           common_vars, "PVAPS1")
db2 <- esp2 |>
  select(all_of(vars2)) |>
  mutate(EARNHRC2 = parse_double(EARNHRC2, na = ".v"),
         C2_Q10 = parse_integer(C2_Q10, na = c(".v", ".d", ".r")),
  ) |>
  rename(EARNHR = EARNHRC2,
         EDCAT7 = EDCAT7_TC1,
         C_Q07_T = C2_Q07_T,
         C_D05 = C2_D05,
         C_Q09 = C2_Q10,
         )

