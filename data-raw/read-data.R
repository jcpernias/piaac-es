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
           "C_Q09", "D_Q03", "D_Q04", "D_Q05a1", "D_Q09",
           "D_Q12a", "D_Q12c", "J_Q01", "J_Q02a",
           "J_Q02c", "J_Q03a", "J_Q03b","J_Q04a",
           "J_Q04c1", "J_Q04c2", "J_Q06a", "J_Q06b",
           "J_Q07a", "J_Q07b", "J_Q08",
           common_vars)
db1 <- esp1 |>
  select(all_of(vars1)) |>
  mutate(EARNHR = parse_double(EARNHR, na = "V"),
         EDCAT7 = parse_integer(EDCAT7, na = "N"),
         C_Q07_T = parse_integer(C_Q07_T, na = c("N", "M")),
         C_D05 = parse_integer(C_D05, na = "N"),
         C_Q09 = parse_integer(C_Q09, na = c("N", "V", "D", "R")),
         D_Q03 = parse_integer(D_Q03, na = c("V", "R", "D", "N")),
         D_Q04 = parse_integer(D_Q04, na = c("V", "N", "R", "D")),
         D_Q05a1 = parse_integer(D_Q05a1, na = c("V", "N", "R", "D")),
         D_Q09 = parse_integer(D_Q09, na = c("V", "N", "R", "D")),
         D2_Q10_C = if_else(D_Q09 >= 3, 3, D_Q09),
         D_Q12a = parse_integer(D_Q12a, na = c("V", "N", "R", "D")),
         D_Q12c = parse_integer(D_Q12c, na = c("V", "N", "R", "D")),
         J_Q01 = parse_integer(J_Q01, na = c("N", "R", "D")),
         J_Q02a = parse_integer(J_Q02a, na = c("V", "N", "R")),
         J_Q02c = parse_integer(J_Q02c, na = c("V", "N", "R", "D")),
         J_Q03a = parse_integer(J_Q03a, na = c("N", "R")),
         J_Q03b = parse_integer(J_Q03b, na = c("V", "N")),
         J_Q04a = parse_integer(J_Q04a, na = c("N", "R")),
         J_Q04c1 = parse_integer(J_Q04c1, na = c("V", "N")),
         J_Q04c2 = parse_integer(J_Q04c2, na = c("V", "N")),
         J_Q06a = parse_integer(J_Q06b, na = c("D", "N", "R")),
         J_Q06b = parse_integer(J_Q06b, na = c("D", "N", "R")),
         J_Q07a = parse_integer(J_Q07a, na = c("N", "D", "R")),
         J_Q07b = parse_integer(J_Q07b, na = c("N", "D", "R")),
         J_Q08 = parse_integer(J_Q08, na = c("N", "D", "R")),
  ) |>
  select(-D_Q09)

vars2 <- c("EARNHRC2", "EDCAT7_TC1", "C2_Q07_T", "C2_D05",
           "C2_Q10", "D2_Q03", "D2_Q04", "D2_Q05a1", "D2_Q10_C",
           "D2_Q12a_TC1", "D2_Q12d", "J2_Q01", "J2_Q02a",
           "J2_Q02b", "J2_Q03a", "J2_Q03b", "A2_Q03a",
           "A2_Q03c1", "A2_Q03c2", "A2_Q03d", "J2_Q04c",
           "A2_Q03e", "J2_Q05c", "J2_Q06",
           common_vars, "PVAPS1")
db2 <- esp2 |>
  select(all_of(vars2)) |>
  mutate(EARNHRC2 = parse_double(EARNHRC2, na = ".v"),
         C2_Q10 = parse_integer(C2_Q10, na = c(".v", ".d", ".r")),
         D2_Q03 = parse_integer(D2_Q03, na = c(".v", ".r", ".d")),
         D2_Q04 = parse_integer(D2_Q04, na = c(".v", ".d", ".r")),
         D2_Q05a1 = parse_integer(D2_Q05a1, na = c(".v", ".r", ".d")),
         D2_Q10_C = parse_integer(D2_Q10_C, na = c(".v", ".r", ".d")),
         D2_Q12a_TC1 = parse_integer(D2_Q12a_TC1, na = c(".v", ".r", ".d")),
         D2_Q12d = parse_integer(D2_Q12d, na = c(".v", ".r", ".d")),
         J2_Q02a = parse_integer(J2_Q02a, na = c(".v", ".r", ".d")),
         J2_Q02b = parse_integer(J2_Q02b, na = c(".v", ".r", ".d")),
         J2_Q03b = parse_integer(J2_Q03b, na = c(".v", ".r")),
         A2_Q03c1 = parse_integer(A2_Q03c1, na = c(".v", ".r")),
         A2_Q03c2 = parse_integer(A2_Q03c2, na = c(".v", ".r")),
         J2_Q04c = parse_integer(J2_Q04c, na = c(".d", ".r")),
         A2_Q03e = parse_integer(A2_Q03e, na = c(".d")),
         J2_Q05c = parse_integer(J2_Q05c, na = c(".d", ".r")),
         J2_Q06 = parse_integer(J2_Q06, na = c(".d", ".r")),
  ) |>
  rename(EARNHR = EARNHRC2,
         EDCAT7 = EDCAT7_TC1,
         C_Q07_T = C2_Q07_T,
         C_D05 = C2_D05,
         C_Q09 = C2_Q10,
         D_Q03 = D2_Q03,
         D_Q04 = D2_Q04,
         D_Q05a1 = D2_Q05a1,
         D_Q12c = D2_Q12d,
         J_Q01 = J2_Q01,
         J_Q02a = J2_Q02a,
         J_Q02c = J2_Q02b,
         J_Q03a = J2_Q03a,
         J_Q03b = J2_Q03b,
         J_Q04a = A2_Q03a,
         J_Q04c1 = A2_Q03c1,
         J_Q04c2 = A2_Q03c2,
         J_Q06a = A2_Q03d,
         J_Q06b = J2_Q04c,
         J_Q07a = A2_Q03e,
         J_Q07b = J2_Q05c,
         J_Q08 = J2_Q06,
  )

