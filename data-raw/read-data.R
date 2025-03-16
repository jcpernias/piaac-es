library(dplyr)
library(readr)
library(glue)
library(purrr)

get_col_types <- function(cycle) {
  fname <- glue("data-raw/col_codes{cycle}.csv")
  tab <- read_csv(fname, col_types = "icc")
  tab$type |> as.list() |> setNames(tab$name)
}

# Lee los datos del primer ciclo
cols1 <- get_col_types(1)
esp1 <- read_csv("data-raw/prgespp1.csv.gz", col_types = cols1)

# Lee los datos del primer ciclo
cols1 <- get_col_types(2)
esp2 <- read_delim("data-raw/prgespp2.csv.gz", col_types = cols2,
                   delim = ";", na = ".")

