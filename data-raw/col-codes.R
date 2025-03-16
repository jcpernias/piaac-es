library(dplyr)
library(readr)
library(purrr)

# Códigos de los tipos de columna
#
# En algunas variables del segundo ciclo, se registra una secuencia de números.
# Esas variables se deben leer como caracteres.
col_type_codes <- c("collector_double" = "d",
                    "collector_character" = "c",
                    "collector_logical" = "l",
                    "collector_number" = "c")

get_type <- function(x) {
  cls <- class(x)[1]
  col_type_codes[cls]
}


# ------------------------------------------------------------
# Lee los datos del primer ciclo
# ------------------------------------------------------------
esp1 <- read_csv("data-raw/prgespp1.csv.gz",
                 show_col_types = FALSE) |>
  suppressWarnings()

# Tabla con nombres y tipos de variables
cols1 <- spec(esp1)[[1]]
vars1 <- tibble(col = 1:length(cols1),
                name = names(cols1),
                type = map(cols1, get_type))

# Problemas en la lectura de datos
probl1 <- problems(esp1) |>
  select(-file)

# Guardamos códigos y problemas
write_csv(vars1, "data-raw/col_codes1.csv")
write_csv(probl1, "data-raw/probl1.csv")

# ------------------------------------------------------------
# Lee los datos del segundo ciclo
# ------------------------------------------------------------
esp2 <- read_delim("data-raw/prgespp2.csv.gz",
                   delim = ";", na = ".",
                   show_col_types = FALSE) |>
  suppressWarnings()

# Tabla con nombres y tipos de variables
cols2 <- spec(esp2)[[1]]

vars2 <- tibble(col = 1:length(cols2),
                name = names(cols2),
                type = map(cols2, get_type))
probl2 <- problems(esp2) |>
  filter(!between(col, 2106, 2108)) |>
  select(-file)

# Guardamos códigos y problemas
write_csv(vars2, "data-raw/col_codes2.csv")
write_csv(probl2, "data-raw/probl2.csv")


