# Determina el tipo de columna a partir de la conjeturas de readr
get_type <- function(x) {
  col_type_codes <- c("collector_double" = "d",
                      "collector_character" = "c",
                      "collector_logical" = "l",
                      "collector_number" = "c")
  cls <- class(x)[1]
  col_type_codes[cls] |> unname()
}


# Lee fichero con datos
read_puf <- function(file, delim = ",", na = c("", "NA")) {
  read_delim(file, delim = delim, na = na,
             show_col_types = FALSE) |>
    suppressWarnings()
}
