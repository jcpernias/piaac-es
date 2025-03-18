# Lee fichero con datos
read_puf <- function(file, delim = ",", na = c("", "NA")) {
  read_delim(file, delim = delim, na = na,
             show_col_types = FALSE) |>
    suppressWarnings()
}
