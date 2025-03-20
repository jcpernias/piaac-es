# Lee fichero con datos
read_puf <- function(file, delim = ",", na = c("", "NA")) {
  read_delim(file, delim = delim, na = na,
             show_col_types = FALSE) |>
    suppressWarnings()
}

# Cambia el tipo de columna
parse_col <- function(x, type, na = "") {
  fns <- list("d" = parse_double, "i" = parse_integer)
  na <- str_split(na, fixed("|"))[[1]]
  fns[[type]](x, na = na)
}

# Utiliza el separador dado par divide cadenas
split_sep <- function(str, sep) {
  str_split(str, fixed(sep))
}

# Cambia la cadena "NA" por ""
recode_na <- function(x) {
  switch(x, "NA" = "", x)
}

# Recodifica valores de una columna
recode_col <- function(col, cases) {
  # Separa cada caso
  split_sep(cases, "/")[[1]] |>
    # Separa los valores iniciales y el valor final
    split_sep("=") |>
    map(\(x) list(from = split_sep(x[[1]], "|")[[1]],
                  to = recode_na(x[[2]]))) |>
    # Recodifica los valores
    walk(\(x) col[col %in% as.numeric(x$from)] <<- as.numeric(x$to))

  # Devuelve la columna modificada
  col
}

# Selecciona las variables de un ciclo, cambia el
# tipo, recodifica valores y cambia los nombres
# de algunas columnas
select_cycle_vars <- function(cycle, db_cycle, vars) {
  # Selecciona las variables del primer ciclo
  vars_cycle <- vars |>
    filter(str_detect(.data$cycle,
                      fixed(as.character(.env$cycle))))

  db <- db_cycle |>
    select(all_of(vars_cycle$name))

  # Columnas a las que hay que cambiar el tipo
  vars_cycle |>
    select(name, type, na) |>
    filter(!is.na(type)) |>
    mutate(na = coalesce(na, "")) |>
    pwalk(\(name, type, na)
      db[[name]] <<- parse_col(db[[name]], type, na)
    )

  # Recodifica valores
  if (any(!is.na(vars_cycle$recode))) {
    vars_cycle |>
      filter(!is.na(recode)) |>
      select(name, recode) |>
      pwalk(\(name, recode)
        db[[name]] <<- recode_col(db[[name]], recode)
      )
  }


  # Cambia los nombres de las columnas
  if (any(!is.na(vars_cycle$rename))) {
    names(db) <- coalesce(vars_cycle$rename, vars_cycle$name)
  }

  db
}

make_db <- function(c1, c2, vars_file) {
  # Lee las variables a seleccionar en los dos ciclos
  vars <- read_csv(vars_file, col_types = "c")

  # Lee los datos de los dos ciclos. Añade una columna adicional
  # identificando el ciclo
  db1 <- select_cycle_vars(1, c1, vars) |>
    mutate(cycle = 1)
  db2 <- select_cycle_vars(2, c2, vars) |>
    mutate(cycle = 2)

  # Une los datos de los dos ciclos
  bind_rows(db1, db2)
}

store_db <- function(db) {
  if (!dir.exists("data")) {
    dir.create("data")
  }
  path <- file.path("data", "piaac-es.csv")
  write_csv(db, path, na = "")
  path
}
