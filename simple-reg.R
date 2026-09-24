library(tidyverse)
library(sandwich)
library(lmtest)
library(car)
library(writexl)

dummy <- function(x) {
  as.integer(x)
}

piaac <- read_csv("data/piaac-es.csv") |>
  mutate(y2023 = dummy(cycle == 2),
         female = dummy(GENDER_R == 2),
         ed_prim = dummy(EDCAT7 == 1),
         ed_sec = dummy(EDCAT7 %in% c(2, 3, 4)),
         ed_sup = dummy(EDCAT7 >= 5),
         exper = C_Q09,
         exper_sq = exper^2,
         wage = EARNHRBONUS,
         lwage = log(wage),
         private = dummy(D_Q03 == 1)) |>
  filter_out(is.na(wage))


ols_2012 <- piaac |>
  filter(y2023 == 0) |>
  lm(lwage ~ female + ed_sec + ed_sup + exper, data = _)

coeftest(ols_2012, vcov. = vcovHC, type = "HC1")

ols_2023 <- piaac |>
  filter(y2023 == 1) |>
  lm(lwage ~ female + ed_sec + ed_sup + exper, data = _)

coeftest(ols_2023, vcov. = vcovHC, type = "HC1")

ols_pool <- piaac |>
  lm(lwage ~ y2023 + female + ed_sec + ed_sup + exper, data = _)

coeftest(ols_pool, vcov. = vcovHC, type = "HC1")

ols_chow <- piaac |>
  lm(lwage ~ y2023 * (female + ed_sec + ed_sup + exper), data = _)

coeftest(ols_chow, vcov. = vcovHC, type = "HC1")

lht(ols_chow, matchCoefs(ols_chow, ":"), vcov. = vcovHC, type = "HC1")


piaac |>
  select(y2023, wage, female, ed_prim, ed_sec, ed_sup, exper) |>
  write_xlsx("piaac-es.xlsx")
