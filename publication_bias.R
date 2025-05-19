#install.packages("dplyr")
#install.packages("devtools")
#install.packages("metafor")
#devtools::install_github("MathiasHarrer/dmetar")

library(dmetar)   # pcurve()
library(metafor)  # escalc()
library(dplyr)    # %>% i filtrowanie

# Funkcja symulująca jedno “badanie”
hack_the_study_curve <- function(n0 = 40, max_extra = 40, trim_sd = 2) {
  # Helper: policz g i vi dla dwóch prób
  calc_es <- function(a, b) {
    escalc(measure = "SMD",
           m1i = mean(a), sd1i = sd(a), n1i = length(a),
           m2i = mean(b), sd2i = sd(b), n2i = length(b))
  }
  
  x <- rnorm(n0);
  y <- rnorm(n0)
  es <- calc_es(x, y)
  p  <- t.test(x, y, var.equal = TRUE)$p.value
  if (p < .05)
    return(c(p = p, yi = es$yi, vi = es$vi))
  
  ## 1) Trimming outlierów i ponowny t
  x_trim <- x[abs(scale(x)) < trim_sd]
  y_trim <- y[abs(scale(y)) < trim_sd]
  if (length(x_trim) > 5 && length(y_trim) > 5) {
    p_trim <- t.test(x_trim, y_trim, var.equal = TRUE)$p.value
    es_trim <- calc_es(x_trim, y_trim)
    if (p_trim < .05)
      return(c(p = p_trim, yi = es_trim$yi, vi = es_trim$vi))
  }
  
  ## 2) Sekwencyjne dobieranie próby
  add_each <- 2
  reps <- max_extra / add_each
  for (i in seq_len(reps)) {
    x <- c(x, rnorm(add_each))
    y <- c(y, rnorm(add_each))
    p_seq <- t.test(x, y, var.equal = TRUE)$p.value
    if (p_seq < .05) {
      es_seq <- calc_es(x, y)
      return(c(p = p_seq, yi = es_seq$yi, vi = es_seq$vi))
    }
  }
  
  ## Brak istotności: zwróć ostatnie statystyki
  es_final <- calc_es(x, y)
  c(p = p_seq, yi = es_final$yi, vi = es_final$vi)
}

set.seed(2025)
B <- 5000
results <- replicate(B, hack_the_study_curve(), simplify = FALSE) %>%
  bind_rows(.id = "study_id") %>%
  mutate(studlab = "studlab", TE = yi, seTE = sqrt(vi))

pcurve(results)