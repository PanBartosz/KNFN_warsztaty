#install.packages("dplyr")
#install.packages("devtools")
#install.packages("metafor")
#install.packages("devtools")
#devtools::install_github("MathiasHarrer/dmetar")

library(dmetar)   # pcurve()
library(metafor)  # escalc()
library(dplyr)    # %>% i filtrowanie

bias_the_publications <- function(n0 = 40, # bazowa liczebność próby
                                  effsize = 0.5, # wielkość efektu w odchyleniach standardowych
                                  var_effsize = 0.5, # wariancja wielkości efektu między badaniami
                                  var_n = 200) { # wariancja liczebności próby między badaniami
  # Helper: policz wielkość efektu i jego wariancje dla dwóch prób
  calc_es <- function(a, b) {
    escalc(
      measure = "SMD",
      m1i = mean(a),
      sd1i = sd(a),
      n1i = length(a),
      m2i = mean(b),
      sd2i = sd(b),
      n2i = length(b)
    )
  }
  # Ustalamy liczebność próby dla badania (2*bazowa + wariancja)
  n <- n0 + round(rnorm(1, n0, sqrt(var_n))) 
  # Losujemy wielkość efektu dla danego badania
  esize <- rnorm(1, effsize, sqrt(var_effsize))
  # Losujemy pierwszą próbę
  x <- rnorm(n)
  # Losujemy drugą próbę
  y <- rnorm(n, esize)
  # Obliczamy wielkość efektu
  es <- calc_es(y, x)
  # Przeprowadzamy test i obliczamy p-value
  p  <- t.test(x, y, var.equal = TRUE)$p.value
  if (p < .05)
    # Jeśli p_value jest pod progiem, to badanie jest opublikowane.
    return(c(p = p, yi = es$yi, vi = es$vi))
}


set.seed(2025)
B <- 300 # powtórzeń eksperymentu
results <- replicate(B,
                     bias_the_publications(effsize = 0.25,
                                           var_effsize = 0.1,
                                           n0 = 50, # 500, 50, 20
                                           var_n = 200),
                     simplify = FALSE) %>%
  bind_rows(.id = "study_id") %>%
  mutate(studlab = study_id,
         TE = yi,
         seTE = sqrt(vi))

# Metaanaliza
res.meta <- rma(yi, vi, data = results)
summary(res.meta)

# Forest plot
forest(res.meta)

# Funnel plot pozwala nam ocenić wielkość *publication bias*
funnel(res.meta)

# Metoda trim & fill pozwala nam włączyć do analizy "wirtualne" badania,
# które mogły zosta,c pominięte przez *publication bias*
trimfill(res.meta)
funnel(trimfill(res.meta))


# Ta sama symulacja tylko dla większej liczby eksperymentów (dla niedowiarków)
set.seed(2025)
B <- 3000
results <- replicate(B,
                     bias_the_publications(effsize = 0.25, var_effsize = 0.1),
                     simplify = FALSE) %>%
  bind_rows(.id = "study_id") %>%
  mutate(studlab = study_id,
         TE = yi,
         seTE = sqrt(vi))
res.meta <- rma(yi, vi, data = results)
summary(res.meta)

