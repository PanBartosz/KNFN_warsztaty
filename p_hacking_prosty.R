set.seed(2025)

hack_the_p <- function(n0 = 40, # początkowy N na grupę
                       max_extra = 40, # ile osób max można dobrać
                       trim_sd = 2) { # próg przycinania outlierów
  
  # Generujemy brak efektu
  x <- rnorm(n0)
  y <- rnorm(n0)
  
  # 1) Klasyczny, dwustronny test t
  p <- t.test(x, y, var.equal = TRUE)$p.value
  if (p < .05) return(p)
  
  # 2) Jednostronny test w "korzystniejszym" kierunku
  p_one <- min(t.test(x, y, alternative = "less")$p.value,
               t.test(x, y, alternative = "greater")$p.value)
  if (p_one < .05) return(p_one)
  
  # 2) Usuwamy outliery (> trim_sd SD) i znów przeprowadzamy dwustronny test, licząc, że będzie bardziej korzystny
  x_trim <- x[abs(scale(x)) < trim_sd]
  y_trim <- y[abs(scale(y)) < trim_sd]
  p_trim <- t.test(x_trim, y_trim, var.equal = TRUE)$p.value
  if (p_trim < .05) return(p_trim)
  
  # 3) Test nieparametryczny (Wilcoxon), licząć, że on "lepiej wyjdzie"
  p_rank <- wilcox.test(x, y)$p.value
  if (p_rank < .05) return(p_rank)
  
  # 4) Sekwencyjne dobieranie próby ("peeking")
  add_each <- 2                      # dobieramy po dwie osoby na grupę
  reps <- max_extra / add_each
  for (i in seq_len(reps)) {
    x <- c(x, rnorm(add_each))
    y <- c(y, rnorm(add_each))
    p_seq <- t.test(x, y, var.equal = TRUE)$p.value # przeprowadzamy test ponownie
    if (p_seq < .05) return(p_seq)
  }
  
  # jeśli nigdy p < .05, zwracamy najmniejsze p z tych uzyskanych (najbardziej "sexy" wynik)
  return(min(p, p_one, p_rank, p_seq))
}

B <- 5000
p_vals <- replicate(B, hack_the_p())

hist(p_vals, 
     breaks = 40, 
     main = "Rozkład końcowych p-values\n(p-hacking)",
     xlab = "p-value",
     col = "steelblue",
     border = NA)
abline(v = .05, lty = 2, lwd = 2)

hist(p_vals[p_vals < 0.05],
     breaks = 10,
     main = "Rozkład końcowych p-values\n(p-hacking)",
     xlab = "p-value", 
     col = "steelblue", 
     border = NA)