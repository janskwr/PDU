przybliz_e <- function(n) {
  # funkcja monte carlo najpierw runifem losuje 10 obserwacji
  # następnie za pomocą cumsuma sumuje wyrazy w kolejnych podciągach
  # później za pomocą whicha sprawdza które są większe od 1
  # funkcja min natomiast podaje najniższy indeks dla którego warunek zachodzi
  monte_carlo <- function() {
    min(which(cumsum(runif(10)) > 1))
  }
  # za pomocą replicata powtarzamy całą operację zawartą w funkcji monte_carlo n-razy
  # liczymy średnią za pomocą mean()
  mean(replicate(n, monte_carlo()))
}

set.seed(11234)

first_approximation <- przybliz_e(10)
cat(sprintf("Pierwszy pomiar: %s", first_approximation))
cat(sprintf("\nRóżnica: %s\n\n", abs(exp(1) - first_approximation)))

second_approximation <- przybliz_e(1000)
cat(sprintf("Drugi pomiar: %s", second_approximation))
cat(sprintf("\nRóżnica: %s\n\n", abs(exp(1) - second_approximation)))

third_approximation <- przybliz_e(100000)
cat(sprintf("Trzeci pomiar: %s", third_approximation))
cat(sprintf("\nRóżnica: %s\n\n", abs(exp(1) - third_approximation)))

