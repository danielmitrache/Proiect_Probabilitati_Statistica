# Rezolvarea exercitiului 10 

#' Simuleaza churn-ul aleator
#' 
#' @param q Probabilitatea ca un utilizator sa paraseasca platforma aleator
#' @return TRUE daca utilizatorul pleaca, FALSE altfel

simuleaza_churn_aleator <- function(q = 0.05) {
  return(runif(1) < q)
}

#' Simuleaza churn-ul conditionat de performanta tehnica
#' Utilizatorul pleaca daca in fereastra de monitorizare apar prea multe erori.
#' 
#' @param m Dimensiunea ferestrei (numarul de cereri recente)
#' @param k Pragul de erori (minim k esecuri declanseaza plecarea)
#' @param p_succes Probabilitatea de succes a unei singure cereri
#' @return TRUE daca utilizatorul pleaca din cauza erorilor, FALSE altfel

simuleaza_churn_conditionat <- function(m = 20, k = 5, p_succes = 0.9) {
  # Numarul de esecuri intr-o secventa de lungime m (dist Binomiala)
  nr_esecuri <- rbinom(1, size = m, prob = 1 - p_succes)
  
  # Verificam daca s-a depasit pragul de k esecuri
  if (nr_esecuri >= k) {
    return(TRUE)
  } 
  else {
    return(FALSE)
  }
}

# EXECUTIE SI ANALIZA

if (sys.nframe() == 0) {
  set.seed(123) 
  
  M <- 100000 # Numarul de utilizatori
  
  #REZOLVARE a)
  
  #Simulamrea vectorului pentru Churn Aleator
  vec_aleator <- replicate(M, simuleaza_churn_aleator(q = 0.05))
  
  #Simularea vectorului pentru Churn Conditionat
  vec_conditionat <- replicate(M, simuleaza_churn_conditionat(m = 20, k = 4, 
                                                              p_succes = 0.75))

  #REZOLVARE b)
  
  #Calculul Churn-ului total
  vec_total <- (vec_aleator | vec_conditionat)
  
  #Estimari
  prob_aleator     <- mean(vec_aleator)
  prob_conditionat <- mean(vec_conditionat)
  prob_total       <- mean(vec_total)
  
  #Afisare Rezultate
  cat("Analiza Churn cu urmatorii parametri:\n")
  cat("m =", 20, ", k =", 5, ", p_succes =", 0.75, "\n")
  
  cat("\nProbabilitati estimate (M =", M, "):\n")
  cat("1. P(Churn Aleator):     ", round(prob_aleator, 4), "\n")
  cat("2. P(Churn Conditionat): ", round(prob_conditionat, 4), "\n")
  cat("3. P(Churn TOTAL):       ", round(prob_total, 4), "\n")
}
