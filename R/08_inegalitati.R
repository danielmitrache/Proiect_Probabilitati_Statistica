# R/08_inegalitati.R 
# Exercitiul 8

#' Simuleaza procesul si returneaza timpul si nr de esecuri
#' 
#' @param n_max Numar maxim de incercari
#' @param p_succes Probabilitate de succes
#' @param medie_S_initial Media timpului initial
#' @param backoff_fix Timp de asteptare (backoff)
#' @param factor_latenta Factor de crestere a latentei
#' @return Lista cu timpul total si numarul de esecuri
simuleaza_avansat <- function(n_max = 3, p_succes = 0.7, medie_S_initial = 150, 
                              backoff_fix = 50, factor_latenta = 1.0) {
  
  timp_total <- 0
  medie_curenta <- medie_S_initial
  nr_esecuri <- 0
  
  for (i in 1:n_max) {
    # Generam timpul curent
    s_i <- rexp(1, rate = 1/medie_curenta)
    timp_total <- timp_total + s_i
    
    # Verificam daca avem succes
    if (runif(1) < p_succes) {
      break # Am terminat
    } else {
      # Caz de esec
      nr_esecuri <- nr_esecuri + 1
      
      # Daca mai avem incercari, adaugam backoff si penalizare
      if (i < n_max) {
        timp_total <- timp_total + backoff_fix
        medie_curenta <- medie_curenta * factor_latenta
      }
    }
  }
  
  return(list(timp = timp_total, esecuri = nr_esecuri))
}

# Calculam valorile teoretice pentru a verifica simularea
calculeaza_teoretic_T <- function(n_max, p, mu, backoff, factor) {
  
  # Probabilitatea sa ne oprim la pasul k
  probs <- numeric(n_max)
  for(k in 1:(n_max-1)) {
    probs[k] <- (1-p)^(k-1) * p
  }
  probs[n_max] <- (1-p)^(n_max-1)
  
  # Calculam media si varianta conditionata de pasul k
  E_T_cond <- numeric(n_max)
  E_T2_cond <- numeric(n_max) 
  
  for(k in 1:n_max) {
    # Factorii de penalizare pentru cei k pasi
    if(factor == 1) {
      f_pows <- rep(1, k)
    } else {
      f_pows <- factor^(0:(k-1))
    }
    
    # Media: suma mediilor individuale + backoff-urile
    sum_mu <- mu * sum(f_pows)
    mean_val <- sum_mu + (k-1) * backoff
    
    E_T_cond[k] <- mean_val
    
    # Varianta sumei este suma variantelor (pentru exp, var = medie^2)
    sum_var <- (mu^2) * sum(f_pows^2)
    
    # E[X^2] = Var(X) + E[X]^2
    E_T2_cond[k] <- sum_var + mean_val^2
  }
  
  # Total expectations
  E_T_total <- sum(probs * E_T_cond)
  E_T2_total <- sum(probs * E_T2_cond)
  Var_T_total <- E_T2_total - E_T_total^2
  
  return(list(media = E_T_total, varianta = Var_T_total))
}

# EXECUTIE
if (sys.nframe() == 0) {
  set.seed(42)
  M <- 10000 
  
  # Parametri simulare
  n_max <- 5 
  p <- 0.4
  mu <- 100
  bf <- 20
  fact <- 1.5 
  
  # 1. Rulam simularea de M ori
  rezultate <- replicate(M, simuleaza_avansat(n_max, p, mu, bf, fact), simplify = FALSE)
  
  # Extragem vectorii de interes
  T_vals <- sapply(rezultate, function(x) x$timp)
  Esc_vals <- sapply(rezultate, function(x) x$esecuri)
  
  # 2. Valori teoretice
  teoretic <- calculeaza_teoretic_T(n_max, p, mu, bf, fact)
  media_teoretica <- teoretic$media
  var_teoretica <- teoretic$varianta
  
  cat("=== Rezultate M =", M, "===\n")
  cat("Media Empirica:", mean(T_vals), "vs Teoretica:", media_teoretica, "\n")
  cat("Varianta Empirica:", var(T_vals), "vs Teoretica:", var_teoretica, "\n\n")
  
  # a) Verificare Markov si Cebisev
  
  # Markov: P(T >= a) <= E[T] / a
  alpha <- 2.0
  a_markov <- alpha * media_teoretica
  
  prob_markov <- mean(T_vals >= a_markov)
  limita_markov <- 1/alpha
  
  cat("a) Markov (factor 2):\n")
  cat("   P(T >= 2*E[T]) =", prob_markov, "<=", limita_markov, "\n")
  
  # Cebisev: P(|T - E| >= k*sd) <= 1/k^2
  k <- 2 
  dist <- k * sqrt(var_teoretica)
  
  prob_ceb <- mean(abs(T_vals - media_teoretica) >= dist)
  limita_ceb <- 1/(k^2)
  
  cat("a) Cebisev (2 deviatii):\n")
  cat("   P(|dist| >= 2sigma) =", prob_ceb, "<=", limita_ceb, "\n\n")
  
  # b) Chernoff pentru Esecuri
  
  x0 <- 3 # Verificam prob >= 3 esecuri
  if (x0 > n_max) x0 <- n_max
  
  # Estimam MGF empiric
  t_val <- 0.5
  mgf <- mean(exp(t_val * Esc_vals))
  
  prob_esec <- mean(Esc_vals >= x0)
  bound <- mgf / exp(t_val * x0)
  
  cat("b) Chernoff (Esecuri >= ", x0, "):\n")
  cat("   Probabilitate reala:", prob_esec, "\n")
  cat("   Limita Chernoff:", bound, "\n\n")
  
  # c) Interpretare utilitate
  cat("c) Utilitate:\n")
  cat("   Aceste limite ne ajuta sa punem bariere 'worst-case' cand nu stim distributia exacta.\n")
  cat("   Sunt utile pentru garantii de siguranta (SLA).\n\n")
  
  # d) Inegalitatea lui Jensen
  # Verificam: phi(E[T]) <= E[phi(T)] pentru convex
  
  phi <- function(x) x^2
  lhs <- phi(mean(T_vals))
  rhs <- mean(phi(T_vals))
  
  cat("d) Jensen (x^2):\n")
  cat("   phi(E[T]) =", lhs, "<= E[phi(T)] =", rhs, "\n")
  
  # e) Contextul riscului
  cat("\n e) Interpretare Risc:\n")
  cat("   Daca functia de cost (phi) este convexa, variatia creste costul mediu.\n")
  cat("   Calculul costului bazat doar pe timpul mediu subestimeaza riscul total.\n")
}
