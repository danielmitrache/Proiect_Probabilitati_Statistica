# -- Rezolvarea Exercitiului 3 --

#' Simuleaza o singura cerere care poate avea mai multe retry-uri
#' 
#' @param n_max Numarul maxim de incercari permise (retry-uri + prima incercare)
#' @param p_succes Probabilitatea de succes a unei singure incercari (U_i)
#' @param t_0 Pragul de timp pentru respectarea SLA (Service Level Agreement)
#' @param medie_S Timpul mediu de procesare pentru o singura incercare (S_i)
#' @param backoff_fix Timpul de asteptare fix adaugat intre doua reincercari (B_i)
#' 
#' @return O lista continand:
#' \itemize{
#'   \item I: Indicator de succes final (1 daca a reusit, 0 altfel)
#'   \item T: Timpul total pana la succes sau abandon (suma S_i si B_i)
#'   \item N: Numarul total de incercari efectuate
#'   \item D: Indicator logic (TRUE daca a existat cel putin un esec pe parcurs)
#' }
simuleaza_o_cerere <- function(n_max = 3, p_succes = 0.8, t_0 = 500, medie_S = 150, backoff_fix = 50) {
  timp_total <- 0
  succes_final <- 0
  nr_incercari <- 0
  a_esuat_vreodata <- FALSE
  
  for (i in 1:n_max) {
    nr_incercari <- i
    
    # 1. Generam timpul de raspuns S_i folosind modelul exponential
    # S_i reprezinta timpul cat dureaza procesarea cererii la incercarea i
    s_i <- rexp(1, rate = 1/medie_S)
    timp_total <- timp_total + s_i
    
    # 2. Verificam daca incercarea i are succes (U_i)
    # Folosim runif(1) pentru a simula o variabila de tip Bernoulli
    if (runif(1) < p_succes) {
      succes_final <- 1
      break # Am reusit, oprim procesul de retry
    } else {
      a_esuat_vreodata <- TRUE
      # Daca nu este ultima incercare admisa, adaugam timpul de backoff (B_i)
      if (i < n_max) {
        timp_total <- timp_total + backoff_fix
      }
    }
  }
  
  return(list(I = succes_final, T = timp_total, N = nr_incercari, D = a_esuat_vreodata))
}

# --- Executie si Analiza Probabilistica ---

if (sys.nframe() == 0) {
  set.seed(123) # Pentru reproductibilitatea simularilor
  M <- 10000    # Numarul de cereri totale simulate pentru estimare
  n0_prag <- 2  # Pragul pentru numarul de incercari (Evenimentul C)
  t0_prag <- 400 # Pragul de timp pentru SLA (Evenimentul B)
  
  # Generam esantionul de date prin replicarea functiei de simulare
  rezultate <- replicate(M, simuleaza_o_cerere(n_max = 3, p_succes = 0.7, t_0 = t0_prag))
  df_cereri <- as.data.frame(t(rezultate))
  
  # Convertim listele rezultate in vectori numerici/logici pentru calcule statistice
  df_cereri[] <- lapply(df_cereri, unlist)
  
  # -- Cerinta 3a: Estimarea empirica a probabilitatilor --
  
  # P(A): Probabilitatea de succes final
  prob_A <- mean(df_cereri$I == 1)
  
  # P(B): Probabilitatea de a respecta SLA (timp total sub prag)
  prob_B <- mean(df_cereri$T <= t0_prag)
  
  # P(C): Probabilitatea de a rezolva cererea din maxim n0 incercari
  prob_C <- mean(df_cereri$N <= n0_prag)
  
  # P(A intersect B): Succes final SI incadrare in timp (Performanta optima)
  prob_A_intersect_B <- mean(df_cereri$I == 1 & df_cereri$T <= t0_prag)
  
  # P(A union D): Succes final SAU existenta a cel putin unui esec initial
  prob_A_union_D <- mean(df_cereri$I == 1 | df_cereri$D == TRUE)
  
  # -- Cerinta 3b: Verificarea numerica a formulelor --
  
  prob_D <- mean(df_cereri$D == TRUE)
  # Calculam intersectia pentru formula: P(A union D) = P(A) + P(D) - P(A intersect D)
  prob_A_intersect_D <- mean(df_cereri$I == 1 & df_cereri$D == TRUE)
  formula_reuniune <- prob_A + prob_D - prob_A_intersect_D
  
  # Afisare rezultate in consola
  cat("--- Estimari Empirice (M =", M, "simulari) ---\n")
  cat("P(A) [Succes]:             ", prob_A, "\n")
  cat("P(B) [SLA]:                ", prob_B, "\n")
  cat("P(C) [N <=", n0_prag, "]:          ", prob_C, "\n")
  cat("P(A intersect B):          ", prob_A_intersect_B, "\n")
  cat("P(A union D):              ", prob_A_union_D, "\n")
  
  cat("\n--- Verificarea Formulei Reuniunii ---\n")
  cat("P(A union D) calculat direct: ", prob_A_union_D, "\n")
  cat("P(A) + P(D) - P(A intersect D):", formula_reuniune, "\n")
}