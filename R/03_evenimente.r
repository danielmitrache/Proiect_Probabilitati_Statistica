# R/03_evenimente.R

# -- Rezolvarea Exercițiului 3 --
#' Simuleaza o singura cerere care poate avea mai multe retry-uri
#' @return O lista cu: I (succes final), T (timp total), N (nr incercari), FailedAtLeastOnce (D)
simuleaza_o_cerere <- function(n_max = 3, p_succes = 0.8, t_0 = 500, medie_S = 150, backoff_fix = 50) {
  timp_total <- 0
  succes_final <- 0
  nr_incercari <- 0
  a_esuat_vreodata <- FALSE
  
  for (i in 1:n_max) {
    nr_incercari <- i
    
    # 1. Generam timpul de raspuns S_i (folosim modelul exponential din ex 2)
    s_i <- rexp(1, rate = 1/medie_S)
    timp_total <- timp_total + s_i
    
    # 2. Verificam daca incercarea i are succes (U_i)
    if (runif(1) < p_succes) {
      succes_final <- 1
      break # Am reusit, iesim din bucla
    } else {
      a_esuat_vreodata <- TRUE
      # Daca nu e ultima incercare, adaugam backoff
      if (i < n_max) {
        timp_total <- timp_total + backoff_fix
      }
    }
  }
  
  return(list(I = succes_final, T = timp_total, N = nr_incercari, D = a_esuat_vreodata))
}

# --- Executie si Analiza ---
if (sys.nframe() == 0) {
  set.seed(123) # Pentru reproductibilitate
  M <- 10000 # Numar de cereri simulate
  n0_prag <- 2
  t0_prag <- 400
  
  # Colectam datele intr-un data.frame
  rezultate <- replicate(M, simuleaza_o_cerere(n_max = 3, p_succes = 0.7, t_0 = t0_prag))
  df_cereri <- as.data.frame(t(rezultate))
  # Convertim list-columns in vectori numerici pentru calcul
  df_cereri[] <- lapply(df_cereri, unlist)
  
  # a) Estimarea empirica a probabilitatilor
  prob_A <- mean(df_cereri$I == 1)
  prob_B <- mean(df_cereri$T <= t0_prag)
  prob_C <- mean(df_cereri$N <= n0_prag)
  
  # Intersectie: A si B (Succes SI in timp util)
  prob_A_intersect_B <- mean(df_cereri$I == 1 & df_cereri$T <= t0_prag)
  
  # Reuniune: A sau D (Succes final SAU a existat cel putin un esec pe parcurs)
  prob_A_union_D <- mean(df_cereri$I == 1 | df_cereri$D == TRUE)
  
  # b) Verificarea formulei reuniunii: P(A U D) = P(A) + P(D) - P(A intersect D)
  prob_D <- mean(df_cereri$D == TRUE)
  prob_A_intersect_D <- mean(df_cereri$I == 1 & df_cereri$D == TRUE)
  formula_reuniune <- prob_A + prob_D - prob_A_intersect_D
  
  # Afisare rezultate
  cat("--- Estimari Empirice ---\n")
  cat("P(A) [Succes final]:", prob_A, "\n")
  cat("P(B) [SLA - sub prag timp]:", prob_B, "\n")
  cat("P(C) [Retry-uri putine]:", prob_C, "\n")
  cat("P(A intersect B):", prob_A_intersect_B, "\n")
  cat("P(A union D):", prob_A_union_D, "\n")
  cat("\n--- Verificare Formula ---\n")
  cat("P(A union D) direct:", prob_A_union_D, "\n")
  cat("P(A) + P(D) - P(A intersect D):", formula_reuniune, "\n")
}