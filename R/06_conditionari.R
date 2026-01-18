path_ex3 <- "R/03_evenimente.r"
env3 <- new.env()
sys.source(path_ex3, envir = env3)
simuleaza_o_cerere <- env3$simuleaza_o_cerere
simuleaza_date_din_ex3 <- function(M = 100000, n_max = 3, p_succes = 0.7,t_0 = 400, 
                                 medie_S = 150, backoff_fix = 50) 
{
  rezultate <- replicate(
    M,
    simuleaza_o_cerere(
      n_max = n_max, p_succes = p_succes,
      t_0 = t_0, medie_S = medie_S, backoff_fix = backoff_fix))
  df <- as.data.frame(t(rezultate))
  df[] <- lapply(df, unlist) # transforma coloanele (liste) in vectori simpli
  
  df$N <-as.integer(df$N)
  df$I <- as.integer(df$I)
  df$T <-as.numeric(df$T)
  
  df[, c("N", "T", "I")]
}
if(sys.nframe()==0)
{
  set.seed(42)
  
  M <- 100000
  n_max <- 3
  p_succes <- 0.7
  t_0 <- 400
  medie_S <- 150
  backoff_fix <- 50
  
  n_0 <-2 #t_0 e deja pragul SLA folosit si in ex 3
  
  df <- simuleaza_date_din_ex3(M = M, n_max = n_max, p_succes = p_succes, t_0 = t_0, 
                          medie_S = medie_S, backoff_fix = backoff_fix)
  A <- (df$I == 1L)
  B <- (df$T <= t_0)
  C <- (df$N <= n_0)
  
  #a) P(A|N<=n_0)
  # P(A|C) = nr(A & C)/nr(C)
  P_A_given_C <- mean(A[C])
  cat("\n P(A|N<=n_0) = P(I=1|N <=",n_0,")=",round(P_A_given_C, 4),"\n")
  
  #a) P(B | A)
  # P(B|A) = nr(B & A) / nr(A)
  P_B_given_A <- mean(B[A])
  cat("P(B|A) = P(T<=t_0|I=1) =",round(P_B_given_A, 4),"\n")
  
  # c) E(T|I=1) si E(T | I=0)
  E_T_given_success <- mean(df$T[df$I == 1L])
  E_T_given_fail <- mean(df$T[df$I == 0L])
  
  cat("E(T|I=1) =", round(E_T_given_success, 4), "\n")
  cat("E(T|I=0) =", round(E_T_given_fail, 4), "\n")
  
  # Interpretare scurta
  cat("\n Interpretare \n")
  cat("P(I=1 | N <= n_0) arata cat de des reusim rapid (cu putine retry-uri).\n")
  cat("P(T <= t_0 | I=1) arata cat de des respectam SLA conditionat de faptul ca am reusit.\n")
  cat("E(T | I=1) vs E(T | I=0): in general, esecurile finale pot avea T mai mare deoarece consuma toate incercarile.\n")
}