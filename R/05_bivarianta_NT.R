path_ex3 <- "R/03_evenimente.r"
env3 <- new.env()
sys.source(path_ex3, envir = env3)
simuleaza_o_cerere <- env3$simuleaza_o_cerere
#' @return data.frame cu N, T, I
simuleaza_NT_din_ex3 <- function(M = 100000, n_max = 3, p_succes = 0.7,t_0 = 400, 
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
  
  #Simulam
  df_NT <- simuleaza_NT_din_ex3(M = M, n_max = n_max, p_succes = p_succes, t_0 = t_0, 
                                medie_S = medie_S, backoff_fix = backoff_fix)
  #a)
  plot(df_NT$N,df_NT$T,xlab="N (numar incercari)",ylab="T (timp total)",main="Scatterplot:(N,T)",
       pch=16, cex=0.4)
  
  #adaugam o linie de regresie ca sa se vada trendul
  abline(lm(T ~ N, data = df_NT), lwd = 2)
  
  #b) Medii, variante, covarianta, corelatie
  
  mean_N <- mean(df_NT$N)
  mean_T <- mean(df_NT$T)
  
  var_N <- var(df_NT$N)
  var_T <- var(df_NT$T)
  
  cov_NT <- cov(df_NT$N, df_NT$T)
  cor_NT <- cor(df_NT$N,df_NT$T)
  
  cat("\n Statistici pentru (N, T) \n")
  cat("E[N] =",round(mean_N, 4),"\n")
  cat("E[T] =",round(mean_T, 4),"\n")
  cat("Var(N) =",round(var_N, 4),"\n")
  cat("Var(T) =",round(var_T, 4),"\n")
  cat("Cov(N,T) =",round(cov_NT, 4),"\n")
  cat("Corr(N,T) =",round(cor_NT, 4),"\n")
  
  
  #c) Interpretare (tiparit)
  cat("\n Interpretare \n")
  cat("Ne asteptam la corelatie POZITIVA: cu cat avem mai multe retry-uri (N mai mare),\n")
  cat("cu atat timpul total T creste (se aduna mai multe S_i si eventual backoff-uri).\n")
  cat("Daca p_succes scade sau backoff_fix creste, corelatia tinde sa creasca.\n")
}