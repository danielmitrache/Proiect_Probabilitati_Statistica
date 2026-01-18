#Folosim functia simuleaza_o_cerere din fisierul cerintei 03

path_ex3 <- "R/03_evenimente.r"
env3 <- new.env()
sys.source(path_ex3, envir = env3)
simuleaza_o_cerere <- env3$simuleaza_o_cerere

#' @param M numarul de cereri simulate
#' @param n_max numarul maxim de incercari
#' @param p_succes probabilitatea de succes pe incercare
#' @param medie_S media pentru exponentiala timpilor de raspuns (din ex 2)
#' @param backoff_fix backoff adaugat intre incercari esuate
#' @return data.frame cu N, F, I

simuleaza_NF_din_ex3 <- function(M = 100000, n_max = 3, p_succes = 0.7,t_0 = 400, 
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

#Construim F din (I,N) -> daca I=1 => F=N-1, I=0 => F=N
df$F <- ifelse (df$I == 1L, df$N - 1L, df$N)

return(df[,c("N","F","I")])
}

if(sys.nframe()==0)
{
  set.seed(42)
  
  M <-100000
  n_max <- 3
  p_succes <- 0.7
  t_0 <- 400
  medie_S <- 150
  backoff_fix <- 50
  
  #1) Simulam (N,F)
  df_NF <- simuleaza_NF_din_ex3(M=M,n_max=n_max,p_succes=p_succes,t_0=t_0,medie_S=medie_S,
                                backoff_fix=backoff_fix)
  #a)
  tab_NF <- table (df_NF$N,df_NF$F)
  prob_NF <- prop.table(tab_NF)
  
  cat("\nDistributia comuna(frecvente)\n ")
  print(tab_NF)
  
  cat("\nDistributia comuna(probabilitati empirice)\n ")
  print(round(prob_NF,4))
  
  #b) Marginale
  
  marg_N <- margin.table (prob_NF,1)
  marg_F <- margin.table(prob_NF,2)
  
  cat("\n Marginala lui N: P(N=n) \n")
  print(round(marg_N,4))
  
  cat("\n Marginala lui F: P(F=f) \n")
  print(round(marg_F,4))
  
  #c)
  cat("\nTest de independenta (Chi-patrat) pentru (N,F)\n")
  test_ind <- chisq.test(tab_NF, simulate.p.value = TRUE, B = 2000)
  print(test_ind)
  
  #d) Vizualizare (heatmap + mosaicplot)
  prob_mat <-as.matrix(prob_NF)
  x_vals <- as.numeric (rownames(prob_mat)) #N
  y_vals <- as.numeric (colnames(prob_mat)) #F
  
  par(mfrow=c(1,2))
  
  image(
    x = x_vals, y = y_vals, z = t(prob_mat),
    xlab = "N (nr. incercari)", ylab = "F (nr. esecuri)",
    main = "Heatmap: P(N,F) (empiric)",
    axes = FALSE
  )
  axis(1, at = x_vals, labels = x_vals)
  axis(2, at = y_vals, labels = y_vals)
  
  mosaicplot(
    tab_NF,
    main = "Mosaicplot: frecvente (N,F)",
    xlab = "N", ylab = "F"
  )
  
  par(mfrow = c(1, 1))
  cat("\nInterpretare\n")
  cat("In modelul de retry, F depinde puternic de N:\n")
  cat("- daca cererea reuseste la incercarea N => F = N-1\n")
  cat("- daca esueaza complet => (N,F) = (n_max, n_max)\n")
  cat("De aceea, testul chi-patrat va respinge de obicei independenta.\n")
}





  
