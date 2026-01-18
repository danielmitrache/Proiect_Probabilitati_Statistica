# Rezolvarea exercitiului 9

#' Simuleaza suma totala pentru o zi (Latenta Totala)
#' 
#' Functia genereaza numarul de cereri dintr-o zi (Poisson) si
#' insumeaza latentele individuale (Exponentiale) ale acestora.
#' 
#' @param lambda_trafic Media traficului zilnic (numar cereri/zi)
#' @param medie_latenta Media latentei per cerere (S_i)
#' 
#' @return Suma totala pentru o zi
simuleaza_suma_totala <- function(lambda_trafic = 1000, medie_latenta = 150) {
  
  #Generam numarul de cereri pentru o zi (Poisson)
  k_cereri <- rpois(1, lambda = lambda_trafic)
  
  suma_zi <- 0
  
  if (k_cereri > 0) {
    #Generam latentele individuale (S_i)
    #Folosim distributia exponentiala
    latente_individuale <- rexp(k_cereri, rate = 1/medie_latenta)
    
    #Suma totala
    suma_zi <- sum(latente_individuale)
  }
  
  return(suma_zi)
}

# EXECUTIE SI ANALIZA

if (sys.nframe() == 0) {
  set.seed(123) 
  
  M <- 1000 #nr zile simulate
  
  #Simulare
  latenta_totala_zi <- replicate(M, simuleaza_suma_totala(lambda_trafic = 1000, 
                                                        medie_latenta = 150))
  
  #Calculam parametrii empirici pentru ajustarea distributiei normale
  media_agregat <- mean(latenta_totala_zi)
  sd_agregat <- sd(latenta_totala_zi)
  
  #Afisare rezultate
  cat("Media Empirica a sumei:    ", round(media_agregat, 2), "\n")
  cat("Deviatia Standard Empirica:", round(sd_agregat, 2), "\n")
  
  #Vizualizare Comparativa
  par(mfrow = c(1, 1))
  
  #Histograma datelor agregate
  hist(latenta_totala_zi, 
       probability = TRUE, 
       breaks = 30,
       col = "cornflowerblue", 
       border = "white",
       main = "Latenta totala zilnica",
       xlab = "Latenta Totala (ms/zi)",
       ylab = "Densitate")
  
  #Adaugam curba Teoretica Normala (linia rosie)
  curve(dnorm(x, mean = media_agregat, sd = sd_agregat), 
        add = TRUE, 
        col = "red", 
        lwd = 2)
}
