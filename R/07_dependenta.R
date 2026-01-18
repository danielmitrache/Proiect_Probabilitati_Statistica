# Rezolvarea exercitiului 7

#' Simuleaza o cerere cu posibilitatea dependentei intre esecuri si latenta
#' 
#' @param n_max Numarul maxim de incercari
#' @param p_succes Probabilitatea de succes a unei incercari
#' @param medie_S_initial Timpul mediu de procesare initial (fara penalizari)
#' @param backoff_fix Timpul de asteptare intre incercari
#' @param factor_latenta Factorul cu care creste media timpului de raspuns dupa un esec. 
#'                          Daca este 1.0, timpii sunt independenti.
#'                          Daca este > 1.0, sistemul devine mai lent dupa esecuri (dependenta).
#' 
#' @return Timpul total T
simuleaza_cerere_dependenta <- function(n_max = 3, p_succes = 0.7, medie_S_initial = 150, 
                                        backoff_fix = 50, factor_latenta = 1.0) {
  
  timp_total <- 0
  medie_curenta <- medie_S_initial
  succes <- FALSE
  
  for (i in 1:n_max) {
    #Generam timpul de raspuns S_i
    #Daca factor_penalizare > 1 si am avut esecuri anterioare, medie_curenta va fi mai mare
    s_i <- rexp(1, rate = 1/medie_curenta)
    timp_total <- timp_total + s_i
    
    #Verificam succesul
    if (runif(1) < p_succes) {
      succes <- TRUE
      break #Daca am reusit, iesim
    } 
    else {
      #Daca am esuat
      #Adaugam backoff daca mai avem incercari
      if (i < n_max) {
        timp_total <- timp_total + backoff_fix
        
        #Aplicam dependenta: latenta creste pentru urmatoarea incercare
        medie_curenta <- medie_curenta * factor_latenta
      }
    }
  }
  
  return(timp_total)
}

#EXECUTIE SI ANALIZA

if (sys.nframe() == 0) {
  set.seed(123) 
  M <- 10000 #Numar simulare
  
  #Simulare 7a)
  
  #Simulare timpi independenti -> factor_latenta = 1.0
  timp_independent <- replicate(M, simuleaza_cerere_dependenta(
    n_max = 3, p_succes = 0.7, medie_S_initial = 150, 
    backoff_fix = 50, factor_latenta = 1.0
  ))
  
  #Simulare timpi dependenti -> factor_latenta = 2.0
  #In acest caz, dupa fiecare esec, latenta se dubleaza
  timp_dependent <- replicate(M, simuleaza_cerere_dependenta(
    n_max = 3, p_succes = 0.7, medie_S_initial = 150, 
    backoff_fix = 50, factor_latenta = 2.0
  ))
  
  #Comparatie 7b)
  
  var_indep <- var(timp_independent)
  var_dep   <- var(timp_dependent)
  mean_indep <- mean(timp_independent)
  mean_dep   <- mean(timp_dependent)
  
  cat("* Rezultate pentru M =", M, " simulari\n")
  cat("Simulare independenta (factor = 1.0): \n")
  cat("  Media T:   ", round(mean_indep, 2), "\n")
  cat("  Varianta T:", round(var_indep, 2), "\n")
  
  cat("\nSimulare dedependenta (factor = 2.0): \n")
  cat("  Media T:   ", round(mean_dep, 2), "\n")
  cat("  Varianta T:", round(var_dep, 2), "\n")
  
  #Grafic pentru comparatia dintre dependent si independent
  par(mfrow = c(1, 1))
  
  #Setam limita graficului pentru lizibilitate
  x_max <- quantile(timp_dependent, 0.99) 

  #Afisam cazul dependent primul pentru a ne asigura ca incape in grafic
  plot(density(timp_dependent), col = "red", lwd = 2, xlim = c(0, x_max),
       main = "Distributie: Dependent / Independent",
       xlab = "Timp Total (ms)", ylab = "Densitate")

  #Cazul independent
  lines(density(timp_independent), col = "blue", lwd = 2)
}

