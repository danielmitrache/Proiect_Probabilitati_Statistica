# Fisier: R/02_latenta.R

# Rezolvarea cerintei a)
#' Simuleaza timpii de raspuns (S)
#' @param n Numarul de cereri (esantionul)
#' @param tip Tipul distributiei: "exponentiala" sau "normala"
#' @param medie Timpul mediu dorit (ex: 200 ms) atat pentru distributia exponentiala cat si cea normala
#' @param sd Deviatia standard pentru distributia normala
simuleaza_latente <- function(n, tip = "exponentiala", medie = 200, sd = 50) {
  if (tip == "exponentiala") {
    rate_param <- 1 / medie
    valori <- rexp(n, rate = rate_param)
  } else if (tip == "normala") {
    valori <- rnorm(n, mean = medie, sd = sd)
    
    # Timpul nu poate fi negativ. Orice valoare < 1ms devine 1ms.
    valori <- pmax(valori, 1)
  } else {
    stop("Tip distributie necunoscut! Alege 'exponentiala' sau 'normala'.")
  }
  
  return(valori)
}


# Rezolvarea cerintei b)
#' Vizualizeaza histograma si densitatea teoretica
#' @param vector_latente Datele generate de functia simuleaza_latente
#' @param tip "exponentiala" sau "normala"
#' @param medie Media folosita la generare (pt curba teoretica)
#' @param sd Deviatia standard (doar pt normala)
vizualizeaza_latente <- function(vector_latente, tip = "exponentiala", medie = 200, sd = 50) {
  # Desenam Histograma
  if (tip == "exponentiala") {
    titlu <- "Latente: Distribuție Exponențială (Asimetrică)"
    culoare <- "wheat"
  } else {
    titlu <- "Latente: Distribuție Normală (Simetrică)"
    culoare <- "lightblue"
  }
  
  hist(vector_latente, 
       probability = TRUE, 
       breaks = 30, 
       col = culoare, 
       border = "white",
       main = titlu,
       xlab = "Timp de Răspuns (ms)",
       ylab = "Densitate")
  
  # Suprapunem densitatea teoretica (formula matematica)

  if (tip == "exponentiala") {
    # Functia de densitate exponentiala este dexp
    curve(dexp(x, rate = 1/medie), 
          col = "red", lwd = 3, add = TRUE)
  } else {
    # Functia de densitate normala este dnorm
    curve(dnorm(x, mean = medie, sd = sd), 
          col = "darkblue", lwd = 3, add = TRUE)
  }
}



# Rezolvarea cerintei c)
#' Calculeaza statisticile descriptive (Media, Varianta, Mediana, Modul)
#' si returneaza un tabel comparativ.
#'
#' @param v_exp Vectorul cu datele distributiei Exponentiale
#' @param v_norm Vectorul cu datele distributiei Normale
#' @return Un Data Frame cu statisticile rotunjite la 2 zecimale
comparatie_statistica_latente <- function(v_exp, v_norm) {
  
  # Pentru variabile continue, Modul este varful curbei de densitate
  calculeaza_modul <- function(v) {
    d <- density(v)
    return(d$x[which.max(d$y)])
  }
  
  # Calculam statisticile pentru exponentiala
  stats_exp <- c(
    Media    = mean(v_exp),
    Varianta = var(v_exp),
    Mediana  = median(v_exp),
    Modul    = calculeaza_modul(v_exp)
  )
  
  # Calculam statisticile pentru normala
  stats_norm <- c(
    Media    = mean(v_norm),
    Varianta = var(v_norm),
    Mediana  = median(v_norm),
    Modul    = calculeaza_modul(v_norm)
  )
  
  # Construim Data Frame-ul final
  df_stat <- data.frame(
    Distributia = c("Exponentiala", "Normala"),
    Media    = c(stats_exp["Media"],    stats_norm["Media"]),
    Varianta = c(stats_exp["Varianta"], stats_norm["Varianta"]),
    Mediana  = c(stats_exp["Mediana"],  stats_norm["Mediana"]),
    Modul    = c(stats_exp["Modul"],    stats_norm["Modul"])
  )
  
  # Rotunjim valorile numerice pentru un aspect curat in tabel
  # (Coloanele 2 pana la 5 sunt numerice)
  df_stat[, 2:5] <- round(df_stat[, 2:5], 2)
  
  return(df_stat)
}