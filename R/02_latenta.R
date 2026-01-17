# Fisier: R/02_latenta.R

# -- Rezolvarea cerintei a) --
#' Simuleaza timpii de raspuns (S)
#' @param n Numarul de cereri (esantionul)
#' @param tip Tipul distributiei: "exponential" sau "normala"
#' @param medie_target Timpul mediu dorit (ex: 200 ms)
simuleaza_latente <- function(n, tip = "exponentiala", medie_target = 200) {
  
  if (tip == "exponentiala") {
    # 1. Distributia EXPONENTIALA (Asimetrica - Foarte comuna in cozi de asteptare)
    rate_param <- 1 / medie_target
    valori <- rexp(n, rate = rate_param)
    
  } else if (tip == "normala") {
    # 2. Distributia NORMALA (Simetrica - Ideala)
    # Alegem o deviatie standard proportionala cu media
    sd_param <- medie_target / 4
    valori <- rnorm(n, mean = medie_target, sd = sd_param)
    
    # TRUNCHIERE (Valori pozitive):
    # Timpul nu poate fi negativ. Orice valoare < 1ms devine 1ms.
    valori <- pmax(valori, 1)
    
  } else {
    stop("Tip distributie necunoscut! Alege 'exponentiala' sau 'normala'.")
  }
  
  return(valori)
}


# -- Rezolvarea cerintelor b), c) --
if (sys.nframe() == 0){
  # Cerinta B
  # Parametrii simularii
  nr_cereri <- 10^5   # Presupunem 100.000 de cereri
  media_target <- 200 # Timpul mediu dorit (ms)
  
  # Generam datele
  latenta_exp <- simuleaza_latente(nr_cereri, tip="exponentiala", medie_target = media_target)
  latenta_norm <- simuleaza_latente(nr_cereri, tip="normala", medie_target = media_target)
  
  # Setam layout-ul: 1 rand, 2 coloane
  par(mfrow=c(1, 2))
  
  # 1. Distributia EXPONENTIALA
  hist(latenta_exp, 
       probability = TRUE, 
       breaks = 50, 
       col = "wheat", 
       border = "white",
       main = "Distributie Exponentiala (Asimetrica)", 
       xlab = "Timp raspuns (ms)")
  
  # Densitatea teoretica (formula matematica)
  # Rata (lambda) este 1 / media
  curve(dexp(x, rate = 1/media_target), 
        add = TRUE,    # Se pune PESTE graficul existent
        col = "red",   # Culoare rosie
        lwd = 2)       # Grosimea liniei
  
  # 2. Distributia NORMALA
  hist(latenta_norm, 
       probability = TRUE, 
       breaks = 50, 
       col = "lightblue", 
       border = "white",
       main = "Distributie Normala (Simetrica)", 
       xlab = "Timp raspuns (ms)")
  
  # Densitatea teoretica
  # SD-ul folosit in functie este media / 4
  curve(dnorm(x, mean = media_target, sd = media_target/4), 
        add = TRUE, 
        col = "blue", 
        lwd = 2)
  
  # Resetam layout-ul
  par(mfrow=c(1, 1))
  
  
  # Cerinta C
  calculeaza_modul <- function(v) {
    d <- density(v)
    return(d$x[which.max(d$y)])
  }
  # --- 1. Statistici pentru EXPONENTIALA ---
  mean_exp <- mean(latenta_exp)
  var_exp  <- var(latenta_exp)
  med_exp  <- median(latenta_exp)
  mod_exp  <- calculeaza_modul(latenta_exp)
  
  # --- 2. Statistici pentru NORMALA ---
  mean_norm <- mean(latenta_norm)
  var_norm  <- var(latenta_norm)
  med_norm  <- median(latenta_norm)
  mod_norm  <- calculeaza_modul(latenta_norm)
  
  df_stat <- data.frame(
    Distributia = c("Exponentiala", "Normala"),
    Media    = c(mean_exp, mean_norm),
    Varianta = c(var_exp, var_norm),
    Mediana  = c(med_exp, med_norm),
    Modul    = c(mod_exp, mod_norm)
  )
  
  print(df_stat)
}