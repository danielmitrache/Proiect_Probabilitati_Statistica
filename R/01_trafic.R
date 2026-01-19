# R/01_trafic.R

# -- Rezolvarea cerintei a) --
#' Simuleaza traficul zilnic (Numarul de clienti Kd)
#' @param n_zile Numarul de zile pentru simulare
#' @param metoda Tipul distributiei: "poisson" sau "binomiala"
#' @param lambda Media estimata a clientilor (pentru Poisson)
#' @param n_max Capacitatea maxima teoretica (pentru Binomial)
#' @param p Probabilitatea ca un potential client sa fie activ (pentru Binomial)
simuleaza_trafic <- function(n_zile, metoda = "poisson", lambda = 1000, n_max = 2000, p = 0.5) {
  
  trafic <- numeric(n_zile)

  if (metoda == "poisson") {
    trafic <- rpois(n = n_zile, lambda = lambda)
  } else if (metoda == "binomiala") {
    trafic <- rbinom(n = n_zile, size = n_max, prob = p)
  } else {
    stop("Metoda necunoscuta! Alege 'poisson' sau 'binomiala'.")
  }
  
  return(trafic)
}


# -- Rezolvarea cerintelor b), c) --
if (sys.nframe() == 0) {
  zile_total <- 365 * 3
  
  trafic_poisson <- simuleaza_trafic(zile_total, metoda="poisson", lambda=1000)
  trafic_binomial <- simuleaza_trafic(zile_total, metoda="binomiala", n_max=2000, p=0.5)
  
  data_start <- as.Date("2024-01-01")
  vector_date <- seq(from = data_start, by = "day", length.out = zile_total)
  
  # Cream un tabel (Data Frame) pentru a putea filtra usor
  # Folosim Poisson ca exemplu principal
  df_poisson <- data.frame(
    valoare = trafic_poisson,
    an      = format(vector_date, "%Y"),
    luna    = format(vector_date, "%m")
  )
  
  # Cerinta B
  
  # A. Comparatie pe ANI
  # Vrem sa vedem daca traficul difera de la an la an
  par(mfrow = c(1, 3))
  
  ani <- unique(df_poisson$an)
  for(an_crt in ani) {
    hist(df_poisson$valoare[df_poisson$an == an_crt],
         col = "cornflowerblue",
         main = paste("Anul", an_crt),
         xlab = "Nr. Clienti",
         xlim = c(900, 1100), ylim = c(0, 120))
  }
  
  # B. Comparatie pe LUNI (Sezonalitate)
  # Verificam Ianuarie vs Iulie
  par(mfrow = c(1, 2)) 
  
  ianuarie <- df_poisson$valoare[df_poisson$luna == "01"]
  iulie    <- df_poisson$valoare[df_poisson$luna == "07"]
  
  hist(ianuarie, col="orange", main="Ianuarie (Toti anii)", 
       xlab="Nr. Clienti", xlim=c(900, 1100))
  
  hist(iulie, col="gold", main="Iulie (Toti anii)", 
       xlab="Nr. Clienti", xlim=c(900, 1100))
  
  # Resetam layout-ul grafic
  par(mfrow = c(1, 1))
  
  
  # Cerinta C
  # 1. Calculam Media Empirica pe ani
  medii_anuale <- aggregate(valoare ~ an, data = df_poisson, FUN = mean)
  
  # 2. Calculam Varianta Empirica pe ani
  variante_anuale <- aggregate(valoare ~ an, data = df_poisson, FUN = var)
  
  # 3. Construim un tabel comparativ
  tabel_comparativ <- data.frame(
    Anul = medii_anuale$an,
    Media_Empirica = round(medii_anuale$valoare, 2),
    Media_Teoretica = 1000,
    Varianta_Empirica = round(variante_anuale$valoare, 2),
    Varianta_Teoretica = 1000 
  )
  
  # Afisam tabelul
  print(tabel_comparativ)
}
