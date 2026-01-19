# R/01_trafic.R

# Rezolvarea cerintei a)
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


# Rezolvarea cerintei b)
#' Construieste un data frame (un tabel) in care sunt reprezentate ambele distributii
#' si fiecare valoare are asociata o un an, o luna si o zi
#' @param n_zile Numarul de zile pentru simulare
#' @param metoda Tipul distributiei: "poisson" sau "binomiala"
#' @param lambda Media estimata a clientilor (pentru Poisson)
#' @param n_max Capacitatea maxima teoretica (pentru Binomial)
#' @param p Probabilitatea ca un potential client sa fie activ (pentru Binomial)
#' @param data_start Data de la care incepe simularea, data_final e implicit data_start + n_zile
df_trafic_zile <- function(n_zile, lambda = 1000, n_max = 2000, p = 0.5, data_start = "2026-01-01") {
  trafic_poisson <- simuleaza_trafic(n_zile, metoda="poisson", lambda=lambda)
  trafic_binomial <- simuleaza_trafic(n_zile, metoda="binomiala", n_max=n_max, p=p)
  
  data_start <- as.Date(data_start)
  vector_date <- seq(from = data_start, by = "day", length.out = n_zile)
  
  # Cream un Data Frame pentru a putea filtra usor
  df <- data.frame(
    valoare_poisson  = trafic_poisson,
    valoare_binomial = trafic_binomial,
    an               = format(vector_date, "%Y"),
    luna             = format(vector_date, "%m"),
    zi               = format(vector_date, "%d")
  )
  
  return(df)
}


#' Functia deseneaza 2 histograme, una pentru valorile traficului modelat folosind poisson iar a doua pentru valorile traficului modelat folosint binomiala
#' @param df_trafic_zile dataframe returnat de functia df_trafic_zile
comparatie_histograme_pois_bin <- function(df_trafic_zile) {
  par(mfrow = c(1, 2)) # Impartim ecranul in 2
  
  hist(df_trafic_zile$valoare_poisson,
       breaks = 30,
       probability = TRUE,
       col = "skyblue",
       border = "white",
       main = "Model Poisson\n(Trafic Nerestricționat)",
       xlab = "Nr. Clienti / Zi",
       ylab = "Densitate / Probabilitate") 
  
  hist(df_trafic_zile$valoare_binomial,
       breaks = 30,
       probability = TRUE,
       col = "lightgreen",
       border = "white",
       main = "Model Binomial\n(Capacitate Limitată)",
       xlab = "Nr. Clienti / Zi",
       ylab = "Densitate / Probabilitate")
  
  par(mfrow = c(1, 1))
}


#' Deseneaza histograma traficului pentru o luna specifica
#'
#' Aceasta functie filtreaza datele pentru o anumita luna si un anumit an,
#' apoi afiseaza histograma distributiei selectate. Utila pentru a observa
#' variabilitatea pe esantioane mici (n ~ 30).
#'
#' @param df_trafic_zile Data frame-ul generat de df_trafic_zile()
#' @param luna Numarul lunii ca string (ex: "01", "12")
#' @param an Anul din care extragem luna (ex: "2026")
#' @param distributie Tipul coloanei de desenat: "poisson" sau "binomiala"
#' @return Nu returneaza nimic, doar genereaza graficul.
histograma_luna_distrib <- function(df_trafic_zile, 
                                    luna = "01", 
                                    an = "2026",
                                    distributie = "poisson") {
  
  # 1. Filtram datele (Doar luna si anul cerut)
  date_filtrate <- df_trafic_zile[df_trafic_zile$luna == luna & df_trafic_zile$an == an, ]
  
  # 2. Selectam coloana corecta pe baza parametrului 'distributie'
  if (distributie == "poisson") {
    valori <- date_filtrate$valoare_poisson
    titlu_model <- "Poisson"
  } else {
    valori <- date_filtrate$valoare_binomial
    titlu_model <- "Binomiala"
  }
  
  # 3. Desenam Histograma
  hist(valori,
       breaks = 10, 
       probability = TRUE,
       col = "gold",
       border = "white",
       main = paste0("Model ", titlu_model, " - Luna ", luna, "/", an),
       xlab = "Nr. Clienți / Zi",
       ylab = "Frecvență")
}


#' Deseneaza histograma traficului pentru un an intreg
#'
#' Aceasta functie filtreaza datele pentru un an specific si afiseaza
#' histograma. Utila pentru a observa stabilitatea distributiei pe termen lung.
#'
#' @param df_trafic_zile Data frame-ul generat de df_trafic_zile()
#' @param an Anul dorit ca string (ex: "2026")
#' @param distributie Tipul coloanei de desenat: "poisson" sau "binomiala"
#' @return Nu returneaza nimic, doar genereaza graficul.
histograma_an_distrib <- function(df_trafic_zile,
                                  an = "2026",
                                  distributie = "poisson") {
  
  # 1. Filtram datele (Doar anul cerut)
  date_filtrate <- df_trafic_zile[df_trafic_zile$an == an, ]
  
  # 2. Selectam coloana
  if (distributie == "poisson") {
    valori <- date_filtrate$valoare_poisson
    titlu_model <- "Poisson"
  } else {
    valori <- date_filtrate$valoare_binomial
    titlu_model <- "Binomiala"
  }
  
  # 3. Desenam Histograma
  hist(valori,
       breaks = 30,
       probability = TRUE,
       col = "orange",
       border = "white",
       main = paste0("Model ", titlu_model, " - Anul ", an),
       xlab = "Nr. Clienti / Zi",
       ylab = "Densitate")
}



# Rezolvarea cerintei c)
#' Calculeaza tabelul comparativ (Empiric vs Teoretic)
#' 
#' @param df_trafic_zile Data frame-ul cu datele simulate
#' @param distributie "poisson" sau "binomiala"
#' @param lambda Parametrul lambda (pt Poisson)
#' @param n_max Parametrul n (pt Binomiala)
#' @param p Parametrul p (pt Binomiala)
df_estimari_empirice <- function(df_trafic_zile, 
                                 distributie = "poisson",
                                 lambda = 1000, 
                                 n_max = 2000, 
                                 p = 0.5) {
  
  # 1. Selectam coloana de date si calculam Teoria
  if (distributie == "poisson") {
    # Selectam coloana specifica Poisson
    valori_observate <- df_trafic_zile$valoare_poisson
    
    # Formule teoretice Poisson
    media_teo <- lambda
    varianta_teo <- lambda
    
  } else {
    # Selectam coloana specifica Binomiala
    valori_observate <- df_trafic_zile$valoare_binomial
    
    # Formule teoretice Binomiala
    media_teo <- n_max * p
    varianta_teo <- n_max * p * (1 - p)
  }
  
  # Cream un mic data frame temporar doar pentru agregare
  df_temp <- data.frame(
    an = df_trafic_zile$an,
    valoare = valori_observate
  )
  
  # 2. Calculam Statisticile EMPIRICE (din date) pe ani
  # aggregate returneaza un tabel cu coloanele "an" si "valoare"
  medii_empirice <- aggregate(valoare ~ an, data = df_temp, FUN = mean)
  variante_empirice <- aggregate(valoare ~ an, data = df_temp, FUN = var)
  
  # 3. Construim Tabelul Final
  tabel_comparativ <- data.frame(
    Anul = medii_empirice$an,
    
    Media_Empirica = round(medii_empirice$valoare, 2),
    Media_Teoretica = media_teo,
    
    Varianta_Empirica = round(variante_empirice$valoare, 2),
    Varianta_Teoretica = varianta_teo
  )
  
  return(tabel_comparativ)
}



# Rezolvarea cerintei d)
#' Functie ajutatoare la compararea celor doua modele - Poisson si Binomiala
#' @param df_trafic_zile variabila returnata de functia df_trafic_zile
comparatie_densitati_suprapuse <- function(df_trafic_zile) {
  
  # Calculam densitatile (forma curbelor)
  dens_pois <- density(df_trafic_zile$valoare_poisson)
  dens_bin  <- density(df_trafic_zile$valoare_binomial)
  
  # Calculam limitele pentru a incapea ambele
  limita_x <- range(c(dens_pois$x, dens_bin$x))
  limita_y <- range(c(dens_pois$y, dens_bin$y))
  
  # Desenam prima curba (Poisson)
  plot(dens_pois, 
       xlim = limita_x, ylim = limita_y,
       col = "blue", lwd = 3,
       main = "Comparatie Directa: Poisson vs Binomial",
       xlab = "Nr. Clienti / Zi", ylab = "Probabilitate")
  
  # Umplem suprafata Poisson cu albastru transparent
  polygon(dens_pois, col = rgb(0, 0, 1, 0.2), border = NA)
  
  # Adaugam a doua curba (Binomiala) peste prima
  lines(dens_bin, col = "forestgreen", lwd = 3)
  
  # Umplem suprafata Binomiala cu verde transparent
  polygon(dens_bin, col = rgb(0, 0.5, 0, 0.2), border = NA)
  
  # Legenda
  legend("topright", 
         legend = c("Poisson", "Binomial"),
         col = c("blue", "forestgreen"), 
         lwd = 3, fill = c(rgb(0,0,1,0.2), rgb(0,0.5,0,0.2)))
  
  grid()
}
