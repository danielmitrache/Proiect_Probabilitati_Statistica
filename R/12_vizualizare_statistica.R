# R/12_vizualizare_statistica.R

# Incarcam dependentele
if(file.exists("R/11_impact_economic.R")) source("R/11_impact_economic.R")


#' Deseneaza Histogramele pentru Timp si Profit (Cerinta 12a)
#' @param vector_timp
#' @param vector_profit
#' @return Nimic, doar deseneaza histogramele
plot_histograme_12a <- function(vector_timp, vector_profit) {
  # Impartim ecranul in 2 (stanga/dreapta)
  par(mfrow = c(1, 2))
  
  # 1. Histograma Timpului (T)
  hist(vector_timp, 
       col = "lightblue", 
       border = "white", 
       main = "Distributia Timpului Total (T)",
       xlab = "Timp (ms)", 
       ylab = "Frecventa",
       probability = TRUE)

  # 2. Histograma Profitului
  hist(vector_profit, 
       col = "lightgreen", 
       border = "white",
       main = "Distributia Profitului Zilnic",
       xlab = "Profit (EUR)",
       ylab = "Frecventa")

  # Resetam layout-ul
  par(mfrow = c(1, 1))
}

#' Deseneaza Boxplot-uri comparative (Cerinta 12b)
#' @param df_cereri
#' @param df_scenariu_1
#' @param df_scenariu_2
#' @return Nimic, doar desebeaza boxplot-uri
plot_boxplots_12b <- function(df_cereri, df_scenariu_1, df_scenariu_2) {
  
  # Layout: 1 rand, 2 coloane
  par(mfrow = c(1, 2))
  
  # 1. Boxplot Conditionat: Succes vs Esec
  # Transformam 0/1 in etichete text
  etichete <- factor(df_cereri$I, levels = c(0, 1), labels = c("Esec", "Succes"))
  
  boxplot(df_cereri$T ~ etichete,
          col = c("salmon", "lightgreen"),
          main = "Timp (T) conditionat de Rezultat",
          ylab = "Timp Total (ms)",
          xlab = "Rezultat Final")
  
  # 2. Boxplot Comparatie Scenarii (ex: Backoff Mic vs Backoff Mare)
  # Cream o lista pentru boxplot
  boxplot(df_scenariu_1$T, df_scenariu_2$T,
          names = c("Scenariu A", "Scenariu B"),
          col = c("lightblue", "gold"),
          main = "Comparatie Scenarii (Timp)",
          ylab = "Timp Total (ms)")
  
  par(mfrow = c(1, 1))
}



if(sys.nframe() == 0) {
  #A.
  
  df_cereri_viz <- simuleaza_lot_cereri(n_simulari = 5000, 
                                        n_max = 3, 
                                        p_succes = 0.8, 
                                        medie_S = 150)
  
  vector_profit_viz <- replicate(200, {
    # Generam o zi cu trafic mediu (1000 clienti)
    rez <- simuleaza_profit_zi(nr_clienti = 1000, 
                               castig_per_succes = 2.0, 
                               cost_churn = 20, 
                               rata_churn_aleator = 0.001)
    return(rez["Profit"])
  })
  
  plot_histograme_12a(vector_timp = df_cereri_viz$T, vector_profit = vector_profit_viz)

  
  
  # B.
  # Generam datele pentru Scenariul A (Standard: 3 incercari)
  df_standard <- simuleaza_lot_cereri(n_simulari = 5000, 
                                      n_max = 3, 
                                      p_succes = 0.8, 
                                      medie_S = 150)
  
  # Generam datele pentru Scenariul B (Fail-Fast: 1 incercare)
  # Schimbam parametrul n_max la 1 pentru a simula lipsa retry-urilor
  df_fail_fast <- simuleaza_lot_cereri(n_simulari = 5000, 
                                       n_max = 1, 
                                       p_succes = 0.8, 
                                       medie_S = 150)
  
  plot_boxplots_12b(df_standard, df_standard, df_fail_fast)
}