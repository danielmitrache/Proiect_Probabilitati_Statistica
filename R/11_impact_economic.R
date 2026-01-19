# R/11_impact_economic.R

# Incarcam dependentele
if(file.exists("R/01_trafic.R")) source("R/01_trafic.R")
if(file.exists("R/03_evenimente.R")) source("R/03_evenimente.R")
if(file.exists("R/10_churn.R")) source("R/10_churn.R")

simuleaza_lot_cereri <- function(n_simulari,
                             n_max = 3,
                             p_succes = 0.8,
                             t_0 = 500,
                             medie_S = 150,
                             backoff_fix = 50) {
  
  rezultate <- replicate(n_simulari, simuleaza_o_cerere(n_max = n_max,
                                                        p_succes = p_succes,
                                                        t_0 = t_0,
                                                        medie_S = medie_S,
                                                        backoff_fix = backoff_fix))
  df <- as.data.frame(t(rezultate))
  
  # Convertim listele rezultate in vectori numerici/logici pentru calcule statistice
  df[] <- lapply(df, unlist)
  
  return(df)
}

#' Calculeaza bilantul economic pentru o zi de activitate
#' @param nr_clienti Numarul de clienti din acea zi (Kd)
#' @param castig_per_succes Venitul generat de o cerere reusita
#' @param cost_churn Pierderea generata de un client care pleaca
#' @param penalizare_sla Costul daca se depaseste timpul limita
#' @param rata_churn_aleator Probabilitatea ca un utilizator sa paraseasca platforma aleator
#' @param dim_churn_cond Dimensiunea ferestrei (numarul de cereri recente)
#' @param prag_erori_churn_cond Pragul de erori (minim k esecuri declanseaza plecarea)
#' @param prob_succes_churn_cond Probabilitatea de succes a unei singure cereri
#' @return Un vector numit cu indicatorii economici: Profit, Venit, Costuri si Numarul de utilizatori pierduti.
simuleaza_profit_zi <- function(nr_clienti, 
                                castig_per_succes = 0.5, 
                                cost_churn = 50, 
                                penalizare_sla = 2.0,
                                rata_churn_aleator = 0.05,
                                dim_churn_cond = 20,
                                prag_erori_churn_cond = 5,
                                prob_succes_churn_cond = 0.9) {
  
  rezultate_zi <- simuleaza_lot_cereri(nr_clienti)
  
  nr_succese <- sum(rezultate_zi$I == 1)
  
  nr_sla_fail <- sum(rezultate_zi$T > 800)
  
  
  # CHURN
  plecari_aleatoare <- replicate(nr_clienti, simuleaza_churn_aleator(q = rata_churn_aleator))

  plecari_conditionate <- replicate(nr_clienti, simuleaza_churn_conditionat(m = dim_churn_cond,
                                                                            k = prag_erori_churn_cond,
                                                                            p_succes = prob_succes_churn_cond))
  plecari_totale <- plecari_aleatoare | plecari_conditionate
  
  nr_churn = sum(plecari_totale)
  
  
  # PARTEA FINANCIARA
  venit_total <- nr_succese * castig_per_succes
  cost_penalizari <- nr_sla_fail * penalizare_sla
  pierdere_churn <- nr_churn * cost_churn
  
  profit_net <- venit_total - pierdere_churn - cost_penalizari
  
  return(c(
    Profit = profit_net,
    Venit = venit_total,
    Cost_SLA = cost_penalizari,
    Cost_Churn = pierdere_churn,
    Nr_Churn = nr_churn
  ))
}



#' Calculeaza statistici descriptive si interval de incredere
#' @param vector_valori Un vector numeric (ex: profiturile zilnice)
#' @return Un vector cu Media, Varianta si limitele Intervalului de Incredere 95%
calculeaza_statistici_profit <- function(vector_valori) {
  
  # 1. Calcule de baza
  media <- mean(vector_valori)
  varianta <- var(vector_valori)
  dev_std <- sd(vector_valori)
  n <- length(vector_valori)
  
  # 2. Calcul Interval de Incredere 95%
  # Formula: Media +/- 1.96 * (SD / sqrt(n))
  eroare_std <- dev_std / sqrt(n)
  limita_inf <- media - 1.96 * eroare_std
  limita_sup <- media + 1.96 * eroare_std
  
  # 3. Returnam totul impachetat frumos
  return(c(
    Media = media,
    Varianta = varianta,
    Deviatie_Std = dev_std,
    IC_Min = limita_inf,
    IC_Max = limita_sup
  ))
}



#' Simuleaza un scenariu economic complet pe o perioada data
#' Aceasta functie impacheteaza tot procesul: Trafic -> Simulare Zi -> Statistici
#' 
#' @param n_zile Durata simularii (zile)
#' @param model_trafic Tipul distributiei traficului: "poisson" sau "binomiala"
#' @param lambda_trafic_pois Media clientilor pe zi (pentru Poisson)
#' @param nmax_trafic_bin Capacitatea maxima teoretica (pentru Binomial)
#' @param p_trafic_bin Probabilitatea ca un potential client sa fie activ (pentru Binomial)
#' 
#' @param castig_per_succes Venit per succes
#' @param cost_churn Pierdere per client plecat
#' @param cost_sla Penalizare SLA
#' @param rata_churn Rata de churn aleator
#' 
#' @param rata_churn_aleator Probabilitatea ca un utilizator sa paraseasca platforma aleator
#' @param dim_churn_cond Dimensiunea ferestrei (numarul de cereri recente)
#' @param prag_erori_churn_cond Pragul de erori (minim k esecuri declanseaza plecarea)
#' @param prob_succes_churn_cond Probabilitatea de succes a unei singure cereri
#' 
#' @return Vectorul cu statistici (Media, Varianta, IC)
simuleaza_scenariu_economic <- function(n_zile = 100,
                                        model_trafic = "poisson",
                                        lambda_trafic = 1000,
                                        nmax_trafic_bin = 2000,
                                        p_trafic_bin = 0.5,
                                        castig_per_succes = 0.6,
                                        cost_churn = 50,
                                        cost_sla = 2.0,
                                        rata_churn_aleator = 0.05,
                                        dim_churn_cond = 20,
                                        prag_erori_churn_cond = 5,
                                        prob_succes_churn_cond = 0.9) {
  
  # 1. Generam Traficul (folosim functia din Ex 1)
  trafic_zilnic <- simuleaza_trafic(n_zile,
                                    metoda = model_trafic,
                                    lambda = lambda_trafic,
                                    n_max = nmax_trafic_bin,
                                    p = p_trafic_bin)
  
  # 2. Alocam spatiu pentru rezultate
  profituri <- numeric(n_zile)
  
  # 3. Rulam simularea zi de zi
  for(i in 1:n_zile) {
    # Apelam functia de zi pe care am facut-o anterior
    rezultat_zi <- simuleaza_profit_zi(nr_clienti = trafic_zilnic[i],
                                       castig_per_succes = castig_per_succes,
                                       cost_churn = cost_churn,
                                       penalizare_sla = cost_sla,
                                       rata_churn_aleator = rata_churn_aleator,
                                       dim_churn_cond = dim_churn_cond, 
                                       prag_erori_churn_cond = prag_erori_churn_cond,
                                       prob_succes_churn_cond = prob_succes_churn_cond)
    
    profituri[i] <- rezultat_zi["Profit"]
  }
  
  # 4. Calculam si returnam statisticile
  statistici <- calculeaza_statistici_profit(profituri)
  return(statistici)
}



#' Genereaza date pentru analiza de senzitivitate
#' @param start Rata de succes de start
#' @param end Rata de succes de final
#' @param step Pasul de incrementare
#' @param ... Orice alti parametri transmisi catre simuleaza_scenariu_economic 
#'            (ex: n_zile, castig_per_succes, etc.)
genereaza_date_senzitivitate <- function(start = 0.85, end = 0.99, step = 0.01, ...) {
  
  # Generam secventa de rate de succes
  rate_succes <- seq(start, end, by = step)
  profituri_medii <- numeric(length(rate_succes))
  
  # Iteram prin fiecare rata
  for(i in seq_along(rate_succes)) {
    p <- rate_succes[i]
    
    # suprascriem prob_succes_churn_cond cu valoarea curenta 'p'
    stats <- simuleaza_scenariu_economic(..., prob_succes_churn_cond = p)
    
    profituri_medii[i] <- stats["Media"]
  }
  
  return(data.frame(
    Rata_Succes = rate_succes,
    Profit_Mediu = profituri_medii
  ))
}


if(sys.nframe() == 0) {
  # B. Testare si Estimare
  
  # Rulam simularea pe 100 de zile pentru test
  statistici <- simuleaza_scenariu_economic(
    n_zile = 100, 
    rata_churn_aleator = 0.001, # 0.1% churn
    cost_churn = 20,
    castig_per_succes = 2
  ) 
  
  cat("\n--- Rezultate Finale ---\n")
  # Corectat 'statstici' -> 'statistici'
  cat("Media Profitului:   ", round(statistici["Media"], 2), "EUR\n")
  cat("Varianta Profitului:", round(statistici["Varianta"], 2), "\n")
  cat("Interval Incredere: [", round(statistici["IC_Min"], 2), ",", round(statistici["IC_Max"], 2), "]\n")

  # C. 
  df_grafic <- genereaza_date_senzitivitate(
    start = 0.85,  # De la 85% succes
    end = 0.99,    # Pana la 99% succes
    step = 0.01,   # Pas de 1%
    n_zile = 50,   # 50 zile per punct (mai rapid pt test)
    rata_churn_aleator = 0.001,
    cost_churn = 20,
    castig_per_succes = 2
  )
  
  print(df_grafic)
  
  # Desenam Graficul (Profit vs Rata Succes)
  plot(df_grafic$Rata_Succes * 100, df_grafic$Profit_Mediu, 
       type = "b",        # b = both (linii si puncte)
       pch = 19,          # puncte pline
       col = "blue",      # culoare albastra
       lwd = 2,           # grosime linie
       main = "Senzitivitate: Cum influențează calitatea tehnică profitul?",
       xlab = "Rata de Succes a Sistemului (%)",
       ylab = "Profit Mediu Zilnic (EUR)")
  
  # Adaugam linia de faliment (Zero)
  abline(h = 0, col = "red", lty = 2, lwd = 2)
  grid()
}
