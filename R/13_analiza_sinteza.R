# R/13_analiza_sinteza.R
# Exercitiul 13: Analiza de Sinteza

analiza_sinteza <- function() {
  cat("\n================================================================\n")
  cat("           EXERCITIUI 13: ANALIZA DE SINTEZA A SISTEMULUI       \n")
  cat("================================================================\n\n")
  
  # a) Rolul probabilitatii empirice
  cat("a) ROLUL PROBABILITATII EMPIRICE\n")
  cat("----------------------------------------------------------------\n")
  cat("   In acest proiect, probabilitatea empirica (obtinuta prin simulari Monte Carlo)\n")
  cat("   a servit drept mecanism de VALIDARE pentru modelele teoretice.\n")
  cat("   De exemplu, in ex. 8, am comparat media empirica a timpului de raspuns\n")
  cat("   cu media teoretica. Convergenta valorilor pentru N mare a confirmat\n")
  cat("   corectitudinea implementarii. De asemenea, cand distributiile sunt\n")
  cat("   prea complexe pentru a fi calculate analitic (ex: sistem cu backoff\n")
  cat("   exponential si churn dinamic), simularea ramane singura metoda viabila.\n\n")
  
  # b) Informatiile aduse de conditionari
  cat("b) INFORMATIILE ADUSE DE CONDITIONARI\n")
  cat("----------------------------------------------------------------\n")
  cat("   Conditionarea reduce incertitudinea (entropia) sistemului.\n")
  cat("   1. Diagnostic: Stiind ca un client a plecat (Churn), probabilitatea ca\n")
  cat("      el sa fi intampinat erori tehnice creste (Bayes).\n")
  cat("   2. Predictie: Cunoscand istoricul recent (ultimele k cereri), putem\n")
  cat("      estima mult mai precis riscul de esec imediat decat folosind media generala.\n")
  cat("   Acest lucru permite sisteme adaptive: daca P(Esec | Istoric) > prag,\n")
  cat("   declansam masuri preventive (ex: circuit breaker).\n\n")
  
  # c) Utilitatea inegalitatilor probabilistice
  cat("c) UTILITATEA INEGALITATILOR PROBABILISTICE\n")
  cat("----------------------------------------------------------------\n")
  cat("   Inegalitatile (Markov, Cebisev, Chernoff) ofera garantii de tip WORST-CASE.\n")
  cat("   - Markov ne da o limita superioara 'hard' pentru media cozilor/timpilor.\n")
  cat("   - Cebisev ne arata stabilitatea sistemului (cat de rar deviem de la medie).\n")
  cat("   - Chernoff este esentiala pentru SLA (Service Level Agreements), oferind\n")
  cat("     limite exponentiale pentru evenimente rare dar catastrofale (ex: caderea\n")
  cat("     simultana a serverelor). Ele permit dimensionarea sigura a resurselor.\n\n")
  
  # d) Legatura performanta tehnica - impact economic
  cat("d) LEGATURA PERFORMANTA TEHNICA - IMPACT ECONOMIC\n")
  cat("----------------------------------------------------------------\n")
  cat("   Relatia nu este liniara, ci puternic asimetrica.\n")
  cat("   - O scadere mica a performantei tehnice (ex: p_succes scade de la 99% la 95%)\n")
  cat("     poate duce la pierderi economice disproportionate din cauza efectului de\n")
  cat("     compunere (utilizatorii pleaca, viralitate negativa - Churn).\n")
  cat("   - Exista un punct de 'diminishing returns': imbunatatirea de la 99.9% la \n")
  cat("     99.99% costa enorm tehnic, dar poate aduce beneficii marginale economic,\n")
  cat("     daca utilizatorii nu percep diferenta.\n\n")
  
  # e) Parametri cheie si imbunatatiri
  cat("e) PARAMETRI CHEIE SI IMBUNATATIRI\n")
  cat("----------------------------------------------------------------\n")
  cat("   Cei mai influenti parametri identificati sunt:\n")
  cat("   1. Probabilitatea de succes primara (p_succes): influenteaza tot lantul.\n")
  cat("   2. Factorul de Backoff: determina cat de repede se congestioneaza sistemul.\n\n")
  cat("   MODIFICARI PROPUSE:\n")
  cat("   - Backoff Adaptiv: In loc de backoff fix/exponential simplu, sistemul sa\n")
  cat("     primeasca feedback de la server (ex: 'retry-after' header).\n")
  cat("   - Prioritizare Inteligenta: Cererile clientilor fideli (Customer Lifetime\n")
  cat("     Value mare) sa aiba prioritate la resurse in momentele de criza.\n")
  cat("================================================================\n")
}

if(sys.nframe() == 0) {
  analiza_sinteza()
}
