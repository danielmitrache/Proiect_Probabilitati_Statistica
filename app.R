library(shiny)

# --- 1. INCARCAREA SURSELOR ---
source("R/01_trafic.R")
source("R/02_latenta.R")
source("R/03_evenimente.R")
source("R/04_bivarianta_NF.R")
source("R/05_bivarianta_NT.R")
source("R/06_conditionari.R")
source("R/07_dependenta.R")
source("R/08_inegalitati.R")
source("R/09_aproximare.R")
source("R/10_churn.R")
source("R/11_impact_economic.R")
source("R/12_vizualizare_statistica.R")


# Define UI
ui <- navbarPage(
  title = "Proiect Probabilitati",
  theme = NULL, 
  
  # --------------------------------------------------------------------------
  # TAB 1: TRAFIC
  # --------------------------------------------------------------------------
  tabPanel("1. Trafic",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Trafic"),
               numericInput("tr_n_zile", "Durata (zile):", value = 730, min = 365), # Am pus 730 (2 ani) ca sa ai ce compara
               
               numericInput("tr_lambda", "Media (Poisson):", value = 1000, min = 10),
               numericInput("tr_n_max", "Capacitate Max (Binomial):", value = 2000, min = 10),
               sliderInput("tr_prob", 
                            "Probabilitate Activ (Binomial):", 
                            value = 0.5, 
                            min = 0.01, 
                            max = 0.99,
                            step = 0.01),               
               actionButton("btn_sim_trafic", "Simulează Trafic", class = "btn-primary"),
               hr(),
               helpText("Apasa butonul pentru a genera datele initiale.")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Sub-tab 1
                 tabPanel("Comparatie Generală (1a, 1d)", 
                          br(),
                          h4("Comparatie pe întreaga perioadă"),
                          plotOutput("plot_tr_comparatie"),
                          h4("Comparatie Densitati"),
                          plotOutput("plot_tr_densitati")
                 ),
                 
                 # Sub-tab 2
                 tabPanel("Analiză Temporală (1b)",
                          br(),
                          p("Selectează un an și o lună pentru a observa variabilitatea traficului."),
                          
                          fluidRow(
                            column(4, selectInput("tr_select_an", "Alege Anul:", choices = NULL)),
                            column(4, selectInput("tr_select_luna", "Alege Luna:", 
                                                  choices = c("Ianuarie"="01", "Februarie"="02", "Martie"="03", 
                                                              "Aprilie"="04", "Mai"="05", "Iunie"="06",
                                                              "Iulie"="07", "August"="08", "Septembrie"="09",
                                                              "Octombrie"="10", "Noiembrie"="11", "Decembrie"="12"))),
                            column(4, selectInput("tr_select_dist", "Distributia:", 
                                                  choices = c("Poisson"="poisson", "Binomiala"="binomiala")))
                          ),
                          hr(),
                          
                          fluidRow(
                            column(6, 
                                   h4("Histograma Anuală"),
                                   plotOutput("plot_tr_anual", height = "300px")
                            ),
                            column(6, 
                                   h4("Histograma Lunară"),
                                   plotOutput("plot_tr_lunar", height = "300px")
                            )
                          )
                 ),
                 
                 # Sub-tab 3
                 tabPanel("Statistici (1c)", 
                          h4("Tabel Comparativ Empiric vs Teoretic"),
                          selectInput("tr_model_stat", "Model analizat:", c("poisson", "binomiala")),
                          tableOutput("tbl_tr_stats"))
               )
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 2: LATENTA
  # --------------------------------------------------------------------------
  tabPanel("2. Latență",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Latență"),
               numericInput("lat_n", "Marime Esantion:", value = 5000, min = 100),
               numericInput("lat_medie", "Timp Mediu (ms):", value = 200, min = 10),
               numericInput("lat_sd", "Deviatie Std (Normala):", value = 50, min = 1),
               
               actionButton("btn_sim_latenta", "Simulează Latențe", class = "btn-primary")
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Vizualizare Grafică",
                          h4("Exponențială vs Normală"),
                          plotOutput("plot_lat_exp", height = "300px"),
                          plotOutput("plot_lat_norm", height = "300px")
                 ),
                 
                 tabPanel("Analiză Statistică",
                          h3("Comparație Indicatori"),
                          tableOutput("tbl_lat_comparatie")
                 )
               )
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 3: EVENIMENTE (EXERCITIUL 3)
  # --------------------------------------------------------------------------
  tabPanel("3. Evenimente",
           sidebarLayout(
             sidebarPanel(
               h4("Simulare Cerere Individuală"),
               numericInput("ev_n_sim", "Număr Simulări (M):", value = 5000, step = 100),
               numericInput("ev_n_max", "Max Încercări (n_max):", value = 3, min = 1, max = 10),
               sliderInput("ev_p_succes", "Probabilitate Succes (p):", min = 0.1, max = 0.99, value = 0.7, step = 0.05),
               
               hr(),
               h5("Parametri de Timp (ms):"),
               numericInput("ev_t0", "Prag SLA (B):", value = 400),
               numericInput("ev_medie_s", "Medie Procesare (S):", value = 150),
               numericInput("ev_backoff", "Timp Backoff Fix:", value = 50),
               
               actionButton("btn_sim_evenimente", "Calculează Probabilități", class = "btn-success")
             ),
             
             mainPanel(
               h3("Rezultate Probabilistice"),
               p("Estimarea empirică a probabilităților pentru evenimentele definite."),
               tableOutput("tbl_ev_probs"),
               
               hr(),
               h4("Validare Teoretică"),
               verbatimTextOutput("txt_ev_validare")
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 8: INEGALITATI (EXERCITIUL 8)
  # --------------------------------------------------------------------------
  tabPanel("8. Inegalități",
           sidebarLayout(
             sidebarPanel(
               h4("Verificare Inegalități"),
               numericInput("in_n_sim", "Nr Simulări:", value = 5000),
               
               h5("Parametri Sistem"),
               numericInput("in_n_max", "Max Încercări:", value = 5),
               sliderInput("in_p", "Probabilitate Succes:", min = 0.1, max = 0.99, value = 0.4),
               numericInput("in_mu", "Medie Procesare:", value = 100),
               numericInput("in_bf", "Backoff:", value = 20),
               numericInput("in_fact", "Factor Latență:", value = 1.5, step = 0.1),
               
               hr(),
               h5("Parametri Bounds"),
               numericInput("in_markov_alpha", "Markov (a = alpha * E[T]):", value = 2.0, step = 0.5),
               numericInput("in_ceb_k", "Cebîșev (k deviatii):", value = 2, min = 1),
               
               actionButton("btn_sim_inegalitati", "Verifică Bounds", class = "btn-danger")
             ),
             
             mainPanel(
               h3("Statistici Desciptive"),
               verbatimTextOutput("txt_in_stats"),
               
               hr(),
               h3("Verificare Bounds"),
               tableOutput("tbl_in_bounds"),
               
               hr(),
               h3("Inegalitatea lui Jensen"),
               p("Verificare pentru functia convexa phi(x) = x^2"),
               verbatimTextOutput("txt_in_jensen")
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 11: IMPACT ECONOMIC (Exercitiul 11) - CONTROL TOTAL
  # --------------------------------------------------------------------------
  tabPanel("11. Impact Economic",
           sidebarLayout(
             sidebarPanel(
               # --- GRUP 1: TRAFIC ---
               h4("1. Model Trafic"),
               selectInput("eco_trafic_model", "Tip Distribuție:", 
                           choices = c("Poisson" = "poisson", "Binomială" = "binomiala")),
               
               # Afisam conditionat in functie de model
               conditionalPanel(
                 condition = "input.eco_trafic_model == 'poisson'",
                 numericInput("eco_trafic_lambda", "Medie Clienți (Lambda):", value = 1000, min = 10)
               ),
               conditionalPanel(
                 condition = "input.eco_trafic_model == 'binomiala'",
                 numericInput("eco_trafic_bin_n", "Capacitate Max (N):", value = 2000, min = 100),
                 numericInput("eco_trafic_bin_p", "Probabilitate (p):", value = 0.5, min = 0, max = 1, step = 0.05)
               ),
               
               hr(),
               
               # --- GRUP 2: PERFORMANTA TEHNICA ---
               h4("2. Performanță Tehnică"),
               numericInput("eco_medie_s", "Viteză Server (Medie S ms):", value = 150, min = 10),
               numericInput("eco_t0", "Timeout Tehnic (t0 ms):", value = 500, min = 50),
               numericInput("eco_n_max", "Max Retry-uri (N_max):", value = 3, min = 1),
               numericInput("eco_backoff", "Backoff (ms):", value = 50, min = 0),
               
               hr(),
               
               # --- GRUP 3: CHURN (PLECARE CLIENTI) ---
               h4("3. Comportament Churn"),
               numericInput("eco_dim_churn", "Fereastra Monitorizare (m):", value = 20, min = 5),
               numericInput("eco_prag_churn", "Prag Erori (k):", value = 5, min = 1),
               sliderInput("eco_churn_rate", "Rata Churn Aleator:", min = 0.000, max = 0.1, value = 0.001, step = 0.001),
               
               hr(),
               
               # --- GRUP 4: FINANCIAR ---
               h4("4. Parametri Financiari"),
               numericInput("eco_castig", "Câștig per Succes (EUR):", value = 2.0, step = 0.1),
               numericInput("eco_cost_churn", "Cost Churn (Pierdere Client):", value = 40, step = 5),
               numericInput("eco_cost_sla", "Penalizare SLA:", value = 2.0, step = 0.5),
               
               hr(),
               
               # --- GRUP 5: SIMULARE SI VARIABILE ---
               h4("5. Setări Simulare (Senzitivitate)"),
               
               numericInput("eco_n_zile", "Durata Simulare (zile):", value = 100, min = 10),
               
               # AICI ESTE PARAMETRUL prob_succes_churn_cond
               helpText("Slider-ul de mai jos controlează 'Probabilitate Succes (Churn Cond)' pe axa X."),
               sliderInput("eco_range", "Interval Rata Succes (Start - Final):", 
                           min = 0.80, max = 0.99, value = c(0.85, 0.99), step = 0.01),
               
               br(),
               actionButton("btn_sim_economic", "Generează Analiza", class = "btn-success btn-lg", width = "100%")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Tab existent - Grafic
                 tabPanel("Analiza Senzitivitate (11c)",
                          br(),
                          plotOutput("plot_eco_senzitivitate", height = "450px"),
                          p(class="text-info", "Graficul arată cum evoluează media profitului când variem calitatea tehnică.")
                 ),
                 
                 # Tab existent - Date
                 tabPanel("Date Grafic",
                          h4("Datele din spatele graficului"),
                          tableOutput("tbl_eco_data")
                 ),
                 
                 # --- TAB NOU PENTRU CERINTA 11 b) ---
                 tabPanel("Statistici Punctuale (11b)",
                          br(),
                          h4("Analiza detaliată pentru o rată de succes fixă"),
                          p("Aici analizăm distribuția profitului zilnic pentru un scenariu specific."),
                          
                          # Input specific pentru acest tab
                          wellPanel(
                            fluidRow(
                              column(6, 
                                     numericInput("eco_fixed_p", "Alege Rata de Succes de analizat (p):", 
                                                  value = 0.95, min = 0.1, max = 1.0, step = 0.01)
                              ),
                              column(6,
                                     h5("Rezultate:"),
                                     verbatimTextOutput("txt_eco_stats_fix")
                              )
                            )
                          ),
                          
                          h4("Distribuția Profitului Zilnic"),
                          plotOutput("plot_eco_hist_fix")
                 )
               )
             )
           )
  ),
  
  
  # --------------------------------------------------------------------------
  # TAB 12: VIZUALIZARE STATISTICA (Exercitiul 12) - PARAMETRI COMPLETI
  # --------------------------------------------------------------------------
  tabPanel("12. Vizualizare",
           sidebarLayout(
             sidebarPanel(
               h4("1. Parametri Generali"),
               numericInput("viz_n_sim", "Număr Simulări (Cereri):", value = 3000, step = 500),
               
               hr(),
               h4("2. Parametri Tehnici Compleți"),
               helpText("Acești parametri controlează simularea fizică a cererilor."),
               
               sliderInput("viz_p_succes", "Probabilitate Succes (p):", min = 0.1, max = 0.99, value = 0.8, step = 0.05),
               numericInput("viz_medie_s", "Viteză Server (Medie S ms):", value = 150, min = 10),
               numericInput("viz_t0", "Timeout (t0 ms):", value = 500, min = 50),
               numericInput("viz_backoff", "Backoff (ms):", value = 50, min = 0),
               
               hr(),
               h4("3. Configurare Scenarii (Boxplot)"),
               numericInput("viz_n_max_A", "Scenariu A (Standard) - Max Retry:", value = 3, min = 1),
               numericInput("viz_n_max_B", "Scenariu B (Alternativ) - Max Retry:", value = 1, min = 1),
               
               hr(),
               h4("4. Parametri Profit (Histogramă)"),
               numericInput("viz_n_zile", "Zile Simulate:", value = 200, min = 50),
               numericInput("viz_castig", "Câștig per Succes:", value = 2.0),
               numericInput("viz_cost_churn", "Cost Churn:", value = 20),
               
               br(),
               actionButton("btn_vizualizare", "Generează Grafice", class = "btn-primary btn-lg", width = "100%")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Tab pentru Histograme (12a)
                 tabPanel("Histograme (12a)",
                          br(),
                          plotOutput("plot_viz_histograme", height = "500px"),
                          div(style = "margin-top: 10px;",
                              p(strong("Interpretare:"), "Histograma din stânga arată distribuția timpului de răspuns."),
                              p("Histograma din dreapta arată distribuția profitului zilnic.")
                          )
                 ),
                 
                 # Tab pentru Boxplot-uri (12b)
                 tabPanel("Boxplot-uri (12b)",
                          br(),
                          plotOutput("plot_viz_boxplots", height = "500px"),
                          div(style = "margin-top: 10px;",
                              p(strong("Stânga:"), "Comparăm timpul consumat de cererile cu SUCCES vs. EȘEC."),
                              p(strong("Dreapta:"), "Comparăm performanța între Scenariul A (Retry = ", textOutput("txt_n_max_A", inline=TRUE), ") și Scenariul B (Retry = ", textOutput("txt_n_max_B", inline=TRUE), ").")
                          )
                 )
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    trafic_data = NULL,
    latenta_exp = NULL,
    latenta_norm = NULL,
    # Rezultate Ex 3
    ev_df = NULL,
    # Rezultate Ex 8
    in_rez = NULL,      # lista raw
    in_teoretic = NULL  # valori teoretice
  )
  
  # --- LOGICA TRAFIC (Ex 1) ---
  observeEvent(input$btn_sim_trafic, {
    rv$trafic_data <- df_trafic_zile(
      n_zile = input$tr_n_zile,
      lambda = input$tr_lambda,
      n_max = input$tr_n_max,
      p = input$tr_prob
    )
    ani_disponibili <- unique(rv$trafic_data$an)
    updateSelectInput(session, "tr_select_an", choices = ani_disponibili)
  })
  
  output$plot_tr_comparatie <- renderPlot({
    req(rv$trafic_data)
    comparatie_histograme_pois_bin(rv$trafic_data)
  })
  
  output$plot_tr_densitati <- renderPlot({
    req(rv$trafic_data)
    comparatie_densitati_suprapuse(rv$trafic_data)
  })
  
  output$plot_tr_anual <- renderPlot({
    req(rv$trafic_data, input$tr_select_an) 
    histograma_an_distrib(rv$trafic_data, an = input$tr_select_an, distributie = input$tr_select_dist)
  })
  
  output$plot_tr_lunar <- renderPlot({
    req(rv$trafic_data, input$tr_select_an, input$tr_select_luna)
    histograma_luna_distrib(rv$trafic_data, luna = input$tr_select_luna, an = input$tr_select_an, distributie = input$tr_select_dist)
  })
  
  output$tbl_tr_stats <- renderTable({
    req(rv$trafic_data)
    df_estimari_empirice(rv$trafic_data, distributie = input$tr_model_stat, lambda = input$tr_lambda, n_max = input$tr_n_max, p = input$tr_prob)
  })
  
  # --- LOGICA LATENTA (Ex 2) ---
  observeEvent(input$btn_sim_latenta, {
    rv$latenta_exp <- simuleaza_latente(input$lat_n, "exponentiala", input$lat_medie)
    rv$latenta_norm <- simuleaza_latente(input$lat_n, "normala", input$lat_medie, input$lat_sd)
  })
  
  output$plot_lat_exp <- renderPlot({
    req(rv$latenta_exp)
    vizualizeaza_latente(rv$latenta_exp, tip = "exponentiala", medie = input$lat_medie)
  })
  
  output$plot_lat_norm <- renderPlot({
    req(rv$latenta_norm)
    vizualizeaza_latente(rv$latenta_norm, tip = "normala", medie = input$lat_medie, sd = input$lat_sd)
  })
  
  output$tbl_lat_comparatie <- renderTable({
    req(rv$latenta_exp, rv$latenta_norm)
    comparatie_statistica_latente(rv$latenta_exp, rv$latenta_norm)
  })

  # --- LOGICA EVENIMENTE (Ex 3) ---
  observeEvent(input$btn_sim_evenimente, {
    # Rulam simularea de M ori folosind functia din R/03_evenimente.R
    rez <- replicate(input$ev_n_sim, 
                     simuleaza_o_cerere(n_max = input$ev_n_max,
                                        p_succes = input$ev_p_succes,
                                        t_0 = input$ev_t0,
                                        medie_S = input$ev_medie_s,
                                        backoff_fix = input$ev_backoff))
    
    # Procesam rezultatele intr-un data frame
    df <- as.data.frame(t(rez))
    df[] <- lapply(df, unlist)
    rv$ev_df <- df
  })
  
  output$tbl_ev_probs <- renderTable({
    req(rv$ev_df)
    df <- rv$ev_df
    
    # Calculam probabilitatile
    p_A <- mean(df$I == 1)
    p_B <- mean(df$T <= input$ev_t0)
    p_C <- mean(df$N <= 2) # Hardcoded n0=2 conform cerintei tipice, sau putem pune input
    p_A_int_B <- mean(df$I == 1 & df$T <= input$ev_t0)
    p_A_union_D <- mean(df$I == 1 | df$D == TRUE)
    
    data.frame(
      Eveniment = c("P(A) [Succes]", "P(B) [Respectare SLA]", "P(C) [Max 2 incercari]", "P(A n B) [Succes Rapid]", "P(A u D) [Reuniune]"),
      Probabilitate = c(p_A, p_B, p_C, p_A_int_B, p_A_union_D)
    )
  })
  
  output$txt_ev_validare <- renderText({
    req(rv$ev_df)
    df <- rv$ev_df
    
    p_A <- mean(df$I == 1)
    p_D <- mean(df$D == TRUE)
    p_A_int_D <- mean(df$I == 1 & df$D == TRUE)
    
    calc_reuniune <- p_A + p_D - p_A_int_D
    empiric_reuniune <- mean(df$I == 1 | df$D == TRUE)
    
    paste0("Verificare Formula Reuniunii P(A u D):\n",
           "Calculat (P(A)+P(D)-P(AnD)): ", round(calc_reuniune, 5), "\n",
           "Empiric (din date):          ", round(empiric_reuniune, 5), "\n",
           "Diferenta:                   ", abs(round(calc_reuniune - empiric_reuniune, 7)))
  })

  # --- LOGICA INEGALITATI (Ex 8) ---
  observeEvent(input$btn_sim_inegalitati, {
    # 1. Simulare
    # Folosim functia din R/08_inegalitati.R
    rv$in_rez <- replicate(input$in_n_sim, 
                           simuleaza_avansat(n_max = input$in_n_max, 
                                             p_succes = input$in_p, 
                                             medie_S_initial = input$in_mu, 
                                             backoff_fix = input$in_bf, 
                                             factor_latenta = input$in_fact), 
                           simplify = FALSE)
    
    # 2. Valori Teoretice
    rv$in_teoretic <- calculeaza_teoretic_T(n_max = input$in_n_max, 
                                            p = input$in_p, 
                                            mu = input$in_mu, 
                                            backoff = input$in_bf, 
                                            factor = input$in_fact)
  })
  
  output$txt_in_stats <- renderText({
    req(rv$in_rez, rv$in_teoretic)
    T_vals <- sapply(rv$in_rez, function(x) x$timp)
    
    paste0("Media Empirica:   ", round(mean(T_vals), 2), 
           " vs Teoretica: ", round(rv$in_teoretic$media, 2), "\n",
           "Varianta Empirica:", round(var(T_vals), 2), 
           " vs Teoretica: ", round(rv$in_teoretic$varianta, 2))
  })
  
  output$tbl_in_bounds <- renderTable({
    req(rv$in_rez, rv$in_teoretic)
    T_vals <- sapply(rv$in_rez, function(x) x$timp)
    mu_T <- rv$in_teoretic$media
    var_T <- rv$in_teoretic$varianta
    
    # Markov
    a_val <- input$in_markov_alpha * mu_T
    p_markov_emp <- mean(T_vals >= a_val)
    bound_markov <- 1 / input$in_markov_alpha
    
    # Cebisev
    dist <- input$in_ceb_k * sqrt(var_T)
    p_ceb_emp <- mean(abs(T_vals - mu_T) >= dist)
    bound_ceb <- 1 / (input$in_ceb_k^2)
    
    data.frame(
      Inegalitate = c(paste0("Markov (alpha=", input$in_markov_alpha, ")"), 
                      paste0("Cebisev (k=", input$in_ceb_k, ")")),
      Empiric = c(p_markov_emp, p_ceb_emp),
      Limita_Teoretica = c(bound_markov, bound_ceb),
      Respectat = c(p_markov_emp <= bound_markov, p_ceb_emp <= bound_ceb)
    )
  })
  
  output$txt_in_jensen <- renderText({
    req(rv$in_rez)
    T_vals <- sapply(rv$in_rez, function(x) x$timp)
    
    # Functie convexa simpla: x^2
    lhs <- mean(T_vals)^2       # phi(E[T])
    rhs <- mean(T_vals^2)       # E[phi(T)]
    
    paste0("Functia phi(x) = x^2 (Convexa)\n",
           "Stanga (phi(E[T])): ", round(lhs, 2), "\n",
           "Dreapta (E[phi(T)]): ", round(rhs, 2), "\n",
           "Verificare (Stanga <= Dreapta): ", lhs <= rhs)
  })
  
  # --- LOGICA ECONOMIC (TAB 11) ---
  
  eco_data <- eventReactive(input$btn_sim_economic, {
    showNotification("Se rulează simularea economică complexă...", type = "message", duration = 2)
    
    # Validare simpla pentru trafic binomial
    p_bin <- input$eco_trafic_bin_p
    if(is.na(p_bin) || p_bin < 0 || p_bin > 1) p_bin <- 0.5
    
    # Apelam functia principala
    df_rezultat <- genereaza_date_senzitivitate(
      # Parametrii pentru bucla de senzitivitate (Axa X a graficului)
      start = input$eco_range[1], 
      end = input$eco_range[2], 
      step = 0.01,
      
      # --- TOTI CEILALTI PARAMETRI SUNT TRANSMI SI PRIN '...' ---
      
      # 1. Simulare Generala
      n_zile = input$eco_n_zile,
      
      # 2. Model Trafic (Noii parametrii adaugati)
      model_trafic = input$eco_trafic_model,
      lambda_trafic = input$eco_trafic_lambda,
      nmax_trafic_bin = input$eco_trafic_bin_n,
      p_trafic_bin = p_bin,
      
      # 3. Financiar
      castig_per_succes = input$eco_castig,
      cost_churn = input$eco_cost_churn,
      cost_sla = input$eco_cost_sla,
      
      # 4. Churn
      rata_churn_aleator = input$eco_churn_rate,
      dim_churn_cond = input$eco_dim_churn,       
      prag_erori_churn_cond = input$eco_prag_churn, 
      
      # 5. Tehnic / Performanta
      n_max = input$eco_n_max,
      backoff_fix = input$eco_backoff,
      t_0 = input$eco_t0,
      medie_S = input$eco_medie_s
    )
    
    return(df_rezultat)
  })
  
  # Randare Grafic
  output$plot_eco_senzitivitate <- renderPlot({
    req(eco_data())
    df <- eco_data()
    
    y_min <- min(df$Profit_Mediu)
    y_max <- max(df$Profit_Mediu)
    
    # Ajustam limitele sa arate bine
    if (y_min == y_max) { y_min <- y_min - 10; y_max <- y_max + 10 }
    
    plot(df$Rata_Succes * 100, df$Profit_Mediu, 
         type = "b", pch = 19, col = "darkblue", lwd = 2,
         ylim = c(y_min, y_max),
         main = "Analiza de Senzitivitate Completa",
         xlab = "Rata de Succes a Sistemului (%)", 
         ylab = "Profit Mediu Zilnic (EUR)")
    
    abline(h = 0, col = "red", lty = 2, lwd = 2)
    grid()
    
    # Adaugam text pentru pragul de rentabilitate daca exista
    intersectie <- approx(df$Profit_Mediu, df$Rata_Succes, xout = 0)$y
    if (!is.na(intersectie)) {
      abline(v = intersectie * 100, col = "darkgreen", lty = 3)
      text(x = intersectie * 100, y = y_min, 
           labels = paste0("Prag: ", round(intersectie * 100, 1), "%"), 
           col = "darkgreen", pos = 4)
    }
  })
  
  # Randare Tabel
  output$tbl_eco_data <- renderTable({
    req(eco_data())
    eco_data()
  })
  
  # Calculam vectorul de profituri zilnice pentru scenariul FIX
  eco_sim_fixa <- eventReactive(input$btn_sim_economic, {
    
    # Rulam functia care returneaza vectorul brut de profituri (nu doar media)
    profituri <- simuleaza_scenariu_economic(
      n_zile = input$eco_n_zile,
      
      # Parametri fixi din UI
      model_trafic = input$eco_trafic_model,
      lambda_trafic = input$eco_trafic_lambda,
      nmax_trafic_bin = input$eco_trafic_bin_n,
      p_trafic_bin = input$eco_trafic_bin_p,
      
      castig_per_succes = input$eco_castig,
      cost_churn = input$eco_cost_churn,
      cost_sla = input$eco_cost_sla,
      rata_churn_aleator = input$eco_churn_rate,
      
      dim_churn_cond = input$eco_dim_churn,       
      prag_erori_churn_cond = input$eco_prag_churn, 
      
      n_max = input$eco_n_max,
      backoff_fix = input$eco_backoff,
      t_0 = input$eco_t0,
      medie_S = input$eco_medie_s,
      
      # AICI ESTE CHEIA: Folosim p-ul ales in tab-ul 3, nu intervalul
      prob_succes_churn_cond = input$eco_fixed_p 
    )
    
    return(profituri)
  })
  
  # Afisam Textul cu Media, Varianta, Interval de Incredere
  output$txt_eco_stats_fix <- renderPrint({
    req(eco_sim_fixa())
    vals <- eco_sim_fixa()
    
    # Folosim functia ta existenta pentru calcule
    stats <- calculeaza_statistici_profit(vals)
    
    cat("Statistici pentru p =", input$eco_fixed_p, ":\n")
    cat("---------------------------------\n")
    cat("Media Profitului:     ", round(stats["Media"], 2), "EUR\n")
    cat("Varianța Profitului:  ", round(stats["Varianta"], 2), "\n")
    cat("Deviația Standard:    ", round(sqrt(stats["Varianta"]), 2), "\n")
    cat("Interval Încredere (95%): [", round(stats["IC_Min"], 2), ", ", round(stats["IC_Max"], 2), "]\n")
    
    if(stats["Media"] < 0) {
      cat("\nATENȚIE: Sistemul este în pierdere!\n")
    } else {
      cat("\nSistemul este profitabil.\n")
    }
  })
  
  # Afisam Histograma Profiturilor
  output$plot_eco_hist_fix <- renderPlot({
    req(eco_sim_fixa())
    vals <- eco_sim_fixa()
    
    media <- mean(vals)
    
    hist(vals, breaks = 20, col = "lightgreen", border = "white",
         main = paste("Histograma Profitului Zilnic (p =", input$eco_fixed_p, ")"),
         xlab = "Profit Zilnic (EUR)", ylab = "Frecvență")
    
    abline(v = media, col = "blue", lwd = 3)
    legend("topright", legend = c("Media"), col = c("blue"), lwd = 3)
  })
  
  # --- LOGICA VIZUALIZARE (TAB 12) ---
  
  # Variabila reactiva pentru datele de vizualizare
  viz_data <- eventReactive(input$btn_vizualizare, {
    showNotification("Se generează graficele statistice...", type = "message", duration = 2)
    
    # 1. Generam date pentru Histograme (12a)
    # a) Vector Timp (folosim Scenariul A si parametrii tehnici din UI)
    df_timp <- simuleaza_lot_cereri(n_simulari = input$viz_n_sim, 
                                    n_max = input$viz_n_max_A,
                                    # Parametrii adaugati:
                                    p_succes = input$viz_p_succes,
                                    t_0 = input$viz_t0,
                                    medie_S = input$viz_medie_s,
                                    backoff_fix = input$viz_backoff)
    
    # b) Vector Profit
    vec_profit <- replicate(input$viz_n_zile, {
      rez <- simuleaza_profit_zi(nr_clienti = 1000, 
                                 castig_per_succes = input$viz_castig,
                                 cost_churn = input$viz_cost_churn,
                                 # Transmitem parametrii tehnici si aici:
                                 n_max = input$viz_n_max_A,
                                 prob_succes_churn_cond = input$viz_p_succes, # Mapam p_succes aici
                                 t_0 = input$viz_t0,
                                 medie_S = input$viz_medie_s,
                                 backoff_fix = input$viz_backoff)
      return(rez["Profit"])
    })
    
    # 2. Generam date pentru Boxplots (12b) - Scenariul B
    df_scenariu_B <- simuleaza_lot_cereri(n_simulari = input$viz_n_sim,
                                          n_max = input$viz_n_max_B,
                                          # Parametrii tehnici (aceiasi ca la A, difera doar n_max)
                                          p_succes = input$viz_p_succes,
                                          t_0 = input$viz_t0,
                                          medie_S = input$viz_medie_s,
                                          backoff_fix = input$viz_backoff)
    
    list(
      timp_A = df_timp$T,
      profit = vec_profit,
      df_A = df_timp,         
      df_B = df_scenariu_B    
    )
  })
  
  # Randare Histograme
  output$plot_viz_histograme <- renderPlot({
    req(viz_data())
    data <- viz_data()
    plot_histograme_12a(vector_timp = data$timp_A, vector_profit = data$profit)
  })
  
  # Randare Boxplots
  output$plot_viz_boxplots <- renderPlot({
    req(viz_data())
    data <- viz_data()
    plot_boxplots_12b(df_cereri = data$df_A, df_scenariu_1 = data$df_A, df_scenariu_2 = data$df_B)
  })
  
  # Output-uri text mici pentru descriere
  output$txt_n_max_A <- renderText({ input$viz_n_max_A })
  output$txt_n_max_B <- renderText({ input$viz_n_max_B })
}

shinyApp(ui = ui, server = server)