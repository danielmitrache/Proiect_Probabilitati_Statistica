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
  # TAB 4: IMPACT ECONOMIC (EXERCITIUL 11 & 12) - PASTRAT (Optional redenumim in 11)
  # --------------------------------------------------------------------------
   tabPanel("11. Impact Economic",
             # ... (Poti adauga UI-ul existent pentru ex 11 daca e cazul, 
             # dar dat fiind ca nu era in snippet-ul initial explicit, il lasam placeholder 
             # sau il omitem daca nu e cerut explicit acum. Presupunem ca userul vrea doar 3 si 8 adaugate.)
             h4("Sectiune Rezervata pentru Analiza Economica (deja implementata in R/11)")
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
}

shinyApp(ui = ui, server = server)