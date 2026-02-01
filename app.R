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
  #TAB 4: BIVARIANTA (N, F) (EXERCITIUL 4)
  # --------------------------------------------------------------------------
  
  tabPanel("4. (N,F)",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Simualare (N,F)"),
               numericInput("nf_M","Număr Simulări (M):",value=5000,step=500,min=500),
               numericInput("nf_n_max","Max Incercări(n_max):", value=3, min=1, max=10),
               sliderInput("nf_p_succes","Probabilitate Succes (p):",min=0.1,max=0.99,value=0.7,step=0.05),
               
               hr(),
               h5("Parametri de Timp (ms):"),
               numericInput("nf_t0","Prag SLA (t0):",value=400),
               numericInput("nf_medie_s","Medie Procesare(S):", value=150),
               numericInput("nf_backoff","Timp Backoff Fix:",value=50),
               
               actionButton("btn_sim_nf","Generează (N,F)", class ="btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Distribuții",
                          h4("Distribuția comună P(N=n, F=f) (empiric)"),
                          tableOutput("tbl_nf_joint"),
                          
                          hr(),
                          h4("Distribuții marginale"),
                          fluidRow(
                            column(6,h5("P(N=n)"),tableOutput("tbl_nf_marg_N")),
                            column(6,h5("P(F=f)"),tableOutput("tbl_nf_marg_F"))
                          )
                 ),
                 tabPanel("Test Independență",
                          h4("Test Chi-patrat pentru independență (N,F)"),
                          verbatimTextOutput("txt_nf_chi")
                 ),
                 tabPanel("Vizualizare",
                          h4("Heatmap+Mosaicplot"),
                          plotOutput("plot_nf_heat_mosaic",height="450px")
                 )
               )
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 5: BIVARIANTA (N, T) (EXERCITIUL 5)
  # --------------------------------------------------------------------------
  tabPanel("5. (N, T)",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Simulare (N, T)"),
               numericInput("nt_M", "Număr Simulări (M):", value = 5000, step = 500, min = 500),
               numericInput("nt_n_max", "Max Încercări (n_max):", value = 3, min = 1, max = 10),
               sliderInput("nt_p_succes", "Probabilitate Succes (p):", min = 0.1, max = 0.99, value = 0.7, step = 0.05),
               
               hr(),
               h5("Parametri de Timp (ms):"),
               numericInput("nt_t0", "Prag SLA (t0):", value = 400),
               numericInput("nt_medie_s", "Medie Procesare (S):", value = 150),
               numericInput("nt_backoff", "Timp Backoff Fix:", value = 50),
               
               actionButton("btn_sim_nt", "Generează (N, T)", class = "btn-primary")
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Scatterplot",
                          h4("Reprezentare: N vs T"),
                          plotOutput("plot_nt_scatter", height = "450px")
                 ),
                 
                 tabPanel("Statistici",
                          h4("Medii / Var / Cov / Corelație"),
                          tableOutput("tbl_nt_stats"),
                          hr(),
                          verbatimTextOutput("txt_nt_interp")
                 )
               )
             )
           )
  ),
  
  # --------------------------------------------------------------------------
  # TAB 6: CONDITIONARI (EXERCITIUL 6)
  # --------------------------------------------------------------------------
  tabPanel("6. Condiționări",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Simulare (condiționări)"),
               numericInput("cond_M", "Număr Simulări (M):", value = 5000, step = 500, min = 500),
               numericInput("cond_n_max", "Max Încercări (n_max):", value = 3, min = 1, max = 10),
               sliderInput("cond_p_succes", "Probabilitate Succes (p):", min = 0.1, max = 0.99, value = 0.7, step = 0.05),
               
               hr(),
               h5("Parametri de Timp (ms):"),
               numericInput("cond_t0", "Prag SLA (t0):", value = 400),
               numericInput("cond_medie_s", "Medie Procesare (S):", value = 150),
               numericInput("cond_backoff", "Timp Backoff Fix:", value = 50),
               
               hr(),
               h5("Prag pentru N (n0):"),
               numericInput("cond_n0", "n0 (pentru evenimentul C: N <= n0):", value = 2, min = 1, max = 10),
               
               actionButton("btn_sim_cond", "Calculează condiționări", class = "btn-success")
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Probabilități",
                          h4("Probabilități condiționate"),
                          tableOutput("tbl_cond_probs")
                 ),
                 
                 tabPanel("Speranțe condiționate",
                          h4("E(T | I = 1) și E(T | I = 0)"),
                          tableOutput("tbl_cond_expect"),
                          hr(),
                          plotOutput("plot_cond_box", height = "350px")
                 ),
                 
                 tabPanel("Interpretare",
                          verbatimTextOutput("txt_cond_interp")
                 )
               )
             )
           )
  ),
  
  
  # --------------------------------------------------------------------------
  # TAB 7: DEPENDENTA (EXERCITIUL 7)
  # --------------------------------------------------------------------------
  tabPanel("7. Dependență",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Simulare Dependență"),
               numericInput("dep_n_sim", "Număr Simulări (M):", value = 5000, min = 100, step = 500),
               numericInput("dep_n_max", "Max Încercări (n_max):", value = 3, min = 1, max = 10),
               sliderInput("dep_p_succes", "Probabilitate Succes (p):", 
                           min = 0.1, max = 0.99, value = 0.7, step = 0.05),
               
               hr(),
               h5("Parametri de Timp"),
               numericInput("dep_medie_s", "Medie Procesare Inițială (ms):", value = 150, min = 10),
               numericInput("dep_backoff", "Timp Backoff Fix (ms):", value = 50, min = 0),
               numericInput("dep_factor_latenta", "Factor Latență (>1 = dependent):", 
                            value = 2.0, min = 1.0, max = 5.0, step = 0.1),
               
               hr(),
               helpText("Factor = 1.0: Timpii sunt independenți"),
               helpText("Factor > 1.0: Latența crește după fiecare eșec (dependență)"),
               
               br(),
               actionButton("btn_dep_simuleaza", "Compară Scenarii", class = "btn-warning btn-lg", width = "100%")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Sub-tab 1: Comparație Grafică
                 tabPanel("Distribuții (7a)",
                          br(),
                          h4("Comparație: Independent vs Dependent"),
                          p("Acest grafic compară distribuția timpului total pentru scenariile:"),
                          tags$ul(
                            tags$li(strong("Independent (albastru):"), " Factor = 1.0 - timpii nu depind de eșecuri anterioare"),
                            tags$li(strong("Dependent (roșu):"), " Factor = ", textOutput("txt_dep_factor", inline = TRUE), 
                                    " - latența crește după fiecare eșec")
                          ),
                          
                          plotOutput("plot_dep_distributii", height = "450px")
                 ),
                 
                 # Sub-tab 2: Statistici Comparative
                 tabPanel("Statistici (7b)",
                          br(),
                          h4("Statistici Comparative"),
                          p("Comparația numerică între cele două scenarii:"),
                          tableOutput("tbl_dep_comparatie"),
                          
                          hr(),
                          div(style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px;",
                              h5("Interpretare"),
                              p("În cazul dependent, după fiecare eșec latența medie crește cu factorul specificat."),
                              p("Aceasta modelează situații reale unde eșecurile succesive degradează performanța sistemului."),
                              p(strong("Impact observat:"), "Media și varianta timpului total sunt semnificativ mai mari în cazul dependent.")
                          )
                 ),
                 
                 # Sub-tab 3: Concluzii
                 tabPanel("Concluzii (7c)",
                          br(),
                          h4("Concluzii privind Riscul și Stabilitatea Sistemului"),
                          
                          div(style = "background-color: #fff9e6; padding: 20px; border-left: 5px solid #ffa500; margin-bottom: 20px;",
                              h5(strong("Impact asupra Riscului Operațional")),
                              p("Dependența dintre încercările succesive crește semnificativ riscul operațional:"),
                              tags$ul(
                                tags$li("Timpul mediu de răspuns crește exponențial cu numărul de eșecuri"),
                                tags$li("Variabilitatea (varianta) este mult mai mare în scenariul dependent"),
                                tags$li("Probabilitatea de a depăși pragurile SLA crește dramatic")
                              )
                          ),
                          
                          div(style = "background-color: #ffe6e6; padding: 20px; border-left: 5px solid #dc3545; margin-bottom: 20px;",
                              h5(strong("Stabilitatea Sistemului")),
                              p("Degradarea progresivă a performanței indică instabilitate:"),
                              tags$ul(
                                tags$li("Sistemul devine mai lent odată ce începe să eșueze"),
                                tags$li("Riscul de cascadă a eșecurilor (failure cascade)"),
                                tags$li("Necesitatea de circuit breakers și mecanisme de protecție")
                              )
                          ),
                          
                          div(style = "background-color: #e6f7ff; padding: 20px; border-left: 5px solid #0066cc;",
                              h5(strong("Recomandări")),
                              tags$ul(
                                tags$li(strong("Monitorizare proactivă:"), " Detectarea timpurie a degradării performanței"),
                                tags$li(strong("Limite de retry adaptive:"), " Reducerea numărului de încercări când sistemul este sub stres"),
                                tags$li(strong("Load balancing:"), " Distribuirea sarcinii pentru a evita suprasolicitarea"),
                                tags$li(strong("Timeout-uri dinamice:"), " Ajustarea pragurilor în funcție de starea sistemului")
                              )
                          )
                 )
               )
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
  # TAB 9: APROXIMARE (EXERCITIUL 9)
  # --------------------------------------------------------------------------
  tabPanel("9. Aproximare",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Aproximare TLC"),
               helpText("Teorema Limită Centrală: Suma latențelor tinde spre distribuție normală"),
               
               numericInput("aprox_m_zile", "Număr Zile Simulate (M):", value = 1000, min = 100, step = 100),
               numericInput("aprox_lambda", "Medie Trafic Zilnic (λ):", value = 1000, min = 10),
               numericInput("aprox_mu", "Medie Latență per Cerere (μ, ms):", value = 150, min = 10),
               
               hr(),
               h5("Parametri Teoretici"),
               p(strong("Media teoretică:"), "λ × μ"),
               p(strong("Dev. Std teoretică:"), "√(2λ) × μ"),
               
               br(),
               actionButton("btn_aprox_simuleaza", "Simulează", class = "btn-info btn-lg", width = "100%")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Sub-tab 1: Vizualizare Grafică
                 tabPanel("Histogramă și Aproximare (9a)",
                          br(),
                          h4("Distribuția Sumei Totale Zilnice"),
                          p("Pentru fiecare zi, generăm numărul de cereri (Poisson) și sumăm latențele (Exponențiale)."),
                          p("Conform TLC, suma totală tinde spre o distribuție normală."),
                          
                          plotOutput("plot_aprox_histograma", height = "500px"),
                          
                          div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 15px;",
                              h5("Despre Aproximarea Normală"),
                              p("Curba roșie reprezintă distribuția normală teoretică."),
                              p("Histograma albastră reprezintă datele empirice din simulare."),
                              p("Dacă TLC se aplică, cele două ar trebui să se suprapună bine.")
                          )
                 ),
                 
                 # Sub-tab 2: Validare Statistică
                 tabPanel("Validare Statistică (9b)",
                          br(),
                          h4("Validare Statistică"),
                          p("Comparația dintre valorile empirice (din simulare) și cele teoretice (normale):"),
                          
                          tableOutput("tbl_aprox_validare"),
                          
                          hr(),
                          div(style = "background-color: #f0f9ff; padding: 20px; border-left: 5px solid #0288d1;",
                              h5(strong("Formulele Teoretice")),
                              p(strong("Media teoretică:"), " E[Σ S_i] = E[N] × E[S] = λ × μ"),
                              p(strong("Varianta teoretică:"), " Var[Σ S_i] = E[N] × Var[S] + Var[N] × E[S]² = λ × μ² + λ × μ² = 2λμ²"),
                              p(strong("Deviația Standard:"), " σ = √(2λμ²) = √(2λ) × μ")
                          ),
                          
                          hr(),
                          div(style = "background-color: #fff3e0; padding: 20px; border-left: 5px solid #ff9800;",
                              h5(strong("Interpretare")),
                              p("Dacă diferențele sunt mici (< 5%), aproximarea normală este validă."),
                              p("TLC funcționează bine când λ este suficient de mare (>30 cereri/zi)."),
                              p("Aceasta permite predicții rapide fără simulări complexe.")
                          )
                 )
               )
             )
           )
  ),
  
  
  # --------------------------------------------------------------------------
  # TAB 10: CHURN (EXERCITIUL 10)
  # --------------------------------------------------------------------------
  tabPanel("10. Churn",
           sidebarLayout(
             sidebarPanel(
               h4("Parametri Analiză Churn"),
               numericInput("churn_M", "Număr Utilizatori (M):", value = 10000, min = 100, step = 1000),
               
               hr(),
               h5("1. Churn Aleator"),
               numericInput("churn_q", "Rata Churn Aleator (q):", value = 0.05, min = 0.01, max = 0.5, step = 0.01),
               helpText("Probabilitatea că un utilizator pleacă din motive externe"),
               
               hr(),
               h5("2. Churn Condiționat de Performanță"),
               numericInput("churn_m", "Fereastră Monitorizare (m cereri):", value = 20, min = 5, max = 100),
               numericInput("churn_k", "Prag Erori (k):", value = 5, min = 1, max = 50),
               sliderInput("churn_p_succes", "Probabilitate Succes Tehnic (p):", 
                           min = 0.5, max = 0.99, value = 0.9, step = 0.01),
               helpText("Utilizatorul pleacă dacă are ≥ k esecuri în m încercări"),
               
               br(),
               actionButton("btn_churn_calculeaza", "Calculează Rate Churn", class = "btn-primary btn-lg", width = "100%")
             ),
             
             mainPanel(
               h3("Rezultate Analiză Churn"),
               
               tableOutput("tbl_churn_rate"),
               
               hr(),
               h4("Explicație Detaliată"),
               verbatimTextOutput("txt_churn_explicatie"),
               
               hr(),
               div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
                   h5("Modelul de Churn"),
                   p(strong("Churn Aleator:"), "Modelat cu distribuție Bernoulli (q)."),
                   p(strong("Churn Condiționat:"), "Bazat pe numărul de eșecuri (Binomial)."),
                   p(strong("Churn Total:"), "Reuniunea celor două evenimente (operatorul OR logic).")
               )
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
               
               numericInput("eco_n_zile", "Durata Simulare (zile):", value = 10, min = 5),
               
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
    in_teoretic = NULL, # valori teoretice
    # Rezultate Ex 7 - Dependenta
    dep_timp_indep = NULL,
    dep_timp_dep = NULL,
    # Rezultate Ex 9 - Aproximare
    aprox_sume = NULL,
    # Rezultate Ex 10 - Churn
    churn_rezultate = NULL
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
  
  # ============================================================================
  # EXERCITIUL 7: DEPENDENTA - Server Logic
  # ============================================================================
  
  # Server logic pentru Ex 7 - Dependenta
  observeEvent(input$btn_dep_simuleaza, {
    # Simulare cazul INDEPENDENT (factor = 1.0)
    rv$dep_timp_indep <- replicate(input$dep_n_sim, 
                                    simuleaza_cerere_dependenta(
                                      n_max = input$dep_n_max,
                                      p_succes = input$dep_p_succes,
                                      medie_S_initial = input$dep_medie_s,
                                      backoff_fix = input$dep_backoff,
                                      factor_latenta = 1.0
                                    ))
    
    # Simulare cazul DEPENDENT (factor = input)
    rv$dep_timp_dep <- replicate(input$dep_n_sim,
                                  simuleaza_cerere_dependenta(
                                    n_max = input$dep_n_max,
                                    p_succes = input$dep_p_succes,
                                    medie_S_initial = input$dep_medie_s,
                                    backoff_fix = input$dep_backoff,
                                    factor_latenta = input$dep_factor_latenta
                                  ))
  })
  
  output$plot_dep_distributii <- renderPlot({
    req(rv$dep_timp_indep, rv$dep_timp_dep)
    
    # Limitare axa X pentru vizualizare
    x_max <- quantile(rv$dep_timp_dep, 0.99)
    
    par(mfrow = c(1, 1))
    plot(density(rv$dep_timp_dep), col = "red", lwd = 2, 
         xlim = c(0, x_max),
         main = "Comparație: Independent vs Dependent",
         xlab = "Timp Total (ms)", ylab = "Densitate")
    lines(density(rv$dep_timp_indep), col = "blue", lwd = 2)
    legend("topright", 
           legend = c("Dependent", "Independent"), 
           col = c("red", "blue"), 
           lwd = 2)
  })
  
  output$tbl_dep_comparatie <- renderTable({
    req(rv$dep_timp_indep, rv$dep_timp_dep)
    
    data.frame(
      Scenariu = c("Independent (factor=1.0)", 
                   paste0("Dependent (factor=", input$dep_factor_latenta, ")")),
      Media = c(mean(rv$dep_timp_indep), mean(rv$dep_timp_dep)),
      Varianta = c(var(rv$dep_timp_indep), var(rv$dep_timp_dep)),
      Dev_Std = c(sd(rv$dep_timp_indep), sd(rv$dep_timp_dep))
    )
  })
  
  # ============================================================================
  # EXERCITIUL 9: APROXIMARE - Server Logic
  # ============================================================================
  
  # Server logic pentru Ex 9 - Aproximare
  observeEvent(input$btn_aprox_simuleaza, {
    rv$aprox_sume <- replicate(input$aprox_m_zile,
                                simuleaza_suma_totala(
                                  lambda_trafic = input$aprox_lambda,
                                  medie_latenta = input$aprox_mu
                                ))
  })
  
  output$plot_aprox_histograma <- renderPlot({
    req(rv$aprox_sume)
    
    # Parametri teoretici pentru distributia normala
    media_teoretic <- input$aprox_lambda * input$aprox_mu
    sd_teoretic <- sqrt(2 * input$aprox_lambda) * input$aprox_mu
    
    par(mfrow = c(1, 1))
    hist(rv$aprox_sume, 
         probability = TRUE,
         breaks = 30,
         col = "cornflowerblue",
         border = "white",
         main = "Latență Totală Zilnică cu Aproximare Normală",
         xlab = "Latență Totală (ms/zi)",
         ylab = "Densitate")
    
    # Adaugam curba teoretica normala
    curve(dnorm(x, mean = media_teoretic, sd = sd_teoretic),
          add = TRUE,
          col = "red",
          lwd = 2)
    
    legend("topright",
           legend = c("Date Empirice", "Aproximare Normală"),
           col = c("cornflowerblue", "red"),
           lwd = c(10, 2))
  })
  
  output$tbl_aprox_validare <- renderTable({
    req(rv$aprox_sume)
    
    # Statistici empirice
    media_emp <- mean(rv$aprox_sume)
    sd_emp <- sd(rv$aprox_sume)
    
    # Statistici teoretice
    media_teor <- input$aprox_lambda * input$aprox_mu
    sd_teor <- sqrt(2 * input$aprox_lambda) * input$aprox_mu
    
    data.frame(
      Indicator = c("Media", "Deviație Standard"),
      Empiric = c(media_emp, sd_emp),
      Teoretic = c(media_teor, sd_teor),
      Diferenta = c(abs(media_emp - media_teor), abs(sd_emp - sd_teor))
    )
  })
  
  # ============================================================================
  # EXERCITIUL 10: CHURN - Server Logic
  # ============================================================================
  
  # Server logic pentru Ex 10 - Churn
  observeEvent(input$btn_churn_calculeaza, {
    # Simulare churn aleator
    churn_aleator_vec <- replicate(input$churn_M,
                                    simuleaza_churn_aleator(q = input$churn_q))
    
    # Simulare churn conditionat
    churn_cond_vec <- replicate(input$churn_M,
                                 simuleaza_churn_conditionat(
                                   m = input$churn_m,
                                   k = input$churn_k,
                                   p_succes = input$churn_p_succes
                                 ))
    
    # Churn total (reuniune)
    churn_total_vec <- (churn_aleator_vec | churn_cond_vec)
    
    # Salvare rezultate
    rv$churn_rezultate <- list(
      prob_aleator = mean(churn_aleator_vec),
      prob_cond = mean(churn_cond_vec),
      prob_total = mean(churn_total_vec),
      nr_total_utilizatori = input$churn_M
    )
  })
  
  output$tbl_churn_rate <- renderTable({
    req(rv$churn_rezultate)
    
    r <- rv$churn_rezultate
    
    data.frame(
      Tip_Churn = c("Churn Aleator (q)", 
                    "Churn Condiționat (Erori Tehnice)", 
                    "Churn TOTAL (A ∪ B)"),
      Probabilitate = c(r$prob_aleator, r$prob_cond, r$prob_total),
      Nr_Utilizatori_Estimat = c(
        round(r$prob_aleator * r$nr_total_utilizatori),
        round(r$prob_cond * r$nr_total_utilizatori),
        round(r$prob_total * r$nr_total_utilizatori)
      )
    )
  })
  
  output$txt_churn_explicatie <- renderText({
    req(rv$churn_rezultate)
    
    paste0(
      "Rezultate pentru M = ", input$churn_M, " utilizatori:\n\n",
      "• Churn Aleator: Utilizatori care părăsesc platforma din motive externe (job, ",
      "preferințe personale, etc.)\n",
      "  Probabilitate q = ", input$churn_q, "\n\n",
      "• Churn Condiționat: Utilizatori care părăsesc din cauza performanței tehnice slabe\n",
      "  Parametri: m = ", input$churn_m, " cereri, prag k = ", input$churn_k, 
      " erori, p_succes = ", input$churn_p_succes, "\n\n",
      "• Churn Total: Utilizatori care pleacă din oricare motiv (reuniune evenimentelor)\n\n",
      "Notă: Rata totală NU este suma celor două rate individuale, ",
      "deoarece unii utilizatori pot fi afectați de ambele tipuri de churn simultan."
    )
  })
  
  # --------------------------------------------------------------------------
  # EXERCITIILE 4, 5, 6 (N,F) / (N,T) / CONDITIONARI
  # --------------------------------------------------------------------------
  rv_456 <- reactiveValues(
    nf_df = NULL,
    nt_df = NULL,
    cond_df = NULL
  )
  
  # --- LOGICA (N, F) (Ex 4) ---
  observeEvent(input$btn_sim_nf, {
    rv_456$nf_df <- simuleaza_NF_din_ex3(
      M = input$nf_M,
      n_max = input$nf_n_max,
      p_succes = input$nf_p_succes,
      t_0 = input$nf_t0,
      medie_S = input$nf_medie_s,
      backoff_fix = input$nf_backoff
    )
  })
  
  output$tbl_nf_joint <- renderTable({
    req(rv_456$nf_df)
    tab <- prop.table(table(rv_456$nf_df$N, rv_456$nf_df$F))
    mat <- as.matrix(round(tab, 4))
    data.frame(N = rownames(mat), mat, check.names = FALSE)
  })
  
  output$tbl_nf_marg_N <- renderTable({
    req(rv_456$nf_df)
    tab <- prop.table(table(rv_456$nf_df$N, rv_456$nf_df$F))
    marg_N <- margin.table(tab, 1)
    data.frame(N = as.numeric(names(marg_N)), P = round(as.numeric(marg_N), 4))
  })
  
  output$tbl_nf_marg_F <- renderTable({
    req(rv_456$nf_df)
    tab <- prop.table(table(rv_456$nf_df$N, rv_456$nf_df$F))
    marg_F <- margin.table(tab, 2)
    data.frame(F = as.numeric(names(marg_F)), P = round(as.numeric(marg_F), 4))
  })
  
  output$txt_nf_chi <- renderPrint({
    req(rv_456$nf_df)
    tab_freq <- table(rv_456$nf_df$N, rv_456$nf_df$F)
    chisq.test(tab_freq, simulate.p.value = TRUE, B = 2000)
  })
  
  output$plot_nf_heat_mosaic <- renderPlot({
    req(rv_456$nf_df)
    tab <- prop.table(table(rv_456$nf_df$N, rv_456$nf_df$F))
    prob_mat <- as.matrix(tab)
    x_vals <- as.numeric(rownames(prob_mat))  # N
    y_vals <- as.numeric(colnames(prob_mat))  # F
    
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mfrow = c(1, 2))
    
    image(
      x = x_vals, y = y_vals, z = prob_mat,
      xlab = "N (nr. incercari)", ylab = "F (nr. esecuri)",
      main = "Heatmap: P(N,F) (empiric)",
      axes = FALSE
    )
    
    axis(1, at = x_vals, labels = x_vals)
    axis(2, at = y_vals, labels = y_vals)
    
    mosaicplot(
      table(rv_456$nf_df$N, rv_456$nf_df$F),
      main = "Mosaicplot: frecvente (N, F)",
      xlab = "N", ylab = "F"
    )
  })
  
  # --- LOGICA (N, T) (Ex 5) ---
  observeEvent(input$btn_sim_nt, {
    rv_456$nt_df <- simuleaza_NT_din_ex3(
      M = input$nt_M,
      n_max = input$nt_n_max,
      p_succes = input$nt_p_succes,
      t_0 = input$nt_t0,
      medie_S = input$nt_medie_s,
      backoff_fix = input$nt_backoff
    )
  })
  
  output$plot_nt_scatter <- renderPlot({
    req(rv_456$nt_df)
    
    plot(
      rv_456$nt_df$N, rv_456$nt_df$T,
      xlab = "N (numar incercari)", ylab = "T (timp total)",
      main = "Scatterplot: (N, T)",
      pch = 16, cex = 0.4
    )
    abline(lm(T ~ N, data = rv_456$nt_df), lwd = 2)
  })
  
  output$tbl_nt_stats <- renderTable({
    req(rv_456$nt_df)
    df <- rv_456$nt_df
    data.frame(
      Statistica = c("E[N]", "E[T]", "Var(N)", "Var(T)", "Cov(N,T)", "Corr(N,T)"),
      Valoare = c(
        mean(df$N),
        mean(df$T),
        var(df$N),
        var(df$T),
        cov(df$N, df$T),
        cor(df$N, df$T)
      )
    )
  })
  
  output$txt_nt_interp <- renderText({
    req(rv_456$nt_df)
    paste0(
      "Interpretare:\n",
      "De regula corelatia este POZITIVA: daca avem mai multe retry-uri (N creste),\n",
      "timpul total T creste (se aduna mai multe S_i si eventual backoff-uri)."
    )
  })
  
  # --- LOGICA CONDITIONARI (Ex 6) ---
  observeEvent(input$btn_sim_cond, {
    rv_456$cond_df <- simuleaza_date_din_ex3(
      M = input$cond_M,
      n_max = input$cond_n_max,
      p_succes = input$cond_p_succes,
      t_0 = input$cond_t0,
      medie_S = input$cond_medie_s,
      backoff_fix = input$cond_backoff
    )
  })
  
  output$tbl_cond_probs <- renderTable({
    req(rv_456$cond_df)
    df <- rv_456$cond_df
    n0 <- input$cond_n0
    t0 <- input$cond_t0
    
    A <- (df$I == 1L)
    B <- (df$T <= t0)
    C <- (df$N <= n0)
    
    data.frame(
      Probabilitate = c(
        paste0("P(A | N <= ", n0, ")"),
        "P(B | A) = P(T <= t0 | I=1)"
      ),
      Valoare = c(
        mean(A[C]),
        mean(B[A])
      )
    )
  })
  
  output$tbl_cond_expect <- renderTable({
    req(rv_456$cond_df)
    df <- rv_456$cond_df
    
    data.frame(
      Cantitate = c("E(T | I = 1) [Succes]", "E(T | I = 0) [Eșec final]"),
      Valoare = c(
        mean(df$T[df$I == 1L]),
        mean(df$T[df$I == 0L])
      )
    )
  })
  
  output$plot_cond_box <- renderPlot({
    req(rv_456$cond_df)
    df <- rv_456$cond_df
    
    boxplot(
      T ~ I, data = df,
      names = c("Eșec (I=0)", "Succes (I=1)"),
      main = "Distribuția lui T condiționat de I",
      ylab = "T (timp total)"
    )
  })
  
  output$txt_cond_interp <- renderText({
    req(rv_456$cond_df)
    n0 <- input$cond_n0
    paste0(
      "Interpretare:\n",
      "- P(I=1 | N <= ", n0, ") masoara cat de des reusim rapid (cu putine retry-uri).\n",
      "- P(T <= t0 | I=1) masoara cat de des respectam SLA conditionat de succes.\n",
      "- In general E(T | I=0) tinde sa fie mai mare, deoarece cererile esuate consuma toate incercarile\n",
      "  si acumuleaza timpi de raspuns + backoff-uri."
    )
  })
  
  # Output pentru afisarea factorului in UI (Tab 7)
  output$txt_dep_factor <- renderText({
    input$dep_factor_latenta
  })
}

shinyApp(ui = ui, server = server)
