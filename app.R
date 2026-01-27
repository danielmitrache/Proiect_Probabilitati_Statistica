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
  )
)

# Server
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    trafic_data = NULL,
    latenta_exp = NULL,
    latenta_norm = NULL
  )
  
  # --- LOGICA TRAFIC ---
  observeEvent(input$btn_sim_trafic, {
    rv$trafic_data <- df_trafic_zile(
      n_zile = input$tr_n_zile,
      lambda = input$tr_lambda,
      n_max = input$tr_n_max,
      p = input$tr_prob
    )
    
    # ### FIX CRITIC: ACTUALIZAM LISTA DE ANI IN UI ###
    # Fara linia asta, dropdown-ul "Alege Anul" ramane gol!
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
    # req asigura ca nu desenam daca nu avem date SAU an selectat
    req(rv$trafic_data, input$tr_select_an) 
    histograma_an_distrib(rv$trafic_data, 
                          an = input$tr_select_an, 
                          distributie = input$tr_select_dist)
  })
  
  output$plot_tr_lunar <- renderPlot({
    req(rv$trafic_data, input$tr_select_an, input$tr_select_luna)
    histograma_luna_distrib(rv$trafic_data, 
                            luna = input$tr_select_luna, 
                            an = input$tr_select_an, 
                            distributie = input$tr_select_dist)
  })
  
  output$tbl_tr_stats <- renderTable({
    req(rv$trafic_data)
    df_estimari_empirice(rv$trafic_data, 
                         distributie = input$tr_model_stat,
                         lambda = input$tr_lambda,
                         n_max = input$tr_n_max,
                         p = input$tr_prob)
  })
  
  # --- LOGICA LATENTA ---
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
}

shinyApp(ui = ui, server = server)