ui <- navbarPage(
  title = "Détection du Cancer du Sein",
  theme = shinythemes::shinytheme("flatly"),
  
  # Onglet Présentation
  tabPanel("Présentation",
           fluidPage(
             h2("Analyse du Cancer du Sein"),
             p("Cette application permet une exploration approfondie des caractéristiques des tumeurs mammaires."),
             hr(),
             h4("Description du dataset"),
             p("Le jeu de données contient les mesures calculées à partir d'images numérisées de biopsies de masses mammaires."),
             p(textOutput("dataset_info")),
             h4("Variables clés:"),
             tags$ul(
               tags$li("Diagnostic: B (Bénin) ou M (Malin)"),
               tags$li("Caractéristiques moyennes: rayon, texture, périmètre, aire, etc."),
               tags$li("Valeurs d'écart-type (variabilité)"),
               tags$li("Valeurs extrêmes (pires observations)")
             ),
             hr(),
             plotOutput("corrplot", height = "600px")
           )
  ),
  
  # Onglet Exploration
  tabPanel("Exploration Univariée",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable", "Choisir une variable :", choices = NULL),
               hr(),
               h4("Statistiques descriptives"),
               verbatimTextOutput("summary")
             ),
             mainPanel(
               plotOutput("boxplot", height = "400px"),
               hr(),
               h4("Tableau des données"),
               DT::dataTableOutput("datatable")
             )
           )
  ),
  
  # Onglet Analyse Multivariée
  tabPanel("Analyse Multivariée",
           sidebarLayout(
             sidebarPanel(
               selectInput("xvar", "Variable X:", choices = NULL),
               selectInput("yvar", "Variable Y:", choices = NULL),
               hr(),
               p("Explorez les relations entre différentes caractéristiques.")
             ),
             mainPanel(
               plotOutput("scatterplot", height = "500px")
             )
           )
  ),
  
  # Onglet Jeux de Données
  tabPanel("Jeux de Données",
           sidebarLayout(
             sidebarPanel(
               selectInput("dataset_choice", "Choisir le jeu de données :",
                           choices = c("data" = "data",
                                       "SEER Breast Cancer" = "seer")),
               uiOutput("dataset_details"),
               br(),
               downloadButton("download_data", "Télécharger (.csv)")
             ),
             mainPanel(
               h3(textOutput("dataset_title")),
               DT::dataTableOutput("dataset_table"),
               htmlOutput("dataset_dimensions")
             )
           )
  ),
  
  # Onglet Modélisation
  tabPanel("Modélisation Prédictive",
           fluidPage(
             h3("Construction de modèles prédictifs")
           )
  )
)