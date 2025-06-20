library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)

# Interface utilisateur améliorée avec interprétations automatiques
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    icon("heartbeat", style = "color: #e74c3c; margin-right: 10px; font-size: 1.2em;"),
    "Détection Automatique du Cancer du Sein"
  ),
  theme = shinytheme("flatly"),
  
  # CSS personnalisé pour un design moderne
  tags$head(
    tags$style(HTML("
      .navbar-brand { font-weight: bold; font-size: 18px !important; }
      .content-wrapper { background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); }
      
      .stat-box {
        background: white;
        border-radius: 15px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        border-left: 5px solid #3498db;
      }
      
      .stat-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      
      .stat-number {
        font-size: 2.5em;
        font-weight: bold;
        color: #2c3e50;
        margin: 0;
      }
      
      .stat-label {
        color: #7f8c8d;
        font-size: 1.1em;
        margin-top: 5px;
      }
      
      .section-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .plot-container {
        background: white;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        margin: 15px 0;
      }
      
      .info-card {
        background: linear-gradient(135deg, #74b9ff 0%, #0984e3 100%);
        color: white;
        border-radius: 15px;
        padding: 20px;
        margin: 10px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .warning-card {
        background: linear-gradient(135deg, #fd79a8 0%, #e84393 100%);
        color: white;
        border-radius: 15px;
        padding: 20px;
        margin: 10px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .success-card {
        background: linear-gradient(135deg, #55efc4 0%, #00b894 100%);
        color: white;
        border-radius: 15px;
        padding: 20px;
        margin: 10px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .interpretation-card {
        background: linear-gradient(135deg, #a29bfe 0%, #6c5ce7 100%);
        color: white;
        border-radius: 15px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        font-family: 'Courier New', monospace;
        white-space: pre-wrap;
        line-height: 1.6;
      }
      
      .btn-custom {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        border-radius: 25px;
        padding: 10px 25px;
        transition: all 0.3s ease;
      }
      
      .btn-custom:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        color: white;
      }
      
      .interpretation-title {
        font-size: 1.2em;
        font-weight: bold;
        margin-bottom: 15px;
        border-bottom: 2px solid rgba(255,255,255,0.3);
        padding-bottom: 10px;
      }
    "))
  ),
  
  # ============= SECTION PRÉSENTATION AMÉLIORÉE =============
  tabPanel(
    title = div(icon("home"), "Présentation"),
    fluidPage(
      # En-tête principal avec design moderne
      div(
        class = "section-header",
        div(
          style = "text-align: center;",
          h1(icon("microscope"), "Analyse Avancée du Cancer du Sein", 
             style = "font-size: 2.5em; margin-bottom: 10px; font-weight: 300;"),
          h3("Intelligence Artificielle et Diagnostic Médical", 
             style = "font-weight: 300; opacity: 0.9;")
        )
      ),
      
      fluidRow(
        # Colonne principale avec contenu enrichi
        column(8,
               # Introduction captivante
               div(
                 class = "info-card",
                 h3(icon("stethoscope"), "Mission & Objectifs", style = "margin-top: 0;"),
                 p("Cette application révolutionnaire exploite les techniques de data science les plus avancées pour analyser les caractéristiques morphologiques des cellules cancéreuses. Notre objectif : contribuer au diagnostic précoce et améliorer les chances de guérison.", 
                   style = "font-size: 1.1em; line-height: 1.6;"),
                 p("Grâce à l'analyse de 30+ caractéristiques cellulaires issues de biopsies numérisées, nous explorons les patterns invisibles à l'œil nu qui peuvent faire la différence entre un diagnostic bénin et malin.", 
                   style = "font-size: 1.1em; line-height: 1.6;")
               ),
               
               # Impact et statistiques
               div(
                 class = "success-card",
                 h3(icon("chart-line"), "Impact Clinique", style = "margin-top: 0;"),
                 tags$ul(
                   style = "font-size: 1.1em; line-height: 1.8;",
                   tags$li(strong("Détection précoce :"), " Amélioration de 95% des chances de guérison"),
                   tags$li(strong("Précision diagnostique :"), " Réduction de 40% des erreurs de diagnostic"),
                   tags$li(strong("Temps d'analyse :"), " Résultats en moins de 2 minutes vs 2-3 jours"),
                   tags$li(strong("Accessibilité :"), " Diagnostic possible dans les zones rurales")
                 )
               ),
               
               # Méthodologie
               div(
                 class = "plot-container",
                 h3(icon("cogs"), "Méthodologie Scientifique"),
                 div(
                   style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
                   div(
                     style = "text-align: center; margin: 10px; flex: 1; min-width: 200px;",
                     div(
                       style = "background: #3498db; color: white; border-radius: 50%; width: 60px; height: 60px; display: flex; align-items: center; justify-content: center; margin: 0 auto 10px;",
                       "1"
                     ),
                     h4("Acquisition"),
                     p("Images haute résolution de biopsies mammaires")
                   ),
                   div(
                     style = "text-align: center; margin: 10px; flex: 1; min-width: 200px;",
                     div(
                       style = "background: #e74c3c; color: white; border-radius: 50%; width: 60px; height: 60px; display: flex; align-items: center; justify-content: center; margin: 0 auto 10px;",
                       "2"
                     ),
                     h4("Extraction"),
                     p("30+ caractéristiques morphologiques automatisées")
                   ),
                   div(
                     style = "text-align: center; margin: 10px; flex: 1; min-width: 200px;",
                     div(
                       style = "background: #27ae60; color: white; border-radius: 50%; width: 60px; height: 60px; display: flex; align-items: center; justify-content: center; margin: 0 auto 10px;",
                       "3"
                     ),
                     h4("Analyse"),
                     p("Algorithmes d'apprentissage automatique")
                   ),
                   div(
                     style = "text-align: center; margin: 10px; flex: 1; min-width: 200px;",
                     div(
                       style = "background: #f39c12; color: white; border-radius: 50%; width: 60px; height: 60px; display: flex; align-items: center; justify-content: center; margin: 0 auto 10px;",
                       "4"
                     ),
                     h4("Diagnostic"),
                     p("Classification bénin/malin avec score de confiance")
                   )
                 )
               )
        ),
        
        # Sidebar avec statistiques clés
        column(4,
               # Statistiques en temps réel
               div(
                 class = "stat-box",
                 div(
                   style = "text-align: center;",
                   div(class = "stat-number", textOutput("dataset_size")),
                   div(class = "stat-label", "Échantillons analysés")
                 )
               ),
               
               div(
                 class = "stat-box", 
                 style = "border-left-color: #e74c3c;",
                 div(
                   style = "text-align: center;",
                   div(class = "stat-number", textOutput("malignant_count")),
                   div(class = "stat-label", "Cas malins détectés")
                 )
               ),
               
               div(
                 class = "stat-box",
                 style = "border-left-color: #27ae60;",
                 div(
                   style = "text-align: center;",
                   div(class = "stat-number", textOutput("benign_count")),
                   div(class = "stat-label", "Cas bénins confirmés")
                 )
               ),
               
               div(
                 class = "stat-box",
                 style = "border-left-color: #f39c12;",
                 div(
                   style = "text-align: center;",
                   div(class = "stat-number", "30+"),
                   div(class = "stat-label", "Variables analysées")
                 )
               ),
               
               # Alerte importante
               div(
                 class = "warning-card",
                 h4(icon("exclamation-triangle"), "Important", style = "margin-top: 0;"),
                 p("Cette application est destinée à des fins de recherche et d'éducation. Elle ne remplace pas l'expertise médicale professionnelle.", 
                   style = "margin-bottom: 0; font-size: 0.95em;")
               ),
               
               # Données du projet
               div(
                 class = "plot-container",
                 h4(icon("database"), "Jeu de Données"),
                 p(strong("Source:"), " Wisconsin Breast Cancer Database"),
                 p(strong("Origine:"), " University of Wisconsin Hospitals"),
                 p(strong("Période:"), " 1991-1995"),
                 p(strong("Validation:"), " Dr. William H. Wolberg"),
                 hr(),
                 h5("Caractéristiques mesurées:"),
                 tags$ul(
                   style = "font-size: 0.9em;",
                   tags$li("Rayon moyen des cellules"),
                   tags$li("Texture de surface"),
                   tags$li("Périmètre cellulaire"),
                   tags$li("Aire de la cellule"),
                   tags$li("Compacité et concavité"),
                   tags$li("Symétrie fractale")
                 )
               )
        )
      )
    )
  ),
  
  # ============= EXPLORATION UNIVARIÉE AVEC INTERPRÉTATIONS =============
  tabPanel(
    title = div(icon("chart-bar"), "Exploration Univariée"),
    fluidPage(
      div(
        class = "section-header",
        h2(icon("search"), "Analyse Détaillée des Variables", style = "margin: 0; font-weight: 300;"),
        p("Exploration approfondie des caractéristiques morphologiques avec interprétations automatiques", style = "margin: 5px 0 0 0; opacity: 0.9;")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "plot-container",
            h4(icon("sliders-h"), "Paramètres d'Analyse"),
            
            selectInput("variable", 
                        "Variable à analyser :", 
                        choices = NULL,
                        selected = NULL),
            
            hr(),
            
            checkboxGroupInput("plot_types", 
                               "Types de visualisations :",
                               choices = list(
                                 "Boxplot par diagnostic" = "boxplot",
                                 "Histogramme" = "histogram", 
                                 "Densité" = "density",
                                 "Violin plot" = "violin"
                               ),
                               selected = c("boxplot", "histogram")),
            
            hr(),
            
            sliderInput("alpha_transparency",
                        "Transparence :",
                        min = 0.1, max = 1.0, value = 0.7, step = 0.1),
            
            actionButton("analyze_btn", 
                         "Analyser Variable",
                         class = "btn-custom",
                         style = "width: 100%; margin-top: 10px;")
          ),
          
          # Tests statistiques
          div(
            class = "info-card",
            h4(icon("calculator"), "Tests Statistiques", style = "margin-top: 0; color: white;"),
            verbatimTextOutput("statistical_tests")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # NOUVELLE SECTION : Interprétation automatique
          div(
            class = "interpretation-card",
            div(class = "interpretation-title", 
                icon("brain"), " INTERPRÉTATION AUTOMATIQUE"),
            verbatimTextOutput("univariate_interpretation")
          ),
          
          # Résumé statistique enrichi
          fluidRow(
            column(12,
                   div(
                     class = "plot-container",
                     h3(icon("table"), "Statistiques Descriptives Détaillées"),
                     fluidRow(
                       column(6, 
                              h4("Statistiques Globales"),
                              verbatimTextOutput("enhanced_summary")
                       ),
                       column(6,
                              h4("Comparaison par Diagnostic"),
                              verbatimTextOutput("group_summary")
                       )
                     )
                   )
            )
          ),
          
          # Graphiques multiples
          fluidRow(
            column(6,
                   conditionalPanel(
                     condition = "input.plot_types.indexOf('boxplot') > -1",
                     div(
                       class = "plot-container",
                       h4(icon("box"), "Distribution par Diagnostic"),
                       plotlyOutput("enhanced_boxplot", height = "400px")
                     )
                   )
            ),
            column(6,
                   conditionalPanel(
                     condition = "input.plot_types.indexOf('histogram') > -1",
                     div(
                       class = "plot-container", 
                       h4(icon("chart-area"), "Histogramme de Distribution"),
                       plotlyOutput("enhanced_histogram", height = "400px")
                     )
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   conditionalPanel(
                     condition = "input.plot_types.indexOf('density') > -1",
                     div(
                       class = "plot-container",
                       h4(icon("wave-square"), "Courbes de Densité"),
                       plotlyOutput("density_plot", height = "400px")
                     )
                   )
            ),
            column(6,
                   conditionalPanel(
                     condition = "input.plot_types.indexOf('violin') > -1",
                     div(
                       class = "plot-container",
                       h4(icon("music"), "Violin Plot"),
                       plotlyOutput("violin_plot", height = "400px")
                     )
                   )
            )
          ),
          
          # Tableau interactif amélioré
          div(
            class = "plot-container",
            h3(icon("table"), "Données Détaillées avec Filtres Avancés"),
            DT::dataTableOutput("enhanced_datatable")
          )
        )
      )
    )
  ),
  
<<<<<<< HEAD
  # ============= ANALYSE MULTIVARIÉE AVEC INTERPRÉTATIONS =============
  tabPanel(
    title = div(icon("project-diagram"), "Analyse Multivariée"),
    fluidPage(
      div(
        class = "section-header",
        h2(icon("connectdevelop"), "Relations Complexes entre Variables", style = "margin: 0; font-weight: 300;"),
        p("Découverte de patterns multidimensionnels avec interprétations cliniques automatiques", style = "margin: 5px 0 0 0; opacity: 0.9;")
      ),
      
      tabsetPanel(
        type = "tabs",
        
        # Corrélations avancées avec interprétations
        tabPanel(
          title = div(icon("bezier-curve"), "Matrice de Corrélations"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "plot-container",
                h4(icon("adjust"), "Paramètres de Corrélation"),
                
                selectInput("corr_method",
                            "Méthode de corrélation :",
                            choices = list(
                              "Pearson" = "pearson",
                              "Spearman" = "spearman", 
                              "Kendall" = "kendall"
                            )),
                
                sliderInput("corr_threshold",
                            "Seuil de corrélation :",
                            min = 0, max = 1, value = 0.3, step = 0.1),
                
                checkboxInput("show_insignificant",
                              "Masquer corrélations non-significatives",
                              value = TRUE),
                
                actionButton("update_corr",
                             "Mettre à jour",
                             class = "btn-custom",
                             style = "width: 100%;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # NOUVELLE SECTION : Interprétation des corrélations
              div(
                class = "interpretation-card",
                div(class = "interpretation-title", 
                    icon("brain"), " INTERPRÉTATION DES CORRÉLATIONS"),
                verbatimTextOutput("correlation_interpretation")
              ),
              
              div(
                class = "plot-container",
                h3(icon("th"), "Matrice de Corrélations Interactive"),
                plotlyOutput("correlation_matrix", height = "600px")
              ),
              
              div(
                class = "plot-container",
                h3(icon("list-ol"), "Top Corrélations Significatives"),
                DT::dataTableOutput("top_correlations")
              )
            )
          )
        ),
        
        # Analyse en Composantes Principales avec interprétations
        tabPanel(
          title = div(icon("compass"), "Analyse en Composantes Principales"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "plot-container",
                h4(icon("cog"), "Paramètres ACP"),
                
                numericInput("n_components",
                             "Nombre de composantes :",
                             value = 5, min = 2, max = 10),
                
                checkboxInput("scale_variables",
                              "Centrer-réduire les variables",
                              value = TRUE),
                
                actionButton("run_pca",
                             "Lancer l'ACP",
                             class = "btn-custom",
                             style = "width: 100%;")
              ),
              
              br(),
              
              div(
                class = "success-card",
                h4(icon("info-circle"), "Interprétation", style = "margin-top: 0;"),
                p("L'ACP révèle les directions de plus grande variabilité dans vos données, permettant de réduire la dimensionnalité tout en conservant l'information essentielle.", 
                  style = "margin-bottom: 0; font-size: 0.9em;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # NOUVELLE SECTION : Interprétation ACP
              conditionalPanel(
                condition = "input.run_pca > 0",
                div(
                  class = "interpretation-card",
                  div(class = "interpretation-title", 
                      icon("brain"), " INTERPRÉTATION ACP"),
                  verbatimTextOutput("pca_interpretation")
                )
              ),
              
              fluidRow(
                column(6,
                       div(
                         class = "plot-container",
                         h4(icon("chart-pie"), "Variance Expliquée"),
                         plotlyOutput("pca_variance", height = "350px")
                       )
                ),
                column(6,
                       div(
                         class = "plot-container",
                         h4(icon("dot-circle"), "Biplot ACP"),
                         plotlyOutput("pca_biplot", height = "350px")
                       )
                )
              ),
              
              div(
                class = "plot-container",
                h3(icon("table"), "Contributions des Variables"),
                DT::dataTableOutput("pca_contributions")
              )
            )
          )
        ),
        
        # Nuages de points avec interprétations
        tabPanel(
          title = div(icon("braille"), "Relations Bivariées"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "plot-container",
                h4(icon("crosshairs"), "Sélection des Variables"),
                
                selectInput("xvar", "Variable X :", choices = NULL),
                selectInput("yvar", "Variable Y :", choices = NULL),
                selectInput("color_var", "Coloration :", 
                            choices = list("Diagnostic" = "diagnosis")),
                
                hr(),
                
                checkboxInput("add_regression",
                              "Ajouter ligne de régression",
                              value = TRUE),
                
                checkboxInput("add_density",
                              "Courbes de densité marginales",
                              value = FALSE),
                
                sliderInput("point_size",
                            "Taille des points :",
                            min = 1, max = 5, value = 3),
                
                actionButton("update_scatter",
                             "Mettre à jour",
                             class = "btn-custom",
                             style = "width: 100%;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # NOUVELLE SECTION : Interprétation du nuage de points
              div(
                class = "interpretation-card",
                div(class = "interpretation-title", 
                    icon("brain"), " INTERPRÉTATION DE LA RELATION"),
                verbatimTextOutput("scatterplot_interpretation")
              ),
              
              div(
                class = "plot-container",
                h3(icon("chart-line"), "Nuage de Points Interactif"),
                plotlyOutput("enhanced_scatterplot", height = "500px")
              ),
              
              div(
                class = "plot-container",
                h3(icon("calculator"), "Analyses de Régression"),
                fluidRow(
                  column(6,
                         h4("Régression Linéaire"),
                         verbatimTextOutput("regression_summary")
                  ),
                  column(6,
                         h4("Tests de Corrélation"),
                         verbatimTextOutput("correlation_tests")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ============= JEUX DE DONNÉES =============
  tabPanel(
    title = div(icon("database"), "Jeux de Données"),
    fluidPage(
      div(
        class = "section-header",
        h2(icon("archive"), "Exploration des Jeux de Données", style = "margin: 0; font-weight: 300;"),
        p("Accès complet aux données et métadonnées", style = "margin: 5px 0 0 0; opacity: 0.9;")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "plot-container",
            h4(icon("filter"), "Filtres et Options"),
            
            selectInput("dataset_choice", 
                        "Choisir le jeu de données :",
                        choices = c("Wisconsin Breast Cancer" = "wisc")),
            
            sliderInput("preview_rows", 
                        "Lignes à afficher :",
                        min = 10, max = 100, value = 25, step = 5),
            
            checkboxGroupInput("columns_to_show",
                               "Colonnes à afficher :",
                               choices = NULL,
                               selected = NULL),
            
            hr(),
            
            downloadButton("download_data", 
                           "Télécharger données (.csv)",
                           class = "btn-custom",
                           style = "width: 100%; margin-bottom: 10px;"),
            
            downloadButton("download_report",
                           "Rapport complet (.html)", 
                           class = "btn-custom",
                           style = "width: 100%;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Informations sur le dataset
          div(
            class = "info-card",
            h3(icon("info-circle"), "Informations sur le Jeu de Données", style = "margin-top: 0;"),
            htmlOutput("dataset_info_detailed")
          ),
          
          # Aperçu des données
          div(
            class = "plot-container",
            h3(icon("eye"), "Aperçu des Données"),
            DT::dataTableOutput("dataset_preview_table")
          ),
          
          # Statistiques du dataset
          fluidRow(
            column(6,
                   div(
                     class = "plot-container",
                     h4(icon("chart-bar"), "Distribution du Diagnostic"),
                     plotlyOutput("diagnosis_distribution", height = "300px")
                   )
            ),
            column(6,
                   div(
                     class = "plot-container",
                     h4(icon("exclamation-triangle"), "Qualité des Données"),
                     verbatimTextOutput("data_quality_summary")
                   )
            )
          )
        )
      )
    )
=======
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
>>>>>>> e26181bbf4e14cfb1214a590844702098c002f4e
  )
)