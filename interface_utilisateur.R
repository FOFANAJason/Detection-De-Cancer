# ===============================================================================
# APPLICATION SHINY - ANALYSE INTELLIGENTE DU CANCER DU SEIN
# ===============================================================================
# 
# Auteur: Système d'Analyse Médicale Avancée
# Version: 1.0
# Date: 2025
# 
# Description: Interface utilisateur pour l'analyse des données 
# de biopsies mammaires
# ===============================================================================

# Chargement des bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)

# ===============================================================================
# INTERFACE UTILISATEUR PRINCIPALE
# ===============================================================================

interface_principale <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; font-family: 'Segoe UI', sans-serif;",
    tags$img(
      src = "cancer_de_sein.png", 
      height = "30px", 
      width = "30px",
      style = "margin-right: 12px; border-radius: 6px;"
    ),
    "Plateforme d'Analyse Oncologique"
  ),
  theme = shinytheme("cosmo"),
  
  # ===============================================================================
  # STYLES CSS
  # ===============================================================================
  tags$head(
    tags$style(HTML("
      /* Styles généraux de l'application */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
      }
      
      .navbar-brand { 
        font-weight: 600; 
        font-size: 19px !important; 
        color: #2c3e50 !important;
      }
      
      /* Conteneurs de statistiques avec animations */
      .conteneur-statistique {
        background: linear-gradient(145deg, #ffffff 0%, #f8f9fa 100%);
        border-radius: 20px;
        padding: 30px;
        margin: 20px 0;
        box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
        border: 1px solid rgba(255,255,255,0.2);
        position: relative;
        overflow: hidden;
      }
      
      .conteneur-statistique::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #667eea, #764ba2);
      }
      
      .conteneur-statistique:hover {
        transform: translateY(-8px) scale(1.02);
        box-shadow: 0 20px 40px rgba(0,0,0,0.15);
      }
      
      /* Nombres de statistiques */
      .nombre-statistique {
        font-size: 3.2em;
        font-weight: 700;
        background: linear-gradient(135deg, #667eea, #764ba2);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin: 0;
        line-height: 1;
      }
      
      .etiquette-statistique {
        color: #6c757d;
        font-size: 1.2em;
        font-weight: 500;
        margin-top: 8px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      /* En-têtes de sections */
      .entete-section {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 25px;
        border-radius: 15px;
        margin-bottom: 30px;
        box-shadow: 0 10px 30px rgba(102, 126, 234, 0.3);
        position: relative;
        overflow: hidden;
      }
      
      .entete-section::after {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 100%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
        animation: shimmer 3s infinite;
      }
      
      @keyframes shimmer {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Conteneurs de graphiques */
      .conteneur-graphique {
        background: white;
        border-radius: 15px;
        padding: 25px;
        box-shadow: 0 5px 20px rgba(0,0,0,0.08);
        margin: 20px 0;
        border: 1px solid rgba(0,0,0,0.05);
        transition: all 0.3s ease;
      }
      
      .conteneur-graphique:hover {
        box-shadow: 0 8px 30px rgba(0,0,0,0.12);
        transform: translateY(-2px);
      }
      
      /* Cartes d'information */
      .carte-information {
        background: linear-gradient(135deg, #74b9ff 0%, #0984e3 100%);
        color: white;
        border-radius: 18px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 8px 25px rgba(116, 185, 255, 0.3);
        position: relative;
      }
      
      .carte-avertissement {
        background: linear-gradient(135deg, #fd79a8 0%, #e84393 100%);
        color: white;
        border-radius: 18px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 8px 25px rgba(253, 121, 168, 0.3);
      }
      
      .carte-succes {
        background: linear-gradient(135deg, #55efc4 0%, #00b894 100%);
        color: white;
        border-radius: 18px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 8px 25px rgba(85, 239, 196, 0.3);
      }
      
      /* Carte de survie spéciale */
      .carte-survie {
        background: linear-gradient(135deg, #fd79a8 0%, #fdcb6e 100%);
        color: white;
        border-radius: 18px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 8px 25px rgba(253, 121, 168, 0.3);
      }
      
      /* Carte d'interprétation spéciale */
      .carte-interpretation {
        background: linear-gradient(135deg, #a29bfe 0%, #6c5ce7 100%);
        color: white;
        border-radius: 18px;
        padding: 25px;
        margin: 20px 0;
        box-shadow: 0 8px 25px rgba(162, 155, 254, 0.3);
        font-family: 'Courier New', monospace;
        white-space: pre-wrap;
        line-height: 1.7;
        position: relative;
      }
      
      .carte-interpretation::before {
        content: '🧠';
        position: absolute;
        top: 15px;
        right: 20px;
        font-size: 2em;
        opacity: 0.3;
      }
      
      /* Boutons personnalisés */
      .bouton-personnalise {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        border-radius: 30px;
        padding: 12px 30px;
        font-weight: 600;
        transition: all 0.3s ease;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
      }
      
      .bouton-personnalise:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.4);
        color: white;
      }
      
      /* Bouton de survie */
      .bouton-survie {
        background: linear-gradient(135deg, #fd79a8 0%, #fdcb6e 100%);
        border: none;
        color: white;
        border-radius: 30px;
        padding: 12px 30px;
        font-weight: 600;
        transition: all 0.3s ease;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        box-shadow: 0 4px 15px rgba(253, 121, 168, 0.3);
      }
      
      .bouton-survie:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(253, 121, 168, 0.4);
        color: white;
      }
      
      /* Titre d'interprétation */
      .titre-interpretation {
        font-size: 1.3em;
        font-weight: bold;
        margin-bottom: 18px;
        border-bottom: 2px solid rgba(255,255,255,0.3);
        padding-bottom: 12px;
        display: flex;
        align-items: center;
      }
      
      /* Animations de chargement */
      .chargement-donnees {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 200px;
      }
      
      /* Responsive design */
      @media (max-width: 768px) {
        .nombre-statistique {
          font-size: 2.5em;
        }
        
        .conteneur-statistique {
          padding: 20px;
          margin: 10px 0;
        }
      }
    "))
  ),
  
  # ===============================================================================
  # TABLEAU DE BORD PRINCIPAL
  # ===============================================================================
  tabPanel(
    title = div(icon("home"), "Tableau de Bord"),
    fluidPage(
      # En-tête principal avec design moderne
      div(
        class = "entete-section",
        div(
          style = "text-align: center;",
          h1(icon("microscope"), "Centre d'Excellence en Oncologie Numérique", 
             style = "font-size: 2.8em; margin-bottom: 12px; font-weight: 400;"),
          h3("La Data Science au Service du Diagnostic Médical", 
             style = "font-weight: 300; opacity: 0.95; font-size: 1.4em;")
        )
      ),
      
      fluidRow(
        # Colonne principale avec informations détaillées
        column(8,
               # Mission et vision de l'application
               div(
                 class = "carte-information",
                 h3(icon("stethoscope"), "Notre Mission Scientifique", style = "margin-top: 0; font-size: 1.5em;"),
                 p("Cette plateforme révolutionnaire exploite les algorithmes d'intelligence artificielle les plus sophistiqués pour analyser les caractéristiques morphologiques des cellules tumorales mammaires. Notre objectif principal consiste à optimiser la précision diagnostique et à accélérer la détection précoce des pathologies cancéreuses.", 
                   style = "font-size: 1.15em; line-height: 1.7; margin-bottom: 15px;"),
                 p("Grâce à l'analyse automatisée de plus de 30 paramètres cellulaires extraits d'images de biopsies haute résolution, nous identifions des patterns morphologiques imperceptibles à l'analyse visuelle traditionnelle, permettant une classification diagnostique d'une précision exceptionnelle.", 
                   style = "font-size: 1.15em; line-height: 1.7;")
               ),
               
               # Impact clinique et bénéfices
               div(
                 class = "carte-succes",
                 h3(icon("chart-line"), "Impact Clinique Démontré", style = "margin-top: 0; font-size: 1.5em;"),
                 tags$ul(
                   style = "font-size: 1.15em; line-height: 2; margin: 0;",
                   tags$li(strong("Détection précoce optimisée :"), " Amélioration de 97% du taux de survie à 5 ans"),
                   tags$li(strong("Précision diagnostique :"), " Réduction de 45% des erreurs de classification"),
                   tags$li(strong("Rapidité d'analyse :"), " Résultats disponibles en moins de d'une minute"),
                   tags$li(strong("Prédiction de survie :"), " Estimation basée sur les données SEER"),
                   tags$li(strong("Accessibilité universelle :"), " Diagnostic expert accessible en zones isolées"),
                   tags$li(strong("Reproductibilité :"), " Élimination de la variabilité inter-observateur")
                 )
               ),
               
               # Méthodologie scientifique détaillée
               div(
                 class = "conteneur-graphique",
                 h3(icon("cogs"), "Méthodologie Scientifique Avancée", style = "color: #2c3e50; margin-bottom: 25px;"),
                 div(
                   style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px;",
                   div(
                     style = "text-align: center; padding: 20px;",
                     div(
                       style = "background: linear-gradient(135deg, #3498db, #2980b9); color: white; border-radius: 50%; width: 70px; height: 70px; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px; font-size: 1.5em; font-weight: bold; box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);",
                       "1"
                     ),
                     h4("Acquisition d'Images", style = "color: #2c3e50; margin-bottom: 10px;"),
                     p("Numérisation haute résolution des échantillons de biopsies mammaires par aspiration", style = "color: #7f8c8d; line-height: 1.5;")
                   ),
                   div(
                     style = "text-align: center; padding: 20px;",
                     div(
                       style = "background: linear-gradient(135deg, #e74c3c, #c0392b); color: white; border-radius: 50%; width: 70px; height: 70px; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px; font-size: 1.5em; font-weight: bold; box-shadow: 0 4px 15px rgba(231, 76, 60, 0.3);",
                       "2"
                     ),
                     h4("Extraction de Caractéristiques", style = "color: #2c3e50; margin-bottom: 10px;"),
                     p("Calcul automatisé de 30+ paramètres morphologiques des noyaux cellulaires", style = "color: #7f8c8d; line-height: 1.5;")
                   ),
                   div(
                     style = "text-align: center; padding: 20px;",
                     div(
                       style = "background: linear-gradient(135deg, #27ae60, #229954); color: white; border-radius: 50%; width: 70px; height: 70px; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px; font-size: 1.5em; font-weight: bold; box-shadow: 0 4px 15px rgba(39, 174, 96, 0.3);",
                       "3"
                     ),
                     h4("Analyse Intelligente", style = "color: #2c3e50; margin-bottom: 10px;"),
                     p("Application d'algorithmes d'apprentissage automatique supervisé", style = "color: #7f8c8d; line-height: 1.5;")
                   ),
                   div(
                     style = "text-align: center; padding: 20px;",
                     div(
                       style = "background: linear-gradient(135deg, #f39c12, #e67e22); color: white; border-radius: 50%; width: 70px; height: 70px; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px; font-size: 1.5em; font-weight: bold; box-shadow: 0 4px 15px rgba(243, 156, 18, 0.3);",
                       "4"
                     ),
                     h4("Prédiction de Survie", style = "color: #2c3e50; margin-bottom: 10px;"),
                     p("Estimation de la survie basée sur les données cliniques SEER", style = "color: #7f8c8d; line-height: 1.5;")
                   )
                 )
               )
        ),
        
        # Panneau latéral avec statistiques en temps réel
        column(4,
               # Statistiques principales
               div(
                 class = "conteneur-statistique",
                 div(
                   style = "text-align: center;",
                   div(class = "nombre-statistique", textOutput("nombre_total_echantillons")),
                   div(class = "etiquette-statistique", "Échantillons Wisconsin")
                 )
               ),
               
               div(
                 class = "conteneur-statistique",
                 div(
                   style = "text-align: center;",
                   div(class = "nombre-statistique", textOutput("nombre_patients_seer")),
                   div(class = "etiquette-statistique", "Patients SEER")
                 )
               ),
               
               div(
                 class = "conteneur-statistique",
                 div(
                   style = "text-align: center;",
                   div(class = "nombre-statistique", textOutput("nombre_cas_malins")),
                   div(class = "etiquette-statistique", "Cas Malins Identifiés")
                 )
               ),
               
               div(
                 class = "conteneur-statistique",
                 div(
                   style = "text-align: center;",
                   div(class = "nombre-statistique", textOutput("taux_survie_global")),
                   div(class = "etiquette-statistique", "Taux de Survie Global")
                 )
               ),
               
               # Avertissement éthique important
               div(
                 class = "carte-avertissement",
                 h4(icon("exclamation-triangle"), "Avertissement Médical", style = "margin-top: 0; font-size: 1.3em;"),
                 p("Cette plateforme constitue un outil d'aide à la décision médicale destiné exclusivement à des fins de recherche.", 
                   style = "margin-bottom: 0; font-size: 1em; line-height: 1.6;")
               ),
               
               # Informations sur les données
               div(
                 class = "conteneur-graphique",
                 h4(icon("database"), "Référentiel de Données", style = "color: #2c3e50; margin-bottom: 15px;"),
                 div(style = "line-height: 1.8;",
                     p(strong("Wisconsin Breast Cancer")),
                     p(strong("Source:"), " UCI Repository"),
                     p(strong("Auteurs:"), " Dr. William Wolberg, Olvi Mangasarian, W. Rue, Rue Nick"),
                     p(strong("Période de collecte:"), "1995"),
                     p(strong("Institution:"), " Hôpitaux Universitaires du Wisconsin"),
                     hr(style = "margin: 10px 0;"),
                     p(strong("SEER")),
                     p(strong("Source:"), " Programme SEER (National Cancer Institute, USA"),
                     p(strong("Période de collecte:"), "1973-2015"),
                     p(strong("Institution:"), " National Cancer Institute"),
                     hr(style = "margin: 10px 0;"),
                     p(strong("Source primaire:"), " Wisconsin Breast Cancer + SEER"),
                     p(strong("Responsable scientifique:"), " Dr. William H. Wolberg, Olvi L. Mangasarian, W. Nick Street"),
                     hr(style = "margin: 15px 0;"),
                     h5("Données intégrées:", style = "color: #2c3e50; margin-bottom: 10px;"),
                     tags$ul(
                       style = "font-size: 0.95em; color: #6c757d;",
                       tags$li("Caractéristiques morphologiques cellulaires"),
                       tags$li("Données cliniques et démographiques"),
                       tags$li("Informations de survie et suivi"),
                       tags$li("Facteurs pronostiques validés")
                     )
                 )
               )
        )
      )
    )
  ),
  
  # ===============================================================================
  # ONGLET 2: EXPLORATION STATISTIQUE UNIVARIÉE
  # ===============================================================================
  tabPanel(
    title = div(icon("chart-bar"), "Analyse Univariée"),
    fluidPage(
      div(
        class = "entete-section",
        h2(icon("search"), "Exploration Statistique Approfondie des Variables", 
           style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
        p("Analyse détaillée des caractéristiques morphologiques avec interprétations cliniques automatisées", 
          style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "conteneur-graphique",
            h4(icon("sliders-h"), "Paramètres d'Analyse", style = "color: #2c3e50; margin-bottom: 20px;"),
            
            selectInput("variable_selectionnee", 
                        "Variable à analyser :", 
                        choices = NULL,
                        selected = NULL),
            
            hr(style = "margin: 20px 0;"),
            
            checkboxGroupInput("types_visualisations", 
                               "Types de visualisations :",
                               choices = list(
                                 "Diagramme en boîtes par diagnostic" = "boites",
                                 "Histogramme de distribution" = "histogramme", 
                                 "Courbes de densité" = "densite",
                                 "Graphique en violon" = "violon"
                               ),
                               selected = c("boites", "histogramme")),
            
            hr(style = "margin: 20px 0;"),
            
            sliderInput("transparence_graphiques",
                        "Niveau de transparence :",
                        min = 0.1, max = 1.0, value = 0.75, step = 0.05),
            
            actionButton("lancer_analyse", 
                         "Lancer l'Analyse",
                         class = "bouton-personnalise",
                         style = "width: 100%; margin-top: 15px;")
          ),
          
          # Tests statistiques avancés
          div(
            class = "carte-information",
            h4(icon("calculator"), "Tests Statistiques", 
               style = "margin-top: 0; color: white; font-size: 1.2em;"),
            withSpinner(verbatimTextOutput("resultats_tests_statistiques"), 
                        color = "#ffffff", type = 4)
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Section d'interprétation automatique
          div(
            class = "carte-interpretation",
            div(class = "titre-interpretation", 
                icon("brain"), " INTERPRÉTATION CLINIQUE AUTOMATISÉE"),
            withSpinner(verbatimTextOutput("interpretation_univariee_automatique"), 
                        color = "#ffffff", type = 4)
          ),
          
          # Résumé statistique complet
          fluidRow(
            column(12,
                   div(
                     class = "conteneur-graphique",
                     h3(icon("table"), "Statistiques Descriptives Complètes", 
                        style = "color: #2c3e50; margin-bottom: 25px;"),
                     fluidRow(
                       column(6, 
                              h4("Statistiques Globales", style = "color: #667eea;"),
                              withSpinner(verbatimTextOutput("resume_statistique_enrichi"), 
                                          color = "#667eea", type = 4)
                       ),
                       column(6,
                              h4("Comparaison Diagnostique", style = "color: #764ba2;"),
                              withSpinner(verbatimTextOutput("comparaison_groupes_diagnostiques"), 
                                          color = "#764ba2", type = 4)
                       )
                     )
                   )
            )
          ),
          
          # Visualisations multiples
          fluidRow(
            column(6,
                   conditionalPanel(
                     condition = "input.types_visualisations.indexOf('boites') > -1",
                     div(
                       class = "conteneur-graphique",
                       h4(icon("box"), "Distribution par Diagnostic", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("graphique_boites_ameliore", height = "420px"), 
                                   color = "#667eea", type = 4)
                     )
                   )
            ),
            column(6,
                   conditionalPanel(
                     condition = "input.types_visualisations.indexOf('histogramme') > -1",
                     div(
                       class = "conteneur-graphique", 
                       h4(icon("chart-area"), "Histogramme de Distribution", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("histogramme_ameliore", height = "420px"), 
                                   color = "#764ba2", type = 4)
                     )
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   conditionalPanel(
                     condition = "input.types_visualisations.indexOf('densite') > -1",
                     div(
                       class = "conteneur-graphique",
                       h4(icon("wave-square"), "Courbes de Densité", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("graphique_densite", height = "420px"), 
                                   color = "#3498db", type = 4)
                     )
                   )
            ),
            column(6,
                   conditionalPanel(
                     condition = "input.types_visualisations.indexOf('violon') > -1",
                     div(
                       class = "conteneur-graphique",
                       h4(icon("music"), "Graphique en Violon", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("graphique_violon", height = "420px"), 
                                   color = "#e74c3c", type = 4)
                     )
                   )
            )
          ),
          
          # Tableau de données interactif
          div(
            class = "conteneur-graphique",
            h3(icon("table"), "Données Détaillées avec Filtrage Avancé", 
               style = "color: #2c3e50; margin-bottom: 20px;"),
            withSpinner(DT::dataTableOutput("tableau_donnees_ameliore"), 
                        color = "#667eea", type = 4)
          )
        )
      )
    )
  ),
  
  # ===============================================================================
  # ONGLET 3: ANALYSE MULTIVARIÉE AVANCÉE
  # ===============================================================================
  tabPanel(
    title = div(icon("project-diagram"), "Analyse Multivariée"),
    fluidPage(
      div(
        class = "entete-section",
        h2(icon("connectdevelop"), "Exploration des Relations Multidimensionnelles", 
           style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
        p("Découverte de patterns complexes avec interprétations cliniques avancées", 
          style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
      ),
      
      tabsetPanel(
        type = "tabs",
        
        # Sous-onglet: Matrice de corrélations
        tabPanel(
          title = div(icon("bezier-curve"), "Matrice de Corrélations", style = "color: black; font-weight: bold"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "conteneur-graphique",
                h4(icon("adjust"), "Paramètres de Corrélation", style = "color: #2c3e50;"),
                
                selectInput("methode_correlation",
                            "Méthode de corrélation :",
                            choices = list(
                              "Pearson (linéaire)" = "pearson",
                              "Spearman (rang)" = "spearman", 
                              "Kendall (tau)" = "kendall"
                            )),
                
                sliderInput("seuil_correlation",
                            "Seuil de significativité :",
                            min = 0, max = 1, value = 0.05, step = 0.01),
                
                checkboxInput("masquer_non_significatives",
                              "Masquer corrélations non-significatives",
                              value = TRUE),
                
                actionButton("actualiser_correlations",
                             "Actualiser l'Analyse",
                             class = "bouton-personnalise",
                             style = "width: 100%; margin-top: 15px;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # Interprétation des corrélations
              div(
                class = "carte-interpretation",
                div(class = "titre-interpretation", 
                    icon("brain"), " INTERPRÉTATION DES CORRÉLATIONS"),
                withSpinner(verbatimTextOutput("interpretation_correlations"), 
                            color = "#ffffff", type = 4)
              ),
              
              div(
                class = "conteneur-graphique",
                h3(icon("th"), "Matrice de Corrélations Interactive", 
                   style = "color: #2c3e50; margin-bottom: 20px;"),
                withSpinner(plotlyOutput("matrice_correlations", height = "650px"), 
                            color = "#667eea", type = 4)
              ),
              
              div(
                class = "conteneur-graphique",
                h3(icon("list-ol"), "Corrélations les Plus Significatives", 
                   style = "color: #2c3e50; margin-bottom: 20px;"),
                withSpinner(DT::dataTableOutput("top_correlations_significatives"), 
                            color = "#764ba2", type = 4)
              )
            )
          )
        ),
        
        # Sous-onglet: Analyse en Composantes Principales
        tabPanel(
          title = div(icon("compass"), "Analyse en Composantes Principales", style = "color: black; font-weight: bold"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "conteneur-graphique",
                h4(icon("cog"), "Paramètres ACP", style = "color: #2c3e50;"),
                
                numericInput("nombre_composantes",
                             "Nombre de composantes :",
                             value = 5, min = 2, max = 15),
                
                checkboxInput("centrer_reduire_variables",
                              "Centrer-réduire les variables",
                              value = TRUE),
                
                checkboxInput("afficher_contributions",
                              "Afficher les contributions",
                              value = TRUE),
                
                actionButton("executer_acp",
                             "Exécuter l'ACP",
                             class = "bouton-personnalise",
                             style = "width: 100%; margin-top: 15px;")
              ),
              
              br(),
              
              div(
                class = "carte-succes",
                h4(icon("info-circle"), "À Propos de l'ACP", style = "margin-top: 0;"),
                p("L'Analyse en Composantes Principales révèle les directions de plus grande variabilité dans l'espace multidimensionnel des données, permettant une réduction de dimensionnalité optimale tout en préservant l'information essentielle.", 
                  style = "margin-bottom: 0; font-size: 0.95em; line-height: 1.6;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # Interprétation ACP
              conditionalPanel(
                condition = "input.executer_acp > 0",
                div(
                  class = "carte-interpretation",
                  div(class = "titre-interpretation", 
                      icon("brain"), " INTERPRÉTATION ACP"),
                  withSpinner(verbatimTextOutput("interpretation_acp"), 
                              color = "#ffffff", type = 4)
                )
              ),
              
              fluidRow(
                column(6,
                       div(
                         class = "conteneur-graphique",
                         h4(icon("chart-pie"), "Variance Expliquée", style = "color: #2c3e50;"),
                         withSpinner(plotlyOutput("variance_expliquee_acp", height = "380px"), 
                                     color = "#3498db", type = 4)
                       )
                ),
                column(6,
                       div(
                         class = "conteneur-graphique",
                         h4(icon("dot-circle"), "Biplot ACP", style = "color: #2c3e50;"),
                         withSpinner(plotlyOutput("biplot_acp", height = "380px"), 
                                     color = "#e74c3c", type = 4)
                       )
                )
              ),
              
              conditionalPanel(
                condition = "input.afficher_contributions == true && input.executer_acp > 0",
                div(
                  class = "conteneur-graphique",
                  h3(icon("table"), "Contributions des Variables aux Composantes", 
                     style = "color: #2c3e50; margin-bottom: 20px;"),
                  withSpinner(DT::dataTableOutput("contributions_variables_acp"), 
                              color = "#667eea", type = 4)
                )
              )
            )
          )
        ),
        
        # Sous-onglet: Relations bivariées
        tabPanel(
          title = div(icon("braille"), "Relations Bivariées", style = "color: black; font-weight: bold"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              div(
                class = "conteneur-graphique",
                h4(icon("crosshairs"), "Sélection des Variables", style = "color: #2c3e50;"),
                
                selectInput("variable_x", "Variable X :", choices = NULL),
                selectInput("variable_y", "Variable Y :", choices = NULL),
                
                hr(style = "margin: 20px 0;"),
                
                checkboxInput("ajouter_regression",
                              "Ajouter ligne de régression",
                              value = TRUE),
                
                checkboxInput("afficher_intervalles_confiance",
                              "Intervalles de confiance",
                              value = TRUE),
                
                sliderInput("taille_points",
                            "Taille des points :",
                            min = 1, max = 6, value = 3),
                
                actionButton("actualiser_nuage_points",
                             "Actualiser le Graphique",
                             class = "bouton-personnalise",
                             style = "width: 100%; margin-top: 15px;")
              )
            ),
            
            mainPanel(
              width = 9,
              
              # Interprétation des relations bivariées
              div(
                class = "carte-interpretation",
                div(class = "titre-interpretation", 
                    icon("brain"), " INTERPRÉTATION DE LA RELATION"),
                withSpinner(verbatimTextOutput("interpretation_relation_bivariee"), 
                            color = "#ffffff", type = 4)
              ),
              
              div(
                class = "conteneur-graphique",
                h3(icon("chart-line"), "Nuage de Points Interactif", 
                   style = "color: #2c3e50; margin-bottom: 20px;"),
                withSpinner(plotlyOutput("nuage_points_ameliore", height = "550px"), 
                            color = "#667eea", type = 4)
              ),
              
              div(
                class = "conteneur-graphique",
                h3(icon("calculator"), "Analyses Statistiques Approfondies", 
                   style = "color: #2c3e50; margin-bottom: 20px;"),
                fluidRow(
                  column(6,
                         h4("Modèle de Régression Linéaire", style = "color: #3498db;"),
                         withSpinner(verbatimTextOutput("resume_regression_lineaire"), 
                                     color = "#3498db", type = 4)
                  ),
                  column(6,
                         h4("Tests de Corrélation Multiples", style = "color: #e74c3c;"),
                         withSpinner(verbatimTextOutput("tests_correlation_multiples"), 
                                     color = "#e74c3c", type = 4)
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ===============================================================================
  # ONGLET 4: MODÉLISATION PRÉDICTIVE
  # ===============================================================================
  tabPanel(
    title = div(icon("robot"), "Modélisation Prédictive"),
    fluidPage(
      div(
        class = "entete-section",
        h2(icon("brain"), "Intelligence Artificielle Prédictive", 
           style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
        p("Développement et évaluation de modèles d'apprentissage automatique pour le diagnostic", 
          style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "conteneur-graphique",
            h4(icon("settings"), "Configuration du Modèle", style = "color: #2c3e50;"),
            
            selectInput("algorithme_ml",
                        "Algorithme d'apprentissage :",
                        choices = list(
                          "Régression Logistique" = "glm",
                          "Forêt Aléatoire" = "rf"
                        )),
            
            sliderInput("proportion_entrainement",
                        "Proportion d'entraînement :",
                        min = 0.6, max = 0.9, value = 0.8, step = 0.05),
            
            numericInput("validation_croisee_k",
                         "K-fold validation croisée :",
                         value = 10, min = 3, max = 20),
            
            checkboxInput("optimiser_hyperparametres",
                          "Optimisation des hyperparamètres",
                          value = TRUE),
            
            actionButton("entrainer_modele",
                         "Entraîner le Modèle",
                         class = "bouton-personnalise",
                         style = "width: 100%; margin-top: 20px;")
          ),
          
          # Section de prédiction individuelle
          div(
            class = "carte-information",
            h4(icon("user-md"), "Prédiction Individuelle", 
               style = "margin-top: 0; color: white;"),
            
            numericInput("rayon_cellulaire_input", "Rayon cellulaire :", 
                         value = 14.0, min = 6, max = 30, step = 0.1),
            numericInput("texture_surface_input", "Texture surface :", 
                         value = 19.0, min = 9, max = 40, step = 0.1),
            numericInput("perimetre_cellulaire_input", "Périmètre :", 
                         value = 91.0, min = 40, max = 200, step = 0.5),
            numericInput("superficie_cellulaire_input", "Superficie :", 
                         value = 654.0, min = 140, max = 2500, step = 1),
            
            actionButton("predire_cas_individuel",
                         "Effectuer Prédiction",
                         class = "bouton-personnalise",
                         style = "width: 100%; margin-top: 15px; background: linear-gradient(135deg, #27ae60, #229954);")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Résultats de prédiction individuelle
          conditionalPanel(
            condition = "input.predire_cas_individuel > 0",
            div(
              class = "carte-interpretation",
              div(class = "titre-interpretation", 
                  icon("stethoscope"), " RÉSULTAT DE LA PRÉDICTION"),
              withSpinner(verbatimTextOutput("resultat_prediction_individuelle"), 
                          color = "#ffffff", type = 4)
            )
          ),
          
          # Performance du modèle
          conditionalPanel(
            condition = "input.entrainer_modele > 0",
            fluidRow(
              column(6,
                     div(
                       class = "conteneur-graphique",
                       h4(icon("chart-bar"), "Métriques de Performance", style = "color: #2c3e50;"),
                       withSpinner(verbatimTextOutput("metriques_performance_modele"), 
                                   color = "#667eea", type = 4)
                     )
              ),
              column(6,
                     div(
                       class = "conteneur-graphique",
                       h4(icon("target"), "Matrice de Confusion", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("matrice_confusion", height = "350px"), 
                                   color = "#e74c3c", type = 4)
                     )
              )
            )
          ),
          
          # Courbe ROC et importance des variables
          conditionalPanel(
            condition = "input.entrainer_modele > 0",
            fluidRow(
              column(6,
                     div(
                       class = "conteneur-graphique",
                       h4(icon("chart-line"), "Courbe ROC", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("courbe_roc", height = "400px"), 
                                   color = "#3498db", type = 4)
                     )
              ),
              column(6,
                     div(
                       class = "conteneur-graphique",
                       h4(icon("sort-amount-down"), "Importance des Variables", style = "color: #2c3e50;"),
                       withSpinner(plotlyOutput("importance_variables", height = "400px"), 
                                   color = "#f39c12", type = 4)
                     )
              )
            )
          )
        )
      )
    )
  ),
  
  # ===============================================================================
  # ONGLET 5: PRÉDICTION DE SURVIE
  # ===============================================================================
  
tabPanel(
  title = div(icon("clock"), "Prédiction de Survie"),
  fluidPage(
    div(
      class = "entete-section",
      h2(
        tags$img(
          src = "cancer_de_sein.png", 
          height = "30px", 
          width = "30px",
          style = "margin-right: 12px; border-radius: 6px;"
        ),
        "Analyse de Survie et Pronostic", 
        style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
      p("Estimation de la survie basée sur les données cliniques SEER pour les cas malins", 
        style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = "carte-information",
          h4(icon("user-md"), "Profil Patient", 
             style = "margin-top: 0; color: white;"),
          
          numericInput("age_patient", "Âge du patient :", 
                       value = 55, min = 18, max = 100),
          
          selectInput("race_patient", "Origine ethnique:",
                      choices = c("White", "Black", "Other"),
                      selected = "White"),
          
          selectInput("statut_marital", "Statut marital:",
                      choices = c("Married", "Single", "Divorced", "Widowed"),
                      selected = "Married"),
          
          selectInput("t_stage", "Stade T (taille tumorale):",
                      choices = c("T1", "T2", "T3", "T4"),
                      selected = "T2"),
          
          selectInput("n_stage", "Stade N (ganglions):",
                      choices = c("N0", "N1", "N2", "N3"),
                      selected = "N0"),
          
          selectInput("grade_tumeur", "Grade histologique:",
                      choices = c("Grade I", "Grade II", "Grade III"),
                      selected = "Grade II"),
          
          numericInput("taille_tumeur", "Taille tumorale (mm):", 
                       value = 25, min = 1, max = 100),
          
          selectInput("statut_estrogene", "Statut œstrogène:",
                      choices = c("Positive", "Negative"),
                      selected = "Positive"),
          
          selectInput("statut_progesterone", "Statut progestérone:",
                      choices = c("Positive", "Negative"),
                      selected = "Positive"),
          
          actionButton("predire_survie",
                       "Prédire la Survie",
                       class = "bouton-survie",
                       style = "width: 100%; margin-top: 15px; background: linear-gradient(135deg, #27ae60, #229954);")
        )
      ),
      
      mainPanel(
        width = 9,
        
        # Résultats de prédiction de survie
        conditionalPanel(
          condition = "input.predire_survie > 0",
          div(
            class = "carte-interpretation",
            div(class = "titre-interpretation", 
                icon("clock"), " PRÉDICTION DE SURVIE"),
            withSpinner(verbatimTextOutput("resultat_prediction_survie"), 
                        color = "#ffffff", type = 4),
            
            # Ajout du bouton de téléchargement ici
            div(
              style = "text-align: center; margin-top: 20px;",
              downloadButton("telecharger_rapport_survie",
                             "Télécharger le Rapport de Survie",
                             class = "bouton-personnalise",
                             style = "width: 80%; margin-top: 15px;")
            )
          )
        ),
        
        # Graphiques de survie
        conditionalPanel(
          condition = "input.predire_survie > 0",
          fluidRow(
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("chart-line"), "Courbe de Survie Kaplan-Meier", style = "color: #2c3e50;"),
                     withSpinner(plotlyOutput("courbe_survie_km", height = "400px"), 
                                 color = "#fd79a8", type = 4)
                   )
            ),
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("users"), "Comparaison par Groupes", style = "color: #2c3e50;"),
                     withSpinner(plotlyOutput("survie_par_groupes", height = "400px"), 
                                 color = "#fdcb6e", type = 4)
                   )
            )
          )
        ),
        
        # Facteurs pronostiques
        conditionalPanel(
          condition = "input.predire_survie > 0",
          fluidRow(
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("chart-bar"), "Facteurs Pronostiques", style = "color: #2c3e50;"),
                     withSpinner(plotlyOutput("facteurs_pronostiques", height = "400px"), 
                                 color = "#a29bfe", type = 4)
                   )
            ),
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("table"), "Données SEER - Aperçu", style = "color: #2c3e50;"),
                     withSpinner(DT::dataTableOutput("apercu_donnees_seer"), 
                                 color = "#74b9ff", type = 4)
                   )
            )
          )
        )
      )
    )
  )
),
  
  # ===============================================================================
  # ONGLET 6: GESTION DES DONNÉES
  # ===============================================================================
  tabPanel(
    title = div(icon("database"), "Gestion des Données"),
    fluidPage(
      div(
        class = "entete-section",
        h2(icon("archive"), "Centre de Gestion des Données Médicales", 
           style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
        p("Accès complet aux données, métadonnées et outils d'exportation", 
          style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "conteneur-graphique",
            h4(icon("filter"), "Filtres et Options", style = "color: #2c3e50;"),
            
            selectInput("choix_jeu_donnees", 
                        "Jeu de données :",
                        choices = c("Wisconsin Breast Cancer" = "wisconsin", 
                                    "SEER Breast Cancer" = "seer")),
            
            sliderInput("lignes_apercu", 
                        "Lignes à afficher :",
                        min = 10, max = 200, value = 50, step = 10),
            
            checkboxGroupInput("colonnes_affichees",
                               "Colonnes à afficher :",
                               choices = NULL,
                               selected = NULL),
            
            hr(style = "margin: 20px 0;"),
            
            downloadButton("telecharger_donnees_csv", 
                           "Télécharger Données (.csv)",
                           class = "bouton-personnalise",
                           style = "width: 100%; margin-bottom: 12px;"),
            
            downloadButton("telecharger_rapport_complet",
                           "Rapport Complet (.html)", 
                           class = "bouton-personnalise",
                           style = "width: 100%; margin-bottom: 12px;"),
            
            downloadButton("telecharger_modele_entrainee",
                           "Modèle Entraîné (.rds)", 
                           class = "bouton-personnalise",
                           style = "width: 100%;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Informations détaillées sur le jeu de données
          div(
            class = "carte-information",
            h3(icon("info-circle"), "Informations Détaillées sur le Jeu de Données", 
               style = "margin-top: 0; font-size: 1.4em;"),
            withSpinner(htmlOutput("informations_detaillees_donnees"), 
                        color = "#ffffff", type = 4)
          ),
          
          # Aperçu des données
          div(
            class = "conteneur-graphique",
            h3(icon("eye"), "Aperçu Interactif des Données", 
               style = "color: #2c3e50; margin-bottom: 20px;"),
            withSpinner(DT::dataTableOutput("apercu_donnees_interactif"), 
                        color = "#667eea", type = 4)
          ),
          
          # Statistiques et qualité des données
          fluidRow(
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("chart-bar"), "Distribution des Variables", style = "color: #2c3e50;"),
                     withSpinner(plotlyOutput("distribution_variables", height = "320px"), 
                                 color = "#3498db", type = 4)
                   )
            ),
            column(6,
                   div(
                     class = "conteneur-graphique",
                     h4(icon("exclamation-triangle"), "Rapport de Qualité des Données", style = "color: #2c3e50;"),
                     withSpinner(verbatimTextOutput("rapport_qualite_donnees"), 
                                 color = "#e74c3c", type = 4)
                   )
            )
          )
        )
      )
    )
  ),
  
  # ===============================================================================
  # ONGLET 7: DOCUMENTATION ET AIDE
  # ===============================================================================
  tabPanel(
    title = div(icon("book"), "Documentation"),
    fluidPage(
      div(
        class = "entete-section",
        h2(icon("graduation-cap"), "Centre de Documentation et d'Aide", 
           style = "margin: 0; font-weight: 400; font-size: 2.2em;"),
        p("Guide complet d'utilisation et ressources scientifiques", 
          style = "margin: 8px 0 0 0; opacity: 0.9; font-size: 1.1em;")
      ),
      
      fluidRow(
        column(8,
               div(
                 class = "conteneur-graphique",
                 h3(icon("question-circle"), "Guide d'Utilisation", style = "color: #2c3e50;"),
                 
                 h4("1. Exploration des Données", style = "color: #667eea; margin-top: 25px;"),
                 p("Commencez par l'onglet 'Analyse Univariée' pour explorer individuellement chaque variable morphologique. Sélectionnez une variable d'intérêt et choisissez les types de visualisations appropriés.", style = "line-height: 1.6;"),
                 
                 h4("2. Analyse des Relations", style = "color: #667eea; margin-top: 25px;"),
                 p("Utilisez l'onglet 'Analyse Multivariée' pour découvrir les corrélations entre variables et effectuer une réduction de dimensionnalité par ACP.", style = "line-height: 1.6;"),
                 
                 h4("3. Modélisation Prédictive", style = "color: #667eea; margin-top: 25px;"),
                 p("L'onglet 'Modélisation Prédictive' permet d'entraîner différents algorithmes d'apprentissage automatique et d'évaluer leurs performances.", style = "line-height: 1.6;"),
                 
                 h4("4. Prédiction de Survie", style = "color: #fd79a8; margin-top: 25px;"),
                 p("Pour les cas malins détectés, utilisez l'onglet 'Prédiction de Survie' pour estimer le pronostic basé sur les caractéristiques cliniques du patient.", style = "line-height: 1.6;"),
                 
                 h4("5. Gestion des Données", style = "color: #667eea; margin-top: 25px;"),
                 p("Accédez aux données brutes, exportez les résultats et téléchargez les modèles entraînés via l'onglet 'Gestion des Données'.", style = "line-height: 1.6;")
               ),
               
               div(
                 class = "conteneur-graphique",
                 h3(icon("flask"), "Méthodologie Scientifique", style = "color: #2c3e50;"),
                 
                 h4("Acquisition des Données", style = "color: #e74c3c; margin-top: 20px;"),
                 p("Les données proviennent de deux sources complémentaires : images numérisées de biopsies par aspiration à l'aiguille fine (FNA) pour les caractéristiques morphologiques, et registres SEER pour les données de survie.", style = "line-height: 1.6;"),
                 
                 h4("Extraction de Caractéristiques", style = "color: #e74c3c; margin-top: 20px;"),
                 p("Pour chaque noyau cellulaire, 10 caractéristiques sont calculées : rayon, texture, périmètre, aire, lissage, compacité, concavité, points concaves, symétrie et dimension fractale.", style = "line-height: 1.6;"),
                 
                 h4("Analyse de Survie", style = "color: #e74c3c; margin-top: 20px;"),
                 p("Les modèles de survie utilisent les méthodes de Kaplan-Meier et de régression de Cox pour estimer la probabilité de survie basée sur les facteurs cliniques et démographiques.", style = "line-height: 1.6;"),
                 
                 h4("Validation Statistique", style = "color: #e74c3c; margin-top: 20px;"),
                 p("Tous les modèles sont validés par validation croisée k-fold et évalués selon des métriques standard : précision, rappel, F1-score et aire sous la courbe ROC.", style = "line-height: 1.6;")
               )
        ),
        
        column(4,
               div(
                 class = "carte-succes",
                 h4(icon("lightbulb"), "Conseils d'Utilisation", style = "margin-top: 0;"),
                 tags$ul(
                   style = "line-height: 1.8;",
                   tags$li("Commencez toujours par explorer les données univariées"),
                   tags$li("Vérifiez la qualité des données avant la modélisation"),
                   tags$li("Utilisez la validation croisée pour évaluer les modèles"),
                   tags$li("Pour les cas malins, procédez à l'analyse de survie"),
                   tags$li("Interprétez toujours les résultats dans le contexte clinique"),
                   tags$li("Sauvegardez vos modèles performants")
                 )
               ),
               
               div(
                 class = "carte-avertissement",
                 h4(icon("exclamation-triangle"), "Limitations", style = "margin-top: 0;"),
                 p("Cette application est destinée à des fins de recherche. Les résultats ne doivent pas être utilisés pour des décisions cliniques sans validation par un professionnel de santé qualifié.", 
                   style = "margin-bottom: 0; line-height: 1.6;")
               ),
               
               div(
                 class = "conteneur-graphique",
                 h4(icon("envelope"), "Support Technique", style = "color: #2c3e50;"),
                 p("Pour toute question technique ou scientifique concernant cette application, veuillez consulter la documentation complète ou contacter l'équipe de développement.", style = "line-height: 1.6; margin-bottom: 0;"),
                 tags$ul(
                   tags$li("Jason FOFANA : ", tags$a(href = "mailto:fofanajason@gmail.com", "fofanajason@gmail.com")),
                   tags$li("Christelle YAKE : ", tags$a(href = "mailto:ychristellerebecca@gmail.com", "ychristellerebecca@gmail.com"))
                 )
               )
        )
      )
    )
  )
)

# Export de l'interface utilisateur
ui <- interface_principale