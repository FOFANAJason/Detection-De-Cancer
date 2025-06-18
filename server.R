library(shiny)
library(shiny)
library(dplyr)       # Pour les opérations de manipulation de données (%>%, select, mutate, etc.)
library(ggplot2)     # Pour les visualisations
library(corrplot)    # Pour la matrice de corrélation
library(factoextra)  # Pour l'analyse ACP
library(DT)  

function(input, output, session) {
  # Chargement des données avec mémorisation pour meilleure performance
  dataset <- reactive({
    data <- read.csv("Dataset/data.csv") %>% 
      select(-id, -X) %>%  # Suppression des colonnes inutiles
      mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Bénin", "Malin")))
    
    # Conversion des noms de variables en français pour une meilleure lisibilité
    french_names <- c(
      "diagnosis" = "Diagnostic",
      "radius_mean" = "Rayon_moyen",
      "texture_mean" = "Texture_moyenne",
      "perimeter_mean" = "Périmètre_moyen",
      "area_mean" = "Aire_moyenne",
      "smoothness_mean" = "Lissité_moyenne",
      "compactness_mean" = "Compacité_moyenne",
      "concavity_mean" = "Concavité_moyenne",
      "concave_points_mean" = "Points_concaves_moyens",
      "symmetry_mean" = "Symétrie_moyenne",
      "fractal_dimension_mean" = "Dimension_fractale_moyenne"
    )
    
    nom_courrant <- names(data)
    nouveau_nom <- french_names[nom_courrant]
    
    nouveau_nom <- ifelse(is.na(nouveau_nom), nom_courrant, nouveau_nom)
    names(data) <- nouveau_nom
    
    # Vérification des noms de colonnes
    validate(
      need(!any(is.na(names(data))), "Erreur : Noms de colonnes invalides"),
      need("Diagnostic" %in% names(data), "La colonne 'Diagnostic' est manquante")
    )
    
    data
    
    
  })
  
  # Dictionnaire de traduction SEER
  french_names_seer <- c(
    "Age" ="Age",
    "Marital.Status" = "Etat_civil",
    "Status"="Statut_Survie",
    "T.Stage"="Stade_T",
    "X6th.Stage"="X6th.Stage",
    "Tumor.size" = "Taille_tumeur",
    "Regional.Node.Examined" = "Ganglions_examinés",
    "Survival.Months" = "Mois_de_survie",
    "N.Stage"= "Stade_N",
    "A.Stage"= "Stade_Metastatique",
    "Grade"= "Grade_Tumoral",
    "Tumor.Size"= "Taille_Tumeur",
    "Estrogen.Status"= "Statut_Recepteurs_Estrogenes",
    "Progesterone.Status"= "Statut_Recepteurs_Progesterone",
    "Reginol.Node.Positive"= "Ganglions_Positifs",
    "Race"="Race"
  )
  
  # 
  output$dataset_details <- renderUI({
    if(input$dataset_choice == "data") {
      tagList(
        h4("Détails du jeu de données 'data'"),
        p("Ce jeu de données contient des caractéristiques dérivées d'images numérisées de masses mammaires, utilisées pour diagnostiquer des tumeurs cancéreuses. Chaque ligne représente une observation de cellule,décrivant des attributs morphologiques."),
        p("Modifications apportées:"),
        tags$ul(
          tags$li("Suppression des colonnes 'id' et 'X'"),
          tags$li("Recodage de la variable 'diagnosis' : B -> Bénin, M -> Malin"),
          tags$li("Changement du nom des variables")
        )
      )
    } else if(input$dataset_choice == "seer") {
      tagList(
        h4("Détails du jeu de données 'SEER Breast Cancer'"),
        p("Cet ensemble de données cliniques capture le parcours médical des patientes atteintes d'un cancer du sein, documenté par le programme SEER (Surveillance, Epidemiology, and End Results). Chaque observation révèle l'interaction complexe entre les caractéristiques tumorales et les facteurs démographiques qui influencent le pronostic vital."),
        p("Après nettoyage (suppression d'une colonne vide), 8 variables sont réellement importantes pour prédire Statut_Survie:"),
        tags$ul(
          tags$li(tags$b("Stade_T"), ": Stade de la tumeur primaire (T1 à T4). Plus élevé = pronostic plus sombre."),
          tags$li(tags$b("Stage_N"), ": Atteinte des ganglions lymphatiques (N0 à N3). Critère pronostique majeur."),
          tags$li(tags$b("Stade_Metastatique"), ": Présence de métastases (Regional vs Distant). Les métastases réduisent radicalement la survie."),
          tags$li(tags$b("Grade_Tumoral"), ": Agressivité tumorale (Grade I à IV). Grade III/IV = risque accru de décès."),
          tags$li(tags$b("Taille_Tumeur"), ": Taille réelle de la tumeur (en mm). >20 mm = pronostic réservé."),
          tags$li(tags$b("Statut_Recepteurs_Estrogenes"), ": Tumeurs ER+ = meilleure réponse au traitement hormonal."),
          tags$li(tags$b("Statut_Recepteurs_Progesterone"), ": Tumeurs PR+ = survie prolongée."),
          tags$li(tags$b("Ganglions_Positifs"), ": Nombre de ganglions envahis. >3 ganglions = pronostic défavorable.")
        ),
        p("Modifications apportées:"),
        tags$ul(
          tags$li("Suppression de la colonne 'X' contenant des NA"),
          tags$li("Changement du nom des variables")
        )
      )
    }
  })
  
  output$dataset_info <- renderText({
    data <- dataset()
    n_obs <- nrow(data)
    n_vars <- ncol(data)
    paste("Le dataset contient", n_obs, "observations et", n_vars, 
          "variables (Diagnostic, radius_mean +", n_vars-2, "caractéristiques).")
  })
  
  # Mise à jour dynamique des sélecteurs
  observe({
    updateSelectInput(session, "variable", 
                      choices = setdiff(names(dataset()), "Diagnostic"),
                      selected = "Rayon_moyen")
    
    updateSelectInput(session, "xvar", 
                      choices = setdiff(names(dataset()), "Diagnostic"),
                      selected = "Rayon_moyen")
    
    updateSelectInput(session, "yvar", 
                      choices = setdiff(names(dataset()), "Diagnostic"),
                      selected = "Texture_moyenne")
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(input$variable)
    ggplot(dataset(), aes_string(x = "Diagnostic", y = input$variable, fill = "Diagnostic")) +
      geom_boxplot() +
      scale_fill_manual(values = c("Bénin" = "lightblue", "Malin" = "salmon")) +
      theme_minimal() +
      labs(title = paste("Distribution de", input$variable, "par diagnostic"),
           x = "Diagnostic", y = input$variable)
  })
  
  # Nuage de points
  output$scatterplot <- renderPlot({
    req(input$xvar, input$yvar)
    ggplot(dataset(), aes_string(x = input$xvar, y = input$yvar, color = "Diagnostic")) +
      geom_point(alpha = 0.7, size = 5) +
      scale_color_manual(values = c("Bénin" = "blue", "Malin" = "red")) +
      theme_minimal() +
      labs(title = paste("Relation entre", input$xvar, "et", input$yvar),
           x = input$xvar, y = input$yvar)
  })
  
  # Statistiques descriptives
  output$summary <- renderPrint({
    req(input$variable)
    df <- dataset()
    var <- df[[input$variable]]
    
    if(is.numeric(var)) {
      summary(var)
    } else {
      table(var)
    }
  })
  
  # Matrice de corrélation
  output$corrplot <- renderPlot({
    numeric_data <- dataset() %>% 
      select_if(is.numeric) %>% 
      select(-contains("_se"), -contains("_worst"))  # On garde que les moyennes
    
    cor_matrix <- cor(numeric_data)
    
    corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                       tl.col = "black", tl.srt = 45, 
                       addCoef.col = "black", number.cex = 0.7)
  })
  
  # Tableau de données interactif
  output$datatable <- renderDT({
    req(dataset())
    datatable(dataset(),
              options = list(
                pageLength = 10,
                language = list(
                  search = "Rechercher:",
                  paginate = list(previous = 'Précédent', `next` = 'Suivant')
                )
              ))
  })
  # Fonction pour lire les datasets avec aperçu
  load_dataset_preview <- function(dataset_name, n_rows = 50) {
    tryCatch({
      if(dataset_name == "data") {
        data <- read.csv("Dataset/data.csv", nrows = n_rows) %>% 
          select(-id, -X) %>% 
          mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Bénin", "Malin")))
        return(data)
      }
      else if(dataset_name == "seer") {
        data <- read.csv("Dataset/SEER_Breast_Cancer_Dataset.csv", nrows = n_rows) %>%
        select(-X)  # Suppression de la colonne X
        
        # Application la traduction des variables
        idx <- match(names(data), names(french_names_seer))
        new_names <- ifelse(is.na(idx), names(data), french_names_seer[idx])
        
        names(data) <- new_names
        return(data)
        
      }
      
    }, error = function(e) {
      data.frame(Error = paste("Erreur de lecture :", e$message))
    })
  }
  
  # Réactive pour l'aperçu des données
  dataset_preview <- reactive({
    req(input$dataset_choice, input$preview_rows)
    load_dataset_preview(input$dataset_choice, input$preview_rows)
  })
  
  # Description des datasets
  output$dataset_desc <- renderUI({
    desc <- switch(input$dataset_choice,
                   "data" = "<b>Data</b><br>569 observations, 32 variables<br>Caractéristiques des noyaux cellulaires",
                   "seer" = "<b>SEER Breast Cancer Dataset</b><br>4024 observations<br>Données épidémiologiques de surveillance")
                   
    
    HTML(paste(desc, "<br><br>Source: Kaggle"))
  })
  
  # Titre du dataset
  output$dataset_title <- renderText({
    switch(input$dataset_choice,
           "data" = "Aperçu: Data",
           "seer" = "Aperçu: SEER Breast Cancer")
  })
  

  # Fonction pour charger les datasets 
  load_full_dataset <- function(dataset_name) {
    if(dataset_name == "data") {
      data <- read.csv("Dataset/data.csv") %>% 
        select(-id, -X) %>% 
        mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Bénin", "Malin")))
      return(data)
    } 
    else if(dataset_name == "seer") {
      data <- read.csv("Dataset/SEER_Breast_Cancer_Dataset.csv") %>%
        select(-X)
      
      # Application la traduction des variables
      idx <- match(names(data), names(french_names_seer))
      new_names <- ifelse(is.na(idx), names(data), french_names_seer[idx])
      names(data) <- new_names
      
      return(data)
    }
  }
  
  # Réactive pour le dataset complet
  full_dataset <- reactive({
    req(input$dataset_choice)
    load_full_dataset(input$dataset_choice)
  })
  
  # Output pour les dimensions
  output$dataset_dimensions <- renderUI({
    data <- full_dataset()
    n_rows <- nrow(data)
    n_cols <- ncol(data)
    
    HTML(paste0("<div class='alert alert-success'>",
                "Dimensions complètes: ", n_rows, " lignes × ", n_cols, " colonnes",
                "</div>"))
  })
  
  # Output pour la table
  output$dataset_table <- renderDT({
    data <- full_dataset()
    
    datatable(data,
              options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100, nrow(data)),
                scrollX = TRUE,
                language = list(
                  search = "Rechercher:",
                  paginate = list(previous = 'Précédent', `next` = 'Suivant'),
                  lengthMenu = "Afficher _MENU_ lignes"
                )
              ))
  })
  
  # Téléchargement
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset_choice, "_data.csv")
    },
    content = function(file) {
      write.csv(full_dataset(), file, row.names = FALSE)
    }
  )

  
  
}

