library(shiny)
library(shiny)
library(dplyr)       # Pour les opérations de manipulation de données (%>%, select, mutate, etc.)
library(ggplot2)     # Pour les visualisations
library(corrplot)    # Pour la matrice de corrélation
library(factoextra)  # Pour l'analyse ACP
library(DT)  

function(input, output, session) {
  # Chargement des données avec mémoisation pour meilleure performance
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
  
  
}
