library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(plotly)
library(DT)
library(factoextra)
library(FactoMineR)
library(moments)
library(car) 
library(moments)  # Ajouté pour skewness et kurtosis
library(car) # Ajouté pour leveneTest

# Fonction utilitaire pour les tests de corrélation (déplacée avant server)
cor.mtest <- function(mat, conf.level = 0.95, method = "pearson") {
  # Validation des entrées
  if(!is.data.frame(mat) && !is.matrix(mat)) {
    stop("L'argument 'mat' doit être un dataframe ou une matrice")
  }
  
  if(ncol(mat) < 2) {
    stop("La matrice doit avoir au moins 2 colonnes")
  }
  
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  # Calcul des p-values pour chaque paire de variables
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- tryCatch({
        cor.test(mat[, i], mat[, j], 
                 method = method,
                 conf.level = conf.level)
      }, error = function(e) {
        list(p.value = NA)
      })
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(list(p = p.mat))
}


# Serveur avec logique améliorée et interprétations automatiques
server <- function(input, output, session) {
  
  # ============= CHARGEMENT ET PRÉPARATION DES DONNÉES =============
  # Chargement des données avec mémorisation pour meilleure performance
  dataset <- reactive({
    # Vérification de l'existence du fichier
    if(!file.exists("Dataset/data.csv")) {
      stop("Le fichier Dataset/data.csv n'existe pas. Veuillez vérifier le chemin.")
    }
    
    data <- read.csv("Dataset/data.csv") %>% 
      select(-matches("^id$|^X$")) %>%  # Suppression plus robuste des colonnes inutiles
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
  
  
  # ============= FONCTIONS D'INTERPRÉTATION AUTOMATIQUE =============
  
  # Interprétation des statistiques univariées
  interpret_univariate <- function(variable_name, benign_mean, malignant_mean, p_value, effect_size) {
    interpretation <- ""
    
    # Analyse de la différence entre groupes
    diff_percent <- abs((malignant_mean - benign_mean) / benign_mean * 100)
    
    if (p_value < 0.001) {
      significance <- "très hautement significative (p < 0.001)"
    } else if (p_value < 0.01) {
      significance <- "hautement significative (p < 0.01)"
    } else if (p_value < 0.05) {
      significance <- "significative (p < 0.05)"
    } else {
      significance <- "non significative (p ≥ 0.05)"
    }
    
    # Taille d'effet
    if (abs(effect_size) > 0.8) {
      effect_desc <- "très important"
    } else if (abs(effect_size) > 0.5) {
      effect_desc <- "important"
    } else if (abs(effect_size) > 0.2) {
      effect_desc <- "modéré"
    } else {
      effect_desc <- "faible"
    }
    
    interpretation <- paste0(
      "📊 INTERPRÉTATION CLINIQUE :\n\n",
      "• La différence entre tumeurs bénignes et malignes est ", significance, "\n",
      "• L'effet clinique est ", effect_desc, " (d de Cohen = ", round(effect_size, 2), ")\n",
      "• Les tumeurs malignes présentent des valeurs ",
      ifelse(malignant_mean > benign_mean, "supérieures", "inférieures"),
      " de ", round(diff_percent, 1), "% en moyenne\n\n"
    )
    
    # Interprétations spécifiques par variable
    specific_interp <- switch(variable_name,
                              "Rayon_moyen" = "🔍 SIGNIFICATION : Un rayon cellulaire plus important indique généralement une croissance cellulaire anormale, caractéristique des tumeurs malignes. Cette mesure est cruciale pour le diagnostic précoce.",
                              
                              "Texture_moyenne" = "🔍 SIGNIFICATION : La texture reflète l'hétérogénéité de la surface cellulaire. Une texture plus rugueuse suggère une désorganisation cellulaire typique des cancers.",
                              
                              "Périmètre_moyen" = "🔍 SIGNIFICATION : Le périmètre est étroitement lié à la taille cellulaire. Des périmètres plus grands indiquent une expansion cellulaire pathologique.",
                              
                              "Aire_moyenne" = "🔍 SIGNIFICATION : L'aire cellulaire augmentée est un marqueur direct de la prolifération cancéreuse. Cette mesure est particulièrement discriminante.",
                              
                              "Compacité_moyenne" = "🔍 SIGNIFICATION : La compacité mesure la régularité de la forme cellulaire. Une compacité élevée indique une déformation cellulaire caractéristique des tumeurs malignes.",
                              
                              "Concavité_moyenne" = "🔍 SIGNIFICATION : Les concavités représentent les indentations de la membrane cellulaire. Plus de concavités suggèrent une morphologie cellulaire altérée.",
                              
                              "Points_concaves_moyens" = "🔍 SIGNIFICATION : Le nombre de points concaves reflète la complexité de la forme cellulaire. Une augmentation indique une déformation pathologique.",
                              
                              "Symétrie_moyenne" = "🔍 SIGNIFICATION : La perte de symétrie cellulaire est un indicateur de dysplasie. Les cellules normales maintiennent une symétrie relative.",
                              
                              "Dimension_fractale_moyenne" = "🔍 SIGNIFICATION : Cette mesure quantifie la complexité géométrique. Une dimension fractale élevée indique une architecture cellulaire désorganisée.",
                              
                              "🔍 SIGNIFICATION : Cette caractéristique morphologique contribue à la différenciation entre tumeurs bénignes et malignes."
    )
    
    # Recommandations cliniques
    clinical_rec <- ""
    if (p_value < 0.05 && abs(effect_size) > 0.5) {
      clinical_rec <- paste0(
        "\n💡 RECOMMANDATION CLINIQUE :\n",
        "Cette variable présente un fort pouvoir discriminant et devrait être considérée comme un biomarqueur important dans l'algorithme de diagnostic automatisé."
      )
    }
    
    return(paste0(interpretation, specific_interp, clinical_rec))
  }
  
  # Interprétation des corrélations
  interpret_correlation <- function(var1, var2, correlation, p_value) {
    interpretation <- ""
    
    # Force de la corrélation
    abs_corr <- abs(correlation)
    if (abs_corr > 0.9) {
      strength <- "très forte"
      clinical_impact <- "Ces variables sont presque redondantes et pourraient être combinées dans un index composite."
    } else if (abs_corr > 0.7) {
      strength <- "forte"
      clinical_impact <- "Cette relation suggère des mécanismes biologiques communs sous-jacents."
    } else if (abs_corr > 0.5) {
      strength <- "modérée"
      clinical_impact <- "Cette association pourrait refléter des processus cellulaires liés."
    } else if (abs_corr > 0.3) {
      strength <- "faible à modérée"
      clinical_impact <- "Cette relation mérite une investigation plus approfondie."
    } else {
      strength <- "faible"
      clinical_impact <- "Ces variables semblent largement indépendantes."
    }
    
    direction <- ifelse(correlation > 0, "positive", "négative")
    
    interpretation <- paste0(
      "📈 ANALYSE DE CORRÉLATION :\n\n",
      "• Corrélation ", direction, " ", strength, " (r = ", round(correlation, 3), ")\n",
      "• Significativité statistique : ", ifelse(p_value < 0.05, "Oui", "Non"), "\n\n",
      "🔬 INTERPRÉTATION BIOLOGIQUE :\n",
      clinical_impact, "\n\n"
    )
    
    # Interprétations spécifiques pour certaines paires
    if (grepl("Rayon|Périmètre|Aire", var1) && grepl("Rayon|Périmètre|Aire", var2)) {
      interpretation <- paste0(interpretation,
                               "💡 NOTE CLINIQUE : Les mesures de taille (rayon, périmètre, aire) sont naturellement corrélées. Cette redondance peut être exploitée pour créer un score de taille composite plus robuste.")
    }
    
    return(interpretation)
  }
  
  # Interprétation de l'ACP
  interpret_pca <- function(variance_explained, n_components) {
    interpretation <- paste0(
      "🔍 ANALYSE EN COMPOSANTES PRINCIPALES :\n\n",
      "• Les ", n_components, " premières composantes expliquent ", 
      round(sum(variance_explained[1:n_components]), 1), "% de la variance totale\n",
      "• Réduction de dimensionnalité : ", length(variance_explained), " variables → ", n_components, " composantes\n\n"
    )
    
    if (variance_explained[1] > 40) {
      interpretation <- paste0(interpretation,
                               "💡 INTERPRÉTATION : La première composante capture une grande partie de la variabilité (", 
                               round(variance_explained[1], 1), "%), suggérant un facteur dominant, probablement lié à la taille cellulaire globale.\n\n")
    }
    
    if (sum(variance_explained[1:2]) > 70) {
      interpretation <- paste0(interpretation,
                               "✅ RECOMMANDATION : Les deux premières composantes suffisent pour capturer la majorité de l'information (", 
                               round(sum(variance_explained[1:2]), 1), "%). Cela simplifie considérablement l'analyse tout en préservant l'essentiel des données.\n\n")
    }
    
    interpretation <- paste0(interpretation,
                             "🎯 APPLICATION CLINIQUE : Cette réduction de dimensionnalité permet de créer des algorithmes de diagnostic plus simples et plus rapides, tout en maintenant la précision diagnostique.")
    
    return(interpretation)
  }
  
  # ============= MISE À JOUR DES SÉLECTEURS =============
  observe({
    choices <- setdiff(names(dataset()), "Diagnostic")
    
    updateSelectInput(session, "variable", choices = choices, selected = choices[1])
    updateSelectInput(session, "xvar", choices = choices, selected = choices[1])
    updateSelectInput(session, "yvar", choices = choices, selected = choices[2])
    updateCheckboxGroupInput(session, "columns_to_show", choices = names(dataset()), 
                             selected = names(dataset())[1:6])
  })
  
  # ============= SECTION PRÉSENTATION - STATISTIQUES =============
  output$dataset_size <- renderText({
    nrow(dataset())
  })
  
  output$malignant_count <- renderText({
    sum(dataset()$Diagnostic == "Malin")
  })
  
  output$benign_count <- renderText({
    sum(dataset()$Diagnostic == "Bénin")
  })
  
  # ============= EXPLORATION UNIVARIÉE AVEC INTERPRÉTATIONS =============
  
  # Statistiques descriptives enrichies
  output$enhanced_summary <- renderPrint({
    req(input$variable)
    var_data <- dataset()[[input$variable]]
    
    if(is.numeric(var_data)) {
      summary_stats <- list(
        "Moyenne" = round(mean(var_data, na.rm = TRUE), 3),
        "Médiane" = round(median(var_data, na.rm = TRUE), 3),
        "Écart-type" = round(sd(var_data, na.rm = TRUE), 3),
        "Variance" = round(var(var_data, na.rm = TRUE), 3),
        "Minimum" = round(min(var_data, na.rm = TRUE), 3),
        "Maximum" = round(max(var_data, na.rm = TRUE), 3),
        "Q1" = round(quantile(var_data, 0.25, na.rm = TRUE), 3),
        "Q3" = round(quantile(var_data, 0.75, na.rm = TRUE), 3),
        "Asymétrie" = round(moments::skewness(var_data, na.rm = TRUE), 3),
        "Aplatissement" = round(moments::kurtosis(var_data, na.rm = TRUE), 3)
      )
      
      cat("=== STATISTIQUES DESCRIPTIVES ===\n")
      for(i in 1:length(summary_stats)) {
        cat(sprintf("%-15s: %s\n", names(summary_stats)[i], summary_stats[[i]]))
      }
    }
  })
  
  # Comparaison par groupe diagnostique avec interprétation
  output$group_summary <- renderPrint({
    req(input$variable)
    df <- dataset()
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      benin_data <- var_data[df$Diagnostic == "Bénin"]
      malin_data <- var_data[df$Diagnostic == "Malin"]
      
      cat("=== COMPARAISON PAR DIAGNOSTIC ===\n\n")
      cat("BÉNIN:\n")
      cat(sprintf("  Moyenne: %.3f\n", mean(benin_data, na.rm = TRUE)))
      cat(sprintf("  Médiane: %.3f\n", median(benin_data, na.rm = TRUE)))
      cat(sprintf("  Écart-type: %.3f\n", sd(benin_data, na.rm = TRUE)))
      
      cat("\nMALIN:\n")
      cat(sprintf("  Moyenne: %.3f\n", mean(malin_data, na.rm = TRUE)))
      cat(sprintf("  Médiane: %.3f\n", median(malin_data, na.rm = TRUE)))
      cat(sprintf("  Écart-type: %.3f\n", sd(malin_data, na.rm = TRUE)))
      
      # Test de différence
      t_test <- t.test(benin_data, malin_data)
      cat(sprintf("\nTEST T DE STUDENT:\n"))
      cat(sprintf("  p-value: %.2e\n", t_test$p.value))
      cat(sprintf("  Différence significative: %s\n", 
                  ifelse(t_test$p.value < 0.05, "OUI", "NON")))
    }
  })
  
  # NOUVELLE SORTIE : Interprétation automatique univariée
  output$univariate_interpretation <- renderText({
    req(input$variable)
    df <- dataset()
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      benin_data <- var_data[df$Diagnostic == "Bénin"]
      malin_data <- var_data[df$Diagnostic == "Malin"]
      
      benin_mean <- mean(benin_data, na.rm = TRUE)
      malin_mean <- mean(malin_data, na.rm = TRUE)
      
      # Test t et taille d'effet
      t_test <- t.test(benin_data, malin_data)
      pooled_sd <- sqrt(((length(benin_data)-1)*var(benin_data) + (length(malin_data)-1)*var(malin_data)) / 
                          (length(benin_data) + length(malin_data) - 2))
      cohens_d <- (malin_mean - benin_mean) / pooled_sd
      
      interpret_univariate(input$variable, benin_mean, malin_mean, t_test$p.value, cohens_d)
    }
  })
  
  # Tests statistiques
  output$statistical_tests <- renderPrint({
    req(input$variable)
    df <- dataset()
    var_data <- df[[input$variable]]
    
    if(is.numeric(var_data)) {
      # Test de normalité
      shapiro_test <- shapiro.test(sample(var_data, min(5000, length(var_data))))
      
      # Test de variance égale
      levene_test <- car::leveneTest(var_data ~ df$Diagnostic)
      
      cat("=== TESTS STATISTIQUES ===\n\n")
      cat("NORMALITÉ (Shapiro-Wilk):\n")
      cat(sprintf("  p-value: %.2e\n", shapiro_test$p.value))
      cat(sprintf("  Distribution normale: %s\n\n", 
                  ifelse(shapiro_test$p.value > 0.05, "OUI", "NON")))
      
      cat("ÉGALITÉ DES VARIANCES (Levene):\n")
      cat(sprintf("  p-value: %.2e\n", levene_test$`Pr(>F)`[1]))
      cat(sprintf("  Variances égales: %s\n", 
                  ifelse(levene_test$`Pr(>F)`[1] > 0.05, "OUI", "NON")))
    }
  })
  
  # Boxplot amélioré
  output$enhanced_boxplot <- renderPlotly({
    req(input$variable)
    
    p <- ggplot(dataset(), aes_string(x = "Diagnostic", y = input$variable, fill = "Diagnostic")) +
      geom_boxplot(alpha = input$alpha_transparency, outlier.shape = 16, outlier.size = 2) +
      geom_jitter(width = 0.3, alpha = 0.4, size = 1) +
      scale_fill_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "none"
      ) +
      labs(
        title = paste("Distribution de", input$variable, "par diagnostic"),
        x = "Diagnostic", 
        y = input$variable,
        caption = "Points individuels superposés pour montrer la distribution"
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })
  
  # Histogramme interactif
  output$enhanced_histogram <- renderPlotly({
    req(input$variable)
    
    p <- ggplot(dataset(), aes_string(x = input$variable, fill = "Diagnostic")) +
      geom_histogram(alpha = input$alpha_transparency, position = "identity", bins = 30) +
      scale_fill_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)
      ) +
      labs(
        title = paste("Histogramme de", input$variable),
        x = input$variable,
        y = "Fréquence",
        fill = "Diagnostic"
      )
    
    ggplotly(p)
  })
  
  # Courbes de densité
  output$density_plot <- renderPlotly({
    req(input$variable)
    
    p <- ggplot(dataset(), aes_string(x = input$variable, fill = "Diagnostic", color = "Diagnostic")) +
      geom_density(alpha = input$alpha_transparency) +
      scale_fill_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      scale_color_manual(values = c("Bénin" = "#2980b9", "Malin" = "#c0392b")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)
      ) +
      labs(
        title = paste("Densité de", input$variable),
        x = input$variable,
        y = "Densité"
      )
    
    ggplotly(p)
  })
  
  # Violin plot
  output$violin_plot <- renderPlotly({
    req(input$variable)
    
    p <- ggplot(dataset(), aes_string(x = "Diagnostic", y = input$variable, fill = "Diagnostic")) +
      geom_violin(alpha = input$alpha_transparency, trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
      scale_fill_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "none"
      ) +
      labs(
        title = paste("Violin plot de", input$variable),
        x = "Diagnostic",
        y = input$variable
      )
    
    ggplotly(p)
  })
  
  # Tableau interactif amélioré
  output$enhanced_datatable <- renderDT({
    datatable(
      dataset(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = "Rechercher:",
          lengthMenu = "Afficher _MENU_ entrées",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
          paginate = list(
            first = 'Premier',
            last = 'Dernier', 
            `next` = 'Suivant',
            previous = 'Précédent'
          )
        )
      ),
      filter = 'top',
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle('Diagnostic',
                  backgroundColor = styleEqual(c('Bénin', 'Malin'), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  # ============= ANALYSE MULTIVARIÉE AVEC INTERPRÉTATIONS =============
  print('Test avant ANALYSE MULTIVARIÉE -------------------')
  
  # Matrice de corrélations interactive
  output$correlation_matrix <- renderPlotly({
    numeric_data <- dataset() %>% 
      select_if(is.numeric)
    
    # Au moin deux colonne numérieque ?
    validate(
      need(ncol(numeric_data) >= 2, "Au moins 2 variables numériques sont nécessaires")
    )
    
    cor_matrix <- cor(numeric_data, method = input$corr_method)
    
    print('Test -------------------')
    
    # Test de significativité
    cor_test_results <- tryCatch({
      if(ncol(numeric_data) > 1) {
        cor.mtest(numeric_data, conf.level = 0.95)
      } else {
        NULL
      }
    }, error = function(e) {
      message("Erreur dans cor.mtest: ", e$message)
      NULL
    })
    
    if(input$show_insignificant) {
      cor_matrix[cor_test_results$p > 0.05] <- NA
    }
    
    # Conversion en format long pour plotly
    cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
    cor_df$value <- as.vector(cor_matrix)
    
    p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#e74c3c", high = "#3498db", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name = "Corrélation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", title = "Matrice de Corrélations")
    
    ggplotly(p)
  })
  
  # NOUVELLE SORTIE : Interprétation des corrélations
  output$correlation_interpretation <- renderText({
    numeric_data <- dataset() %>% select_if(is.numeric)
    cor_matrix <- cor(numeric_data, method = input$corr_method)
    
    # Trouver la corrélation la plus forte (hors diagonale)
    cor_matrix_no_diag <- cor_matrix
    diag(cor_matrix_no_diag) <- NA
    max_cor_idx <- which(abs(cor_matrix_no_diag) == max(abs(cor_matrix_no_diag), na.rm = TRUE), arr.ind = TRUE)[1,]
    
    var1 <- rownames(cor_matrix)[max_cor_idx[1]]
    var2 <- colnames(cor_matrix)[max_cor_idx[2]]
    max_cor <- cor_matrix[max_cor_idx[1], max_cor_idx[2]]
    
    # Test de corrélation pour la p-value
    cor_test <- cor.test(numeric_data[[var1]], numeric_data[[var2]], method = input$corr_method)
    
    interpret_correlation(var1, var2, max_cor, cor_test$p.value)
  })
  
  # Top corrélations
  output$top_correlations <- renderDT({
    numeric_data <- dataset() %>% select_if(is.numeric)
    cor_matrix <- cor(numeric_data, method = input$corr_method)
    
    # Extraire les corrélations en excluant la diagonale
    cor_pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
    correlations <- data.frame(
      Variable1 = rownames(cor_matrix)[cor_pairs[,1]],
      Variable2 = colnames(cor_matrix)[cor_pairs[,2]], 
      Correlation = cor_matrix[cor_pairs],
      Abs_Correlation = abs(cor_matrix[cor_pairs])
    )
    
    correlations <- correlations[order(-correlations$Abs_Correlation), ]
    correlations$Correlation <- round(correlations$Correlation, 3)
    correlations <- correlations[, -4]  # Supprimer colonne absolue
    
    datatable(correlations, options = list(pageLength = 10)) %>%
      formatStyle('Correlation',
                  backgroundColor = styleInterval(c(-0.7, -0.3, 0.3, 0.7),
                                                  c('#ffebee', '#fff3e0', '#ffffff', '#e8f5e8', '#c8e6c9')))
  })
  
  # ACP - Variance expliquée
  output$pca_variance <- renderPlotly({
    if(input$run_pca > 0) {
      numeric_data <- dataset() %>% select_if(is.numeric)
      
      if(input$scale_variables) {
        numeric_data <- scale(numeric_data)
      }
      
      pca_result <- PCA(numeric_data, graph = FALSE, ncp = input$n_components)
      
      variance_df <- data.frame(
        Component = paste0("PC", 1:input$n_components),
        Variance = pca_result$eig[1:input$n_components, 2],
        Cumulative = cumsum(pca_result$eig[1:input$n_components, 2])
      )
      
      p <- ggplot(variance_df, aes(x = Component)) +
        geom_col(aes(y = Variance), fill = "#3498db", alpha = 0.7) +
        geom_line(aes(y = Cumulative, group = 1), color = "#e74c3c", size = 1.2) +
        geom_point(aes(y = Cumulative), color = "#e74c3c", size = 3) +
        theme_minimal() +
        labs(title = "Variance Expliquée par Composante",
             x = "Composante Principale",
             y = "% Variance Expliquée")
      
      ggplotly(p)
    }
  })
  
  # NOUVELLE SORTIE : Interprétation ACP
  output$pca_interpretation <- renderText({
    if(input$run_pca > 0) {
      numeric_data <- dataset() %>% select_if(is.numeric)
      
      if(input$scale_variables) {
        numeric_data <- scale(numeric_data)
      }
      
      pca_result <- PCA(numeric_data, graph = FALSE, ncp = input$n_components)
      variance_explained <- pca_result$eig[, 2]
      
      interpret_pca(variance_explained, input$n_components)
    }
  })
  
  # ACP - Biplot
  output$pca_biplot <- renderPlotly({
    if(input$run_pca > 0) {
      numeric_data <- dataset() %>% select_if(is.numeric)
      
      if(input$scale_variables) {
        numeric_data <- scale(numeric_data)
      }
      
      pca_result <- PCA(numeric_data, graph = FALSE)
      
      # Scores des individus
      scores <- as.data.frame(pca_result$ind$coord[, 1:2])
      scores$Diagnostic <- dataset()$Diagnostic
      
      p <- ggplot(scores, aes(Dim.1, Dim.2, color = Diagnostic)) +
        geom_point(alpha = 0.7, size = 2) +
        scale_color_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
        theme_minimal() +
        labs(title = "Biplot ACP (PC1 vs PC2)",
             x = paste0("PC1 (", round(pca_result$eig[1,2], 1), "%)"),
             y = paste0("PC2 (", round(pca_result$eig[2,2], 1), "%)"))
      
      ggplotly(p)
    }
  })
  
  # Contributions ACP
  output$pca_contributions <- renderDT({
    if(input$run_pca > 0) {
      numeric_data <- dataset() %>% select_if(is.numeric)
      
      if(input$scale_variables) {
        numeric_data <- scale(numeric_data)
      }
      
      pca_result <- PCA(numeric_data, graph = FALSE, ncp = input$n_components)
      
      contrib_df <- as.data.frame(pca_result$var$contrib)
      contrib_df$Variable <- rownames(contrib_df)
      contrib_df <- contrib_df[, c(ncol(contrib_df), 1:(ncol(contrib_df)-1))]
      
      datatable(contrib_df, options = list(pageLength = 10)) %>%
        formatRound(2:ncol(contrib_df), 2)
    }
  })
  
  # Nuage de points avancé
  output$enhanced_scatterplot <- renderPlotly({
    req(input$xvar, input$yvar)
    
    p <- ggplot(dataset(), aes_string(x = input$xvar, y = input$yvar, color = "Diagnostic")) +
      geom_point(alpha = 0.7, size = input$point_size) +
      scale_color_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      labs(title = paste("Relation entre", input$xvar, "et", input$yvar),
           x = input$xvar, y = input$yvar)
    
    if(input$add_regression) {
      p <- p + geom_smooth(method = "lm", se = TRUE, alpha = 0.3)
    }
    
    ggplotly(p)
  })
  
  # NOUVELLE SORTIE : Interprétation du nuage de points
  output$scatterplot_interpretation <- renderText({
    req(input$xvar, input$yvar)
    
    x_data <- dataset()[[input$xvar]]
    y_data <- dataset()[[input$yvar]]
    
    # Corrélation globale
    cor_test <- cor.test(x_data, y_data)
    
    interpret_correlation(input$xvar, input$yvar, cor_test$estimate, cor_test$p.value)
  })
  
  # Analyse de régression
  output$regression_summary <- renderPrint({
    req(input$xvar, input$yvar)
    
    formula_str <- paste(input$yvar, "~", input$xvar)
    lm_model <- lm(as.formula(formula_str), data = dataset())
    
    cat("=== RÉGRESSION LINÉAIRE ===\n")
    cat(sprintf("Modèle: %s\n\n", formula_str))
    
    summary_lm <- summary(lm_model)
    cat(sprintf("R² = %.4f\n", summary_lm$r.squared))
    cat(sprintf("R² ajusté = %.4f\n", summary_lm$adj.r.squared))
    cat(sprintf("p-value = %.2e\n\n", pf(summary_lm$fstatistic[1], 
                                         summary_lm$fstatistic[2], 
                                         summary_lm$fstatistic[3], 
                                         lower.tail = FALSE)))
    
    cat("Coefficients:\n")
    print(round(summary_lm$coefficients, 4))
  })
  
  # Tests de corrélation
  output$correlation_tests <- renderPrint({
    req(input$xvar, input$yvar)
    
    x_data <- dataset()[[input$xvar]]
    y_data <- dataset()[[input$yvar]]
    
    # Tests de corrélation
    cor_pearson <- cor.test(x_data, y_data, method = "pearson")
    cor_spearman <- cor.test(x_data, y_data, method = "spearman")
    
    cat("=== TESTS DE CORRÉLATION ===\n\n")
    cat("PEARSON:\n")
    cat(sprintf("  r = %.4f\n", cor_pearson$estimate))
    cat(sprintf("  p-value = %.2e\n", cor_pearson$p.value))
    cat(sprintf("  IC 95%%: [%.3f, %.3f]\n\n", 
                cor_pearson$conf.int[1], cor_pearson$conf.int[2]))
    
    cat("SPEARMAN:\n")
    cat(sprintf("  ρ = %.4f\n", cor_spearman$estimate))
    cat(sprintf("  p-value = %.2e\n", cor_spearman$p.value))
  })
  
  # ============= SECTION JEUX DE DONNÉES =============
  
  # Informations détaillées sur le dataset
  output$dataset_info_detailed <- renderUI({
    data <- dataset()
    n_obs <- nrow(data)
    n_vars <- ncol(data)
    n_benin <- sum(data$Diagnostic == "Bénin")
    n_malin <- sum(data$Diagnostic == "Malin")
    
    HTML(paste0(
      "<h4><i class='fa fa-database'></i> Wisconsin Breast Cancer Dataset</h4>",
      "<p><strong>Origine:</strong> University of Wisconsin Hospitals, Madison</p>",
      "<p><strong>Période de collecte:</strong> 1991-1995</p>",
      "<p><strong>Responsable:</strong> Dr. William H. Wolberg</p>",
      "<hr>",
      "<p><strong>Dimensions:</strong> ", n_obs, " observations × ", n_vars, " variables</p>",
      "<p><strong>Distribution des diagnostics:</strong></p>",
      "<ul>",
      "<li>Cas bénins: ", n_benin, " (", round(n_benin/n_obs*100, 1), "%)</li>",
      "<li>Cas malins: ", n_malin, " (", round(n_malin/n_obs*100, 1), "%)</li>",
      "</ul>",
      "<p><strong>Variables mesurées:</strong> Caractéristiques morphologiques des noyaux cellulaires obtenues par analyse d'images numérisées de biopsies par aspiration à l'aiguille fine (FNA).</p>"
    ))
  })
  
  # Aperçu du tableau de données
  output$dataset_preview_table <- renderDT({
    data <- dataset()
    
    if(!is.null(input$columns_to_show) && length(input$columns_to_show) > 0) {
      data <- data[, input$columns_to_show, drop = FALSE]
    }
    
    data <- head(data, input$preview_rows)
    
    datatable(
      data,
      options = list(
        scrollX = TRUE,
        pageLength = input$preview_rows,
        dom = 't',
        language = list(
          search = "Rechercher:",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées"
        )
      ),
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle('Diagnostic',
                  backgroundColor = styleEqual(c('Bénin', 'Malin'), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  # Distribution du diagnostic
  output$diagnosis_distribution <- renderPlotly({
    data <- dataset()
    counts <- table(data$Diagnostic)
    
    df <- data.frame(
      Diagnostic = names(counts),
      Count = as.numeric(counts),
      Percentage = round(as.numeric(counts) / sum(counts) * 100, 1)
    )
    
    p <- ggplot(df, aes(x = Diagnostic, y = Count, fill = Diagnostic)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c("Bénin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Distribution des Diagnostics",
           x = "Diagnostic", y = "Nombre de cas") +
      ylim(0, max(df$Count) * 1.1)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Qualité des données
  output$data_quality_summary <- renderPrint({
    data <- dataset()
    
    cat("=== RAPPORT DE QUALITÉ ===\n\n")
    cat("COMPLÉTUDE DES DONNÉES:\n")
    missing_summary <- sapply(data, function(x) sum(is.na(x)))
    for(i in 1:length(missing_summary)) {
      cat(sprintf("  %-20s: %d valeurs manquantes\n", 
                  names(missing_summary)[i], missing_summary[i]))
    }
    
    cat("\nTYPES DE VARIABLES:\n")
    types_summary <- sapply(data, class)
    for(i in 1:length(types_summary)) {
      cat(sprintf("  %-20s: %s\n", names(types_summary)[i], types_summary[i]))
    }
    
    cat("\nVALEURS ABERRANTES:\n")
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    for(var in numeric_vars) {
      Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      outliers <- sum(data[[var]] < (Q1 - 1.5*IQR) | data[[var]] > (Q3 + 1.5*IQR), na.rm = TRUE)
      cat(sprintf("  %-20s: %d valeurs aberrantes\n", var, outliers))
    }
  })
  
  # Téléchargement des données
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("breast_cancer_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  # Téléchargement du rapport
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport_cancer_sein_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Ici vous pouvez générer un rapport HTML complet
      # Pour simplifier, on crée un rapport basique
      html_content <- paste0(
        "<html><head><title>Rapport d'Analyse</title></head><body>",
        "<h1>Rapport d'Analyse - Cancer du Sein</h1>",
        "<p>Date de génération: ", Sys.Date(), "</p>",
        "<p>Nombre d'observations: ", nrow(dataset()), "</p>",
        "<p>Ce rapport contient l'analyse complète des données...</p>",
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
}
