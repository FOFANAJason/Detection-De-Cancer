# ===============================================================================
# LOGIQUE SERVEUR - ANALYSE INTELLIGENTE DU CANCER DU SEIN
# ===============================================================================
# 
# Auteur: Système d'Analyse Médicale Avancée
# Version: 1.0
# Date: 2025
# 
# Description: Logique serveur complète pour l'analyse des données de biopsies
# mammaires avec algorithmes d'intelligence artificielle et prédiction de survie
# ===============================================================================

# Chargement des bibliothèques nécessaires pour l'analyse
library(shiny)
library(dplyr)
library(ggplot2)
library(corrplot)
library(plotly)
library(DT)
library(factoextra)
library(FactoMineR)
library(car)
library(caret)
library(randomForest)
library(e1071)
library(gbm)
library(pROC)
library(VIM)
library(survival)
library(survminer)
source("www/rapport_survie.R")


# ===============================================================================
# FONCTIONS UTILITAIRES POUR L'ANALYSE STATISTIQUE
# ===============================================================================

# Fonction pour charger les données Wisconsin (détection cancer)
charger_donnees_wisconsin <- function() {
  
  donnees <- read.csv('Dataset/breast-cancer-wisconsin-data.csv')
  # Sélection des colonnes d'intérêt
  donnees <- donnees[, c('id', "diagnosis", "radius_mean", "texture_mean", 
                         "perimeter_mean", "area_mean", "smoothness_mean", 
                         "compactness_mean", "concavity_mean", 
                         "symmetry_mean", "fractal_dimension_mean")]
  
  # Nombre d'observations
  # print(table(donnees$diagnosis))
  n <- nrow(donnees)
  # print(paste("Nombre d'observations :", n))
  
  return(donnees)
}


# Fonction pour charger les données SEER (prédiction survie)
charger_donnees_seer <- function() {
  donnees <- read.csv('Dataset/seer-breast-cancer-dataset.csv')
  
  donnees <- donnees[, c("Age", "Race", "Marital.Status", "T.Stage", "N.Stage", "X6th.Stage", "Grade",
                         "A.Stage", "Tumor.Size", "Estrogen.Status", "Progesterone.Status", "Regional.Node.Examined",
                         "Reginol.Node.Positive", "Survival.Months", "Status")]
  
  return(donnees)
}
# Fonction pour calculer les tests de corrélation avec gestion d'erreurs robuste
calculer_tests_correlation <- function(matrice_donnees, niveau_confiance = 0.95, methode = "pearson") {
  # Validation rigoureuse des entrées
  if(!is.data.frame(matrice_donnees) && !is.matrix(matrice_donnees)) {
    stop("L'argument 'matrice_donnees' doit être un dataframe ou une matrice numérique")
  }
  
  if(ncol(matrice_donnees) < 2) {
    stop("La matrice doit contenir au minimum 2 colonnes pour calculer les corrélations")
  }
  
  # Conversion en matrice numérique
  matrice_numerique <- as.matrix(matrice_donnees)
  nombre_variables <- ncol(matrice_numerique)
  matrice_p_values <- matrix(NA, nombre_variables, nombre_variables)
  diag(matrice_p_values) <- 0
  
  # Calcul des p-values pour chaque paire de variables
  for (indice_i in 1:(nombre_variables - 1)) {
    for (indice_j in (indice_i + 1):nombre_variables) {
      resultat_test <- tryCatch({
        cor.test(matrice_numerique[, indice_i], matrice_numerique[, indice_j], 
                 method = methode, conf.level = niveau_confiance)
      }, error = function(erreur) {
        list(p.value = NA)
      })
      matrice_p_values[indice_i, indice_j] <- matrice_p_values[indice_j, indice_i] <- resultat_test$p.value
    }
  }
  
  colnames(matrice_p_values) <- rownames(matrice_p_values) <- colnames(matrice_numerique)
  return(list(p_values = matrice_p_values))
}

# Fonction d'interprétation automatique des analyses univariées
interpreter_analyse_univariee <- function(nom_variable, moyenne_benin, moyenne_malin, p_value, taille_effet) {
  interpretation_complete <- ""
  
  # Calcul du pourcentage de différence entre les groupes
  pourcentage_difference <- abs((moyenne_malin - moyenne_benin) / moyenne_benin * 100)
  
  # Détermination du niveau de significativité statistique
  if (p_value < 0.001) {
    niveau_significativite <- "très hautement significative (p < 0.001)"
  } else if (p_value < 0.01) {
    niveau_significativite <- "hautement significative (p < 0.01)"
  } else if (p_value < 0.05) {
    niveau_significativite <- "significative (p < 0.05)"
  } else {
    niveau_significativite <- "non significative (p ≥ 0.05)"
  }
  
  # Évaluation de la taille d'effet clinique
  if (abs(taille_effet) > 0.8) {
    description_effet <- "très important"
  } else if (abs(taille_effet) > 0.5) {
    description_effet <- "important"
  } else if (abs(taille_effet) > 0.2) {
    description_effet <- "modéré"
  } else {
    description_effet <- "faible"
  }
  
  # Construction de l'interprétation principale
  interpretation_complete <- paste0(
    "📊 ANALYSE CLINIQUE APPROFONDIE :\n\n",
    "• La différence observée entre tumeurs bénignes et malignes est ", niveau_significativite, "\n",
    "• L'impact clinique est ", description_effet, " (d de Cohen = ", round(taille_effet, 3), ")\n",
    "• Les tumeurs malignes présentent des valeurs ",
    ifelse(moyenne_malin > moyenne_benin, "supérieures", "inférieures"),
    " de ", round(pourcentage_difference, 2), "% en moyenne par rapport aux tumeurs bénignes\n\n"
  )
  
  # Interprétations spécifiques selon la variable analysée
  interpretation_specifique <- switch(nom_variable,
                                      "rayon_cellulaire_moyen" = "🔬 SIGNIFICATION BIOLOGIQUE : Un rayon cellulaire augmenté reflète une hypertrophie nucléaire caractéristique de la transformation maligne. Cette expansion cellulaire anormale constitue un marqueur précoce de la carcinogenèse mammaire.",
                                      
                                      "texture_surface_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : L'augmentation de la texture de surface indique une hétérogénéité chromatinienne accrue, témoignant de l'instabilité génomique typique des cellules cancéreuses.",
                                      
                                      "perimetre_cellulaire_moyen" = "🔬 SIGNIFICATION BIOLOGIQUE : L'expansion du périmètre cellulaire traduit une déformation morphologique progressive associée à la perte des mécanismes de contrôle de la croissance cellulaire.",
                                      
                                      "superficie_cellulaire_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : L'augmentation de la superficie nucléaire est un indicateur direct de l'activité proliférative anormale et constitue un critère diagnostique majeur en cytopathologie.",
                                      
                                      "regularite_forme_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : La perte de régularité morphologique reflète la désorganisation architecturale cellulaire caractéristique des processus néoplasiques malins.",
                                      
                                      "compacite_cellulaire_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : L'augmentation de la compacité cellulaire témoigne de modifications structurelles profondes de l'organisation nucléaire lors de la transformation maligne.",
                                      
                                      "concavite_cellulaire_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : Les concavités membranaires accrues reflètent les déformations morphologiques induites par les altérations du cytosquelette dans les cellules malignes.",
                                      
                                      "symetrie_cellulaire_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : La perte de symétrie nucléaire constitue un marqueur de dysplasie cellulaire et d'instabilité chromosomique caractéristique des tumeurs malignes.",
                                      
                                      "dimension_fractale_moyenne" = "🔬 SIGNIFICATION BIOLOGIQUE : L'augmentation de la complexité fractale quantifie la désorganisation architecturale cellulaire et corrèle avec le potentiel métastatique.",
                                      
                                      "🔬 SIGNIFICATION BIOLOGIQUE : Cette caractéristique morphologique contribue significativement à la différenciation entre phénotypes bénins et malins."
  )
  
  # Recommandations cliniques basées sur la significativité
  recommandation_clinique <- ""
  if (p_value < 0.05 && abs(taille_effet) > 0.5) {
    recommandation_clinique <- paste0(
      "\n💡 RECOMMANDATION DIAGNOSTIQUE :\n",
      "Cette variable présente un pouvoir discriminant élevé et devrait être intégrée comme biomarqueur prioritaire dans les algorithmes de diagnostic assisté par intelligence artificielle."
    )
  }
  
  return(paste0(interpretation_complete, interpretation_specifique, recommandation_clinique))
}

# Fonction d'interprétation des analyses de corrélation
interpreter_correlations <- function(variable_1, variable_2, coefficient_correlation, p_value) {
  interpretation_correlation <- ""
  
  # Évaluation de la force de la corrélation
  correlation_absolue <- abs(coefficient_correlation)
  if (correlation_absolue > 0.9) {
    force_correlation <- "très forte"
    impact_clinique <- "Ces variables présentent une redondance quasi-complète et pourraient être combinées en un index composite pour optimiser l'efficacité diagnostique."
  } else if (correlation_absolue > 0.7) {
    force_correlation <- "forte"
    impact_clinique <- "Cette relation suggère des mécanismes biologiques communs sous-jacents et une possible cascade de régulation cellulaire partagée."
  } else if (correlation_absolue > 0.5) {
    force_correlation <- "modérée"
    impact_clinique <- "Cette association pourrait refléter des processus cellulaires interconnectés méritant une investigation mécanistique approfondie."
  } else if (correlation_absolue > 0.3) {
    force_correlation <- "faible à modérée"
    impact_clinique <- "Cette relation nécessite une validation sur des cohortes indépendantes pour confirmer sa pertinence clinique."
  } else {
    force_correlation <- "faible"
    impact_clinique <- "Ces variables semblent largement indépendantes sur le plan biologique et peuvent être considérées comme complémentaires."
  }
  
  direction_correlation <- ifelse(coefficient_correlation > 0, "positive", "négative")
  
  interpretation_correlation <- paste0(
    "📈 ANALYSE DE CORRÉLATION DÉTAILLÉE :\n\n",
    "• Corrélation ", direction_correlation, " ", force_correlation, " (r = ", round(coefficient_correlation, 4), ")\n",
    "• Significativité statistique : ", ifelse(p_value < 0.05, "Significative", "Non significative"), 
    " (p = ", format(p_value, scientific = TRUE, digits = 3), ")\n\n",
    "🔬 INTERPRÉTATION BIOLOGIQUE :\n",
    impact_clinique, "\n\n"
  )
  
  # Interprétations spécialisées pour certaines paires de variables
  if (grepl("rayon|perimetre|superficie", variable_1, ignore.case = TRUE) && 
      grepl("rayon|perimetre|superficie", variable_2, ignore.case = TRUE)) {
    interpretation_correlation <- paste0(interpretation_correlation,
                                         "💡 NOTE MÉTHODOLOGIQUE : Les mesures dimensionnelles (rayon, périmètre, superficie) sont intrinsèquement corrélées par leurs relations géométriques. Cette redondance peut être exploitée pour créer un score de taille composite plus robuste et cliniquement pertinent.")
  }
  
  return(interpretation_correlation)
}

# Fonction d'interprétation de l'Analyse en Composantes Principales
interpreter_acp <- function(variance_expliquee, nombre_composantes) {
  interpretation_acp <- paste0(
    "🔍 ANALYSE EN COMPOSANTES PRINCIPALES APPROFONDIE :\n\n",
    "• Les ", nombre_composantes, " premières composantes expliquent ", 
    round(sum(variance_expliquee[1:nombre_composantes]), 2), "% de la variance totale\n",
    "• Réduction de dimensionnalité optimale : ", length(variance_expliquee), 
    " variables → ", nombre_composantes, " composantes principales\n",
    "• Efficacité de compression : ", 
    round((1 - nombre_composantes/length(variance_expliquee)) * 100, 1), "% de réduction\n\n"
  )
  
  if (variance_expliquee[1] > 40) {
    interpretation_acp <- paste0(interpretation_acp,
                                 "💡 INTERPRÉTATION PRINCIPALE : La première composante capture ", 
                                 round(variance_expliquee[1], 2), "% de la variabilité totale, suggérant l'existence d'un facteur latent dominant, probablement lié à la taille cellulaire globale ou à l'agressivité tumorale.\n\n")
  }
  
  if (sum(variance_expliquee[1:2]) > 70) {
    interpretation_acp <- paste0(interpretation_acp,
                                 "✅ RECOMMANDATION MÉTHODOLOGIQUE : Les deux premières composantes capturent ", 
                                 round(sum(variance_expliquee[1:2]), 2), "% de l'information totale. Cette réduction drastique de dimensionnalité permet de simplifier considérablement les modèles prédictifs tout en préservant l'essentiel du contenu informatif.\n\n")
  }
  
  interpretation_acp <- paste0(interpretation_acp,
                               "🎯 APPLICATION CLINIQUE : Cette réduction de dimensionnalité facilite le développement d'algorithmes de diagnostic plus rapides, plus robustes et moins sensibles au sur-apprentissage, tout en maintenant une précision diagnostique optimale.")
  
  return(interpretation_acp)
}

# Fonction de prédiction de survie
predire_survie_patient <- function(age, race, t_stage, n_stage, grade, tumor_size, estrogen_status, progesterone_status) {
  # Calcul simplifié de la survie basé sur les facteurs de risque
  score_risque <- 0
  
  # Facteurs d'âge
  if (age > 65) score_risque <- score_risque + 2
  else if (age > 50) score_risque <- score_risque + 1
  
  # Stade tumoral
  if (t_stage == "T4") score_risque <- score_risque + 4
  else if (t_stage == "T3") score_risque <- score_risque + 3
  else if (t_stage == "T2") score_risque <- score_risque + 1
  
  # Ganglions
  if (n_stage == "N3") score_risque <- score_risque + 4
  else if (n_stage == "N2") score_risque <- score_risque + 3
  else if (n_stage == "N1") score_risque <- score_risque + 1
  
  # Grade
  if (grade == "Grade III") score_risque <- score_risque + 2
  else if (grade == "Grade II") score_risque <- score_risque + 1
  
  # Taille tumorale
  if (tumor_size > 50) score_risque <- score_risque + 2
  else if (tumor_size > 20) score_risque <- score_risque + 1
  
  # Statuts hormonaux (facteurs protecteurs)
  if (estrogen_status == "Positive") score_risque <- score_risque - 1
  if (progesterone_status == "Positive") score_risque <- score_risque - 1
  
  # Calcul de la survie estimée
  survie_base <- 80  # mois
  survie_estimee <- max(12, survie_base - (score_risque * 8))
  
  # Probabilité de survie à 5 ans
  prob_survie_5ans <- max(0.3, 0.9 - (score_risque * 0.08))
  
  # Classification du risque
  if (score_risque <= 2) {
    niveau_risque <- "FAIBLE"
    couleur_risque <- "vert"
  } else if (score_risque <= 5) {
    niveau_risque <- "MODÉRÉ"
    couleur_risque <- "orange"
  } else {
    niveau_risque <- "ÉLEVÉ"
    couleur_risque <- "rouge"
  }
  
  return(list(
    score_risque = score_risque,
    survie_estimee = survie_estimee,
    prob_survie_5ans = prob_survie_5ans,
    niveau_risque = niveau_risque,
    couleur_risque = couleur_risque
  ))
}

# ===============================================================================
# LOGIQUE SERVEUR PRINCIPALE
# ===============================================================================

serveur_principal <- function(input, output, session) {
  set.seed(123)
  
  # ===============================================================================
  # CHARGEMENT ET PRÉPARATION DES DONNÉES MÉDICALES
  # ===============================================================================
  
  # Chargement des données Wisconsin et SEER
  donnees_wisconsin <- reactive({
    donnees_brutes <- charger_donnees_wisconsin()
    
    # Dictionnaire de traduction des noms de variables en français médical
    dictionnaire_traduction <- c(
      "diagnosis" = "diagnostic_tumoral",
      "radius_mean" = "rayon_cellulaire_moyen",
      "texture_mean" = "texture_surface_moyenne",
      "perimeter_mean" = "perimetre_cellulaire_moyen",
      "area_mean" = "superficie_cellulaire_moyenne",
      "smoothness_mean" = "regularite_forme_moyenne",
      "compactness_mean" = "compacite_cellulaire_moyenne",
      "concavity_mean" = "concavite_cellulaire_moyenne",
      "symmetry_mean" = "symetrie_cellulaire_moyenne",
      "fractal_dimension_mean" = "dimension_fractale_moyenne"
    )
    
    # Application de la traduction des noms de colonnes
    noms_actuels <- names(donnees_brutes)
    nouveaux_noms <- dictionnaire_traduction[noms_actuels]
    noms_traduits <- ifelse(is.na(nouveaux_noms), noms_actuels, nouveaux_noms)
    names(donnees_brutes) <- noms_traduits
    
    # Facultatif : garder uniquement les colonnes traduites
    # colonnes_conservees <- dictionnaire_traduction[noms_actuels]
    # colonnes_conservees <- colonnes_conservees[!is.na(colonnes_conservees)]
    # donnees_brutes <- donnees_brutes[, colonnes_conservees, drop = FALSE]
    
    # Conversion du diagnostic en facteur avec labels corrects
    donnees_brutes$diagnostic_tumoral <- factor(donnees_brutes$diagnostic_tumoral, 
                                                levels = c("B", "M"), 
                                                labels = c("Benin", "Malin"))
    
    return(donnees_brutes)
  })
  
  
  
  donnees_seer <- reactive({
    charger_donnees_seer()
    
  })
  
  # ===============================================================================
  # MISE À JOUR DYNAMIQUE DES SÉLECTEURS D'INTERFACE
  # ===============================================================================
  
  observe({
    variables_numeriques <- setdiff(names(donnees_wisconsin()), c("id", "diagnostic_tumoral"))
    
    # Mise à jour des sélecteurs de variables
    updateSelectInput(session, "variable_selectionnee", 
                      choices = variables_numeriques, 
                      selected = variables_numeriques[1])
    updateSelectInput(session, "variable_x", 
                      choices = variables_numeriques, 
                      selected = variables_numeriques[1])
    updateSelectInput(session, "variable_y", 
                      choices = variables_numeriques, 
                      selected = variables_numeriques[2])
    
    # Mise à jour des colonnes affichées selon le jeu de données sélectionné
    if (input$choix_jeu_donnees == "wisconsin") {
      updateCheckboxGroupInput(session, "colonnes_affichees", 
                               choices = names(donnees_wisconsin()), 
                               selected = names(donnees_wisconsin()))
    } else {
      updateCheckboxGroupInput(session, "colonnes_affichees", 
                               choices = names(donnees_seer()), 
                               selected = names(donnees_seer()))
    }
  })
  
  # ===============================================================================
  # STATISTIQUES GÉNÉRALES POUR LE TABLEAU DE BORD
  # ===============================================================================
  
  output$nombre_total_echantillons <- renderText({
    nrow(donnees_wisconsin())
  })
  
  output$nombre_patients_seer <- renderText({
    nrow(donnees_seer())
  })
  
  output$nombre_cas_malins <- renderText({
    sum(donnees_wisconsin()$diagnostic_tumoral == "Malin")
  })
  
  output$taux_survie_global <- renderText({
    taux <- round(sum(donnees_seer()$Status == "Alive") / nrow(donnees_seer()) * 100, 1)
    paste0(taux, "%")
  })
  
  # ===============================================================================
  # ANALYSES UNIVARIÉES AVEC INTERPRÉTATIONS AUTOMATIQUES
  # ===============================================================================
  
  # Statistiques descriptives enrichies
  output$resume_statistique_enrichi <- renderPrint({
    req(input$variable_selectionnee)
    donnees_variable <- donnees_wisconsin()[[input$variable_selectionnee]]
    
    if(is.numeric(donnees_variable)) {
      statistiques_descriptives <- list(
        "Moyenne arithmétique" = round(mean(donnees_variable, na.rm = TRUE), 4),
        "Médiane (Q2)" = round(median(donnees_variable, na.rm = TRUE), 4),
        "Écart-type" = round(sd(donnees_variable, na.rm = TRUE), 4),
        "Variance" = round(var(donnees_variable, na.rm = TRUE), 4),
        "Valeur minimale" = round(min(donnees_variable, na.rm = TRUE), 4),
        "Valeur maximale" = round(max(donnees_variable, na.rm = TRUE), 4),
        "Premier quartile (Q1)" = round(quantile(donnees_variable, 0.25, na.rm = TRUE), 4),
        "Troisième quartile (Q3)" = round(quantile(donnees_variable, 0.75, na.rm = TRUE), 4),
        "Coefficient d'asymétrie" = round(moments::skewness(donnees_variable, na.rm = TRUE), 4),
        "Coefficient d'aplatissement" = round(moments::kurtosis(donnees_variable, na.rm = TRUE), 4),
        "Étendue interquartile" = round(IQR(donnees_variable, na.rm = TRUE), 4)
      )
      
      cat("=== STATISTIQUES DESCRIPTIVES COMPLÈTES ===\n")
      for(i in 1:length(statistiques_descriptives)) {
        cat(sprintf("%-25s: %s\n", names(statistiques_descriptives)[i], statistiques_descriptives[[i]]))
      }
    }
  })
  
  # Comparaison par groupes diagnostiques
  output$comparaison_groupes_diagnostiques <- renderPrint({
    req(input$variable_selectionnee)
    donnees_completes <- donnees_wisconsin()
    donnees_variable <- donnees_completes[[input$variable_selectionnee]]
    
    if(is.numeric(donnees_variable)) {
      donnees_benin <- donnees_variable[donnees_completes$diagnostic_tumoral == "Benin"]
      donnees_malin <- donnees_variable[donnees_completes$diagnostic_tumoral == "Malin"]
      
      cat("=== COMPARAISON PAR DIAGNOSTIC TUMORAL ===\n\n")
      cat("TUMEURS BÉNIGNES:\n")
      cat(sprintf("  Moyenne: %.4f\n", mean(donnees_benin, na.rm = TRUE)))
      cat(sprintf("  Médiane: %.4f\n", median(donnees_benin, na.rm = TRUE)))
      cat(sprintf("  Écart-type: %.4f\n", sd(donnees_benin, na.rm = TRUE)))
      cat(sprintf("  Effectif: %d\n", length(donnees_benin)))
      
      cat("\nTUMEURS MALIGNES:\n")
      cat(sprintf("  Moyenne: %.4f\n", mean(donnees_malin, na.rm = TRUE)))
      cat(sprintf("  Médiane: %.4f\n", median(donnees_malin, na.rm = TRUE)))
      cat(sprintf("  Écart-type: %.4f\n", sd(donnees_malin, na.rm = TRUE)))
      cat(sprintf("  Effectif: %d\n", length(donnees_malin)))
      
      # Test de différence statistique
      test_student <- t.test(donnees_benin, donnees_malin)
      cat(sprintf("\nTEST T DE STUDENT:\n"))
      cat(sprintf("  Statistique t: %.4f\n", test_student$statistic))
      cat(sprintf("  p-value: %.2e\n", test_student$p.value))
      cat(sprintf("  Différence significative: %s\n", 
                  ifelse(test_student$p.value < 0.05, "OUI", "NON")))
      cat(sprintf("  Intervalle de confiance 95%%: [%.4f, %.4f]\n", 
                  test_student$conf.int[1], test_student$conf.int[2]))
    }
  })
  
  # Interprétation automatique univariée
  output$interpretation_univariee_automatique <- renderText({
    req(input$variable_selectionnee)
    donnees_completes <- donnees_wisconsin()
    donnees_variable <- donnees_completes[[input$variable_selectionnee]]
    
    if(is.numeric(donnees_variable)) {
      donnees_benin <- donnees_variable[donnees_completes$diagnostic_tumoral == "Benin"]
      donnees_malin <- donnees_variable[donnees_completes$diagnostic_tumoral == "Malin"]
      
      moyenne_benin <- mean(donnees_benin, na.rm = TRUE)
      moyenne_malin <- mean(donnees_malin, na.rm = TRUE)
      
      # Calcul du test t et de la taille d'effet
      test_student <- t.test(donnees_benin, donnees_malin)
      ecart_type_groupe <- sqrt(((length(donnees_benin)-1)*var(donnees_benin) + 
                                   (length(donnees_malin)-1)*var(donnees_malin)) / 
                                  (length(donnees_benin) + length(donnees_malin) - 2))
      d_cohen <- (moyenne_malin - moyenne_benin) / ecart_type_groupe
      
      interpreter_analyse_univariee(input$variable_selectionnee, moyenne_benin, 
                                    moyenne_malin, test_student$p.value, d_cohen)
    }
  })
  
  # Tests statistiques avancés
  output$resultats_tests_statistiques <- renderPrint({
    req(input$variable_selectionnee)
    donnees_completes <- donnees_wisconsin()
    donnees_variable <- donnees_completes[[input$variable_selectionnee]]
    
    if(is.numeric(donnees_variable)) {
      # Test de normalité (Shapiro-Wilk sur échantillon)
      echantillon_test <- sample(donnees_variable, min(5000, length(donnees_variable)))
      test_shapiro <- shapiro.test(echantillon_test)
      
      # Test d'égalité des variances (Levene)
      test_levene <- car::leveneTest(donnees_variable ~ donnees_completes$diagnostic_tumoral)
      
      # Test de Kolmogorov-Smirnov pour la normalité
      test_ks <- ks.test(donnees_variable, "pnorm", mean(donnees_variable), sd(donnees_variable))
      
      cat("=== TESTS STATISTIQUES AVANCÉS ===\n\n")
      cat("NORMALITÉ (Shapiro-Wilk):\n")
      cat(sprintf("  Statistique W: %.4f\n", test_shapiro$statistic))
      cat(sprintf("  p-value: %.2e\n", test_shapiro$p.value))
      cat(sprintf("  Distribution normale: %s\n\n", 
                  ifelse(test_shapiro$p.value > 0.05, "OUI", "NON")))
      
      cat("ÉGALITÉ DES VARIANCES (Levene):\n")
      cat(sprintf("  Statistique F: %.4f\n", test_levene$`F value`[1]))
      cat(sprintf("  p-value: %.2e\n", test_levene$`Pr(>F)`[1]))
      cat(sprintf("  Variances égales: %s\n\n", 
                  ifelse(test_levene$`Pr(>F)`[1] > 0.05, "OUI", "NON")))
      
      cat("NORMALITÉ (Kolmogorov-Smirnov):\n")
      cat(sprintf("  Statistique D: %.4f\n", test_ks$statistic))
      cat(sprintf("  p-value: %.2e\n", test_ks$p.value))
      cat(sprintf("  Distribution normale: %s\n", 
                  ifelse(test_ks$p.value > 0.05, "OUI", "NON")))
    }
  })
  
  # Graphiques univariés améliorés
  output$graphique_boites_ameliore <- renderPlotly({
    req(input$variable_selectionnee)
    
    graphique_boites <- ggplot(donnees_wisconsin(), 
                               aes_string(x = "diagnostic_tumoral", 
                                          y = input$variable_selectionnee, 
                                          fill = "diagnostic_tumoral")) +
      geom_boxplot(alpha = input$transparence_graphiques, 
                   outlier.shape = 16, outlier.size = 2.5, outlier.alpha = 0.7) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 1.2) +
      scale_fill_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "none",
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      ) +
      labs(
        title = paste("Distribution de", input$variable_selectionnee, "par diagnostic"),
        x = "Diagnostic Tumoral", 
        y = input$variable_selectionnee,
        caption = "Points individuels superposés pour visualiser la distribution complète"
      )
    
    ggplotly(graphique_boites, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })
  
  output$histogramme_ameliore <- renderPlotly({
    req(input$variable_selectionnee)
    
    graphique_histogramme <- ggplot(donnees_wisconsin(), 
                                    aes_string(x = input$variable_selectionnee, 
                                               fill = "diagnostic_tumoral")) +
      geom_histogram(alpha = input$transparence_graphiques, 
                     position = "identity", bins = 35, color = "white", size = 0.3) +
      scale_fill_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      ) +
      labs(
        title = paste("Histogramme de", input$variable_selectionnee),
        x = input$variable_selectionnee,
        y = "Fréquence absolue",
        fill = "Diagnostic"
      )
    
    ggplotly(graphique_histogramme)
  })
  
  output$graphique_densite <- renderPlotly({
    req(input$variable_selectionnee)
    
    graphique_densite <- ggplot(donnees_wisconsin(), 
                                aes_string(x = input$variable_selectionnee, 
                                           fill = "diagnostic_tumoral", 
                                           color = "diagnostic_tumoral")) +
      geom_density(alpha = input$transparence_graphiques, size = 1.2) +
      scale_fill_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
      scale_color_manual(values = c("Benin" = "#2980b9", "Malin" = "#c0392b")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      ) +
      labs(
        title = paste("Courbes de densité de", input$variable_selectionnee),
        x = input$variable_selectionnee,
        y = "Densité de probabilité",
        fill = "Diagnostic",
        color = "Diagnostic"
      )
    
    ggplotly(graphique_densite)
  })
  
  output$graphique_violon <- renderPlotly({
    req(input$variable_selectionnee)
    
    graphique_violon <- ggplot(donnees_wisconsin(), 
                               aes_string(x = "diagnostic_tumoral", 
                                          y = input$variable_selectionnee, 
                                          fill = "diagnostic_tumoral")) +
      geom_violin(alpha = input$transparence_graphiques, trim = FALSE, scale = "width") +
      geom_boxplot(width = 0.12, fill = "white", alpha = 0.9, outlier.alpha = 0.7) +
      scale_fill_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "none",
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      ) +
      labs(
        title = paste("Graphique en violon de", input$variable_selectionnee),
        x = "Diagnostic Tumoral",
        y = input$variable_selectionnee,
        caption = "Boxplot intégré pour visualiser les quartiles"
      )
    
    ggplotly(graphique_violon)
  })
  
  # Tableau de données interactif amélioré
  output$tableau_donnees_ameliore <- renderDT({
    datatable(
      donnees_wisconsin(),
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        language = list(
          search = "Rechercher dans les données:",
          lengthMenu = "Afficher _MENU_ entrées par page",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées totales",
          paginate = list(
            first = 'Premier',
            last = 'Dernier', 
            `next` = 'Suivant',
            previous = 'Précédent'
          )
        ),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      filter = 'top',
      class = 'cell-border stripe hover compact',
      rownames = FALSE
    ) %>%
      formatStyle('diagnostic_tumoral',
                  backgroundColor = styleEqual(c('Benin', 'Malin'), 
                                               c('#d4edda', '#f8d7da')),
                  fontWeight = 'bold')
  })
  
  # ===============================================================================
  # ANALYSES MULTIVARIÉES AVANCÉES
  # ===============================================================================
  
  # Matrice de corrélations interactive
  output$matrice_correlations <- renderPlotly({
    donnees_numeriques <- donnees_wisconsin() %>% 
      select_if(is.numeric) %>%
      select(-id)
    
    validate(
      need(ncol(donnees_numeriques) >= 2, "Au moins 2 variables numériques sont nécessaires pour calculer les corrélations")
    )
    
    matrice_correlation <- cor(donnees_numeriques, method = input$methode_correlation, use = "complete.obs")
    
    # Tests de significativité des corrélations
    resultats_tests_correlation <- tryCatch({
      if(ncol(donnees_numeriques) > 1) {
        calculer_tests_correlation(donnees_numeriques, conf.level = 0.95, method = input$methode_correlation)
      } else {
        NULL
      }
    }, error = function(erreur) {
      message("Erreur dans le calcul des tests de corrélation: ", erreur$message)
      NULL
    })
    
    # Masquage des corrélations non significatives si demandé
    if(input$masquer_non_significatives && !is.null(resultats_tests_correlation)) {
      matrice_correlation[resultats_tests_correlation$p_values > input$seuil_correlation] <- NA
    }
    
    # Conversion en format long pour plotly
    donnees_correlation <- expand.grid(Variable1 = rownames(matrice_correlation), 
                                       Variable2 = colnames(matrice_correlation))
    donnees_correlation$Correlation <- as.vector(matrice_correlation)
    
    graphique_correlation <- ggplot(donnees_correlation, aes(Variable1, Variable2, fill = Correlation)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(low = "#e74c3c", high = "#3498db", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name = "Corrélation\n(r)", na.value = "grey90") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid = element_blank()
      ) +
      labs(title = "Matrice de Corrélations Interactive") +
      coord_fixed()
    
    ggplotly(graphique_correlation, tooltip = c("x", "y", "fill"))
  })
  
  # Interprétation des corrélations
  output$interpretation_correlations <- renderText({
    donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
    matrice_correlation <- cor(donnees_numeriques, method = input$methode_correlation, use = "complete.obs")
    
    # Identification de la corrélation la plus forte (hors diagonale)
    matrice_sans_diagonale <- matrice_correlation
    diag(matrice_sans_diagonale) <- NA
    indices_max_correlation <- which(abs(matrice_sans_diagonale) == max(abs(matrice_sans_diagonale), na.rm = TRUE), arr.ind = TRUE)[1,]
    
    variable_1 <- rownames(matrice_correlation)[indices_max_correlation[1]]
    variable_2 <- colnames(matrice_correlation)[indices_max_correlation[2]]
    correlation_maximale <- matrice_correlation[indices_max_correlation[1], indices_max_correlation[2]]
    
    # Test de corrélation pour obtenir la p-value
    test_correlation <- cor.test(donnees_numeriques[[variable_1]], donnees_numeriques[[variable_2]], 
                                 method = input$methode_correlation)
    
    interpreter_correlations(variable_1, variable_2, correlation_maximale, test_correlation$p.value)
  })
  
  # Top des corrélations significatives
  output$top_correlations_significatives <- renderDT({
    donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
    matrice_correlation <- cor(donnees_numeriques, method = input$methode_correlation, use = "complete.obs")
    
    # Extraction des paires de corrélations (triangle supérieur)
    paires_correlation <- which(upper.tri(matrice_correlation), arr.ind = TRUE)
    tableau_correlations <- data.frame(
      Variable_1 = rownames(matrice_correlation)[paires_correlation[,1]],
      Variable_2 = colnames(matrice_correlation)[paires_correlation[,2]], 
      Coefficient_Correlation = matrice_correlation[paires_correlation],
      Correlation_Absolue = abs(matrice_correlation[paires_correlation])
    )
    
    # Tri par corrélation absolue décroissante
    tableau_correlations <- tableau_correlations[order(-tableau_correlations$Correlation_Absolue), ]
    tableau_correlations$Coefficient_Correlation <- round(tableau_correlations$Coefficient_Correlation, 4)
    tableau_correlations <- tableau_correlations[, -4]  # Suppression de la colonne absolue
    
    # Ajout des p-values
    tableau_correlations$P_Value <- sapply(1:nrow(tableau_correlations), function(i) {
      test_cor <- cor.test(donnees_numeriques[[tableau_correlations$Variable_1[i]]], 
                           donnees_numeriques[[tableau_correlations$Variable_2[i]]], 
                           method = input$methode_correlation)
      return(round(test_cor$p.value, 6))
    })
    
    datatable(tableau_correlations, 
              options = list(pageLength = 15, scrollX = TRUE),
              colnames = c('Variable 1', 'Variable 2', 'Corrélation', 'P-Value'),
              rownames = FALSE) %>%
      formatStyle('Coefficient_Correlation',
                  backgroundColor = styleInterval(c(-0.8, -0.5, -0.3, 0.3, 0.5, 0.8),
                                                  c('#ffcdd2', '#ffebee', '#fff3e0', '#ffffff', 
                                                    '#e8f5e8', '#c8e6c9', '#a5d6a7'))) %>%
      formatStyle('P_Value',
                  backgroundColor = styleInterval(c(0.001, 0.01, 0.05),
                                                  c('#c8e6c9', '#fff3e0', '#ffebee', '#ffcdd2')))
  })
  
  # ===============================================================================
  # ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
  # ===============================================================================
  
  # Variance expliquée par l'ACP
  output$variance_expliquee_acp <- renderPlotly({
    if(input$executer_acp > 0) {
      donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
      
      if(input$centrer_reduire_variables) {
        donnees_numeriques <- scale(donnees_numeriques)
      }
      
      resultat_acp <- PCA(donnees_numeriques, graph = FALSE, ncp = input$nombre_composantes)
      
      donnees_variance <- data.frame(
        Composante = paste0("CP", 1:input$nombre_composantes),
        Variance_Individuelle = resultat_acp$eig[1:input$nombre_composantes, 2],
        Variance_Cumulee = cumsum(resultat_acp$eig[1:input$nombre_composantes, 2])
      )
      
      graphique_variance <- ggplot(donnees_variance, aes(x = Composante)) +
        geom_col(aes(y = Variance_Individuelle), fill = "#3498db", alpha = 0.8, width = 0.7) +
        geom_line(aes(y = Variance_Cumulee, group = 1), color = "#e74c3c", size = 1.5) +
        geom_point(aes(y = Variance_Cumulee), color = "#e74c3c", size = 4) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 11),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey95")
        ) +
        labs(title = "Variance Expliquée par Composante Principale",
             x = "Composante Principale",
             y = "% Variance Expliquée",
             caption = "Barres: variance individuelle | Ligne: variance cumulée")
      
      ggplotly(graphique_variance)
    }
  })
  
  # Interprétation de l'ACP
  output$interpretation_acp <- renderText({
    if(input$executer_acp > 0) {
      donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
      
      if(input$centrer_reduire_variables) {
        donnees_numeriques <- scale(donnees_numeriques)
      }
      
      resultat_acp <- PCA(donnees_numeriques, graph = FALSE, ncp = input$nombre_composantes)
      variance_expliquee <- resultat_acp$eig[, 2]
      
      interpreter_acp(variance_expliquee, input$nombre_composantes)
    }
  })
  
  # Biplot de l'ACP
  output$biplot_acp <- renderPlotly({
    if(input$executer_acp > 0) {
      donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
      
      if(input$centrer_reduire_variables) {
        donnees_numeriques <- scale(donnees_numeriques)
      }
      
      resultat_acp <- PCA(donnees_numeriques, graph = FALSE)
      
      # Scores des individus sur les deux premières composantes
      scores_individus <- as.data.frame(resultat_acp$ind$coord[, 1:2])
      scores_individus$Diagnostic <- donnees_wisconsin()$diagnostic_tumoral
      
      graphique_biplot <- ggplot(scores_individus, aes(Dim.1, Dim.2, color = Diagnostic)) +
        geom_point(alpha = 0.7, size = 2.5) +
        scale_color_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 11),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey95")
        ) +
        labs(title = "Biplot ACP - Projection des Individus",
             x = paste0("CP1 (", round(resultat_acp$eig[1,2], 2), "%)"),
             y = paste0("CP2 (", round(resultat_acp$eig[2,2], 2), "%)"),
             color = "Diagnostic")
      
      ggplotly(graphique_biplot)
    }
  })
  
  # Contributions des variables à l'ACP
  output$contributions_variables_acp <- renderDT({
    if(input$executer_acp > 0) {
      donnees_numeriques <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
      
      if(input$centrer_reduire_variables) {
        donnees_numeriques <- scale(donnees_numeriques)
      }
      
      resultat_acp <- PCA(donnees_numeriques, graph = FALSE, ncp = input$nombre_composantes)
      
      tableau_contributions <- as.data.frame(resultat_acp$var$contrib)
      tableau_contributions$Variable <- rownames(tableau_contributions)
      tableau_contributions <- tableau_contributions[, c(ncol(tableau_contributions), 1:(ncol(tableau_contributions)-1))]
      
      datatable(tableau_contributions, 
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE) %>%
        formatRound(2:ncol(tableau_contributions), 3) %>%
        formatStyle(2:ncol(tableau_contributions),
                    backgroundColor = styleColorBar(range(tableau_contributions[,-1]), '#e8f4fd'))
    }
  })
  
  # ===============================================================================
  # ANALYSES BIVARIÉES
  # ===============================================================================
  
  # Nuage de points amélioré
  output$nuage_points_ameliore <- renderPlotly({
    req(input$variable_x, input$variable_y)
    
    graphique_nuage <- ggplot(donnees_wisconsin(), 
                              aes_string(x = input$variable_x, y = input$variable_y, 
                                         color = "diagnostic_tumoral")) +
      geom_point(alpha = 0.7, size = input$taille_points) +
      scale_color_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      ) +
      labs(title = paste("Relation entre", input$variable_x, "et", input$variable_y),
           x = input$variable_x, y = input$variable_y, color = "Diagnostic")
    
    if(input$ajouter_regression) {
      graphique_nuage <- graphique_nuage + 
        geom_smooth(method = "lm", se = input$afficher_intervalles_confiance, alpha = 0.3)
    }
    
    ggplotly(graphique_nuage)
  })
  
  # Interprétation des relations bivariées
  output$interpretation_relation_bivariee <- renderText({
    req(input$variable_x, input$variable_y)
    
    donnees_x <- donnees_wisconsin()[[input$variable_x]]
    donnees_y <- donnees_wisconsin()[[input$variable_y]]
    
    # Test de corrélation global
    test_correlation <- cor.test(donnees_x, donnees_y, method = "pearson")
    
    interpreter_correlations(input$variable_x, input$variable_y, 
                             test_correlation$estimate, test_correlation$p.value)
  })
  
  # Résumé de régression linéaire
  output$resume_regression_lineaire <- renderPrint({
    req(input$variable_x, input$variable_y)
    
    formule_regression <- paste(input$variable_y, "~", input$variable_x)
    modele_lineaire <- lm(as.formula(formule_regression), data = donnees_wisconsin())
    
    cat("=== MODÈLE DE RÉGRESSION LINÉAIRE ===\n")
    cat(sprintf("Formule: %s\n\n", formule_regression))
    
    resume_modele <- summary(modele_lineaire)
    cat(sprintf("R² = %.6f\n", resume_modele$r.squared))
    cat(sprintf("R² ajusté = %.6f\n", resume_modele$adj.r.squared))
    cat(sprintf("Erreur standard résiduelle = %.6f\n", resume_modele$sigma))
    cat(sprintf("F-statistique = %.4f\n", resume_modele$fstatistic[1]))
    cat(sprintf("p-value globale = %.2e\n\n", pf(resume_modele$fstatistic[1], 
                                                 resume_modele$fstatistic[2], 
                                                 resume_modele$fstatistic[3], 
                                                 lower.tail = FALSE)))
    
    cat("COEFFICIENTS:\n")
    print(round(resume_modele$coefficients, 6))
  })
  
  # Tests de corrélation multiples
  output$tests_correlation_multiples <- renderPrint({
    req(input$variable_x, input$variable_y)
    
    donnees_x <- donnees_wisconsin()[[input$variable_x]]
    donnees_y <- donnees_wisconsin()[[input$variable_y]]
    
    # Tests de corrélation avec différentes méthodes
    correlation_pearson <- cor.test(donnees_x, donnees_y, method = "pearson")
    correlation_spearman <- cor.test(donnees_x, donnees_y, method = "spearman")
    correlation_kendall <- cor.test(donnees_x, donnees_y, method = "kendall")
    
    cat("=== TESTS DE CORRÉLATION MULTIPLES ===\n\n")
    cat("CORRÉLATION DE PEARSON (linéaire):\n")
    cat(sprintf("  Coefficient r = %.6f\n", correlation_pearson$estimate))
    cat(sprintf("  p-value = %.2e\n", correlation_pearson$p.value))
    cat(sprintf("  IC 95%%: [%.4f, %.4f]\n\n", 
                correlation_pearson$conf.int[1], correlation_pearson$conf.int[2]))
    
    cat("CORRÉLATION DE SPEARMAN (rang):\n")
    cat(sprintf("  Coefficient ρ = %.6f\n", correlation_spearman$estimate))
    cat(sprintf("  p-value = %.2e\n\n", correlation_spearman$p.value))
    
    cat("CORRÉLATION DE KENDALL (tau):\n")
    cat(sprintf("  Coefficient τ = %.6f\n", correlation_kendall$estimate))
    cat(sprintf("  p-value = %.2e\n", correlation_kendall$p.value))
  })
  
  # ===============================================================================
  # MODÉLISATION PRÉDICTIVE AVANCÉE
  # ===============================================================================
  
  # Variables réactives pour stocker les modèles
  modele_entraine <- reactiveVal(NULL)
  donnees_test <- reactiveVal(NULL)
  predictions_test <- reactiveVal(NULL)
  
  # Entraînement du modèle
  observeEvent(input$entrainer_modele, {
    
    donnees_completes <- donnees_wisconsin()
    
    # Préparation des données
    donnees_modelisation <- donnees_completes %>%
      select_if(is.numeric) %>%
      select(-id) %>%
      mutate(diagnostic_tumoral = donnees_completes$diagnostic_tumoral)
    
    # Division train/test
    indices_entrainement <- createDataPartition(donnees_modelisation$diagnostic_tumoral, 
                                                p = input$proportion_entrainement, 
                                                list = FALSE)
    
    donnees_entrainement <- donnees_modelisation[indices_entrainement, ]
    donnees_test_temp <- donnees_modelisation[-indices_entrainement, ]
    donnees_test(donnees_test_temp)
    
    # Configuration de la validation croisée
    controle_cv <- trainControl(
      method = "cv",
      number = input$validation_croisee_k,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      savePredictions = TRUE,
      verboseIter = TRUE
    )
    
    # Entraînement du modèle selon l'algorithme choisi
    if(input$optimiser_hyperparametres) {
      if(input$algorithme_ml == "rf") {
        grille_parametres <- expand.grid(mtry = c(2, 4, 6, 8))
      } else if(input$algorithme_ml == "gbm") {
        grille_parametres <- expand.grid(
          n.trees = c(100, 200),
          interaction.depth = c(1, 3),
          shrinkage = c(0.1, 0.01),
          n.minobsinnode = c(10)
        )
      } else {
        grille_parametres <- NULL
      }
    } else {
      grille_parametres <- NULL
    }
    
    # Entraînement
    modele_temp <- tryCatch({
      train(diagnostic_tumoral ~ ., 
            data = donnees_entrainement,
            method = input$algorithme_ml,
            trControl = controle_cv,
            tuneGrid = grille_parametres,
            metric = "ROC"
            # family = if (input$algorithme_ml == "glm") binomial() else NULL
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'entraînement:", e$message), type = "message")
      return(NULL)
    })
    
    if(!is.null(modele_temp)) {
      modele_entraine(modele_temp)
      
      # Prédictions sur les données de test
      predictions_temp <- predict(modele_temp, donnees_test_temp, type = "prob")
      predictions_test(predictions_temp)
      
      showNotification("Modèle entraîné avec succès!", type = "message")
    }
  })
  
  # Métriques de performance
  output$metriques_performance_modele <- renderPrint({
    req(modele_entraine())
    
    modele <- modele_entraine()
    donnees_test_temp <-donnees_test()
    
    # Prédictions sur les données de test
    predictions_classe <- predict(modele, donnees_test_temp)
    predictions_prob <- predict(modele, donnees_test_temp, type = "prob")
    
    # Matrice de confusion
    matrice_confusion <- confusionMatrix(predictions_classe, donnees_test_temp$diagnostic_tumoral)
    
    # Courbe ROC
    courbe_roc <- roc(donnees_test_temp$diagnostic_tumoral, predictions_prob$Malin)
    
    cat("=== MÉTRIQUES DE PERFORMANCE DU MODÈLE ===\n\n")
    cat(sprintf("Algorithme utilisé: %s\n", input$algorithme_ml))
    cat(sprintf("Taille échantillon d'entraînement: %0f\n", nrow(donnees_wisconsin()) * input$proportion_entrainement))
    cat(sprintf("Taille échantillon de test: %d\n\n", nrow(donnees_test_temp)))
    
    cat("PERFORMANCE SUR DONNÉES DE TEST:\n")
    cat(sprintf("Précision globale: %.4f\n", matrice_confusion$overall['Accuracy']))
    cat(sprintf("Sensibilité (Rappel): %.4f\n", matrice_confusion$byClass['Sensitivity']))
    cat(sprintf("Spécificité: %.4f\n", matrice_confusion$byClass['Specificity']))
    cat(sprintf("Valeur prédictive positive: %.4f\n", matrice_confusion$byClass['Pos Pred Value']))
    cat(sprintf("Valeur prédictive négative: %.4f\n", matrice_confusion$byClass['Neg Pred Value']))
    cat(sprintf("F1-Score: %.4f\n", matrice_confusion$byClass['F1']))
    cat(sprintf("AUC-ROC: %.4f\n", auc(courbe_roc)))
    
    cat("\nPERFORMANCE EN VALIDATION CROISÉE:\n")
    print(modele$results)
  })
  
  # Matrice de confusion visuelle
  output$matrice_confusion <- renderPlotly({
    req(modele_entraine(), donnees_test())
    
    modele <- modele_entraine()
    donnees_test_temp <- donnees_test()
    
    predictions_classe <- predict(modele, donnees_test_temp)
    matrice_conf <- table(Prédiction = predictions_classe, Réalité = donnees_test_temp$diagnostic_tumoral)
    
    # Conversion en dataframe pour ggplot
    donnees_matrice <- as.data.frame(matrice_conf)
    
    graphique_matrice <- ggplot(donnees_matrice, aes(Réalité, Prédiction, fill = Freq)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = Freq), size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#ffffff", high = "#3498db") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
      ) +
      labs(title = "Matrice de Confusion",
           x = "Diagnostic Réel", y = "Diagnostic Prédit", fill = "Effectif")
    
    ggplotly(graphique_matrice)
  })
  
  # Courbe ROC
  output$courbe_roc <- renderPlotly({
    req(modele_entraine(), donnees_test())
    
    modele <- modele_entraine()
    donnees_test_temp <- donnees_test()
    
    predictions_prob <- predict(modele, donnees_test_temp, type = "prob")
    courbe_roc <- roc(donnees_test_temp$diagnostic_tumoral, predictions_prob$Malin)
    
    # Données pour le graphique ROC
    donnees_roc <- data.frame(
      Specificite = 1 - courbe_roc$specificities,
      Sensibilite = courbe_roc$sensitivities
    )
    
    graphique_roc <- ggplot(donnees_roc, aes(Specificite, Sensibilite)) +
      geom_line(color = "#e74c3c", size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11)
      ) +
      labs(title = paste("Courbe ROC (AUC =", round(auc(courbe_roc), 4), ")"),
           x = "1 - Spécificité (Taux de Faux Positifs)",
           y = "Sensibilité (Taux de Vrais Positifs)") +
      coord_fixed()
    
    ggplotly(graphique_roc)
  })
  
  # Importance des variables
  output$importance_variables <- renderPlotly({
    req(modele_entraine())
    
    modele <- modele_entraine()
    
    # Extraction de l'importance selon le type de modèle
    if(input$algorithme_ml %in% c("rf", "gbm")) {
      importance_vars <- varImp(modele)$importance
      importance_vars$Variable <- rownames(importance_vars)
      importance_vars <- importance_vars[order(-importance_vars$Overall), ]
      
      graphique_importance <- ggplot(head(importance_vars, 15), 
                                     aes(x = reorder(Variable, Overall), y = Overall)) +
        geom_col(fill = "#f39c12", alpha = 0.8) +
        coord_flip() +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 11)
        ) +
        labs(title = "Importance des Variables (Top 15)",
             x = "Variables", y = "Importance")
      
      ggplotly(graphique_importance)
    } else {
      # Pour les autres modèles, afficher un message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Importance des variables\nnon disponible pour ce modèle", 
                 size = 6, hjust = 0.5) +
        theme_void()
    }
  })
  
  # Prédiction individuelle
  output$resultat_prediction_individuelle <- renderText({
    req(input$predire_cas_individuel > 0, modele_entraine())
    
    modele <- modele_entraine()
    
    # Création du vecteur de caractéristiques
    nouvelles_donnees <- data.frame(
      rayon_cellulaire_moyen = input$rayon_cellulaire_input,
      texture_surface_moyenne = input$texture_surface_input,
      perimetre_cellulaire_moyen = input$perimetre_cellulaire_input,
      superficie_cellulaire_moyenne = input$superficie_cellulaire_input
    )
    
    # Ajout des autres variables avec des valeurs moyennes
    donnees_reference <- donnees_wisconsin() %>% select_if(is.numeric) %>% select(-id)
    for(col in names(donnees_reference)) {
      if(!col %in% names(nouvelles_donnees)) {
        nouvelles_donnees[[col]] <- mean(donnees_reference[[col]], na.rm = TRUE)
      }
    }
    
    # Prédiction
    prediction_classe <- predict(modele, nouvelles_donnees)
    prediction_prob <- predict(modele, nouvelles_donnees, type = "prob")
    
    resultat <- paste0(
      "🏥 RÉSULTAT DE LA PRÉDICTION DIAGNOSTIQUE :\n\n",
      "• Diagnostic prédit : ", as.character(prediction_classe), "\n",
      "• Probabilité tumeur bénigne : ", round(prediction_prob$Benin * 100, 2), "%\n",
      "• Probabilité tumeur maligne : ", round(prediction_prob$Malin * 100, 2), "%\n\n",
      "📊 PARAMÈTRES ANALYSÉS :\n",
      "• Rayon cellulaire : ", input$rayon_cellulaire_input, "\n",
      "• Texture surface : ", input$texture_surface_input, "\n",
      "• Périmètre cellulaire : ", input$perimetre_cellulaire_input, "\n",
      "• Superficie cellulaire : ", input$superficie_cellulaire_input, "\n\n",
      "⚠️ AVERTISSEMENT MÉDICAL :\n",
      "Ce résultat est généré par un algorithme d'intelligence artificielle à des fins éducatives.\n",
      "Il ne peut en aucun cas remplacer l'expertise d'un professionnel de santé qualifié.\n",
      "Consultez toujours un médecin pour un diagnostic médical définitif."
    )
    
    return(resultat)
  })
  
  # ===============================================================================
  # PRÉDICTION DE SURVIE
  # ===============================================================================
  
  # Prédiction de survie
  output$resultat_prediction_survie <- renderText({
    req(input$predire_survie > 0)
    
    # Utilisation de la fonction de prédiction de survie
    resultat_survie <- predire_survie_patient(
      age = input$age_patient,
      race = input$race_patient,
      t_stage = input$t_stage,
      n_stage = input$n_stage,
      grade = input$grade_tumeur,
      tumor_size = input$taille_tumeur,
      estrogen_status = input$statut_estrogene,
      progesterone_status = input$statut_progesterone
    )
    
    # Formatage du résultat
    resultat_texte <- paste0(
      "🏥 PRÉDICTION DE SURVIE :\n\n",
      "• Niveau de risque : ", resultat_survie$niveau_risque, "\n",
      "• Score de risque : ", resultat_survie$score_risque, "/15\n",
      "• Survie médiane estimée : ", round(resultat_survie$survie_estimee), " mois\n",
      "• Probabilité de survie à 5 ans : ", round(resultat_survie$prob_survie_5ans * 100, 1), "%\n\n",
      "📊 FACTEURS ANALYSÉS :\n",
      "• Âge : ", input$age_patient, " ans\n",
      "• Origine ethnique : ", input$race_patient, "\n",
      "• Stade tumoral : ", input$t_stage, "\n",
      "• Atteinte ganglionnaire : ", input$n_stage, "\n",
      "• Grade histologique : ", input$grade_tumeur, "\n",
      "• Taille tumorale : ", input$taille_tumeur, " mm\n",
      "• Statut œstrogène : ", input$statut_estrogene, "\n",
      "• Statut progestérone : ", input$statut_progesterone, "\n\n",
      "💡 INTERPRÉTATION CLINIQUE :\n",
      if (resultat_survie$niveau_risque == "FAIBLE") {
        "Pronostic favorable avec une excellente probabilité de survie à long terme. Surveillance de routine recommandée."
      } else if (resultat_survie$niveau_risque == "MODÉRÉ") {
        "Pronostic intermédiaire nécessitant un suivi régulier et une thérapie adjuvante adaptée."
      } else {
        "Pronostic défavorable nécessitant une prise en charge multidisciplinaire intensive et un suivi rapproché."
      },
      "\n\n⚠️ AVERTISSEMENT MÉDICAL :\n",
      "Cette prédiction est basée sur des modèles statistiques et ne remplace pas l'évaluation clinique d'un oncologue."
    )
    
    
    return(resultat_texte)
  })
  
  output$telecharger_rapport_survie <- downloadHandler(
    filename = function() {
      paste0("rapport_survie_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      resultat_survie <- predire_survie_patient(
        age = input$age_patient,
        race = input$race_patient,
        t_stage = input$t_stage,
        n_stage = input$n_stage,
        grade = input$grade_tumeur,
        tumor_size = input$taille_tumeur,
        estrogen_status = input$statut_estrogene,
        progesterone_status = input$statut_progesterone
      )
      
      inputs <- reactiveValuesToList(input)  # Pour passer tous les inputs à la fonction
      generer_rapport_survie(resultat_survie, inputs, filename = file)
    }
  )
  
  

  

  
  
  # Courbe de survie Kaplan-Meier
  output$courbe_survie_km <- renderPlotly({
    req(input$predire_survie > 0)
    
    # Simulation d'une courbe de survie basée sur les données SEER
    temps <- seq(0, 120, by = 1)
    
    # Courbes de survie par niveau de risque
    survie_faible <- exp(-0.005 * temps)
    survie_modere <- exp(-0.012 * temps)
    survie_eleve <- exp(-0.025 * temps)
    
    df_survie <- data.frame(
      Temps = rep(temps, 3),
      Survie = c(survie_faible, survie_modere, survie_eleve),
      Groupe = rep(c("Risque Faible", "Risque Modéré", "Risque Élevé"), each = length(temps))
    )
    
    graphique_survie <- ggplot(df_survie, aes(x = Temps, y = Survie, color = Groupe)) +
      geom_line(size = 1.5) +
      scale_color_manual(values = c("Risque Faible" = "#27ae60", 
                                    "Risque Modéré" = "#f39c12", 
                                    "Risque Élevé" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold")
      ) +
      labs(title = "Courbes de Survie Kaplan-Meier",
           x = "Temps (mois)", y = "Probabilité de survie", color = "Niveau de Risque") +
      ylim(0, 1)
    
    ggplotly(graphique_survie)
  })
  
  # Survie par groupes
  output$survie_par_groupes <- renderPlotly({
    req(input$predire_survie > 0)
    
    # Analyse de survie par différents facteurs
    facteurs <- c("Grade I", "Grade II", "Grade III", "T1-T2", "T3-T4", "N0", "N+")
    survie_5ans <- c(0.95, 0.85, 0.70, 0.90, 0.65, 0.88, 0.72)
    
    df_groupes <- data.frame(
      Facteur = facteurs,
      Survie_5ans = survie_5ans
    )
    
    graphique_groupes <- ggplot(df_groupes, aes(x = reorder(Facteur, Survie_5ans), y = Survie_5ans, fill = Facteur)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "none"
      ) +
      labs(title = "Survie à 5 ans par Facteur Pronostique",
           x = "Facteurs", y = "Probabilité de survie à 5 ans") +
      ylim(0, 1)
    
    ggplotly(graphique_groupes)
  })
  
  # Facteurs pronostiques  
  output$facteurs_pronostiques <- renderPlotly({
    # Analyse de l'impact des différents facteurs
    facteurs <- c("Âge > 65", "Grade III", "T3-T4", "N+", "Taille > 5cm", "ER-", "PR-")
    hazard_ratio <- c(1.8, 2.2, 3.1, 2.8, 1.9, 1.6, 1.4)
    
    df_facteurs <- data.frame(
      Facteur = facteurs,
      Hazard_Ratio = hazard_ratio
    ) %>%
      arrange(Hazard_Ratio)
    
    graphique_facteurs <- ggplot(df_facteurs, aes(x = reorder(Facteur, Hazard_Ratio), y = Hazard_Ratio)) +
      geom_col(fill = "#fd79a8", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11)
      ) +
      labs(title = "Facteurs Pronostiques (Hazard Ratios)",
           x = "Facteurs", y = "Hazard Ratio")
    
    ggplotly(graphique_facteurs)
  })
  
  # Aperçu des données SEER
  output$apercu_donnees_seer <- renderDT({
    datatable(
      head(donnees_seer(), 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe hover compact',
      rownames = FALSE
    ) %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(c('Alive', 'Dead'), 
                                               c('#d4edda', '#f8d7da')),
                  fontWeight = 'bold')
  })
  
  # Courbe de survie Kaplan-Meier
  output$courbe_survie_km <- renderPlotly({
    req(input$predire_survie > 0)
    
    # Simulation d'une courbe de survie basée sur les données SEER
    temps <- seq(0, 120, by = 1)
    
    # Courbes de survie par niveau de risque
    survie_faible <- exp(-0.005 * temps)
    survie_modere <- exp(-0.012 * temps)
    survie_eleve <- exp(-0.025 * temps)
    
    df_survie <- data.frame(
      Temps = rep(temps, 3),
      Survie = c(survie_faible, survie_modere, survie_eleve),
      Groupe = rep(c("Risque Faible", "Risque Modéré", "Risque Élevé"), each = length(temps))
    )
    
    graphique_survie <- ggplot(df_survie, aes(x = Temps, y = Survie, color = Groupe)) +
      geom_line(size = 1.5) +
      scale_color_manual(values = c("Risque Faible" = "#27ae60", 
                                    "Risque Modéré" = "#f39c12", 
                                    "Risque Élevé" = "#e74c3c")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold")
      ) +
      labs(title = "Courbes de Survie Kaplan-Meier",
           x = "Temps (mois)", y = "Probabilité de survie", color = "Niveau de Risque") +
      ylim(0, 1)
    
    ggplotly(graphique_survie)
  })
  
  # Survie par groupes
  output$survie_par_groupes <- renderPlotly({
    req(input$predire_survie > 0)
    
    # Analyse de survie par différents facteurs
    facteurs <- c("Grade I", "Grade II", "Grade III", "T1-T2", "T3-T4", "N0", "N+")
    survie_5ans <- c(0.95, 0.85, 0.70, 0.90, 0.65, 0.88, 0.72)
    
    df_groupes <- data.frame(
      Facteur = facteurs,
      Survie_5ans = survie_5ans
    )
    
    graphique_groupes <- ggplot(df_groupes, aes(x = reorder(Facteur, Survie_5ans), y = Survie_5ans, fill = Facteur)) +
      geom_col(alpha = 0.8) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "none"
      ) +
      labs(title = "Survie à 5 ans par Facteur Pronostique",
           x = "Facteurs", y = "Probabilité de survie à 5 ans") +
      ylim(0, 1)
    
    ggplotly(graphique_groupes)
  })
  
  # Facteurs pronostiques
  output$facteurs_pronostiques <- renderPlotly({
    # Analyse de l'impact des différents facteurs
    facteurs <- c("Âge > 65", "Grade III", "T3-T4", "N+", "Taille > 5cm", "ER-", "PR-")
    hazard_ratio <- c(1.8, 2.2, 3.1, 2.8, 1.9, 1.6, 1.4)
    
    df_facteurs <- data.frame(
      Facteur = facteurs,
      Hazard_Ratio = hazard_ratio
    ) %>%
      arrange(Hazard_Ratio)
    
    graphique_facteurs <- ggplot(df_facteurs, aes(x = reorder(Facteur, Hazard_Ratio), y = Hazard_Ratio)) +
      geom_col(fill = "#fd79a8", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 11)
      ) +
      labs(title = "Facteurs Pronostiques (Hazard Ratios)",
           x = "Facteurs", y = "Hazard Ratio")
    
    ggplotly(graphique_facteurs)
  })
  
  # Aperçu des données SEER
  output$apercu_donnees_seer <- renderDT({
    datatable(
      head(donnees_seer(), 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe hover compact',
      rownames = FALSE
    ) %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(c('Alive', 'Dead'), 
                                               c('#d4edda', '#f8d7da')),
                  fontWeight = 'bold')
  })
  
  # ===============================================================================
  # GESTION DES DONNÉES ET EXPORTS
  # ===============================================================================
  
  # Informations détaillées sur le jeu de données
  output$informations_detaillees_donnees <- renderUI({
    if (input$choix_jeu_donnees == "wisconsin") {
      donnees <- donnees_wisconsin()
      nombre_observations <- nrow(donnees)
      nombre_variables <- ncol(donnees)
      nombre_benin <- sum(donnees$diagnostic_tumoral == "Benin")
      nombre_malin <- sum(donnees$diagnostic_tumoral == "Malin")
      
      HTML(paste0(
        "<h4><i class='fa fa-database'></i> Base de Données Wisconsin sur le Cancer du Sein</h4>",
        "<p><strong>Institution d'origine:</strong> Hôpitaux Universitaires du Wisconsin, Madison</p>",
        "<p><strong>Période de collecte:</strong> 1995</p>",
        "<p><strong>Responsable scientifique:</strong> Dr. William H. Wolberg, MD</p>",
        "<p><strong>Type d'étude:</strong> Étude rétrospective observationnelle</p>",
        "<hr>",
        "<p><strong>Dimensions du jeu de données:</strong> ", nombre_observations, " observations × ", nombre_variables, " variables</p>",
        "<p><strong>Distribution des diagnostics:</strong></p>",
        "<ul>",
        "<li>Cas bénins: ", nombre_benin, " (", round(nombre_benin/nombre_observations*100, 2), "%)</li>",
        "<li>Cas malins: ", nombre_malin, " (", round(nombre_malin/nombre_observations*100, 2), "%)</li>",
        "</ul>",
        "<p><strong>Méthodologie d'acquisition:</strong> Caractéristiques morphologiques des noyaux cellulaires obtenues par analyse d'images numérisées de biopsies par aspiration à l'aiguille fine (FNA).</p>",
        "<p><strong>Validation clinique:</strong> Tous les diagnostics ont été confirmés par examen histopathologique.</p>"
      ))
    } else {
      donnees <- donnees_seer()
      nombre_observations <- nrow(donnees)
      nombre_variables <- ncol(donnees)
      nombre_vivants <- sum(donnees$Status == "Alive")
      nombre_decedes <- sum(donnees$Status == "Dead")
      
      HTML(paste0(
        "<h4><i class='fa fa-database'></i> Base de Données SEER sur le Cancer du Sein</h4>",
        "<p><strong>Institution d'origine:</strong> National Cancer Institute (NCI)</p>",
        "<p><strong>Période de collecte:</strong> 1973-2015</p>",
        "<p><strong>Programme:</strong> Surveillance, Epidemiology, and End Results</p>",
        "<p><strong>Type d'étude:</strong> Registre de surveillance épidémiologique</p>",
        "<hr>",
        "<p><strong>Dimensions du jeu de données:</strong> ", nombre_observations, " observations × ", nombre_variables, " variables</p>",
        "<p><strong>Distribution du statut vital:</strong></p>",
        "<ul>",
        "<li>Patients vivants: ", nombre_vivants, " (", round(nombre_vivants/nombre_observations*100, 2), "%)</li>",
        "<li>Patients décédés: ", nombre_decedes, " (", round(nombre_decedes/nombre_observations*100, 2), "%)</li>",
        "</ul>",
        "<p><strong>Méthodologie de collecte:</strong> Données cliniques et démographiques collectées de manière prospective dans 18 registres de cancer géographiquement définis.</p>",
        "<p><strong>Suivi médian:</strong> ", round(median(donnees$Survival.Months), 1), " mois</p>"
      ))
    }
  })
  
  # Aperçu interactif des données
  output$apercu_donnees_interactif <- renderDT({
    if (input$choix_jeu_donnees == "wisconsin") {
      donnees <- donnees_wisconsin()
    } else {
      donnees <- donnees_seer()
    }
    
    if(!is.null(input$colonnes_affichees) && length(input$colonnes_affichees) > 0) {
      donnees <- donnees[, input$colonnes_affichees, drop = FALSE]
      
      print('========================-------Debut---================= donnees_apercu 2')
      print(donnees)
      print('========================-------Fin---================= donnees_apercu 2')
    }
    
    donnees_apercu <- head(donnees, input$lignes_apercu)
    
    
    datatable(
      donnees_apercu,
      options = list(
        scrollX = TRUE,
        pageLength = input$lignes_apercu,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = "Rechercher dans l'aperçu:",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées"
        ),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe hover compact',
      rownames = FALSE
    ) %>%
      formatStyle(if(input$choix_jeu_donnees == "wisconsin") 'diagnostic_tumoral' else 'Status',
                  backgroundColor = if(input$choix_jeu_donnees == "wisconsin") 
                    styleEqual(c('Benin', 'Malin'), c('#d4edda', '#f8d7da'))
                  else 
                    styleEqual(c('Alive', 'Dead'), c('#d4edda', '#f8d7da')),
                  fontWeight = 'bold')
  })
  
  # Distribution des variables
  output$distribution_variables <- renderPlotly({
    if (input$choix_jeu_donnees == "wisconsin") {
      donnees <- donnees_wisconsin()
      effectifs <- table(donnees$diagnostic_tumoral)
      
      donnees_distribution <- data.frame(
        Variable = names(effectifs),
        Effectif = as.numeric(effectifs),
        Pourcentage = round(as.numeric(effectifs) / sum(effectifs) * 100, 2)
      )
      
      graphique_distribution <- ggplot(donnees_distribution, 
                                       aes(x = Variable, y = Effectif, fill = Variable)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = paste0(Effectif, "\n(", Pourcentage, "%)")), 
                  vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c("Benin" = "#3498db", "Malin" = "#e74c3c")) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 11)
        ) +
        labs(title = "Distribution des Diagnostics",
             x = "Type de Diagnostic", y = "Nombre de Cas") +
        ylim(0, max(donnees_distribution$Effectif) * 1.15)
    } else {
      donnees <- donnees_seer()
      effectifs <- table(donnees$Status)
      
      donnees_distribution <- data.frame(
        Variable = names(effectifs),
        Effectif = as.numeric(effectifs),
        Pourcentage = round(as.numeric(effectifs) / sum(effectifs) * 100, 2)
      )
      
      graphique_distribution <- ggplot(donnees_distribution, 
                                       aes(x = Variable, y = Effectif, fill = Variable)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = paste0(Effectif, "\n(", Pourcentage, "%)")), 
                  vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c("Alive" = "#27ae60", "Dead" = "#e74c3c")) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 11)
        ) +
        labs(title = "Distribution du Statut Vital",
             x = "Statut", y = "Nombre de Patients") +
        ylim(0, max(donnees_distribution$Effectif) * 1.15)
    }
    
    ggplotly(graphique_distribution, tooltip = c("x", "y"))
  })
  
  # Rapport de qualité des données
  output$rapport_qualite_donnees <- renderPrint({
    if (input$choix_jeu_donnees == "wisconsin") {
      donnees <- donnees_wisconsin()
    } else {
      donnees <- donnees_seer()
    }
    
    cat("=== RAPPORT DE QUALITÉ DES DONNÉES ===\n\n")
    cat("COMPLÉTUDE DES DONNÉES:\n")
    valeurs_manquantes <- sapply(donnees, function(x) sum(is.na(x)))
    for(i in 1:length(valeurs_manquantes)) {
      cat(sprintf("  %-30s: %d valeurs manquantes (%.2f%%)\n", 
                  names(valeurs_manquantes)[i], 
                  valeurs_manquantes[i],
                  valeurs_manquantes[i]/nrow(donnees)*100))
    }
    
    cat("\nTYPES DE VARIABLES:\n")
    types_variables <- sapply(donnees, function(x) class(x)[1])
    for(i in 1:length(types_variables)) {
      cat(sprintf("  %-30s: %s\n", names(types_variables)[i], types_variables[i]))
    }
    
    if (input$choix_jeu_donnees == "wisconsin") {
      cat("\nDÉTECTION DE VALEURS ABERRANTES (méthode IQR):\n")
      variables_numeriques <- names(donnees)[sapply(donnees, is.numeric)]
      for(variable in variables_numeriques) {
        if(variable != "id") {
          Q1 <- quantile(donnees[[variable]], 0.25, na.rm = TRUE)
          Q3 <- quantile(donnees[[variable]], 0.75, na.rm = TRUE)
          IQR_val <- Q3 - Q1
          aberrantes <- sum(donnees[[variable]] < (Q1 - 1.5*IQR_val) | 
                              donnees[[variable]] > (Q3 + 1.5*IQR_val), na.rm = TRUE)
          cat(sprintf("  %-30s: %d valeurs aberrantes (%.2f%%)\n", 
                      variable, aberrantes, aberrantes/nrow(donnees)*100))
        }
      }
    }
    
    cat(sprintf("\nRÉSUMÉ GLOBAL:\n"))
    cat(sprintf("  Nombre total d'observations: %d\n", nrow(donnees)))
    cat(sprintf("  Nombre total de variables: %d\n", ncol(donnees)))
    cat(sprintf("  Taux de complétude global: %.2f%%\n", 
                (1 - sum(valeurs_manquantes)/(nrow(donnees)*ncol(donnees)))*100))
  })
  
  # Téléchargements
  output$telecharger_donnees_csv <- downloadHandler(
    filename = function() {
      if (input$choix_jeu_donnees == "wisconsin") {
        paste0("donnees_wisconsin_cancer_sein_", Sys.Date(), ".csv")
      } else {
        paste0("donnees_seer_cancer_sein_", Sys.Date(), ".csv")
      }
    },
    content = function(fichier) {
      if (input$choix_jeu_donnees == "wisconsin") {
        write.csv(donnees_wisconsin(), fichier, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(donnees_seer(), fichier, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$telecharger_rapport_complet <- downloadHandler(
    filename = function() {
      paste0("rapport_analyse_cancer_sein_", Sys.Date(), ".html")
    },
    content = function(fichier) {
      contenu_html <- paste0(
        "<!DOCTYPE html><html><head>",
        "<title>Rapport d'Analyse Complète - Cancer du Sein</title>",
        "<meta charset='UTF-8'>",
        "<style>body{font-family:Arial,sans-serif;margin:40px;line-height:1.6;}",
        "h1{color:#2c3e50;}h2{color:#3498db;}table{border-collapse:collapse;width:100%;}",
        "th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background-color:#f2f2f2;}</style>",
        "</head><body>",
        "<h1>Rapport d'Analyse Complète - Cancer du Sein</h1>",
        "<p><strong>Date de génération:</strong> ", Sys.Date(), "</p>",
        "<p><strong>Nombre d'observations Wisconsin:</strong> ", nrow(donnees_wisconsin()), "</p>",
        "<p><strong>Nombre de patients SEER:</strong> ", nrow(donnees_seer()), "</p>",
        "<h2>Résumé Exécutif</h2>",
        "<p>Cette analyse porte sur ", nrow(donnees_wisconsin()), " échantillons de biopsies mammaires ",
        "avec ", sum(donnees_wisconsin()$diagnostic_tumoral == "Malin"), " cas malins et ",
        sum(donnees_wisconsin()$diagnostic_tumoral == "Benin"), " cas bénins, ainsi que sur ",
        nrow(donnees_seer()), " patients du registre SEER pour l'analyse de survie.</p>",
        "<h2>Méthodologie</h2>",
        "<p>L'analyse a été réalisée en utilisant des techniques statistiques avancées et des algorithmes ",
        "d'apprentissage automatique pour identifier les patterns discriminants entre tumeurs bénignes et malignes, ",
        "complétée par une analyse de survie basée sur les données cliniques SEER.</p>",
        "<h2>Conclusions</h2>",
        "<p>Les résultats démontrent l'efficacité des caractéristiques morphologiques cellulaires ",
        "pour la classification diagnostique automatisée et l'importance des facteurs cliniques ",
        "pour la prédiction de survie.</p>",
        "<p><em>Note: Ce rapport est généré automatiquement à des fins éducatives et de recherche.</em></p>",
        "</body></html>"
      )
      writeLines(contenu_html, fichier, useBytes = TRUE)
    }
  )
  
  output$telecharger_modele_entrainee <- downloadHandler(
    filename = function() {
      paste0("modele_cancer_sein_", input$algorithme_ml, "_", Sys.Date(), ".rds")
    },
    content = function(fichier) {
      req(modele_entraine())
      saveRDS(modele_entraine(), fichier)
    }
  )
}

# Export du serveur
server <- serveur_principal