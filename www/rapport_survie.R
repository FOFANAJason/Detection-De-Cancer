library(grid)
library(gridExtra)
library(ggplot2)

generer_rapport_survie <- function(resultat_survie, inputs, filename = "rSurvie.pdf") {
  
  # Configuration du PDF
  pdf(file = filename, width = 8.27, height = 11.69, paper = "a4")  # Format A4
  
  # ---- Page 1 : En-tête professionnel + logo ----
  grid.newpage()
  
  if(file.exists("www/cancer_de_sein.png")) {
    logo <- png::readPNG("www/cancer_de_sein.png")
    grid.raster(logo, x = 0.9, y = 0.95, width = 0.15)
  }
  
  grid.text("RAPPORT DE PRÉDICTION DE SURVIE", x = 0.1, y = 0.9, just = "left", gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text(paste("Date :", Sys.Date()), x = 0.1, y = 0.85, just = "left", gp = gpar(fontsize = 12))
  
  # ---- Page 2 : Résumé de la prédiction ----
  grid.newpage()
  
  # Création d'une fonction pour les sections encadrées
  create_section <- function(title, content, y_position, bg_color = "#f7f7f7", text_color = "#333333") {
    # Arrière-plan de la section
    grid.rect(x = 0.05, y = y_position, width = 0.9, height = 0.15,
              just = c("left", "top"),
              gp = gpar(fill = bg_color, col = bg_color, alpha = 0.7))
    
    # Titre de la section
    grid.text(title, x = 0.1, y = y_position - 0.02, just = c("left", "top"),
              gp = gpar(fontsize = 14, fontface = "bold", col = text_color))
    
    # Contenu
    grid.text(content, x = 0.1, y = y_position - 0.05, just = c("left", "top"),
              gp = gpar(fontsize = 12, col = text_color))
  }
  
  # Texte principal
  rapport_principal <- paste0(
    "PRÉDICTION DE SURVIE :\n\n",
    "• Niveau de risque : ", resultat_survie$niveau_risque, "\n",
    "• Score de risque : ", resultat_survie$score_risque, "/15\n",
    "• Survie médiane estimée : ", round(resultat_survie$survie_estimee), " mois\n",
    "• Probabilité de survie à 5 ans : ", round(resultat_survie$prob_survie_5ans * 100, 1), "%\n\n",
    "FACTEURS ANALYSÉS :\n",
    "• Âge : ", inputs$age_patient, " ans\n",
    "• Origine ethnique : ", inputs$race_patient, "\n",
    "• Stade tumoral : ", inputs$t_stage, "\n",
    "• Atteinte ganglionnaire : ", inputs$n_stage, "\n",
    "• Grade histologique : ", inputs$grade_tumeur, "\n",
    "• Taille tumorale : ", inputs$taille_tumeur, " mm\n",
    "• Statut œstrogène : ", inputs$statut_estrogene, "\n",
    "• Statut progestérone : ", inputs$statut_progesterone, "\n\n"
  )
  
  grid.text(rapport_principal, x = 0.05, y = 0.95, just = c("left", "top"), gp = gpar(fontsize = 12))
  
  # ---- Section INTERPRÉTATION CLINIQUE améliorée ----
  interpretation <- switch(
    resultat_survie$niveau_risque,
    "FAIBLE" = paste0(
      "Pronostic favorable\n\n",
      "• Excellente probabilité de survie à long terme\n",
      "• Surveillance de routine recommandée (suivi annuel)\n",
      "• Bonne réponse attendue aux traitements standards\n",
      "• Risque faible de complications\n",
      "• Qualité de vie généralement bien préservée"
    ),
    "MODÉRÉ" = paste0(
      "Pronostic intermédiaire\n\n",
      "• Nécessite un suivi régulier (tous les 6 mois)\n",
      "• Thérapie adjuvante recommandée\n",
      "• Évaluation nutritionnelle conseillée\n",
      "• Surveillance des effets secondaires nécessaire\n",
      "• Adaptation possible du traitement selon la réponse"
    ),
    paste0(
      "Pronostic défavorable\n\n",
      "• Prise en charge multidisciplinaire intensive requise\n",
      "• Suivi rapproché (tous les 3 mois)\n",
      "• Soutien psychosocial fortement recommandé\n",
      "• Évaluation des options thérapeutiques avancées\n",
      "• Gestion active des symptômes nécessaire"
    )
  )
  
  create_section(
    "INTERPRÉTATION CLINIQUE",
    interpretation,
    y_position = 0.5,
    bg_color = switch(resultat_survie$niveau_risque,
                      "FAIBLE" = "#e6f2ff",
                      "MODÉRÉ" = "#fff2e6",
                      "#ffe6e6"),
    text_color = switch(resultat_survie$niveau_risque,
                        "FAIBLE" = "#004080",
                        "MODÉRÉ" = "#804000",
                        "#800000")
  )
  
  # ---- AVERTISSEMENT MÉDICAL amélioré ----
  avertissement <- paste0(
    "Ceci est une estimation statistique\n\n",
    "• Ne remplace pas l'évaluation clinique d'un spécialiste\n",
    "• Doit être interprété dans le contexte global du patient\n",
    "• Les facteurs individuels peuvent modifier significativement le pronostic\n",
    "• Les décisions thérapeutiques doivent être prises par l'équipe médicale\n",
    "• Consulter immédiatement en cas de nouveaux symptômes"
  )
  
  create_section(
    "AVERTISSEMENT MÉDICAL IMPORTANT",
    avertissement,
    y_position = 0.3,
    bg_color = "#fff8e6",
    text_color = "#cc6600"
  )
  
  dev.off()
}
