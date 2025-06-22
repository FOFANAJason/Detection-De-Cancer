library(grid)
library(gridExtra)
library(ggplot2)

generer_rapport_survie <- function(resultat_survie, inputs, filename = "rSurvie.pdf") {
  
  # Configuration du PDF
  pdf(filename, width = 11, height = 8.5, paper = "a4")  # Format A4
  
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
  rapport <- paste0(
    "🏥 PRÉDICTION DE SURVIE :\n\n",
    "• Niveau de risque : ", resultat_survie$niveau_risque, "\n",
    "• Score de risque : ", resultat_survie$score_risque, "/15\n",
    "• Survie médiane estimée : ", round(resultat_survie$survie_estimee), " mois\n",
    "• Probabilité de survie à 5 ans : ", round(resultat_survie$prob_survie_5ans * 100, 1), "%\n\n",
    "📊 FACTEURS ANALYSÉS :\n",
    "• Âge : ", inputs$age_patient, " ans\n",
    "• Origine ethnique : ", inputs$race_patient, "\n",
    "• Stade tumoral : ", inputs$t_stage, "\n",
    "• Atteinte ganglionnaire : ", inputs$n_stage, "\n",
    "• Grade histologique : ", inputs$grade_tumeur, "\n",
    "• Taille tumorale : ", inputs$taille_tumeur, " mm\n",
    "• Statut œstrogène : ", inputs$statut_estrogene, "\n",
    "• Statut progestérone : ", inputs$statut_progesterone, "\n\n",
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
  
  grid.text(rapport, x = 0.05, y = 0.95, just = c("left", "top"), gp = gpar(fontsize = 12))
  
  dev.off()
}
