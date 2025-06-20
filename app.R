# ===============================================================================
# APPLICATION PRINCIPALE - ANALYSE INTELLIGENTE DU CANCER DU SEIN
# ===============================================================================
# 
# Auteur: Système d'Analyse Médicale Avancée
# Version: 1.0
# Date: 2025
# 
# Description: Application Shiny pour l'analyse des données de biopsies
# mammaires dans le domaine de la data science
# 
# Cette application permet:
# - L'exploration statistique univariée et multivariée des données
# - L'analyse de corrélations et la réduction de dimensionnalité
# - La modélisation prédictive avec différents algorithmes d'IA
# - L'interprétation automatique des résultats
# - La gestion et l'export des données et modèles
# ===============================================================================

# Chargement des composants
source("interface_utilisateur.R")
source("logique_serveur.R")

# Lancement de l'application
shinyApp(ui = ui, server = server)