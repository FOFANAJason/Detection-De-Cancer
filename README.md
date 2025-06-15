# 🧬 Prédiction du Cancer du Sein

## 📌 Description du projet

Ce projet est une application interactive développée avec **R Shiny**, ayant pour objectif de **prédire si une tumeur mammaire est bénigne ou maligne** à partir de variables cliniques issues du jeu de données **Breast Cancer Wisconsin**.  
L'application permet également d'explorer les données, de visualiser les distributions, et d'effectuer des prédictions à l'aide d'un modèle de machine learning.

---

## 💡 Objectifs pédagogiques

- Appliquer les notions de **data science** à un cas concret dans le domaine médical.
- Mettre en œuvre un **modèle prédictif** avec `caret`.
- Créer une **application Shiny interactive** avec une interface claire et fonctionnelle.
- Utiliser des concepts de **visualisation de données**, **jointure de sources externes**, et **nettoyage de données**.

---

## 🗂 Structure de l'application

L'application est structurée en deux fichiers distincts :

- `ui.R` : Définit l’interface utilisateur (UI)
- `server.R` : Contient la logique de traitement (backend)

---

## 📊 Fonctionnalités principales

### . **Présentation**
- Introduction au projet, au jeu de données, et à l’objectif de l’application.

### . **Exploration des données**
- Sélection d’une variable numérique.
- Affichage d’un boxplot comparant la variable entre les classes bénignes et malignes.
- Visualisation d’un tableau interactif.

### . **Prédiction**
- Saisie manuelle des paramètres (ex : `radius`, `texture`, etc.)
- Lancement de la prédiction via un bouton.
- Affichage du résultat (Bénin / Malin).

---

## 🧠 Modèle utilisé

- **Méthode** : Régression logistique (`glm`)  ou Les arbres ou forêts de classification
- **Librairie** : `caret`
- **Variables utilisées** : `radius_mean`, `texture_mean`, `perimeter_mean`, `area_mean`
- **Évaluation** : Possibilité d’ajouter un onglet avec des métriques (accuracy, courbe ROC...)

---

## 📚 Données utilisées

- 📌 Jeu de données principal : [`Breast Cancer Wisconsin Dataset` (inclus dans `mlbench`)](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data)
- 🔗 Sources complémentaires:
  - [SEER Breast Cancer Data (Kaggle)](https://www.kaggle.com/datasets/mansigambhir13/seer-breast-cancer-dataset/code)
  - [METABRIC Gene Expression Breast Cancer](https://www.kaggle.com/datasets/raghadalharbi/breast-cancer-gene-expression-profiles-metabric?select=METABRIC_RNA_Mutation.csv)

---

## ⚙️ Prérequis

```r
install.packages(c("shiny", "mlbench", "dplyr", "ggplot2", "DT", "caret"))
