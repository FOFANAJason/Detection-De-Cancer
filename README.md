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

---

## 📚 Données utilisées

### 🔬 1. Structure de la tumeur — *Breast Cancer Wisconsin Dataset*

Ce jeu de données contient des mesures issues d’images numérisées de biopsies de tumeurs mammaires. Ces données permettent de **caractériser la morphologie de la tumeur**.

**Variables importantes utilisées :**

* `diagnosis` : classe cible (Bénigne ou Maligne)
* `radius_mean` : rayon moyen de la tumeur
* `texture_mean` : variation de la texture tumorale
* `perimeter_mean` : périmètre moyen tumoral
* `area_mean` : surface moyenne de la tumeur
* `concavity_mean` et `concave points_mean` : profondeur et nombre de creux sur les bords de la tumeur
* `fractal_dimension_mean`: Autossimilaire

**Target :**

* `diagnosis` (M / B)

Ce dataset permet d’analyser la structure générale de la tumeur, ce qui éclaire les analyses réalisées ensuite sur le plan clinique.

➡️ Source : [`Breast Cancer Wisconsin Dataset`](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data)

### 🏥 2. Données cliniques — *SEER Breast Cancer Dataset*

Ce jeu de données fournit des informations **cliniques et démographiques** sur les patientes. Il est utilisé pour réaliser des **analyses de survie** et des **prédictions du risque de décès**.

**Variables cliniques retenues :**

* `Age` (facteur pronostique indépendant)
* `T Stage` + `Tumor Size` (taille tumorale)
* `N Stage` + `Reginol Node Positive` (envahissement ganglionnaire)
* `Estrogen Status` / `Progesterone Status` (biologie tumorale)
* `Grade` (agressivité histologique)
* `A Stage` (présence de métastases)

**Target :**

* `Status` (Alive / Dead)
* `Survival Months`

**Analyse possible :**

* Classification (vivant ou non)
* Analyse de survie (durée estimée)

➡️ Source : [`SEER Breast Cancer Data`](https://www.kaggle.com/datasets/mansigambhir13/seer-breast-cancer-dataset/code)

---

## 🧹 Nettoyage de données

Les colonnes inutiles (`id`, `Unnamed: 32`, colonnes vides) ont été supprimées. Les données ont été standardisées et mises en forme pour permettre une analyse croisée entre la structure tumorale (biopsies) et le devenir clinique (statut vital et survie).

---

## ⚙️ Prérequis

```r
install.packages(c("shiny", "mlbench", "dplyr", "ggplot2", "DT", "caret"))