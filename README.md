# README – Projet RShiny DPE Rhône

Ce dépôt contient une application RShiny développée dans le cadre du projet Enedis / GreenTech Solutions.  
L’objectif est d’analyser les Diagnostics de Performance Énergétique (DPE) du département du Rhône et de visualiser différentes statistiques (DPE, émissions de CO₂, consommation, coûts, carte interactive…).

---

## Structure du dépôt

```
iut-sd2-enedis/
│
├── app/                         # Code source de l’application Shiny
│   ├── app.R                    # Fichier principal
│   ├── www/                     # Ressources (images, CSS, JS)
│   └── data/                    # Données locales (CSV)
│
├── data_preparation/            # Scripts d’extraction et de préparation
│   └── extraction_api.R
│
├── rapport/                     # Rapport statistique (Rmd + HTML/PDF)
│   └── rapport_statistique.rmd
│   └── rapport_statistique.pdf
│
├── docs/                        # Documentation du projet
│   ├── technical_doc.md
│   ├── functional_doc.md
│   └── images/                  # Images utilisées dans les documentations
|
└── README.md                    # Présentation du dépôt
```

---

## Fonctionnalités principales

- Visualisation des DPE, émissions, coûts et relations entre variables.
- Carte interactive des logements du Rhône par énergie et code postal.
- Téléchargement des graphiques (PNG) et données filtrées (CSV).
- Mise à jour automatique des données via l’API ADEME.

---

## Lancer l’application

1. Installer R et RStudio  
2. Ouvrir `app/app.R` dans RStudio  
3. Cliquer sur "Run App"  
Les packages nécessaires s’installent automatiquement.
