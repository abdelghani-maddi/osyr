# OSYR — Workflow analytique

Version propre : 07/07/2026  
Objet : package de scripts R pour analyser la base OSYR V2 corrigée, générer les figures, les modèles, le rapport Word et le PowerPoint de restitution.

## Lancement rapide

1. Ouvrir RStudio dans ce dossier.
2. Placer les données dans `data/` :

```text
data/BJ30232 - BDD V2.csv
data/BJ30232 - DATAMAP V2.xlsx
```

3. Lancer :

```r
source("00_lancer_workflow_complet.R")
```

## Ordre des scripts

```text
00_lancer_workflow_complet.R              # script maître
01_analyse_osyr_base_et_modeles.R         # nettoyage, descriptifs, scores, modèles, texte
02_generer_rapport_word.R                 # rapport Word commenté
03_analyses_complementaires_wp2_30062026.R # compléments après retours WP2
04_generer_presentation_powerpoint.R      # PowerPoint de restitution
99_session_info.R                         # archive de l'environnement R
```

Le renommage corrige les anciennes versions qui commençaient à `05_`, `07_`, etc. Ici, la numérotation suit l’ordre logique d’exécution.

## Sorties produites

```text
outputs_osyr_v2_final/                    # sorties principales du script 01
outputs_osyr_v2_rapport_word/             # rapport Word du script 02
outputs_osyr_v2_complements_30062026/      # analyses complémentaires du script 03
outputs_osyr_v2_ppt/                      # PowerPoint du script 04
outputs_osyr_v2_session/                  # infos de session du script 99
```

## Ce que fait le workflow principal

- Lecture de la base et de la datamap.
- Nettoyage des noms de variables et des encodages.
- Recodage de l’année de thèse, de la discipline, de la langue et de l’exposition.
- Construction des tables longues pour Q4, Q5, Q12, Q13 et Q15.
- Descriptifs pondérés.
- Figures principales.
- Scores synthétiques.
- Modèles ajustés.
- Prédictions et interactions.
- Analyse textuelle des trois mots associés à la science ouverte.

## Ce que les compléments WP2 ajoutent

- Sensibilité aux pondérations, y compris sans pondération.
- Sensibilité au regroupement disciplinaire.
- Tests et intervalles de confiance.
- Gap connaissance → usage.
- Analyse des `je ne sais pas`, des `non` et des non-réponses.
- Profil des autoformés.
- Analyse Q8-Q11 : dispositifs, nombre, MOOC, présentiel/distanciel si détectables.
- Croisements avec langue du questionnaire et établissement.
- Liens Q12 / Q15 / Q7.
- Lexicométrie, cadrages cognitifs et cooccurrences des trois mots.

## Précautions d’interprétation

Les résultats sont descriptifs et associatifs. Même les modèles ajustés ne prouvent pas un effet causal des dispositifs. La langue du questionnaire doit être lue comme un proxy prudent, pas comme une nationalité. Les scores de connaissance et d’usage sont déclaratifs.


Voir aussi `DEMARRAGE_RAPIDE.md` pour lancer le workflow sans lire toute la documentation.


## Version v7 — couverture complète des remarques WP2

La version v7 ajoute une réponse systématique aux remarques du CR WP2 du 30/06/2026.

Le point important est que toutes les remarques ne relèvent pas du script 01 :

- `01_analyse_osyr_base_et_modeles.R` produit la base analytique, les scores, les premières tables, figures et modèles.
- `03_analyses_complementaires_wp2_30062026.R` implémente les analyses de robustesse, de sensibilité et d'approfondissement demandées après la présentation WP2.

La matrice de couverture complète se trouve dans :

```text
MATRICE_COUVERTURE_REMARQUES_WP2.md
```

et, après exécution du script 03 :

```text
outputs_osyr_v2_complements_30062026/methodology/coverage_remarques_wp2.csv
```

Trois demandes restent volontairement documentées comme données externes, car elles ne peuvent pas être calculées depuis la base seule :

1. précision OpinionWay à 85 % ;
2. offre réelle de formations en anglais par collège doctoral ;
3. annotation manuelle définitive des trois mots.
