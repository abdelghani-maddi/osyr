# OSYR — Workflow analytique

Version production finale : 21/09/2026  
Objet : scripts R pour analyser la base OSYR V2 corrigée, produire les sorties principales et complémentaires, puis générer le rapport final et la présentation finale selon le plan de dépouillement de septembre 2026.

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
00_lancer_workflow_complet.R               # script maître
01_analyse_osyr_base_et_modeles.R          # nettoyage, descriptifs, scores, figures principales
02_generer_rapport_word.R                  # rapport analytique existant
03_analyses_complementaires_wp2_30062026.R  # compléments, robustesse, tests et approfondissements
04_generer_presentation_powerpoint.R       # présentation existante
05_produire_rapport_final.R                # rapport final structuré selon le plan de dépouillement
06_generer_presentation_finale.R           # présentation finale selon la même trame
99_session_info.R                          # archive de l'environnement R
R/osyr_style.R                             # couleurs, style, plan final et catalogue de figures
```

## Sorties produites

```text
outputs_osyr_v2_final/                     # sorties principales du script 01
outputs_osyr_v2_complements_30062026/       # analyses complémentaires du script 03
outputs_osyr_rapport_final/                # rapport final du script 05
outputs_osyr_presentation_finale/          # présentation finale du script 06
outputs_osyr_v2_session/                   # infos de session du script 99
```

Les anciens dossiers `outputs_osyr_v2_rapport_word/` et `outputs_osyr_v2_ppt/` restent disponibles si les scripts 02 et 04 sont activés, mais la production finale passe désormais par les scripts 05 et 06.

## Ce que fait le workflow principal

- Lecture de la base et de la datamap.
- Nettoyage des noms de variables et des encodages.
- Recodage de l'année de thèse, de la discipline, de la langue et de l'exposition aux dispositifs.
- Construction des tables longues pour Q4, Q5, Q8, Q12, Q13 et Q15.
- Descriptifs pondérés.
- Figures principales.
- Scores synthétiques.
- Modèles ajustés.
- Prédictions et interactions.
- Analyse textuelle des trois mots associés à la science ouverte.

## Ce que les compléments ajoutent

- Sensibilité aux pondérations, y compris sans pondération.
- Sensibilité au regroupement disciplinaire.
- Tests et intervalles de confiance.
- Correction FDR pour les tests multiples.
- Gap connaissance-usage.
- Analyse des `je ne sais pas`, des `non` et des non-réponses.
- Profil des autoformés et des non-formés.
- Analyse Q8-Q11 : dispositifs, nombre de formations, MOOC, présentiel/distanciel.
- Croisements avec langue du questionnaire, année de thèse, discipline et établissement.
- Liens Q12 / Q13 / Q15 / Q7.
- Lexicométrie, cadrages cognitifs et cooccurrences des trois mots.

## Passage à la production finale

La phase de production finale est organisée autour du plan de dépouillement de septembre 2026. Elle vise à stabiliser les sorties à utiliser dans le rapport publié.

La trame retenue est :

1. Parcours de formation
2. Connaissances
3. Pratiques
4. Intentions et attitudes
5. Perceptions
6. Profils et analyses transversales
7. Précautions méthodologiques

Le fichier `R/osyr_style.R` centralise :

- les couleurs OSYR ;
- le plan du rapport ;
- le catalogue des figures ;
- les fonctions de style utilisées par le rapport et la présentation.

Les scripts finaux produisent :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
```

## Style OSYR

Les couleurs principales utilisées dans les nouveaux scripts sont :

```text
Marron : #998A5B
Vert principal : #7FB680
Beige : #FEFAD4
```

Elles sont définies dans `R/osyr_style.R` et appliquées aux tableaux, graphiques et gabarits de restitution.

## Documentation

Voir :

```text
docs/PRODUCTION_FINALE_SEPTEMBRE_2026.md
GUIDE_WORKFLOW_DETAILLE.md
NOTES_METHODOLOGIQUES.md
MATRICE_COUVERTURE_REMARQUES_WP2.md
```

## Précautions d'interprétation

Les résultats sont descriptifs et associatifs. Même les modèles ajustés ne prouvent pas un effet causal des dispositifs. La langue du questionnaire doit être lue comme un proxy prudent, pas comme une nationalité. Les scores de connaissance et d'usage sont déclaratifs.
