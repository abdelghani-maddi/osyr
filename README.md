# OSYR — Workflow analytique

Version de travail : septembre 2026

Ce dépôt contient les scripts R utilisés pour traiter l'enquête OSYR, produire les analyses et générer les livrables.

## Lancement

Placer les deux fichiers d'entrée dans `data/` :

```text
data/BJ30232 - BDD V2.csv
data/BJ30232 - DATAMAP V2.xlsx
```

Puis lancer :

```r
source("00_lancer_workflow_complet.R")
```

## Scripts

```text
00_lancer_workflow_complet.R                # orchestration
01_analyse_osyr_base_et_modeles.R           # nettoyage, variables, scores, descriptifs
03_analyses_complementaires_wp2_30062026.R  # modèles, robustesse, FDR, diagnostics
R/osyr_final_analyses.R                     # analyses destinées aux livrables finaux
R/osyr_plan_depouillement_analyses.R        # audit point par point et analyses manquantes du plan
R/osyr_figure_polish.R                      # mise en forme des figures finales
05_produire_rapport_final.R                 # rapport Word
06_generer_presentation_finale.R            # présentation
99_session_info.R                           # environnement logiciel
```

Les scripts 02 et 04 correspondent aux anciennes versions du rapport Word et du PowerPoint et restent disponibles à titre de compatibilité.

## Pondération

La base comporte une seule variable de pondération : `Poids`.

Le script 01 la transforme en `.weight`. Les analyses complémentaires comparent ensuite :

- un scénario non pondéré, représenté techniquement par `weight_none = 1` ;
- un scénario pondéré utilisant uniquement `.weight`.

Aucune autre variable numérique n'est interprétée comme un poids. Les observations dont `.weight` est manquant ou non positif sont exclues des analyses pondérées.

## DATAMAP

`BJ30232 - DATAMAP V2.xlsx` fournit les libellés des questions, les codes et les modalités. Elle est utilisée pour reconstruire les libellés de Q1, Q2, Q4, Q5, Q7 à Q15 et pour documenter les recodages.

Q2 comporte dix domaines disciplinaires détaillés. Un regroupement en quatre grands domaines est construit uniquement pour certaines analyses de robustesse.

Le script 01 prépare désormais aussi, lorsque les colonnes sont présentes, les formats nécessaires pour Q3 (mots spontanés), Q7, Q9 et Q14, ainsi que les indicateurs présentiel/distanciel issus de Q8.

## Principales sorties

```text
outputs_osyr_v2_final/
outputs_osyr_v2_complements_30062026/
outputs_osyr_rapport_final/
outputs_osyr_presentation_finale/
outputs_osyr_v2_session/
```

Les livrables principaux sont :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_rapport_final/annexe_graphique_osyr.docx
outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
```

## Documentation

Voir notamment :

```text
NOTES_METHODOLOGIQUES.md
GUIDE_WORKFLOW_DETAILLE.md
docs/PRODUCTION_FINALE_SEPTEMBRE_2026.md
```

## Interprétation

Les analyses sont descriptives et associatives. Les modèles ajustés ne permettent pas d'attribuer causalement les différences observées aux dispositifs. Les réponses de connaissance, d'usage, de pratiques, d'intentions et de perceptions sont déclaratives.


## Couverture du plan de dépouillement

La table suivante est régénérée à chaque exécution :

```text
outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv
```

Elle distingue les points analysés, les analyses conditionnelles à la présence des variables et les demandes qui nécessitent une source externe. Elle ne déduit pas la couverture à partir du seul nombre de figures.
