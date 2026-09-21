# Production finale — septembre 2026

Ce document décrit le passage du workflow d'exploration et de compléments WP2 à une chaîne de production finale pour le rapport publié et la présentation de restitution.

## Objectif

L'objectif est de stabiliser un ensemble de figures, tableaux et diagnostics directement utilisables dans le rapport final, tout en gardant une traçabilité complète des traitements.

La trame suivie est celle du plan de dépouillement de septembre 2026 :

1. Parcours de formation
2. Connaissances
3. Pratiques
4. Intentions et attitudes
5. Perceptions
6. Profils et analyses transversales
7. Précautions méthodologiques

## Organisation des scripts

```text
00_lancer_workflow_complet.R               # point d'entrée
01_analyse_osyr_base_et_modeles.R          # base analytique, descriptifs, figures principales
02_generer_rapport_word.R                  # rapport analytique existant
03_analyses_complementaires_wp2_30062026.R # analyses complémentaires et robustesse
04_generer_presentation_powerpoint.R       # présentation existante
05_produire_rapport_final.R                # rapport final structuré
06_generer_presentation_finale.R           # présentation finale
99_session_info.R                          # environnement R
R/osyr_style.R                             # style OSYR, plan final, catalogue de départ
R/osyr_final_analyses.R                    # analyses et figures supplémentaires pour la production finale
```

## Style OSYR

Les couleurs sont centralisées dans `R/osyr_style.R` :

```r
brown = "#998A5B"
green = "#7FB680"
beige = "#FEFAD4"
```

Le fichier ajoute aussi des couleurs secondaires sobres, uniquement pour faciliter les graphiques et les documents : vert foncé, vert pâle, gris, blanc et noir.

## Renforcement analytique

La première version du rapport final était trop proche d'un catalogue de figures existantes. La version actuelle ajoute une couche dédiée :

```r
source("R/osyr_final_analyses.R")
```

Cette couche produit des figures et tableaux complémentaires directement alignés sur le plan de dépouillement :

- exposition aux dispositifs selon l'année de thèse ;
- types de dispositifs Q8 par année et par discipline ;
- volume de formation ;
- évaluation des formations Q11 ;
- connaissance et usage Q5 item par item ;
- scores synthétiques selon l'exposition ;
- connaissance, usage et intentions selon l'année ;
- pratiques Q4 ;
- usages Q5 selon l'année ;
- intentions Q13 selon l'exposition ;
- réponses « je ne sais pas » sur les intentions ;
- intentions selon les niveaux de connaissance et d'usage ;
- perceptions Q15 selon l'exposition ;
- environnement Q12 selon l'exposition ;
- perceptions selon l'environnement perçu ;
- profils exploratoires de répondants ;
- focus non formés / autoformés / dispositif organisé ;
- couverture du plan de dépouillement.

## Rapport final

Le script `05_produire_rapport_final.R` produit :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_rapport_final/tables/plan_rapport_final.csv
outputs_osyr_rapport_final/tables/catalogue_figures_rapport.csv
outputs_osyr_rapport_final/tables/catalogue_figures_finales.csv
outputs_osyr_rapport_final/tables/couverture_plan_de_depouillement.csv
outputs_osyr_rapport_final/tables/figures_manquantes.csv
```

Le rapport contient :

- le plan du rapport ;
- les questions traitées par section ;
- les figures consolidées ;
- les sorties mobilisées ;
- un emplacement clair pour la rédaction des interprétations ;
- une annexe méthodologique.

Le script n'utilise plus les styles Word `Title` ou `Subtitle`, car ils peuvent être absents selon les templates. Il s'appuie sur les styles standards `heading 1`, `heading 2`, `heading 3` et `Normal`.

## Présentation finale

Le script `06_generer_presentation_finale.R` produit :

```text
outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
outputs_osyr_presentation_finale/catalogue_figures_presentation.csv
```

La présentation reprend désormais jusqu'à quatre figures par section lorsque les sorties sont disponibles. Les titres restent descriptifs et alignés sur les sections du plan.

## Catalogue des figures

Le catalogue de départ est défini dans `R/osyr_style.R`. La couche `R/osyr_final_analyses.R` l'enrichit ensuite en produisant :

```text
outputs_osyr_rapport_final/tables/catalogue_figures_finales.csv
```

Ce catalogue consolidé indique pour chaque figure :

- la section du rapport ;
- le bloc analytique ;
- le titre à utiliser ;
- le nom du fichier ;
- le chemin réel ;
- la légende courte ;
- le niveau de priorité ;
- l'état de disponibilité.

## Précautions d'interprétation

Les résultats doivent être présentés comme descriptifs et associatifs. Les modèles ajustés ne permettent pas d'attribuer causalement les différences observées aux dispositifs de formation. Les analyses reposent sur des réponses déclaratives et doivent tenir compte des effets de discipline, d'année de thèse, d'exposition aux formations et de contexte institutionnel.

## Ordre recommandé d'exécution

Pour une production complète :

```r
source("00_lancer_workflow_complet.R")
```

Pour ne produire que les livrables finaux, après avoir déjà généré les sorties des scripts 01 et 03 :

```r
source("05_produire_rapport_final.R")
source("06_generer_presentation_finale.R")
```
