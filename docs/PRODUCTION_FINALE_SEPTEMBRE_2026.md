# Production finale — septembre 2026

Ce document décrit le passage du workflow d'exploration et de compléments WP2 à une chaîne de production finale pour le rapport publié et la présentation de restitution.

## Objectif

L'objectif n'est plus de multiplier les sorties exploratoires, mais de stabiliser un ensemble de figures, tableaux et diagnostics directement utilisables dans le rapport final.

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
00_lancer_workflow_complet.R              # point d'entrée
01_analyse_osyr_base_et_modeles.R         # base analytique, descriptifs, figures principales
02_generer_rapport_word.R                 # rapport analytique existant
03_analyses_complementaires_wp2_30062026.R # analyses complémentaires et robustesse
04_generer_presentation_powerpoint.R      # présentation existante
05_produire_rapport_final.R               # nouveau rapport final structuré
06_generer_presentation_finale.R          # nouvelle présentation finale
99_session_info.R                         # environnement R
R/osyr_style.R                            # style OSYR, plan final, catalogue des figures
```

## Style OSYR

Les couleurs sont centralisées dans `R/osyr_style.R` :

```r
brown = "#998A5B"
green = "#7FB680"
beige = "#FEFAD4"
```

Le fichier ajoute aussi des couleurs secondaires sobres, uniquement pour faciliter les graphiques et les documents : vert foncé, vert pâle, gris, blanc et noir.

## Rapport final

Le script `05_produire_rapport_final.R` produit :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_rapport_final/tables/plan_rapport_final.csv
outputs_osyr_rapport_final/tables/catalogue_figures_rapport.csv
outputs_osyr_rapport_final/tables/figures_manquantes.csv
```

Le rapport est volontairement structuré. Il contient :

- le plan du rapport ;
- les questions traitées par section ;
- les figures disponibles ;
- les sorties mobilisées ;
- un emplacement clair pour la rédaction des interprétations ;
- une annexe méthodologique.

## Présentation finale

Le script `06_generer_presentation_finale.R` produit :

```text
outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
outputs_osyr_presentation_finale/catalogue_figures_presentation.csv
```

La présentation reprend les figures prioritaires du catalogue et évite les intitulés provisoires. Les titres sont descriptifs et alignés sur les sections du plan.

## Catalogue des figures

Le catalogue des figures est défini dans `R/osyr_style.R` par la fonction :

```r
osyr_figure_catalog()
```

Il indique pour chaque figure :

- la section du rapport ;
- le bloc analytique ;
- le titre à utiliser ;
- le nom du fichier ;
- le dossier source ;
- le niveau de priorité.

Après exécution, le catalogue est enrichi avec le chemin réel et un indicateur `available`.

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
