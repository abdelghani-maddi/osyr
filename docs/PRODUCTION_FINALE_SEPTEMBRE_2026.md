# Production OSYR — septembre 2026

## Principe

La production finale repose sur une séparation stricte entre calcul, contrôle méthodologique, synthèse, mise en forme graphique et publication. Les scripts Word et PowerPoint ne recalculent aucune statistique.

## Ordre d'exécution

```text
01_analyse_osyr_base_et_modeles.R
03_analyses_complementaires_wp2_30062026.R
R/osyr_final_analyses.R
R/osyr_plan_depouillement_analyses.R
R/osyr_figure_polish.R
05_produire_rapport_final.R
06_generer_presentation_finale.R
99_session_info.R
```

Le point d'entrée recommandé reste :

```r
source("00_lancer_workflow_complet.R")
```

## Couche analytique

Le script 01 construit le socle, les scores et les diagnostics de dénominateurs.

Le script 03 produit les modèles ajustés, la correction FDR, la balance pondérée et les analyses de sensibilité. Les modèles logistiques Q8 sont restitués en odds ratios.

`R/osyr_final_analyses.R` prépare les synthèses nécessaires aux livrables.

`R/osyr_plan_depouillement_analyses.R` vérifie la couverture point par point du plan et complète Q7-Q15, Q3 et les profils lorsque nécessaire.

## Couche graphique

`R/osyr_figure_polish.R` ne recalcule aucun résultat. Il transforme les tables en figures adaptées au rapport et au diaporama.

Les titres, couleurs et notes de lecture sont centralisés dans `R/osyr_style.R`.

Les formulations internes de production ne doivent pas apparaître dans les livrables. Une note de figure explique seulement le champ, le dénominateur, le caractère multiréponse ou l'incertitude lorsqu'une telle précision est nécessaire.

## Rapport Word

`05_produire_rapport_final.R` génère :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_rapport_final/annexe_graphique_osyr.docx
```

Le corps principal comprend :

1. couverture ;
2. résumé des principaux résultats ;
3. méthode ;
4. parcours de formation ;
5. connaissances ;
6. pratiques ;
7. intentions et attitudes ;
8. perceptions ;
9. profils et analyses transversales ;
10. précautions méthodologiques ;
11. discussion et conclusion.

Les diagnostics techniques et la matrice de couverture du plan restent dans les sorties tabulaires ou l'annexe.

## Présentation

`06_generer_presentation_finale.R` utilise les mêmes synthèses et le même catalogue de figures que le rapport.

Les notes de lecture sont placées discrètement en bas de slide et non comme sous-titres techniques.

## Rédaction

`R/osyr_report_text.R` produit une rédaction déterministe à partir des CSV calculés. Il ne génère aucune nouvelle estimation.

## Contrôles avant diffusion

Vérifier en priorité :

```text
outputs_osyr_v2_final/diagnostics/response_denominator_diagnostics.csv
outputs_osyr_v2_final/diagnostics/q8_exposure_consistency.csv
outputs_osyr_v2_complements_30062026/methodology/covariate_balance_exposed_nonexposed.csv
outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv
```

Une nouvelle génération complète est requise lorsqu'un dénominateur, un score, un regroupement ou une spécification de modèle change.
