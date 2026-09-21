# Production finale OSYR — septembre 2026

## Objectif

La phase de production finale ne consiste plus à empiler les sorties exploratoires. Le workflow distingue désormais quatre niveaux : analyses principales, analyses complémentaires, analyses finales alignées sur le plan de dépouillement, puis génération des livrables publiables.

Le rapport principal est volontairement plus sélectif que les sorties analytiques. Les figures supplémentaires sont conservées dans une annexe graphique séparée.

## Ordre du workflow

Point d'entrée recommandé : `source("00_lancer_workflow_complet.R")`.

Le workflow appelle ensuite, selon les options activées : `01_analyse_osyr_base_et_modeles.R`, `03_analyses_complementaires_wp2_30062026.R`, `R/osyr_final_analyses.R`, `05_produire_rapport_final.R` et `06_generer_presentation_finale.R`.

## Couche finale d'analyses

`R/osyr_final_analyses.R` produit des analyses supplémentaires pour couvrir les sections du plan final : parcours de formation, connaissances, pratiques, intentions et attitudes, perceptions, profils et analyses transversales, précautions méthodologiques.

Les nouvelles analyses sont stockées sous `outputs_osyr_rapport_final/tables/`, `outputs_osyr_rapport_final/figures/` et `outputs_osyr_rapport_final/diagnostics/`.

## Rapport principal

`05_produire_rapport_final.R` génère `outputs_osyr_rapport_final/rapport_final_osyr.docx`.

Le rapport principal ne contient plus le tableau de couverture analytique, le catalogue technique des figures, les messages de type « Points à rédiger » ni les légendes internes du workflow.

Il contient désormais une page de couverture, un résumé des principaux résultats, une section méthode, les sections du plan de dépouillement, des paragraphes factuels produits à partir des tables, un tableau synthétique par section lorsque les données le permettent, au maximum trois figures principales par section, puis une conclusion et les précautions méthodologiques.

Les formulations générées automatiquement restent descriptives et doivent être relues avant publication.

## Annexe graphique

Le même script génère `outputs_osyr_rapport_final/annexe_graphique_osyr.docx`.

Les figures non retenues dans le corps principal y sont conservées. Les sélections sont documentées dans `figures_rapport_principal.csv` et `figures_annexe.csv`.

## Présentation finale

`06_generer_presentation_finale.R` génère `outputs_osyr_presentation_finale/presentation_finale_osyr.pptx`.

La présentation suit une logique plus éditoriale : synthèse générale, slide de section, slide « résultats à retenir », au maximum deux figures principales par section, puis conclusion.

## Charte graphique OSYR

La charte est centralisée dans `R/osyr_style.R`.

Couleurs principales : marron `#998A5B`, vert `#7FB680`, beige `#FEFAD4`, vert sombre `#2F4A35`.

Le thème graphique impose des titres plus courts, des marges plus importantes, des tailles de texte homogènes, des légendes en bas et alignées à gauche, un quadrillage limité et des tableaux cohérents avec le gabarit OSYR.

## Synthèses textuelles

`R/osyr_report_text.R` lit les tables produites par le workflow et génère des résumés factuels pour le rapport et la présentation.

Il ne remplace pas l'interprétation scientifique. Il automatise uniquement des formulations fondées sur les valeurs calculées : valeurs les plus élevées ou faibles, gaps connaissance-usage, écarts exposés/non exposés et résultats les plus stables dans les tests de sensibilité.

## Points du plan encore à surveiller

Le plan de dépouillement mentionne explicitement plusieurs objets qui doivent être produits dès que leur codage est disponible : organisateurs des formations Q9, nombre de formations Q10 si la variable détaillée n'est pas encore mobilisée, raisons de non-adoption Q14, représentations spontanées et analyse lexicale si les sorties textuelles ne sont pas encore intégrées au catalogue final.

Ces éléments ne doivent pas être inventés lorsque les variables correspondantes ne sont pas disponibles. Le rapport final doit signaler le manque de sortie plutôt que le masquer.

## Reproductibilité

Les données brutes ne sont pas versionnées dans GitHub. Elles restent attendues sous `data/BJ30232 - BDD V2.csv` et `data/BJ30232 - DATAMAP V2.xlsx`.

Le workflow produit ensuite les bases nettoyées, les tables, les figures, le rapport et la présentation.