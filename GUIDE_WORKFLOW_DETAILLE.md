# Guide du workflow OSYR

Version du 27 septembre 2026.

## Principe général

Le workflow sépare volontairement **calcul**, **contrôle**, **synthèse**, **mise en forme graphique** et **publication**. Cette séparation évite qu'une modification esthétique change un résultat statistique et permet de retrouver l'étape à l'origine de chaque tableau du rapport.

## 00 — Orchestration

`00_lancer_workflow_complet.R` appelle les scripts dans l'ordre requis. Après une modification méthodologique, un rerun complet est recommandé.

Les scripts 02 et 04 sont historiques et désactivés par défaut.

## 01 — Socle analytique

`01_analyse_osyr_base_et_modeles.R` :

1. lit la base et la DATAMAP ;
2. nettoie les noms et libellés ;
3. crée `.weight`, année, discipline, langue et exposition ;
4. prépare Q3, Q4, Q5, Q8, Q9, Q11, Q12, Q13, Q14 et Q15 au format adapté ;
5. construit les scores ;
6. produit les descriptifs de référence ;
7. contrôle les dénominateurs, la cohérence Q8 et les libellés.

Il correspond au socle commun des cinq blocs substantiels du plan.

## 03 — Modèles et robustesse

`03_analyses_complementaires_wp2_30062026.R` :

- compare pondéré/non pondéré ;
- compare discipline détaillée/agrégée ;
- ajuste les modèles sur année, discipline et langue ;
- réalise les modèles item par item ;
- applique la correction FDR ;
- calcule la balance pondérée des groupes ;
- produit les analyses Q8 selon langue et discipline ;
- génère les synthèses de robustesse.

Les modèles Q8 binaires sont logistiques et rapportés en odds ratios. Les autres indicateurs binaires item par item peuvent être analysés par modèle linéaire de probabilité afin d'obtenir un écart ajusté en points.

## R/osyr_final_analyses.R — synthèses des livrables

Ce script restructure les résultats autour des sept blocs du rapport : formation, connaissances, pratiques, intentions, perceptions, profils et précautions méthodologiques.

Il contient aussi l'ACP exploratoire pondérée et le k-means descriptif.

## R/osyr_plan_depouillement_analyses.R — audit du plan

Ce script complète les points insuffisamment couverts ailleurs :

- Q9 et organisateurs ;
- Q10 et intensité ;
- croisements Q11 ;
- familles Q5 et liens Q4-Q5 ;
- modèles élargis d'usage ;
- Q14 ;
- Q7 ;
- dimensions Q15 ;
- plateformes d'accès non officielles ;
- Q3 ;
- CAH ;
- indice cumulatif.

Il génère `couverture_plan_depouillement_detaillee.csv`. Une analyse n'y est marquée comme couverte que si une sortie correspondante est produite.

## R/osyr_figure_polish.R — figures de publication

Cette étape **ne recalcule pas les statistiques**. Elle relit les tables et produit des figures adaptées à la largeur du rapport et aux slides.

Les titres et notes de lecture sont centralisés dans `R/osyr_style.R`. Les formulations de production (« figure utile », « vue transversale », etc.) ne doivent pas apparaître dans le rapport final.

## R/osyr_report_text.R — rédaction déterministe

Ce fichier transforme les résultats calculés en paragraphes. Les phrases sont conditionnées à la présence des tables et colonnes attendues.

Il ne contient pas d'appel à un modèle génératif et ne crée aucune estimation nouvelle.

## 05 — Rapport Word

`05_produire_rapport_final.R` assemble :

- couverture ;
- résumé des principaux résultats ;
- méthode ;
- cinq blocs de résultats substantiels ;
- profils et analyses transversales ;
- précautions méthodologiques ;
- discussion et conclusion.

Les figures principales sont sélectionnées via le catalogue. Les figures restantes vont dans l'annexe graphique.

## 06 — Présentation

`06_generer_presentation_finale.R` utilise le même catalogue et les mêmes synthèses que le rapport. Les notes méthodologiques des figures apparaissent en bas de slide et non comme sous-titres techniques.

## 99 — Reproductibilité

`99_session_info.R` conserve `sessionInfo()` et la liste des packages installés.

## Repères du plan de dépouillement

| Bloc | Questions principales | Scripts |
| --- | --- | --- |
| Parcours de formation | Q7-Q11 | 01, 03, final, plan |
| Connaissances | Q5, Q3 | 01, final, plan |
| Pratiques | Q4, usage Q5 | 01, 03, final, plan |
| Intentions | Q13, Q14 | 01, 03, final, plan |
| Perceptions | Q12, Q15 | 01, 03, final, plan |
| Profils | scores transversaux | final, plan |
| Robustesse | poids, FDR, balance, sensibilité | 03, plan |

Pour le détail sortie par sortie, voir `docs/CARTOGRAPHIE_CODE_PLAN_DEPOUILLEMENT.md`.
