# Audit du plan de dépouillement – septembre 2026

Ce document rapproche le plan transmis le 27 septembre 2026 des sorties du workflow. Il sert de trace de couverture et ne remplace pas l'interprétation scientifique.

## Parcours de formation

| Point du plan | État après révision | Sorties principales |
| --- | --- | --- |
| Part des formés / non formés | Analysé | plan_formation_exposition_globale.csv ; formation_exposition_par_annee.csv ; plan_formation_exposition_par_discipline.csv |
| Types de dispositifs Q8 | Analysé | distributions détaillées Q8 par année, discipline et langue ; indicateurs présentiel/distanciel |
| Organisateurs Q9 | Conditionnel à la présence des colonnes Q9 | plan_q9_organisateurs_global.csv ; par année ; par discipline ; liens Q11 et scores |
| Nombre de formations Q10 | Analysé | plan_q10_distribution_globale.csv ; plan_q10_par_annee.csv ; plan_q10_par_discipline.csv |
| Q11 croisé avec Q8, Q10, Q12 et Q9 | Analysé, Q9 conditionnel | plan_q11_selon_modalite_q8.csv ; plan_q11_selon_volume_q10.csv ; plan_q11_selon_incitation_q12.csv ; plan_q11_selon_frein_q12.csv ; plan_q11_selon_organisateur_q9.csv |
| Focus non-formés | Analysé | plan_focus_non_formes_autoformes_organises.csv ; plan_focus_non_formes_autoformes_composition.csv |
| Focus distanciel | Analysé | plan_focus_presentiel_distanciel_scores.csv |
| Focus autoformés | Analysé | mêmes sorties de focus |
| Comparaison avec taux de formation des collèges doctoraux | Hors base | Nécessite une source externe comparable |
| Formation, attitudes et mots spontanés | Partiel / conditionnel | corrélations pondérées ; Q3 si les champs textuels sont présents |

## Connaissances

| Point du plan | État après révision | Sorties principales |
| --- | --- | --- |
| Niveau de connaissance et d'usage | Analysé | connaissances_q5_items_gap.csv ; scores_par_exposition.csv |
| Types de connaissances | Analysé | familles Q5 |
| Selon formation | Analysé | plan_q5_familles_par_exposition.csv |
| Selon volume de formation | Analysé | plan_q5_familles_par_volume_formation.csv |
| Présentiel / distanciel | Analysé | plan_q5_familles_par_presentiel_distanciel.csv |
| Selon année | Analysé | plan_q5_familles_par_annee.csv |
| Selon discipline | Analysé | plan_q5_familles_par_discipline.csv |
| Selon direction de thèse / environnement | Analysé | plan_q5_familles_selon_direction_these.csv |
| Représentations spontanées / trois mots | Conditionnel et exploratoire | fréquences, catégories de dictionnaire, cohérence des trois mots, croisements formation/discipline |
| Lien Q4-Q5 | Analysé | plan_lien_q4_connaissances_usages_q5.csv |

Le plan écrit « Q1 » au point consacré aux mots spontanés, mais le commentaire inséré dans le document renvoie à Q3. Le workflow utilise Q3 pour cette partie.

## Pratiques

| Point du plan | État après révision | Sorties principales |
| --- | --- | --- |
| État des pratiques Q4 | Analysé | pratiques_q4_items.csv |
| Gap connaissance-usage Q5 | Analysé | connaissances_q5_items_gap.csv et sorties par discipline |
| Facteurs associés aux pratiques | Analysé | plan_modele_pratiques_q5_elargi.csv |
| Rôle de la formation, du volume et du format | Analysé | plan_modele_pratiques_q5_caracteristiques_formation.csv |
| Prendre en compte la situation de recherche Q4 | Analysé | score Q4 dans le modèle + liens ciblés Q4-Q5 |
| Attitudes envers la SO | Analysé | dimensions Q15 dans le modèle élargi |
| Q11 et pratique réelle | Analysé | plan_q11_evaluation_et_pratique_reelle.csv |
| Point nommé Q6 dans le plan | À rapprocher du questionnaire exact | La batterie correspondante n'est pas identifiée sans ambiguïté dans les sorties actuelles |

## Intentions et attitudes

| Point du plan | État après révision | Sorties principales |
| --- | --- | --- |
| Profils pratiques-intentions | Analysé | plan_profils_pratiques_intentions.csv |
| Profils majoritaires / atypiques et caractéristiques | Analysé de manière exploratoire | K-means existant + CAH + caractérisation année, discipline, formation |
| Cohérence Q13-Q15 | Analysé | plan_correlations_pratiques_intentions_perceptions.csv |
| Q14 raisons de non-adoption | Conditionnel à la présence des colonnes Q14 | plan_q14_raisons_non_adoption.csv ; plan_q14_raisons_par_exposition.csv |
| Q13 non-formés et « je ne sais pas » | Analysé | intentions_q13_par_exposition.csv |
| Selon année et discipline | Analysé | plan_intentions_par_year.csv ; plan_intentions_par_discipline_detail.csv |
| Selon Q11 | Analysé | plan_intentions_selon_evaluation_q11.csv |
| Selon connaissance et usage Q5 | Analysé | plan_intentions_selon_connaissance.csv ; plan_intentions_selon_usage.csv |
| Selon pratique Q4 | Analysé | plan_intentions_selon_pratiques_q4.csv |
| Selon Q7 | Analysé | plan_intentions_par_q7_group.csv |
| Selon profils issus des mots | Conditionnel à Q3 | plan_q3_categories_et_scores.csv |
| Cumul formation + direction + connaissances + usages | Analysé | plan_cumul_formation_environnement_connaissance_usage_intentions.csv |

## Perceptions

| Point du plan | État après révision | Sorties principales |
| --- | --- | --- |
| Q15 par année | Analysé | plan_perceptions_q15_par_year.csv |
| Q15 par discipline | Analysé | plan_perceptions_q15_par_discipline_detail.csv |
| Q15 selon Q4 | Analysé | plan_perceptions_q15_selon_q4_band.csv |
| Q15 selon Q5 | Analysé | plan_perceptions_q15_selon_q5_usage_band.csv |
| Q15 selon Q12 | Analysé | plan_perceptions_q15_selon_q12_incitation_band.csv |
| Q15 selon formation | Analysé | plan_perceptions_q15_par_exposure3.csv |
| Perceptions positives / négatives | Analysé | score_q15_benefits ; score_q15_constraints ; score_q15_risks |
| Accord / désaccord item par item | Analysé | plan_q15_accord_desaccord_global.csv |
| Facteurs associés aux perceptions | Analysé | plan_modeles_perceptions_q15.csv |
| Mots de jugement et perceptions | Conditionnel à Q3 | plan_q3_categories_et_scores.csv |
| Plateformes d'accès non officielles | Analysé avec prudence | plan_perceptions_selon_usage_plateformes_non_officielles.csv |

Le questionnaire mesure l'usage de plateformes d'accès non officielles ; le workflow ne transforme pas cette variable en mesure spécifique de Sci-Hub.

## Précautions méthodologiques et autres approches

Les analyses restent descriptives et associatives. La comparaison avec des données externes de collèges doctoraux n'est pas calculée en l'absence d'un fichier comparable. Les mots Q3 restent soumis à validation du dictionnaire. La CAH est exploratoire ; le nombre de classes est choisi parmi 2 à 6 à partir de la silhouette moyenne. Les classifications ne sont pas interprétées comme des types stables hors échantillon.

La table produite automatiquement à chaque exécution est :

`outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv`.