# Cartographie du code et du plan de dépouillement

Version du 27 septembre 2026.

Ce document relie les points du plan de dépouillement aux scripts, variables, méthodes et sorties du workflow. Il complète la matrice générée automatiquement dans `outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv`.

## 1. Parcours de formation

| Point du plan | Questions / variables | Traitement | Script principal | Sorties |
| --- | --- | --- | --- | --- |
| Part formés / non formés | Q8, `exposure3` | Distribution pondérée des profils d'exposition | 01, final, plan | `plan_formation_exposition_globale.csv`, `formation_exposition_par_annee.csv` |
| Types de dispositifs | Q8 | Modalités originales, multiréponse | 01, final, polish | `q8_device_distribution_detail.csv`, figures Q8 |
| Présentiel / distanciel | Q8 codes 1-4 | Indicateurs de présence des formats | 01, plan | `plan_focus_presentiel_distanciel_scores.csv` |
| Autoformation | Q8 codes 5, 6, 98 | Profil séparé des dispositifs organisés | 01, final, plan | `plan_focus_non_formes_autoformes_organises.csv` |
| Organisateurs | Q9 | Multiréponse, répondant dédupliqué par organisme | 01, plan | `plan_q9_organisateurs_global.csv` |
| Volume | Q10 | 1 ; 2-3 ; 4+ ; ne sait pas | 01, plan | `plan_q10_distribution_globale.csv` |
| Évaluation | Q11 | Accord 3-4 ; score proportionnel sur items valides | 01, final, plan | `formation_evaluation_q11.csv`, croisements Q11 |
| Environnement de la direction | Q12_A1 | frein / neutre / incitation / NSP | plan | `plan_q11_selon_direction_these_q12.csv` |

## 2. Connaissances

| Point du plan | Variables | Traitement | Script | Sorties |
| --- | --- | --- | --- | --- |
| Connaissance Q5 | Q5 | codes 3-4 = bonne connaissance | 01, final | `connaissances_q5_items_gap.csv` |
| Usage Q5 | Q5 | code 4 = a déjà utilisé | 01, final | même table + sorties d'usage |
| Gap connaissance-usage | Q5 | différence de proportions en points | 01, final | tables et figures de gap |
| Familles d'objets | Q5 | regroupements analytiques transparents | 01, plan | `plan_q5_familles_*.csv` |
| Représentations spontanées | Q3 | fréquences et dictionnaire exploratoire | 01, plan | `plan_q3_*.csv` |
| Connaissance et situation de recherche | Q4 x Q5 | liens ciblés articles, données, code | plan | `plan_lien_q4_connaissances_usages_q5.csv` |

## 3. Pratiques

| Point du plan | Variables | Traitement | Script | Sorties |
| --- | --- | --- | --- | --- |
| Pratiques de recherche | Q4 | oui parmi réponses valides | 01, final | `pratiques_q4_items.csv` |
| Usages de SO | Q5 | usage item par item | 01, final | tables Q5 |
| Facteurs associés | score Q5 usage | modèle pondéré ajusté | plan | `plan_modele_pratiques_q5_elargi.csv` |
| Caractéristiques de formation | Q8, Q10, Q11 | modèle sur répondants concernés par une formation | plan | `plan_modele_pratiques_q5_caracteristiques_formation.csv` |
| Robustesse des écarts | scores et items | pondéré/non pondéré, discipline détaillée/agrégée | 03 | tables/models de robustesse |

## 4. Intentions et attitudes

| Point du plan | Variables | Traitement | Script | Sorties |
| --- | --- | --- | --- | --- |
| Intentions | Q13 | oui / non / je ne sais pas distingués | 01, final | `intentions_q13_par_exposition.csv` |
| Raisons de non-adoption | Q14 | multiréponse ; synthèse par intention et par répondant | 01, plan | `plan_q14_raisons_non_adoption.csv`, `plan_q14_raisons_global_respondants.csv` |
| Lien connaissance/usage-intentions | Q5 x Q13 | quartiles et scores | final, plan | `plan_intentions_selon_*.csv` |
| Lien pratiques-intentions | Q4 x Q13 | quartiles de pratique | plan | `plan_intentions_selon_pratiques_q4.csv` |
| Politique d'établissement | Q7 x Q13 | trois modalités Q7 | plan | `plan_intentions_par_q7_group.csv` |
| Indice cumulatif | Q10, Q12, Q5 | quatre conditions observées ; aucune imputation des manquants | plan | `plan_cumul_formation_environnement_connaissance_usage_intentions.csv` |

## 5. Perceptions

| Point du plan | Variables | Traitement | Script | Sorties |
| --- | --- | --- | --- | --- |
| Environnement | Q12 | scores séparés incitation et frein | 01, final | tables/figures Q12 |
| Accord item par item | Q15 | codes 4-5 = accord ; 1-2 = désaccord | 01, final, plan | `plan_q15_accord_desaccord_global.csv` |
| Bénéfices | Q15_A2, A4, A6 | score proportionnel | 01, plan | modèles/synthèses Q15 |
| Contraintes | Q15_A5, A7 | score proportionnel | 01, plan | modèles/synthèses Q15 |
| Risques individuels | Q15_A1, A3 | score proportionnel | 01, plan | modèles/synthèses Q15 |
| Facteurs associés | scores Q15 | modèles pondérés ajustés + FDR | plan | `plan_modeles_perceptions_q15.csv` |
| Plateformes non officielles | Q5 item concerné x Q15 | comparaison descriptive | plan | `plan_perceptions_selon_usage_plateformes_non_officielles.csv` |

## 6. Profils

| Analyse | Méthode | Pondération | Script | Statut |
| --- | --- | --- | --- | --- |
| ACP | covariance après centrage-réduction pondéré | utilisée dans transformation et covariance | final | exploratoire |
| k-means | distance sur variables standardisées pondérées | pas de poids de fréquence dans l'algorithme ; caractérisation pondérée | final | exploratoire |
| CAH | Ward sur variables standardisées pondérées | standardisation pondérée ; caractérisation pondérée | plan | exploratoire |
| Choix du nombre de classes CAH | silhouette moyenne, k = 2 à 6 | non applicable au critère lui-même | plan | exploratoire |

## 7. Robustesse et précautions méthodologiques

| Contrôle | Méthode | Script | Sortie |
| --- | --- | --- | --- |
| Pondéré / non pondéré | même modèle, deux scénarios de poids | 03 | modèles de robustesse |
| Discipline détaillée / agrégée | changement de niveau d'ajustement | 03 | synthèse de robustesse |
| Comparaisons multiples | Benjamini-Hochberg | 03, plan | colonnes `p_fdr` |
| Balance | SMD pondérée | 03 | `covariate_balance_exposed_nonexposed.csv` |
| Dénominateurs | réponses valides / exclues | 01 | `response_denominator_diagnostics.csv` |
| Cohérence Q8 | « aucune » avec autre choix | 01 | `q8_exposure_consistency.csv` |
| Couverture du plan | statut fondé sur sorties réelles | plan | `couverture_plan_depouillement_detaillee.csv` |

## 8. Couche de publication

`R/osyr_figure_polish.R` ne produit aucune estimation. Il transforme les tables en graphiques lisibles.

`R/osyr_style.R` centralise les couleurs, titres et notes de lecture.

`R/osyr_report_text.R` rédige des paragraphes uniquement à partir des résultats calculés.

`05_produire_rapport_final.R` et `06_generer_presentation_finale.R` assemblent les livrables sans recalcul statistique.

Cette séparation doit être conservée lors de toute modification ultérieure.
