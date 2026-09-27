# OSYR — Workflow analytique

Version de travail : 27 septembre 2026

Ce dépôt contient la chaîne R utilisée pour préparer les données de l'enquête OSYR, produire les analyses prévues par le plan de dépouillement, vérifier leur robustesse et générer les livrables Word et PowerPoint.

## Lancement

Placer les deux fichiers d'entrée dans `data/` :

```text
data/BJ30232 - BDD V2.csv
data/BJ30232 - DATAMAP V2.xlsx
```

Puis lancer depuis la racine du projet :

```r
source("00_lancer_workflow_complet.R")
```

## Architecture

```text
00_lancer_workflow_complet.R
01_analyse_osyr_base_et_modeles.R
03_analyses_complementaires_wp2_30062026.R
R/osyr_final_analyses.R
R/osyr_plan_depouillement_analyses.R
R/osyr_figure_polish.R
05_produire_rapport_final.R
06_generer_presentation_finale.R
99_session_info.R
```

Les scripts 02 et 04 sont des versions historiques du rapport et du diaporama. Ils restent dans le dépôt mais sont désactivés par défaut.

## Rôle de chaque couche

- **01** : nettoyage, DATAMAP, pondération, variables analytiques, batteries longues, scores, descriptifs et contrôles qualité.
- **03** : modèles ajustés, tests item par item, intervalles de confiance, FDR, comparaison pondéré/non pondéré, balance et analyses de robustesse.
- **osyr_final_analyses** : synthèses destinées aux livrables.
- **osyr_plan_depouillement_analyses** : audit point par point du plan de septembre 2026 et analyses complémentaires Q7 à Q15, Q3 et profils.
- **osyr_figure_polish** : figures de publication à partir des tables déjà calculées ; cette étape ne recalcule pas les résultats.
- **05/06** : assemblage du rapport Word et du PowerPoint.
- **99** : versions de R et des packages.

## Pondération

La base comporte une seule variable de pondération : `Poids`, recodée en `.weight`.

Les analyses pondérées excluent les poids manquants, nuls ou négatifs. `weight_none = 1` est uniquement une constante technique utilisée pour reproduire un scénario non pondéré ; ce n'est pas une seconde pondération.

## Dénominateurs

Les non-réponses et codes hors champ ne sont pas transformés en réponses négatives. Les proportions sont calculées parmi les réponses valides de l'indicateur concerné.

Q8, Q9 et Q14 sont multiréponses : leurs pourcentages ne sont pas destinés à s'additionner à 100 %. Pour Q8, le dénominateur est limité aux répondants ayant au moins une réponse valide à la question.

Q13 distingue explicitement `Oui = 1`, `Non = 2` et `Je ne sais pas = 97`.

## Modèles et robustesse

Les scores synthétiques sont analysés par régression linéaire pondérée. Les indicateurs binaires item par item utilisent des modèles linéaires de probabilité lorsque l'objectif est d'exprimer un écart ajusté en points.

Les modèles spécifiques à la présence d'une modalité Q8 sont logistiques et sont restitués en **odds ratios** avec intervalle de confiance. Un coefficient logistique n'est pas interprété comme un écart en points de pourcentage.

Les tests multiples sont complétés par une correction de Benjamini-Hochberg lorsque cela est pertinent.

La balance des groupes exposés/non exposés utilise des différences standardisées pondérées (SMD).

## Analyses multivariées

L'ACP exploratoire utilise la pondération dans le centrage, la standardisation et la covariance. Le k-means et la CAH restent des classifications exploratoires d'individus ; les variables sont standardisées avec la pondération et les classes sont caractérisées avec des statistiques pondérées.

Ces analyses ne définissent pas une typologie stable de la population des doctorants.

## DATAMAP

`BJ30232 - DATAMAP V2.xlsx` est la référence pour les libellés, codes et modalités. Le workflow ne reconstruit pas arbitrairement les modalités lorsqu'elles sont disponibles dans la DATAMAP.

Q2 conserve les dix domaines disciplinaires. Le regroupement en quatre domaines n'est utilisé que dans certaines analyses de sensibilité.

## Sorties

```text
outputs_osyr_v2_final/
outputs_osyr_v2_complements_30062026/
outputs_osyr_rapport_final/
outputs_osyr_presentation_finale/
outputs_osyr_v2_session/
```

Livrables :

```text
outputs_osyr_rapport_final/rapport_final_osyr.docx
outputs_osyr_rapport_final/annexe_graphique_osyr.docx
outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
```

Contrôles particulièrement utiles :

```text
outputs_osyr_v2_final/diagnostics/response_denominator_diagnostics.csv
outputs_osyr_v2_final/diagnostics/q8_exposure_consistency.csv
outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv
outputs_osyr_v2_complements_30062026/methodology/covariate_balance_exposed_nonexposed.csv
```

## Documentation

- `GUIDE_WORKFLOW_DETAILLE.md`
- `NOTES_METHODOLOGIQUES.md`
- `docs/AUDIT_PLAN_DEPOUILLEMENT_SEPT2026.md`
- `docs/CARTOGRAPHIE_CODE_PLAN_DEPOUILLEMENT.md`

## Interprétation

Les analyses sont descriptives et associatives. Les ajustements statistiques ne permettent pas d'attribuer causalement les écarts observés aux dispositifs de formation. Les réponses sont déclaratives et les analyses de profils et de mots spontanés restent exploratoires.
