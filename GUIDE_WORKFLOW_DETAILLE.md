# Guide du workflow OSYR

## 00 — Script maître

`00_lancer_workflow_complet.R` est le point d'entrée du traitement. Il appelle les scripts dans l'ordre prévu et permet d'activer ou de désactiver certaines étapes.

## 01 — Analyse principale

`01_analyse_osyr_base_et_modeles.R` assure :

1. la lecture de la base et de la DATAMAP ;
2. le nettoyage des noms de variables et des libellés ;
3. la construction des variables analytiques ;
4. la mise au format long des batteries ;
5. les descriptifs pondérés ;
6. la construction des scores ;
7. les sorties par année, discipline, langue et exposition ;
8. les contrôles qualité.

La DATAMAP V2 est la référence pour les libellés des questions et des modalités.

### Pondération

Une seule colonne est utilisée comme poids d'enquête : `Poids`, recodée en `.weight`.

`weight_none = 1` est une variable technique servant au scénario non pondéré. Elle ne constitue pas une pondération supplémentaire.

## 02 — Rapport Word historique

`02_generer_rapport_word.R` lit les sorties du script 01. Il est conservé pour compatibilité mais n'est plus le livrable principal.

## 03 — Analyses complémentaires

`03_analyses_complementaires_wp2_30062026.R` ajoute :

- la comparaison pondéré / non pondéré ;
- la comparaison discipline détaillée / discipline agrégée ;
- les modèles ajustés sur les scores ;
- les modèles item par item ;
- les intervalles de confiance ;
- la correction FDR ;
- les diagnostics de composition des groupes ;
- les analyses détaillées de Q8 ;
- les synthèses de robustesse.

Le script ne détecte plus automatiquement des « poids » à partir du nom des colonnes. Les seuls scénarios sont :

- non pondéré : `weight_none = 1` ;
- pondéré : `.weight`, issu de `Poids`.

Les observations dont `.weight` est manquant ou non positif sont exclues des analyses pondérées.

## R/osyr_final_analyses.R

Cette étape prépare les tables et figures utilisées dans le rapport final : trajectoires par année de thèse, croisements Q5, intentions, perceptions, profils exploratoires et diagnostics méthodologiques.

## R/osyr_plan_depouillement_analyses.R

Cette étape confronte les sorties au plan de dépouillement de septembre 2026 et ajoute les analyses qui n'étaient pas encore couvertes : Q9, Q10 détaillé, croisements Q11, familles Q5, liens Q4-Q5, Q14, Q7, dimensions positives/négatives de Q15, indice cumulatif, analyse des mots Q3 lorsque les réponses sont disponibles, et CAH exploratoire.

Elle produit notamment `couverture_plan_depouillement_detaillee.csv`. Le statut d'un point dépend de l'existence réelle des variables et sorties, et non du nombre de graphiques.

## R/osyr_figure_polish.R

Cette étape reprend les tables finales et régénère les figures destinées au rapport et à la présentation dans un format plus lisible.

## 05 — Rapport final

`05_produire_rapport_final.R` génère le rapport principal et l'annexe graphique.

## 06 — Présentation finale

`06_generer_presentation_finale.R` génère le diaporama à partir de la même sélection de résultats.

## 99 — Session info

`99_session_info.R` conserve les versions de R et des packages utilisés.

## Variables et questions couvertes

Le traitement s'appuie notamment sur :

- Q1 : année de thèse ;
- Q2 : domaine scientifique principal ;
- Q3 : mots ou expressions spontanés associés à la science ouverte, lorsque les colonnes textuelles sont présentes ;
- Q4 : pratiques de recherche déjà réalisées ;
- Q5 : connaissance et usage de quinze outils ou pratiques ;
- Q7 : politique ou directives de l'établissement ;
- Q8 : dispositifs suivis ;
- Q9 : organismes organisateurs ;
- Q10 : nombre de formations ou actions ;
- Q11 : appréciation des formations ;
- Q12 : freins et incitations ;
- Q13 : intentions ;
- Q14 : raisons de non-adoption ;
- Q15 : représentations de la science ouverte.

Les modalités exactes sont celles de la DATAMAP V2 placée dans `data/BJ30232 - DATAMAP V2.xlsx`.
