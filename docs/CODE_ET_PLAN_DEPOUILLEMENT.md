# Correspondance entre le code et le plan de dépouillement OSYR

Version : 27 septembre 2026.

Ce document décrit la place de chaque script actif dans la chaîne d'analyse et les points du plan de dépouillement auxquels il répond. Il complète les commentaires présents dans le code.

## Ordre d'exécution

1. `01_analyse_osyr_base_et_modeles.R` — construction de la base analytique, codages, batteries longues, scores et descriptifs de base.
2. `03_analyses_complementaires_wp2_30062026.R` — modèles, robustesse, FDR, balance et analyses de sensibilité.
3. `R/osyr_final_analyses.R` — analyses transversales destinées aux livrables.
4. `R/osyr_plan_depouillement_analyses.R` — compléments et audit point par point du plan.
5. `R/osyr_method_checks.R` — contrôles d'intégrité méthodologique bloquants ou documentaires.
6. `R/osyr_figure_polish.R` — mise au format éditorial des figures sans recalcul statistique.
7. `05_produire_rapport_final.R` — rapport Word et annexe graphique.
8. `06_generer_presentation_finale.R` — présentation PowerPoint.
9. `99_session_info.R` — traçabilité de l'environnement logiciel.

Les scripts `02_generer_rapport_word.R` et `04_generer_presentation_powerpoint.R` sont conservés comme sorties historiques et ne sont pas exécutés par défaut.

## Script 01 — socle analytique

### Entrées

- base brute `BJ30232 - BDD V2.csv` ;
- DATAMAP `BJ30232 - DATAMAP V2.xlsx`.

### Principes méthodologiques

- `Poids` est l'unique variable de pondération et devient `.weight` ;
- `weight_none = 1` est uniquement une constante technique pour les scénarios non pondérés ;
- poids manquants, nuls ou négatifs exclus des calculs pondérés ;
- non-réponses et codes hors champ conservés comme valeurs manquantes ;
- Q13 distingue explicitement oui, non et « je ne sais pas » ;
- Q8 contradictoire (`aucune` + autre modalité) classé `Indéterminé` ;
- scores proportionnels construits au niveau du répondant sur les items valides.

### Plan de dépouillement couvert

| Bloc | Variables / analyses |
| --- | --- |
| Parcours de formation | Q7, Q8, Q9, Q10, Q11 ; exposition ; présentiel/distanciel ; organisateurs |
| Connaissances | Q5 connaissance ; familles d'objets ; discipline |
| Pratiques | Q4 ; Q5 usage |
| Intentions | Q13 ; Q14 |
| Perceptions | Q12 ; Q15 |
| Transversal | année de thèse, discipline détaillée et agrégée, langue, Q3 lorsque disponible |

## Script 03 — tests et robustesse

### Objet

Évaluer la stabilité des associations observées et documenter les différences de composition entre groupes.

### Analyses principales

- modèles avec et sans pondération ;
- discipline détaillée et discipline agrégée ;
- intervalles de confiance à 95 % ;
- correction Benjamini-Hochberg pour les familles de tests ;
- effets item par item ;
- effets par discipline ;
- balance des covariables avec différences standardisées pondérées ;
- analyses Q8 selon langue, année et discipline.

Les coefficients sont interprétés comme des associations ajustées, jamais comme des effets causaux.

## `R/osyr_final_analyses.R` — couche transversale

Ce script fabrique les tables directement utilisées par les livrables :

- exposition et dispositifs selon année / discipline ;
- connaissance-usage Q5 ;
- pratiques Q4 ;
- intentions Q13 et « je ne sais pas » ;
- environnement Q12 et perceptions Q15 ;
- focus non-formés / autoformés / dispositif organisé ;
- profils exploratoires.

Il ne doit pas redéfinir la pondération ni les dénominateurs établis dans le script 01.

## `R/osyr_plan_depouillement_analyses.R` — compléments du plan

### Formation

- Q9 organisateurs : global, année, discipline, liens avec Q11 et scores ;
- Q10 : global, année, discipline ;
- Q11 : selon volume, format, environnement et organisateur ;
- focus non-formés, autoformés, présentiel et distanciel.

### Connaissances et pratiques

- familles Q5 selon exposition, volume de formation, année, discipline, environnement et format ;
- liens ciblés Q4-Q5 ;
- modèles élargis de l'usage Q5.

### Intentions

- profils pratiques-intentions ;
- Q14 ;
- intentions selon connaissance, usage, pratiques, Q7 et Q11 ;
- indice cumulatif de conditions favorables.

### Perceptions

- Q15 séparé en bénéfices, contraintes et risques ;
- accord / désaccord item par item ;
- modèles de perceptions ;
- lien avec plateformes d'accès non officielles.

### Analyses exploratoires

- mots Q3 : fréquences, dictionnaire, cohérence et croisements ;
- CAH : Ward, comparaison de 2 à 6 classes par silhouette.

## `R/osyr_method_checks.R` — intégrité méthodologique

Ce script est exécuté avant la mise en forme finale. Il vérifie notamment :

- présence et unicité de la colonne source `Poids` ;
- présence de `.weight` ;
- poids valides ;
- bornes des scores ;
- cohérence du classement Q8 ;
- cohérence logique des batteries Q5, Q12 et Q13 ;
- cohérence des IC, valeurs p et p ajustées FDR ;
- absence de vocabulaire interne dans le catalogue des figures.

Un contrôle de niveau `ERREUR` interrompt le workflow. Un contrôle de niveau `AVERTISSEMENT` est exporté sans bloquer la production.

Sortie : `outputs_osyr_rapport_final/diagnostics/integrite_methodologique.csv`.

## `R/osyr_figure_polish.R` — figures finales

Ce script ne recalcule aucun résultat. Il lit les tables existantes et agit uniquement sur :

- type de représentation ;
- ordre des catégories ;
- taille des textes ;
- dimensions d'export ;
- légendes ;
- densité visuelle ;
- libellés destinés au lecteur.

Les titres sont portés par Word et PowerPoint pour éviter la répétition dans les images.

## Rapport et présentation

`05_produire_rapport_final.R` suit les sept sections du plan de dépouillement et sépare méthode, résultats, discussion et annexes.

`06_generer_presentation_finale.R` utilise la même sélection de résultats mais place les notes méthodologiques en pied de diapositive plutôt qu'en sous-titre.

## Éléments qui restent hors du calcul automatique

- comparaison avec les statistiques externes des collèges doctoraux ;
- validation scientifique définitive du dictionnaire Q3 ;
- interprétation substantielle des classes exploratoires ;
- tout point du questionnaire dont la correspondance avec la DATAMAP n'est pas établie sans ambiguïté.