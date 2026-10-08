# Module exploratoire pour le groupe de travail OSYR

Ce module ajoute **un parcours descriptif distinct** des analyses et livrables existants. Il ne modifie aucun script du workflow principal.

## Usage

1. Executer au prealable `01_analyse_osyr_base_et_modeles.R` (ou le workflow ordinaire).
2. Depuis la racine du depot, lancer : `source("R/exploration_wg/generer_tableaux_wg.R")`.
3. Lire les classeurs du dossier `outputs_osyr_exploration_wg/` dans l'ordre numerote.

La base `outputs_osyr_v2_final/data_clean/osyr_v2_corrigee_clean.rds` est lue sans modification.

## Sorties

- 00_Guide_de_lecture.xlsx : parcours accompagne
- 01_Population_et_completude.xlsx : diagnostic de non-reponse et variables de population
- 02_Tris_a_plat.xlsx : distributions des variables categorielles
- 03_Variables_numeriques.xlsx : statistiques sommaires
- 04_Croisements_exploratoires.xlsx : effectifs, pourcentages en ligne et en colonne
- 05_Variables_et_recodages.xlsx : comparaisons a completer explicitement
- 06_Commentaires_WG.xlsx : support de commentaires du groupe

Les classeurs sont exclusivement **non ponderes** pour cette premiere version. Ne pas les confondre avec les analyses ponderees produites par le workflow (poids `Poids` / `.weight`). L'ajout de tables ponderees documentees, des libelles DATAMAP et du plan de depouillement doit preceder la diffusion definitive au WG.

## Securite et validation

- Aucun micro-enregistrement n'est exporte.
- Les effectifs inferieurs a 5 sont signales dans les tris a plat mais **non automatiquement supprimes**. Une revue des tableaux croises et des risques de divulgation indirecte est requise avant tout partage.
- Les questions a reponses multiples, les filtres de questionnaire et les bases eligibles necessitent des traitements specifiques.
- Le module refuse d'ecraser ses propres fichiers d'une execution anterieure : archiver les sorties avant regeneration.
- Le script n'a pas encore ete valide sur les donnees OSYR reelles.

## Integration

Aucune modification du lanceur `00_lancer_workflow_complet.R`, des scripts 01/03/05/06, des sorties historiques, ou des rapports. La presente branche est destinee a une revue par pull request.
