# Démarrage rapide — OSYR workflow v3

1. Dézipper le dossier.
2. Créer `data/` à la racine.
3. Y placer :
   - `BJ30232 - BDD V2.csv`
   - `BJ30232 - DATAMAP V2.xlsx`
4. Ouvrir le dossier dans RStudio.
5. Lancer :

```r
source("00_lancer_workflow_complet.R")
```

Pour tester étape par étape :

```r
source("01_analyse_osyr_base_et_modeles.R")
source("02_generer_rapport_word.R")
source("03_analyses_complementaires_wp2_30062026.R")
source("04_generer_presentation_powerpoint.R")
```

Cette version corrige :

- l'erreur `item_label` ;
- l'erreur `Can't recycle true (size 15) to size 1` ;
- l'erreur de jointure du script 03 quand aucun modèle n'est estimable ;
- la dépendance inutile au package `rvg`.
