# Guide détaillé du workflow

## 00 — Script maître

`00_lancer_workflow_complet.R` exécute les scripts dans l’ordre. Il est le point d’entrée recommandé.

Les options en haut du fichier permettent d’éviter de relancer certaines étapes :

```r
RUN_01_ANALYSE_PRINCIPALE       <- TRUE
RUN_02_RAPPORT_WORD             <- TRUE
RUN_03_ANALYSES_COMPLEMENTAIRES <- TRUE
RUN_04_POWERPOINT               <- TRUE
```

## 01 — Analyse principale

`01_analyse_osyr_base_et_modeles.R` est le script le plus important.

Il est organisé en grands blocs :

1. préparation des packages et des chemins ;
2. fonctions générales ;
3. lecture de la base et de la datamap ;
4. construction de la base analytique ;
5. contrôles qualité ;
6. structure de l’échantillon et exposition ;
7. tables longues des batteries ;
8. descriptifs item par item ;
9. écarts exposés / non exposés ;
10. scores synthétiques ;
11. modèles ajustés ;
12. interactions et prédictions ;
13. analyse textuelle des trois mots ;
14. exports finaux.

### Correction importante v2

La fonction `plot_ranked_bar()` est désormais robuste si `item_label` est absent d’une table intermédiaire. Elle utilise alors `item`, puis `category`, puis un identifiant de ligne. Cela corrige l’erreur :

```text
object 'item_label' not found
```

## 02 — Rapport Word

`02_generer_rapport_word.R` lit les sorties du script 01 et génère un rapport analytique commenté. Il ne relance pas les analyses principales.

## 03 — Analyses complémentaires WP2

`03_analyses_complementaires_wp2_30062026.R` traduit les retours de réunion en hypothèses testables. Il documente les recodages et ajoute les tests de robustesse.

## 04 — PowerPoint

`04_generer_presentation_powerpoint.R` produit une présentation structurée en séquences : méthode, résultats, analyses complémentaires, synthèse, prochaines étapes.

## 99 — Session info

`99_session_info.R` archive l’environnement logiciel pour faciliter la reproductibilité.


# Complément v7 — ce que fait précisément le script 03

Le script `03_analyses_complementaires_wp2_30062026.R` est désormais structuré en sections numérotées :

1. Packages et options.
2. Chemins.
3. Fonctions utilitaires.
4. Lecture de la base enrichie produite par le script 01.
5. Matrice de couverture des remarques WP2.
6. Documentation enrichie des variables et regroupements.
7. Pondérations : qualité, sensibilité, note méthodologique.
8. Fonctions modèles et écarts avec IC.
9. Tests item par item : exposition avec IC.
10. Acculturation élevée quelle que soit la formation.
11. Gap connaissance → usage, y compris chez les non exposés.
12. Déclaratif / humilité : proxy empirique.
13. Familles d'objets : open data, publications, code, identifiants.
14. Scores, modèles principaux, pondérations et discipline détaillée.
15. Croiser usages, année de thèse et pratiques Q4.
16. Intentions : NSP, pratiques actuelles, freins et discipline.
17. Perceptions, environnement incitatif/frein, Q7, Q12, Q15.
18. Autoformés : profil complet.
19. Dispositifs Q8-Q11 : détail, MOOC, distanciel, présentiel, intensité.
20. Langue : proxy, établissement/collège, exposition, dispositifs.
21. Trois mots : lexicométrie, dictionnaire, annotation, cadrage cognitif.
22. Exports consolidés.

La logique est que le script 01 reste le socle propre et que le script 03 serve de laboratoire méthodologique et analytique post-réunion.
