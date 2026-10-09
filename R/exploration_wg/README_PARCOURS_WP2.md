# Parcours descriptif WP2 — version 2 (proposition)

Cette version s'ajoute au module Excel initial, sans modifier le code analytique ni la branche principale.

## Exécution

Depuis la racine du dépôt, après l'exécution du script 01 :

```r
install.packages(c("openxlsx", "officer", "flextable")) # une seule fois
source("R/exploration_wg/generer_parcours_wp2.R")
```

Le script utilise exclusivement en lecture :
- `outputs_osyr_v2_final/data_clean/osyr_v2_corrigee_clean.rds`
- `outputs_osyr_v2_final/data_clean/question_map_v2.csv`
- `outputs_osyr_v2_final/data_clean/answer_labels_v2.csv`

Le nouveau dossier `outputs_osyr_exploration_wp2_v2/` est créé seulement s'il n'existe pas déjà. Il contient six classeurs Excel thématiques, `OSYR_rapport_descriptif_WP2.docx` (tableaux Word natifs éditables) et `audit_libelles_non_resolus.csv`.

## Parcours de lecture

1. Profil (Q1–Q2)
2. Formation (Q7–Q11)
3. Connaissances et pratiques (Q3–Q5)
4. Environnement (Q12)
5. Intentions et non-adoption (Q13–Q14)
6. Perceptions (Q15)

Les libellés officiels sont rapprochés grâce au DATAMAP. Le fichier d'audit signale les champs pour lesquels un libellé de question n'a pas été trouvé. Les codes de réponse n'ayant pas de correspondance unique ne sont pas inventés et restent visibles.

## Limites de la version à examiner

**Prototype non exécuté sur les données réelles dans cette proposition.** Il faut vérifier avant publication :
- les rattachements des champs aux questions et au plan de dépouillement détaillé ;
- les filtres et bases éligibles, notamment pour les questions conditionnelles ;
- les formats multiréponses Q8/Q9/Q14 (les pourcentages ne doivent pas être interprétés comme une distribution exclusive) ;
- les analyses pondérées (`Poids` / `.weight`) : cette version est **non pondérée** ;
- les valeurs codées résiduelles, le cas des réponses textuelles Q3, les échelles et scores dérivés ;
- la confidentialité des faibles effectifs, signalés mais non supprimés.

Ce premier document Word est un **atlas descriptif** organisé par grandes sections du plan et non une preuve de couverture exhaustive de chacune des analyses du plan. Les croisements ciblés et le dictionnaire des recodages devront être validés avec le WP2 avant diffusion.

## Retour attendu du WP2

Pour chaque chapitre : commenter l'intelligibilité des modalités, les tendances descriptives, les non-réponses, les regroupements controversés et les croisements à prioriser. Mentionner le numéro de question et de tableau dans les commentaires partagés.
