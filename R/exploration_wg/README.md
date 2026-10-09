# OSYR — module d'exploration descriptive WG / WP2 (V3)

La commande historique reste valable :

```r
source("R/exploration_wg/generer_tableaux_wg.R")
```

Elle lance maintenant `R/exploration_wg/osyr_atlas_wp2_v3.R`, qui lit en lecture seule les fichiers `data/BJ30232 - BDD V2.csv` et `data/BJ30232 - DATAMAP V2.xlsx` (ou la variante `(1)`). Dépendances : `readxl`, `openxlsx`, `officer`, `flextable`.

Nouvelles sorties dans `outputs_osyr_exploration_wp2_v3/` :
- `OSYR_atlas_WP2.xlsx` : index des questions et distributions libellées ;
- `OSYR_cahier_WP2.docx` : tableaux éditables, classés selon les six thèmes ;
- `audit_libelles.csv` : codes ou variables sans correspondance vérifiée.

**Attention** : prototype à exécuter et à valider avant diffusion. Descriptifs *non pondérés* ; les bases observées ne constituent pas toujours les bases éligibles des questions filtrées. Les réponses multiples sont présentées comme des parts de répondants. Les petits effectifs sont signalés, **pas supprimés**. Le rapport ne comprend pas encore tous les croisements du plan de dépouillement.

La V3 remplace l'ancien point d'entrée Excel mais ne modifie ni les analyses, ni les modèles, ni les rapports finaux du workflow principal. Les scripts historiques restent dans l'historique Git.
