# Revue d'intégrité méthodologique — 27 septembre 2026

Cette note consigne les vérifications effectuées lors de la passe de cohérence du workflow. Elle documente les décisions susceptibles de modifier les résultats lors du prochain rerun.

## 1. Pondération

**Décision retenue**

- une seule pondération d'enquête : `Poids` → `.weight` ;
- aucun identifiant ou autre variable numérique n'est recherché comme poids ;
- poids manquant, nul ou négatif : exclusion de l'analyse pondérée ;
- `weight_none = 1` : scénario non pondéré uniquement.

**Contrôles**

- `response_denominator_diagnostics.csv` ;
- registre des poids du script 03 ;
- balance calculée avec les poids valides.

## 2. Dénominateurs des batteries

Les non-réponses ne sont pas transformées en `FALSE`.

Q4, Q5, Q11, Q12, Q13 et Q15 utilisent des indicateurs à trois états : positif, négatif valide, manquant/hors champ.

Cette règle est importante pour Q11, qui n'est pas renseigné par l'ensemble de l'échantillon.

## 3. Q8 — champ et classification

La DATAMAP distingue :

1. formation/atelier présentiel ;
2. formation/atelier à distance ;
3. journée d'étude/séminaire présentiel ;
4. journée d'étude/séminaire à distance ;
5. parcours de formation asynchrone (MOOC...) ;
6. autoformation ;
98. autres ;
97. aucune de ces propositions.

### Classification analytique

Les codes **1 à 5** sont considérés comme dispositifs organisés. Le code 5 est explicitement un « parcours de formation » dans la DATAMAP et n'est donc plus assimilé à l'autoformation.

Les codes 6 et 98 constituent la catégorie `Autoformation / autre seulement` lorsqu'aucun dispositif organisé n'est déclaré.

Le code 97 constitue `Aucun dispositif`.

Une combinaison « aucune » + une autre modalité est classée `Indéterminé`.

### Dénominateur

Les proportions par modalité Q8 utilisent uniquement les répondants ayant au moins une réponse valide à Q8. Une non-réponse technique à Q8 n'est pas assimilée à « aucune ».

Les modèles Q8 sont restreints au même champ de répondants valides.

## 4. Q9 et Q14 — multiréponse

Les doublons éventuels sont éliminés au niveau répondant × modalité.

Pour Q14, les tableaux par intention conservent leur dénominateur spécifique. La synthèse globale est calculée au niveau répondant : chaque personne est comptée une seule fois par motif.

## 5. Modèles binaires

Deux stratégies sont distinguées.

### Modèles linéaires de probabilité

Pour les comparaisons item par item Q5/Q12/Q13/Q15, le modèle linéaire de probabilité est utilisé lorsque l'objectif est de communiquer un écart ajusté en points de pourcentage.

### Modèles logistiques Q8

Les modèles spécifiques à la présence d'un dispositif Q8 selon la langue sont logistiques. Ils sont désormais restitués en **odds ratios avec IC à 95 %**.

La version antérieure qui multipliait directement un coefficient logit par 100 et le qualifiait d'« approximation en points » a été supprimée.

## 6. Multiplicité

Les familles de tests item par item et les modèles élargis concernés reçoivent une correction de Benjamini-Hochberg.

Les résultats sont interprétés à partir de l'amplitude, de l'IC à 95 % et de la correction FDR, et non du seul seuil p < 0,05.

## 7. Balance des groupes

La balance exposés/non exposés utilise désormais de vraies différences standardisées pondérées :

- variable quantitative : différence de moyennes / écart-type pondéré combiné ;
- modalité catégorielle : SMD binaire à partir des proportions pondérées.

Les repères |SMD| = 0,10 et 0,20 sont descriptifs.

## 8. ACP et classifications

### ACP

Les variables sont centrées et standardisées avec `.weight`. Les axes sont calculés à partir d'une covariance pondérée.

### k-means

Le k-means reste une classification d'individus sans poids de fréquence direct dans l'algorithme. Il utilise cependant les variables standardisées avec la pondération. Les profils sont caractérisés avec des moyennes pondérées.

### CAH

La CAH utilise les mêmes variables standardisées avec la pondération avant la distance de Ward. Le nombre de classes est comparé par silhouette moyenne. La caractérisation finale est pondérée.

Toutes ces analyses restent exploratoires.

## 9. Indice cumulatif

L'indice formation + direction + connaissance + usage n'est calculé que si les quatre composantes sont observées.

Une composante manquante n'est plus assimilée à zéro.

## 10. Publication des figures

La couche graphique ne recalcule aucune statistique.

Les titres et notes de lecture destinés au lecteur sont centralisés dans `R/osyr_style.R`. Les formulations de suivi interne (« figure utile », « vue transversale », etc.) sont retirées des livrables.

La figure de couverture du plan reste un diagnostic de production et n'entre pas dans le rapport principal.

## 11. Limites qui demeurent

- les modèles restent associatifs ;
- la pondération ne corrige pas l'auto-sélection non observée dans les formations ;
- la langue du questionnaire est un contexte, pas une identité ;
- Q3 reste exploratoire tant que le dictionnaire n'a pas été validé manuellement ;
- les comparaisons avec les données des collèges doctoraux nécessitent une source externe.

## 12. Conséquence opérationnelle

Les modifications de Q8, des dénominateurs, de l'ACP et de l'indice cumulatif peuvent modifier des résultats numériques. La prochaine production doit donc être obtenue par un **rerun complet à partir du script 00**.
