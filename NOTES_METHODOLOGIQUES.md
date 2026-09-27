# Notes méthodologiques OSYR

Version du 27 septembre 2026.

## 1. Source des modalités

La DATAMAP V2 constitue la référence pour les libellés des questions, les codes de réponse et les modalités. Le code privilégie ces libellés plutôt que des listes reconstruites manuellement.

## 2. Pondération

La base comporte une seule variable de pondération : `Poids`, recodée en `.weight`.

Les observations dont `.weight` est manquant, nul ou négatif sont exclues des analyses pondérées. Elles ne reçoivent pas un poids de remplacement.

`weight_none = 1` est une constante technique utilisée uniquement pour comparer les résultats pondérés et non pondérés.

## 3. Valeurs manquantes et dénominateurs

Les indicateurs binaires distinguent trois situations :

- réponse correspondant à l'indicateur ;
- réponse valide ne correspondant pas à l'indicateur ;
- non-réponse ou code hors champ, codé `NA`.

Une valeur manquante n'est donc pas traitée comme une réponse négative.

Le fichier `response_denominator_diagnostics.csv` permet de vérifier, pour chaque batterie, le nombre de réponses valides effectivement utilisé.

## 4. Questions multiréponses

Q8, Q9 et Q14 autorisent plusieurs réponses.

Les pourcentages expriment une part de répondants ayant cité chaque modalité, avec un dénominateur explicite. Ils peuvent donc dépasser 100 % lorsqu'ils sont additionnés.

Les croisements entre deux questions multiréponses sont dédupliqués au niveau du répondant avant agrégation afin d'éviter la multiplication artificielle des lignes.

## 5. Exposition aux dispositifs — Q8

Les huit modalités de Q8 sont conservées pour les descriptifs.

Trois catégories analytiques sont également construites :

- `Aucun dispositif` ;
- `Autoformation / autre seulement` ;
- `Dispositif organisé`.

Les modalités 1 à 4 sont classées comme dispositifs organisés. Le parcours asynchrone/MOOC (5), l'autoformation (6) et « autres » (98) sont distingués des dispositifs organisés dans ce regroupement analytique.

Si « aucune de ces propositions » est sélectionnée avec une autre modalité, le profil d'exposition est classé `Indéterminé` et le cas est comptabilisé dans un diagnostic de cohérence.

Les modèles centraux opposent `Aucun dispositif` et `Dispositif organisé`. Le groupe autoformé est décrit séparément.

## 6. Disciplines

Les dix domaines de Q2 sont conservés dans `discipline_detail`.

Un regroupement plus large en quatre domaines est utilisé uniquement pour certains tests de sensibilité. Les résultats détaillés ne sont pas remplacés par ce regroupement.

## 7. Q4 — pratiques de recherche

Chaque item est codé positif pour `Oui = 1`, négatif pour `Non = 2` et manquant dans les autres cas.

Le score Q4 est la proportion d'items positifs parmi les items valides du répondant.

## 8. Q5 — connaissance et usage

Les quatre modalités Q5 sont exploitées de la manière suivante :

- bonne connaissance : codes 3 ou 4 ;
- usage : code 4 ;
- autres codes valides : négatifs pour l'indicateur concerné.

Connaissance et usage sont toujours analysés séparément.

Le gap connaissance-usage est calculé en points de pourcentage : part déclarant bien connaître moins part déclarant avoir déjà utilisé.

Des familles analytiques sont construites pour certains croisements, mais les analyses item par item restent la référence.

## 9. Q10 et Q11 — intensité et évaluation des formations

Q10 distingue une action, deux ou trois actions, quatre ou plus et « je ne sais pas ».

Le score Q11 est la proportion d'items pour lesquels le répondant est plutôt ou tout à fait d'accord avec l'amélioration déclarée de la compréhension, de la capacité à mettre en œuvre des pratiques et de la motivation.

Q11 est une évaluation déclarée de la formation ; il ne mesure pas directement un changement observé de pratiques.

## 10. Q12 — environnement

Les codes 4-5 définissent une incitation et les codes 1-2 un frein. Le code 3 est neutre. Le code 97 « je ne sais pas » est exclu des scores d'incitation et de frein.

Deux scores distincts sont conservés : incitation et frein.

## 11. Q13 — intentions

Q13 conserve trois modalités analytiques :

- oui = 1 ;
- non = 2 ;
- je ne sais pas = 97.

Le score d'intentions est la proportion de réponses « oui » parmi les réponses Q13 valides. Un score distinct mesure la proportion de « je ne sais pas ».

## 12. Q14 — raisons de non-adoption

Q14 est multiréponse et conditionnelle à chaque intention.

Les analyses par intention conservent leur dénominateur propre. Pour la synthèse globale des motifs, chaque répondant est compté une seule fois par raison, afin d'éviter de moyenner des pourcentages calculés sur des populations différentes.

## 13. Q15 — perceptions

L'accord correspond aux codes 4-5, le désaccord aux codes 1-2 et le code 3 est neutre.

Trois dimensions analytiques sont distinguées :

- bénéfices scientifiques : intégrité, coopération, reproductibilité ;
- contraintes institutionnelles ou économiques : coûts/inégalités et reconnaissance dans l'évaluation ;
- risques individuels : liberté académique et carrière.

Le score global d'accord peut être conservé pour certains descriptifs transversaux, mais il n'est pas utilisé seul pour interpréter des affirmations de sens opposé.

## 14. Modèles ajustés

Les modèles centraux contrôlent au minimum l'année de thèse, la discipline et la langue du questionnaire lorsque ces variables sont disponibles et suffisamment variables.

Les scores compris entre 0 et 1 sont analysés avec des régressions linéaires pondérées.

Pour les indicateurs binaires item par item, des modèles linéaires de probabilité sont utilisés lorsque l'objectif est de communiquer un écart ajusté en points de pourcentage.

Les modèles de présence d'une modalité Q8 selon la langue du questionnaire sont logistiques et leurs coefficients sont restitués sous forme d'odds ratios. Les coefficients logistiques ne sont pas assimilés à des points de pourcentage.

Les observations incomplètes sur une variable incluse dans un modèle sont exclues de ce modèle. `n_model` est obtenu avec `nobs()`.

## 15. Comparaisons multiples

Les analyses item par item et les nouveaux modèles comportant de nombreux coefficients sont complétés par une correction de Benjamini-Hochberg.

Les intervalles de confiance à 95 % et les résultats FDR sont privilégiés par rapport à une lecture binaire des seules valeurs p brutes.

## 16. Balance exposés / non exposés

La composition des groupes est comparée avec des différences standardisées pondérées.

Pour une variable quantitative, la différence de moyennes est divisée par l'écart-type pondéré combiné des deux groupes. Pour une modalité catégorielle, une SMD binaire est calculée à partir des deux proportions pondérées.

Les seuils 0,10 et 0,20 sont utilisés comme repères descriptifs. Ils ne constituent pas des tests d'hypothèse.

## 17. ACP, k-means et CAH

Les analyses de profils sont exploratoires.

Pour l'ACP, les variables sont centrées et standardisées avec la pondération d'enquête, et la covariance pondérée est utilisée pour calculer les axes.

Le k-means reste un regroupement d'individus : chaque répondant constitue une observation, mais les variables utilisées dans la distance ont été standardisées avec les poids.

Pour la CAH, la même standardisation pondérée est appliquée avant le calcul des distances de Ward. Le nombre de classes est comparé par silhouette moyenne. Les classes sont ensuite caractérisées avec des statistiques pondérées.

Ces méthodes ne définissent pas une typologie stable hors de l'échantillon.

## 18. Indice cumulatif

L'indice cumulatif combine quatre conditions observées : volume de formation élevé, direction de thèse incitative, connaissance Q5 élevée et usage Q5 élevé.

Il n'est calculé que lorsque les quatre composantes sont observées. Une composante manquante n'est pas remplacée par zéro.

Cet indice est descriptif et ne mesure pas un effet causal cumulatif.

## 19. Langue du questionnaire

La langue est une variable de contexte et ne doit pas être interprétée comme une mesure de nationalité ou d'origine.

Les écarts selon la langue peuvent refléter des différences d'établissement, de discipline, de public ou d'offre de formation. Les modèles Q8 ajustent sur l'année et la discipline mais ne permettent pas d'établir la cause de l'écart.

## 20. Analyse textuelle Q3

Le dictionnaire est exploratoire et transparent. Les mentions non classées sont conservées.

Toute interprétation substantielle des catégories lexicales doit être précédée d'une validation humaine du codage.

## 21. Portée causale

Toutes les analyses sont descriptives ou associatives. Ni la pondération, ni les ajustements, ni les tests de sensibilité ne permettent d'identifier un effet causal de la formation.

Les résultats peuvent notamment être influencés par l'auto-sélection dans les formations, l'avancement doctoral, la discipline, l'environnement institutionnel et des caractéristiques non observées.

## 22. Éléments externes

Les points suivants nécessitent des informations qui ne figurent pas dans la base :

- comparaison avec des taux de formation produits par les collèges doctoraux ;
- documentation externe sur une éventuelle « précision à 85 % » ;
- offre réelle de formations en anglais par établissement ou collège doctoral ;
- validation manuelle définitive du dictionnaire des mots spontanés.
