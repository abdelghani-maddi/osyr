# Revue d'intégrité méthodologique du workflow OSYR

Date : 27 septembre 2026.

Cette note consigne les conventions qui ont été vérifiées ou corrigées dans le code avant la production des livrables. Elle complète le document méthodologique Word et les contrôles automatisés du workflow.

## 1. Pondération

Décision retenue : la base comporte une seule variable de pondération d'enquête, `Poids`, recodée en `.weight`.

- aucune détection automatique d'autres colonnes comme poids ;
- `weight_none = 1` est une constante technique pour les scénarios non pondérés ;
- les poids manquants, nuls ou négatifs sont exclus des analyses pondérées ;
- l'absence de `.weight` provoque désormais une erreur plutôt qu'un remplacement silencieux par 1.

## 2. Dénominateurs et valeurs manquantes

Les batteries Q4, Q5, Q11, Q12, Q13 et Q15 utilisent des indicateurs à trois états lorsque nécessaire : vrai, faux, manquant.

Une non-réponse ou une modalité hors champ n'est pas transformée en `FALSE`. Le diagnostic `response_denominator_diagnostics.csv` indique le nombre de réponses valides utilisé pour chaque famille d'indicateurs.

Q13 conserve explicitement la modalité `97 = Je ne sais pas`. Le score d'intentions et le score d'incertitude sont documentés séparément.

## 3. Questions multiréponses

Q8, Q9 et Q14 sont traitées comme des questions multiréponses.

- les pourcentages sont calculés au niveau du répondant ;
- une même personne n'est comptée qu'une fois pour une modalité donnée ;
- la somme des pourcentages peut dépasser 100 % ;
- les croisements de deux batteries multiréponses, notamment Q3 × Q9, sont dédupliqués avant agrégation.

Pour Q14, la synthèse globale est calculée au niveau du répondant et ne moyenne pas des pourcentages issus de dénominateurs différents.

## 4. Construction de l'exposition Q8

Trois catégories substantives sont distinguées : aucun dispositif, autoformation / autre seulement, dispositif organisé. Une quatrième catégorie `Indéterminé` est utilisée en cas de réponse contradictoire, notamment si `aucune de ces propositions` est sélectionnée avec une autre modalité.

Les indicateurs présentiel, distanciel, asynchrone et autoformation sont conservés séparément pour les analyses du plan de dépouillement.

## 5. Scores synthétiques

Les scores Q4, Q5, Q11, Q12, Q13 et Q15 sont construits au niveau du répondant comme proportions d'items satisfaisant le critère parmi les items valides.

Q15 n'est plus interprété à partir d'un score global d'accord mélangeant des affirmations de sens opposé. Trois dimensions sont utilisées :

- bénéfices scientifiques ;
- contraintes institutionnelles ou économiques ;
- risques individuels.

Les analyses item par item restent disponibles et priment lorsqu'un score synthétique masquerait une hétérogénéité importante.

## 6. Modèles

Les modèles pondérés utilisent `survey::svydesign(ids = ~1, weights = ~.weight, ...)` car aucune information de strate ou de grappe d'échantillonnage n'est disponible dans les fichiers fournis.

Cette spécification traite donc l'échantillon comme un plan pondéré sans grappes déclarées. Si une documentation d'échantillonnage plus détaillée devient disponible, le design devra être révisé.

Les modèles ajustent selon la question étudiée sur l'année de thèse, la discipline, la langue du questionnaire et, dans les modèles élargis, les pratiques de recherche, l'environnement ou les perceptions.

Les résultats sont interprétés comme des associations ajustées et non comme des effets causaux.

## 7. Multiplicité et incertitude

Les intervalles de confiance à 95 % sont conservés dans les sorties de modèles.

Pour les familles de tests multiples, les valeurs p sont complétées par une correction de Benjamini-Hochberg. Les nouveaux modèles du plan de dépouillement produisent également `p_fdr`.

## 8. Balance des groupes

La balance exposés / non exposés est évaluée par différences standardisées pondérées.

- variables quantitatives : différence de moyennes divisée par l'écart-type pondéré combiné ;
- modalités catégorielles : différence standardisée de deux proportions binaires.

Les seuils absolus 0,10 et 0,20 sont utilisés comme repères descriptifs et non comme tests de significativité.

## 9. Discipline et langue

Les dix disciplines détaillées de Q2 restent le niveau descriptif principal. Le regroupement en quatre grands domaines est utilisé pour certaines analyses de sensibilité et ne remplace pas le niveau détaillé.

La langue du questionnaire est une variable de contexte. Elle ne mesure ni nationalité, ni origine, ni niveau d'anglais. Toute interprétation doit tenir compte des établissements, collèges doctoraux et offres de formation.

## 10. Profils et analyses multivariées

L'ACP utilisée dans les profils applique la pondération lors du centrage, de la standardisation et de la construction de la matrice de covariance.

Le K-means est ensuite appliqué aux scores standardisés. Le nombre de classes est choisi parmi 2 à 4 à l'aide de la silhouette moyenne. Cette classification reste descriptive et exploratoire.

La CAH constitue une analyse exploratoire supplémentaire. Elle ne sert pas de base à des tests d'inférence ou à une typologie généralisée à la population.

## 11. Analyse Q3

Les fréquences des mots spontanés peuvent être décrites directement. La catégorisation lexicale repose en revanche sur un dictionnaire exploratoire qui doit être validé manuellement avant toute interprétation forte sur les représentations cognitives.

## 12. Contrôles automatisés

`R/osyr_method_checks.R` est exécuté avant la mise en forme des figures et des livrables.

Il vérifie notamment : unicité de la colonne Poids, présence de `.weight`, bornes des scores, cohérence Q8, cohérence logique Q5/Q12/Q13, intervalles de confiance, valeurs p/FDR et contenu éditorial du catalogue de figures.

Les contrôles critiques interrompent le workflow. Les avertissements sont exportés dans :

`outputs_osyr_rapport_final/diagnostics/integrite_methodologique.csv`.

## 13. Points qui restent à documenter par une source externe

- documentation précise du plan d'échantillonnage si elle existe ;
- justification externe de la notion de « précision à 85 % » mentionnée dans les échanges ;
- statistiques comparables des collèges doctoraux sur les taux de formation ;
- validation humaine finale du dictionnaire Q3.

Ces points ne sont pas imputés ou reconstruits automatiquement dans le workflow.