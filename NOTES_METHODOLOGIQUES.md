# Notes méthodologiques

## Pondération

La base OSYR comporte une seule variable de pondération : `Poids`. Le script 01 la convertit en numérique et la renomme `.weight`.

`weight_none` n'est pas un poids d'enquête. Il s'agit d'une constante technique égale à 1, utilisée uniquement pour reproduire les mêmes modèles sans pondération et comparer les résultats pondérés et non pondérés.

Le workflow ne recherche plus automatiquement d'autres variables numériques dont le nom pourrait évoquer un poids. Cette détection était susceptible de confondre des identifiants ou des variables techniques avec une pondération.

Dans les analyses pondérées, les observations dont `Poids/.weight` est manquant, nul ou négatif sont exclues du design d'enquête. Elles ne sont plus remplacées par 1.

## Disciplines

Deux niveaux sont conservés.

Le niveau détaillé reprend les dix domaines de Q2 indiqués dans la DATAMAP V2 :

1. Mathématiques ;
2. Physique ;
3. Sciences de la terre et de l'univers, espace ;
4. Chimie ;
5. Biologie, médecine et santé ;
6. Sciences humaines et humanités ;
7. Sciences de la société ;
8. Sciences pour l'ingénieur ;
9. Sciences et technologies de l'information et de la communication ;
10. Sciences agronomiques et écologiques.

Un niveau agrégé en quatre grands domaines est utilisé dans certaines analyses de robustesse. Il ne remplace pas le niveau détaillé.

## Exposition aux dispositifs

Q8 distingue huit modalités : quatre dispositifs organisés, deux modalités d'autoformation ou parcours asynchrone, une modalité « autres » et « aucune de ces propositions ».

Trois catégories analytiques sont construites :

- aucun dispositif ;
- autoformation / autre seulement ;
- dispositif organisé.

Les modèles centraux comparent `Aucun dispositif` et `Dispositif organisé`. Les autoformés sont décrits séparément.

## Connaissance, usage, pratique et intention

Le workflow distingue :

- les pratiques de recherche déjà réalisées (Q4) ;
- la connaissance déclarée et l'usage déclaré des outils et pratiques de science ouverte (Q5) ;
- les intentions (Q13) ;
- les perceptions de l'environnement (Q12) ;
- les représentations de la science ouverte (Q15).

La distinction entre connaissance et usage est maintenue dans toutes les analyses de Q5.

## « Je ne sais pas », « non » et non-réponses

Les codes prévus par le questionnaire sont traités selon leur sens propre.

Pour Q13, `97 = Je ne sais pas` est analysé comme une modalité distincte de `2 = Non`. Une non-réponse technique reste une valeur manquante et n'est pas assimilée à « je ne sais pas ».

Pour Q12, `97 = Je ne sais pas` n'entre ni dans le score d'incitation ni dans le score de frein.

## Langue du questionnaire

La langue du questionnaire est une variable de contexte. Elle ne mesure ni la nationalité ni l'origine des répondants. Les analyses qui l'utilisent doivent être interprétées en tenant compte de l'établissement, du collège doctoral et de l'offre de formation disponible.

## Analyse textuelle

L'analyse des mots associés à la science ouverte repose sur un dictionnaire exploratoire et doit être complétée par une annotation manuelle avant toute interprétation substantielle.

## Causalité

Les analyses sont descriptives et associatives. Les modèles ajustés ne permettent pas d'attribuer causalement les différences observées aux dispositifs de formation.

## Éléments nécessitant une source externe

Certains points ne peuvent pas être établis à partir de la base seule, notamment :

- la documentation OpinionWay relative à la précision à 85 % ;
- l'offre effective de formations en anglais par collège doctoral ;
- la validation définitive de l'annotation des mots associés à la science ouverte.


## Dénominateurs et réponses manquantes

Les indicateurs binaires des batteries Q4, Q5, Q11, Q12, Q13 et Q15 distinguent explicitement réponse positive, réponse négative et valeur manquante ou hors champ. Une non-réponse n'est pas transformée en réponse négative.

Un diagnostic des dénominateurs est exporté dans `outputs_osyr_v2_final/diagnostics/response_denominator_diagnostics.csv`.

## Cohérence de Q8

La modalité « aucune de ces propositions » est incompatible avec la sélection simultanée d'une autre modalité Q8. Lorsqu'une telle combinaison est observée, le répondant est classé `Indéterminé` pour la variable synthétique d'exposition. Le nombre de cas concernés est exporté dans `q8_exposure_consistency.csv`.

## Analyses multivariées exploratoires

L'ACP utilisée pour les profils applique la pondération d'enquête lors du centrage, de la standardisation et de la construction de la matrice de covariance. Le K-means est ensuite appliqué aux scores standardisés et reste exploratoire. Le nombre de classes est choisi parmi 2 à 4 à partir de la silhouette moyenne.

La CAH constitue une analyse exploratoire distincte. Elle ne sert pas de base à une inférence sur la population.

## Contrôles d'intégrité

Avant la production des figures et des livrables, `R/osyr_method_checks.R` vérifie la pondération, les bornes des scores, plusieurs relations logiques entre variables, la cohérence numérique des modèles et certains éléments du catalogue éditorial. Les erreurs critiques interrompent le workflow.
