# Notes méthodologiques

## Pondérations

Le workflow principal utilise `Poids`, recodée en `.weight`. Les compléments testent d’autres variables de pondération lorsqu’elles sont détectées, ainsi qu’une version sans pondération.

## Disciplines

Le script principal construit un regroupement disciplinaire large. Les retours WP2 invitent à tester la sensibilité à un découpage plus détaillé, notamment en 10 postes lorsque la variable le permet.

## Exposition

Trois catégories sont distinguées :

- aucun dispositif ;
- autoformation / autre seulement ;
- dispositif organisé.

Les modèles principaux comparent surtout `Aucun dispositif` et `Dispositif organisé`. Les autoformés font l’objet d’une analyse spécifique.

## Connaissance, usage, pratique et intention

Le workflow distingue :

- connaissances déclarées ;
- usages déclarés ;
- pratiques de recherche déjà réalisées via Q4 ;
- intentions futures via Q13.

Cette distinction répond au point WP2 sur le gap connaissance → mise en action.

## Je ne sais pas / non / non-réponses

Ces modalités sont traitées comme des signaux analytiques : incertitude, difficulté à se projeter, absence d’opportunité ou position négative.

## Langue du questionnaire

La langue est un proxy prudent de contexte/profil international. Elle ne mesure pas la nationalité. Elle doit être croisée avec établissement, collège doctoral et offre réelle de formation.

## Analyse textuelle

Les trois mots associés à la science ouverte sont analysés avec un dictionnaire exploratoire. L’annotation manuelle reste une étape importante pour consolider l’interprétation.

## Causalité

Toutes les analyses sont descriptives/associatives. Les modèles ajustés ne suffisent pas à inférer une causalité.


# Note v7 — distinction entre implémentation et données externes

Le workflow v7 distingue explicitement :

- les remarques directement calculables avec la base OSYR ;
- les remarques calculables seulement si la variable ou le libellé est disponible ;
- les remarques qui nécessitent une information externe.

Exemples d'informations externes :

- la documentation OpinionWay sur la précision à 85 % ;
- l'offre réelle de formations en anglais par collège doctoral ;
- l'annotation manuelle définitive des trois mots.

Ces points sont exportés dans :

```text
outputs_osyr_v2_complements_30062026/todo_external_data/
```
