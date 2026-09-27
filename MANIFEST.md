# Manifest du workflow OSYR

Version de travail : 27 septembre 2026.

## Chaîne active

- `00_lancer_workflow_complet.R` — orchestration de la chaîne.
- `01_analyse_osyr_base_et_modeles.R` — base analytique, recodages, batteries, scores et descriptifs.
- `03_analyses_complementaires_wp2_30062026.R` — modèles, robustesse, FDR et balance des covariables.
- `R/osyr_final_analyses.R` — analyses transversales destinées aux livrables.
- `R/osyr_plan_depouillement_analyses.R` — compléments et audit du plan de dépouillement.
- `R/osyr_method_checks.R` — contrôles d'intégrité méthodologique.
- `R/osyr_figure_polish.R` — figures au format de publication.
- `R/osyr_report_text.R` — rédaction analytique à partir des tables.
- `R/osyr_style.R` — charte, chemins et registre du rapport.
- `05_produire_rapport_final.R` — rapport Word et annexe graphique.
- `06_generer_presentation_finale.R` — présentation PowerPoint.
- `99_session_info.R` — traçabilité de l'environnement logiciel.

## Scripts historiques

- `02_generer_rapport_word.R` — ancienne génération Word.
- `04_generer_presentation_powerpoint.R` — ancienne génération PowerPoint.

Ces deux scripts sont conservés pour comparaison et compatibilité mais ne font pas partie de la production finale exécutée par défaut.

## Documentation

- `README.md`
- `GUIDE_WORKFLOW_DETAILLE.md`
- `NOTES_METHODOLOGIQUES.md`
- `docs/CODE_ET_PLAN_DEPOUILLEMENT.md`
- `docs/AUDIT_PLAN_DEPOUILLEMENT_SEPT2026.md`
- `docs/PRODUCTION_FINALE_SEPTEMBRE_2026.md`

## Données attendues

- `data/BJ30232 - BDD V2.csv`
- `data/BJ30232 - DATAMAP V2.xlsx`

Les données brutes ne sont pas versionnées dans le dépôt.
