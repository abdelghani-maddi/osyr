# =============================================================================
# SCRIPT 00 — ORCHESTRER LE WORKFLOW OSYR
# Version : 27/09/2026
# =============================================================================
# POINT D'ENTRÉE
#   source("00_lancer_workflow_complet.R")
#
# ORDRE DES ÉTAPES
#   1. 01_analyse_osyr_base_et_modeles.R
#      Socle analytique : nettoyage, variables, batteries longues, scores,
#      descriptifs et diagnostics de base.
#   2. 03_analyses_complementaires_wp2_30062026.R
#      Modèles ajustés, tests item par item, FDR, balance et robustesse.
#   3. R/osyr_final_analyses.R
#      Synthèses directement utiles aux livrables.
#   4. R/osyr_plan_depouillement_analyses.R
#      Vérification point par point du plan de septembre 2026 et analyses
#      complémentaires encore nécessaires.
#   5. R/osyr_figure_polish.R
#      Régénération des figures destinées au lecteur, sans recalcul statistique.
#   6. 05_produire_rapport_final.R / 06_generer_presentation_finale.R
#      Publication Word et PowerPoint à partir des mêmes résultats.
#   7. 99_session_info.R
#      Trace de l'environnement R et des versions de packages.
#
# Les scripts 02 et 04 sont historiques et désactivés par défaut.
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE,
  survey.lonely.psu = "adjust"
)

# -----------------------------------------------------------------------------
# Options d'exécution
# -----------------------------------------------------------------------------
# TRUE exécute l'étape ; FALSE la saute. Pour une production finale cohérente
# après modification d'une variable, d'un dénominateur ou d'une méthode, laisser
# toutes les étapes actives à l'exception des scripts historiques 02 et 04.
# -----------------------------------------------------------------------------

RUN_01_ANALYSE_PRINCIPALE       <- TRUE
RUN_02_RAPPORT_WORD_EXISTANT    <- FALSE
RUN_03_ANALYSES_COMPLEMENTAIRES <- TRUE
RUN_04_POWERPOINT_EXISTANT      <- FALSE
RUN_FINAL_ANALYSES              <- TRUE
RUN_PLAN_DEPOUILLEMENT           <- TRUE
RUN_FIGURE_POLISH               <- TRUE
RUN_05_RAPPORT_FINAL            <- TRUE
RUN_06_PRESENTATION_FINALE      <- TRUE
RUN_99_SESSION_INFO             <- TRUE

# -----------------------------------------------------------------------------
# Vérification des fichiers d'entrée
# -----------------------------------------------------------------------------
# La base et la DATAMAP sont nécessaires au script 01. La DATAMAP fournit les
# modalités et codes de référence utilisés pour construire les indicateurs.
# -----------------------------------------------------------------------------

required_data <- c(
  file.path("data", "BJ30232 - BDD V2.csv"),
  file.path("data", "BJ30232 - DATAMAP V2.xlsx")
)

if (RUN_01_ANALYSE_PRINCIPALE) {
  missing_data <- required_data[!file.exists(required_data)]
  if (length(missing_data) > 0) {
    stop(
      "Fichiers de données manquants :\n",
      paste0(" - ", missing_data, collapse = "\n"),
      "\n\nCréez un dossier data/ à la racine et placez-y les fichiers attendus."
    )
  }
}

run_script <- function(path) {
  if (!file.exists(path)) stop("Script introuvable : ", path)
  message("\n============================================================")
  message("Lancement : ", path)
  message("============================================================\n")
  source(path, local = FALSE)
}

# -----------------------------------------------------------------------------
# Exécution séquentielle
# -----------------------------------------------------------------------------
# L'ordre est contraint : chaque couche utilise les sorties de la précédente.
# Le rapport et le diaporama doivent toujours être générés après le polissage
# graphique, lui-même exécuté après les analyses du plan de dépouillement.
# -----------------------------------------------------------------------------

if (RUN_01_ANALYSE_PRINCIPALE) {
  run_script("01_analyse_osyr_base_et_modeles.R")
}

if (RUN_02_RAPPORT_WORD_EXISTANT) {
  run_script("02_generer_rapport_word.R")
}

if (RUN_03_ANALYSES_COMPLEMENTAIRES) {
  run_script("03_analyses_complementaires_wp2_30062026.R")
}

if (RUN_04_POWERPOINT_EXISTANT) {
  run_script("04_generer_presentation_powerpoint.R")
}

if (RUN_FINAL_ANALYSES) {
  run_script(file.path("R", "osyr_final_analyses.R"))
}

if (RUN_PLAN_DEPOUILLEMENT) {
  run_script(file.path("R", "osyr_plan_depouillement_analyses.R"))
}

# Étape de publication graphique : aucune estimation n'est recalculée ici.
# Les tables déjà calculées sont converties en figures lisibles en A4 et en slide.
if (RUN_FIGURE_POLISH) {
  run_script(file.path("R", "osyr_figure_polish.R"))
}

if (RUN_05_RAPPORT_FINAL) {
  run_script("05_produire_rapport_final.R")
}

if (RUN_06_PRESENTATION_FINALE) {
  run_script("06_generer_presentation_finale.R")
}

if (RUN_99_SESSION_INFO && file.exists("99_session_info.R")) {
  run_script("99_session_info.R")
}

message("\nWorkflow terminé.")
message("Sorties attendues :")
message(" - outputs_osyr_v2_final/")
message(" - outputs_osyr_v2_complements_30062026/")
message(" - outputs_osyr_rapport_final/")
message("   - rapport_final_osyr.docx")
message("   - annexe_graphique_osyr.docx")
message(" - outputs_osyr_presentation_finale/")
message("   - presentation_finale_osyr.pptx")
message(" - outputs_osyr_v2_session/")
message("")
message("Contrôles utiles :")
message(" - outputs_osyr_v2_final/diagnostics/response_denominator_diagnostics.csv")
message(" - outputs_osyr_v2_final/diagnostics/q8_exposure_consistency.csv")
message(" - outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv")
message(" - outputs_osyr_v2_complements_30062026/methodology/covariate_balance_exposed_nonexposed.csv")
