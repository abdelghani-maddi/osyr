# =============================================================================
# SCRIPT 00 — LANCER LE WORKFLOW COMPLET OSYR
# Version production finale — 21/09/2026
# =============================================================================
# Ce script est le point d'entrée recommandé.
# Il exécute les scripts dans le bon ordre, depuis la construction de la base
# analytique jusqu'à la production du rapport final et de la présentation finale.
#
# Utilisation :
#   1) Ouvrir RStudio à la racine de ce dossier.
#   2) Créer un dossier data/.
#   3) Y placer :
#        data/BJ30232 - BDD V2.csv
#        data/BJ30232 - DATAMAP V2.xlsx
#   4) Lancer :
#        source("00_lancer_workflow_complet.R")
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE,
  survey.lonely.psu = "adjust"
)

# -----------------------------------------------------------------------------
# Options : mettre FALSE pour ne pas relancer une étape déjà produite.
# -----------------------------------------------------------------------------
RUN_01_ANALYSE_PRINCIPALE       <- TRUE
RUN_02_RAPPORT_WORD_EXISTANT    <- FALSE
RUN_03_ANALYSES_COMPLEMENTAIRES <- TRUE
RUN_04_POWERPOINT_EXISTANT      <- FALSE
RUN_05_RAPPORT_FINAL            <- TRUE
RUN_06_PRESENTATION_FINALE      <- TRUE
RUN_99_SESSION_INFO             <- TRUE

# -----------------------------------------------------------------------------
# Vérification des données attendues.
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
# Exécution séquentielle.
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
message(" - outputs_osyr_presentation_finale/")
message(" - outputs_osyr_v2_session/")
