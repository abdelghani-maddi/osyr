# =============================================================================
# SCRIPT 99 — DOCUMENTER L'ENVIRONNEMENT LOGICIEL
# Version : 27/09/2026
# =============================================================================
# RÔLE DANS LE WORKFLOW
#   Dernière étape. Ce script ne calcule aucun résultat d'enquête. Il conserve
#   les informations nécessaires pour reproduire ou auditer l'exécution :
#   version de R, plateforme et versions des packages installés.
#
# SORTIES
#   outputs_osyr_v2_session/sessionInfo.txt
#   outputs_osyr_v2_session/installed_packages.csv
#
# POINT DU PLAN
#   Transversal — reproductibilité et traçabilité méthodologique.
# =============================================================================

session_dir <- "outputs_osyr_v2_session"
dir.create(session_dir, showWarnings = FALSE, recursive = TRUE)

# sessionInfo() documente R, la plateforme et les packages attachés pendant la
# session. Le sink est refermé immédiatement après l'écriture.
sink(file.path(session_dir, "sessionInfo.txt"))
print(sessionInfo())
sink()

# La liste complète des packages installés permet de retrouver les versions
# utilisées même si certains packages ont été appelés via leur namespace.
pkgs <- installed.packages()
write.csv(
  as.data.frame(pkgs[, c("Package", "Version", "Built")]),
  file.path(session_dir, "installed_packages.csv"),
  row.names = FALSE
)

message("Session info exportée dans ", session_dir, "/")
