# =============================================================================
# SCRIPT 99 — DOCUMENTER L'ENVIRONNEMENT R
# =============================================================================
# À lancer après le workflow pour conserver la version de R et des packages.
#   source("99_session_info.R")
# =============================================================================

dir.create("outputs_osyr_v2_session", showWarnings = FALSE, recursive = TRUE)

sink(file.path("outputs_osyr_v2_session", "sessionInfo.txt"))
print(sessionInfo())
sink()

pkgs <- installed.packages()
write.csv(
  as.data.frame(pkgs[, c("Package", "Version", "Built")]),
  file.path("outputs_osyr_v2_session", "installed_packages.csv"),
  row.names = FALSE
)

message("Session info exportée dans outputs_osyr_v2_session/")
