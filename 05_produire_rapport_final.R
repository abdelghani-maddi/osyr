# =============================================================================
# SCRIPT 05 — PRODUIRE LE RAPPORT FINAL OSYR
# Version 2026-09-21
# =============================================================================
# Ce script correspond à l'étape de production finale.
# Il ne remplace pas les analyses produites par les scripts 01 et 03 : il les
# organise selon le plan de dépouillement de septembre 2026 et prépare un rapport
# Word structuré, documenté et directement relisible.
#
# Entrées attendues :
#   - outputs_osyr_v2_final/
#   - outputs_osyr_v2_complements_30062026/
#
# Sorties :
#   - outputs_osyr_rapport_final/rapport_final_osyr.docx
#   - outputs_osyr_rapport_final/tables/plan_rapport_final.csv
#   - outputs_osyr_rapport_final/tables/catalogue_figures_rapport.csv
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
}

pkgs <- c("tidyverse", "officer", "flextable", "fs", "glue", "scales")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Fonctions de style OSYR
# -----------------------------------------------------------------------------

if (!file.exists("R/osyr_style.R")) {
  stop("Fichier manquant : R/osyr_style.R. Ce fichier centralise le style OSYR et le plan de production finale.")
}
source("R/osyr_style.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "figures"))

# -----------------------------------------------------------------------------
# 2. Plan du rapport et catalogue de figures
# -----------------------------------------------------------------------------

plan_rapport <- osyr_final_plan_registry()
figure_catalog <- build_figure_catalog()

safe_write_csv(plan_rapport, file.path(dirs$report, "tables", "plan_rapport_final.csv"))
safe_write_csv(figure_catalog, file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"))

missing_figures <- figure_catalog |>
  dplyr::filter(!available) |>
  dplyr::select(section, bloc, titre, file, source_dir, priorite)

safe_write_csv(missing_figures, file.path(dirs$report, "tables", "figures_manquantes.csv"))

# Copier les figures disponibles dans le dossier du rapport pour figer l'état de
# production. Le rapport pointe ensuite vers ces copies.
figures_available <- figure_catalog |>
  dplyr::filter(available)

if (nrow(figures_available) > 0) {
  purrr::pwalk(
    figures_available,
    function(section, bloc, titre, file, source_dir, priorite, path, available, ...) {
      fs::file_copy(path, file.path(dirs$report, "figures", file), overwrite = TRUE)
    }
  )
}

# -----------------------------------------------------------------------------
# 3. Helpers Word
# -----------------------------------------------------------------------------

add_osyr_title <- function(doc, title, subtitle = NULL) {
  doc <- officer::body_add_par(doc, title, style = "Title")
  if (!is.null(subtitle)) {
    doc <- officer::body_add_par(doc, subtitle, style = "Subtitle")
  }
  doc
}

add_note <- function(doc, text) {
  officer::body_add_par(doc, text, style = "Normal")
}

add_section_table <- function(doc, data) {
  ft <- flextable::flextable(data) |>
    style_flextable_osyr()
  officer::body_add_flextable(doc, ft)
}

add_figure_if_exists <- function(doc, path, title, caption = NULL, width = 6.4) {
  if (is.na(path) || !file.exists(path)) {
    doc <- officer::body_add_par(doc, paste0("Figure non disponible : ", title), style = "Normal")
    return(doc)
  }
  doc <- officer::body_add_par(doc, title, style = "heading 3")
  doc <- officer::body_add_img(doc, src = path, width = width, height = width * 0.56)
  if (!is.null(caption)) {
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }
  doc
}

# -----------------------------------------------------------------------------
# 4. Rapport Word
# -----------------------------------------------------------------------------

doc <- officer::read_docx()

doc <- add_osyr_title(
  doc,
  osyr_report_title(),
  "Structure de production finale fondée sur le plan de dépouillement de septembre 2026"
)

doc <- add_note(doc, paste0("Date de génération : ", format(Sys.Date(), "%d/%m/%Y")))
doc <- add_note(doc, osyr_method_note())

doc <- officer::body_add_par(doc, "Plan du rapport", style = "heading 1")
plan_table <- plan_rapport |>
  dplyr::select(section, bloc, objectif)
doc <- add_section_table(doc, plan_table)

for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, paste0(sec, ". ", sec_info$bloc), style = "heading 1")
  doc <- officer::body_add_par(doc, sec_info$objectif, style = "Normal")

  doc <- officer::body_add_par(doc, "Questions traitées", style = "heading 2")
  doc <- officer::body_add_par(doc, sec_info$questions_principales, style = "Normal")

  doc <- officer::body_add_par(doc, "Sorties mobilisées", style = "heading 2")
  doc <- officer::body_add_par(doc, sec_info$sorties_attendues, style = "Normal")

  figs <- figure_catalog |>
    dplyr::filter(section == sec, available) |>
    dplyr::arrange(priorite, titre)

  if (nrow(figs) > 0) {
    doc <- officer::body_add_par(doc, "Figures", style = "heading 2")
    for (i in seq_len(nrow(figs))) {
      fig_path <- file.path(dirs$report, "figures", figs$file[i])
      doc <- add_figure_if_exists(
        doc,
        path = fig_path,
        title = figs$titre[i],
        caption = paste0("Source : enquête OSYR, sorties ", figs$source_dir[i], ".")
      )
    }
  }

  doc <- officer::body_add_par(doc, "Éléments d'interprétation à rédiger", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    paste(
      "Cette sous-section doit être rédigée à partir des tableaux et figures produits.",
      "Elle doit distinguer les constats descriptifs, les associations statistiques et les limites d'interprétation."
    ),
    style = "Normal"
  )
}

# Annexes méthodologiques.
doc <- officer::body_add_break(doc)
doc <- officer::body_add_par(doc, "Annexes méthodologiques", style = "heading 1")
doc <- officer::body_add_par(doc, "Catalogue des figures", style = "heading 2")
doc <- add_section_table(
  doc,
  figure_catalog |>
    dplyr::select(section, bloc, titre, file, source_dir, available, priorite)
)

if (nrow(missing_figures) > 0) {
  doc <- officer::body_add_par(doc, "Figures non disponibles au moment de la génération", style = "heading 2")
  doc <- add_section_table(doc, missing_figures)
}

doc <- officer::body_add_par(doc, "Note d'interprétation", style = "heading 2")
doc <- officer::body_add_par(doc, osyr_method_note(), style = "Normal")

out_docx <- file.path(dirs$report, "rapport_final_osyr.docx")
print(doc, target = out_docx)

message("Rapport final généré : ", normalizePath(out_docx, mustWork = FALSE))
message("Catalogue des figures : ", normalizePath(file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"), mustWork = FALSE))
