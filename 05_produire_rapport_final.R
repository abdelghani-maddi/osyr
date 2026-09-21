# =============================================================================
# SCRIPT 05 — PRODUIRE LE RAPPORT FINAL OSYR
# Version 2026-09-21 v2
# =============================================================================
# Étape de production finale.
#
# Ce script :
#   1. relance une couche d'analyses finales alignées sur le plan de
#      dépouillement de septembre 2026 ;
#   2. consolide les figures existantes et les nouvelles figures finales ;
#   3. génère un rapport Word structuré, sans dépendre de styles Word qui peuvent
#      varier selon les machines.
#
# Sorties :
#   - outputs_osyr_rapport_final/rapport_final_osyr.docx
#   - outputs_osyr_rapport_final/tables/catalogue_figures_finales.csv
#   - outputs_osyr_rapport_final/tables/couverture_plan_de_depouillement.csv
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
# 1. Style OSYR et analyses finales
# -----------------------------------------------------------------------------

if (!file.exists("R/osyr_style.R")) {
  stop("Fichier manquant : R/osyr_style.R. Ce fichier centralise le style OSYR et le plan de production finale.")
}
source("R/osyr_style.R")

# La couche d'analyses finales produit davantage de figures et de tableaux que
# le premier squelette de rapport. Elle est volontairement appelée ici pour que
# le rapport final ne soit pas limité au catalogue initial.
if (file.exists("R/osyr_final_analyses.R")) {
  source("R/osyr_final_analyses.R")
} else {
  warning("Fichier R/osyr_final_analyses.R absent : le rapport utilisera uniquement les figures déjà disponibles.")
}

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "figures"))

# -----------------------------------------------------------------------------
# 2. Plan du rapport et catalogue consolidé des figures
# -----------------------------------------------------------------------------

plan_rapport <- osyr_final_plan_registry()
safe_write_csv(plan_rapport, file.path(dirs$report, "tables", "plan_rapport_final.csv"))

catalog_final_path <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")

if (file.exists(catalog_final_path)) {
  figure_catalog <- readr::read_csv(catalog_final_path, show_col_types = FALSE)
} else {
  figure_catalog <- build_figure_catalog()
  figure_catalog <- figure_catalog |>
    dplyr::mutate(
      caption = paste0("Figure issue des sorties ", source_dir, "."),
      path = dplyr::if_else(
        available,
        file.path(dirs$report, "figures", file),
        path
      )
    )

  figures_available <- figure_catalog |>
    dplyr::filter(available)

  if (nrow(figures_available) > 0) {
    purrr::pwalk(
      figures_available,
      function(section, bloc, titre, file, source_dir, priorite, path, available, caption = NULL, ...) {
        src <- resolve_figure_path(file, source_dir)
        if (!is.na(src) && file.exists(src)) {
          fs::file_copy(src, file.path(dirs$report, "figures", file), overwrite = TRUE)
        }
      }
    )
  }
}

figure_catalog <- figure_catalog |>
  dplyr::mutate(
    section = as.integer(section),
    available = !is.na(path) & file.exists(path),
    caption = dplyr::if_else(
      "caption" %in% names(figure_catalog),
      as.character(caption),
      paste0("Figure mobilisée pour la section ", section, ".")
    )
  ) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"))

missing_figures <- figure_catalog |>
  dplyr::filter(!available) |>
  dplyr::select(section, bloc, titre, file, source_dir, priorite)

safe_write_csv(missing_figures, file.path(dirs$report, "tables", "figures_manquantes.csv"))

coverage_path <- file.path(dirs$report, "tables", "couverture_plan_de_depouillement.csv")
coverage_table <- if (file.exists(coverage_path)) {
  readr::read_csv(coverage_path, show_col_types = FALSE)
} else {
  plan_rapport |>
    dplyr::left_join(
      figure_catalog |>
        dplyr::filter(available) |>
        dplyr::count(section, name = "n_figures_disponibles"),
      by = "section"
    ) |>
    dplyr::mutate(n_figures_disponibles = tidyr::replace_na(n_figures_disponibles, 0L))
}

# -----------------------------------------------------------------------------
# 3. Helpers Word robustes
# -----------------------------------------------------------------------------

# Le template Word fourni ne contient pas nécessairement les styles anglais
# "Title" et "Subtitle". On utilise donc uniquement les styles disponibles dans
# le modèle standard/officer : Normal, heading 1, heading 2, heading 3.

add_doc_title <- function(doc, title, subtitle = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle)) doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  doc
}

add_note <- function(doc, text) {
  officer::body_add_par(doc, text, style = "Normal")
}

add_section_table <- function(doc, data) {
  if (!is.data.frame(data) || nrow(data) == 0) return(doc)
  ft <- flextable::flextable(data) |>
    style_flextable_osyr()
  officer::body_add_flextable(doc, ft)
}

add_figure_if_exists <- function(doc, path, title, caption = NULL, width = 6.4) {
  if (is.na(path) || !file.exists(path)) {
    return(doc)
  }

  doc <- officer::body_add_par(doc, title, style = "heading 3")
  doc <- officer::body_add_img(doc, src = path, width = width, height = width * 0.56)

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }

  doc
}

add_interpretation_placeholder <- function(doc, section_name) {
  doc <- officer::body_add_par(doc, "Points à rédiger", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    paste(
      "Rédiger ici les principaux résultats de la section",
      paste0("'", section_name, "'"),
      "en distinguant les constats descriptifs, les associations statistiques et les limites d'interprétation."
    ),
    style = "Normal"
  )
  doc
}

# -----------------------------------------------------------------------------
# 4. Rapport Word
# -----------------------------------------------------------------------------

doc <- officer::read_docx()

doc <- add_doc_title(
  doc,
  osyr_report_title(),
  "Rapport structuré selon le plan de dépouillement de septembre 2026"
)

doc <- add_note(doc, paste0("Date de génération : ", format(Sys.Date(), "%d/%m/%Y")))
doc <- add_note(doc, osyr_method_note())

doc <- officer::body_add_par(doc, "Plan du rapport", style = "heading 1")
plan_table <- plan_rapport |>
  dplyr::select(section, bloc, objectif)
doc <- add_section_table(doc, plan_table)

doc <- officer::body_add_par(doc, "Couverture analytique", style = "heading 1")
coverage_short <- coverage_table |>
  dplyr::select(dplyr::any_of(c("section", "bloc", "n_figures_disponibles", "statut_couverture")))
doc <- add_section_table(doc, coverage_short)

for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, paste0(sec, ". ", sec_info$bloc), style = "heading 1")
  doc <- officer::body_add_par(doc, sec_info$objectif, style = "Normal")

  doc <- officer::body_add_par(doc, "Questions traitées", style = "heading 2")
  doc <- officer::body_add_par(doc, sec_info$questions_principales, style = "Normal")

  doc <- officer::body_add_par(doc, "Sorties produites", style = "heading 2")
  doc <- officer::body_add_par(doc, sec_info$sorties_attendues, style = "Normal")

  figs <- figure_catalog |>
    dplyr::filter(section == sec, available) |>
    dplyr::arrange(priorite, titre)

  if (nrow(figs) > 0) {
    doc <- officer::body_add_par(doc, "Figures", style = "heading 2")
    for (i in seq_len(nrow(figs))) {
      doc <- add_figure_if_exists(
        doc,
        path = figs$path[i],
        title = figs$titre[i],
        caption = figs$caption[i]
      )
    }
  }

  doc <- add_interpretation_placeholder(doc, sec_info$bloc)
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
message("Nombre de figures disponibles : ", sum(figure_catalog$available, na.rm = TRUE))
