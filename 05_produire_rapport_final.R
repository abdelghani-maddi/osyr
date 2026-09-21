# =============================================================================
# SCRIPT 05 — PRODUIRE LE RAPPORT FINAL OSYR
# Version 2026-09-21 v4
# =============================================================================
# Étape de production finale.
#
# Ce script consolide les analyses déjà produites par les scripts 01 et 03,
# relance la couche d'analyses finales si elle est disponible, puis génère un
# rapport Word structuré selon le plan de dépouillement de septembre 2026.
#
# Corrections de compatibilité :
# - pas de style Word "Title" ou "Subtitle" ; uniquement Normal, heading 1,
#   heading 2 et heading 3 ;
# - insertion des tableaux avec flextable::body_add_flextable(), et non
#   officer::body_add_flextable(), car cette fonction est exportée par le
#   package flextable dans les versions utilisées localement.
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
}

pkgs <- c("tidyverse", "officer", "flextable", "fs", "glue", "scales")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Style et production des analyses finales
# -----------------------------------------------------------------------------

if (!file.exists("R/osyr_style.R")) {
  stop("Fichier manquant : R/osyr_style.R")
}
source("R/osyr_style.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "figures"))

# La couche finale ajoute des figures et tableaux directement alignés sur le plan
# de dépouillement. Elle est appelée ici pour éviter un rapport trop pauvre en
# analyses. Si elle a déjà été exécutée, elle régénère proprement le catalogue.
if (file.exists("R/osyr_final_analyses.R")) {
  source("R/osyr_final_analyses.R")
} else {
  warning("R/osyr_final_analyses.R absent : seules les figures déjà disponibles seront utilisées.")
}

# -----------------------------------------------------------------------------
# 2. Plan et catalogue consolidé
# -----------------------------------------------------------------------------

plan_rapport <- osyr_final_plan_registry()
safe_write_csv(plan_rapport, file.path(dirs$report, "tables", "plan_rapport_final.csv"))

catalog_final_path <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")

if (file.exists(catalog_final_path)) {
  figure_catalog <- readr::read_csv(catalog_final_path, show_col_types = FALSE)
} else {
  figure_catalog <- build_figure_catalog()

  figures_available <- figure_catalog |>
    dplyr::filter(available)

  if (nrow(figures_available) > 0) {
    purrr::pwalk(
      figures_available,
      function(section, bloc, titre, file, source_dir, priorite, path, available, ...) {
        src <- resolve_figure_path(file, source_dir)
        dst <- file.path(dirs$report, "figures", file)
        if (!is.na(src) && file.exists(src)) fs::file_copy(src, dst, overwrite = TRUE)
      }
    )
  }

  figure_catalog <- figure_catalog |>
    dplyr::mutate(
      path = file.path(dirs$report, "figures", file),
      caption = paste0("Figure issue des sorties ", source_dir, ".")
    )
}

if (!"caption" %in% names(figure_catalog)) {
  figure_catalog$caption <- paste0("Figure mobilisée pour la section ", figure_catalog$section, ".")
}
if (!"source_dir" %in% names(figure_catalog)) figure_catalog$source_dir <- "rapport_final"
if (!"priorite" %in% names(figure_catalog)) figure_catalog$priorite <- 9

figure_catalog <- figure_catalog |>
  dplyr::mutate(
    section = as.integer(section),
    path = as.character(path),
    caption = as.character(caption),
    available = !is.na(path) & file.exists(path)
  ) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"))

missing_figures <- figure_catalog |>
  dplyr::filter(!available) |>
  dplyr::select(dplyr::any_of(c("section", "bloc", "titre", "file", "source_dir", "priorite")))

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

safe_write_csv(coverage_table, file.path(dirs$report, "tables", "couverture_plan_de_depouillement.csv"))

# -----------------------------------------------------------------------------
# 3. Helpers Word
# -----------------------------------------------------------------------------

add_doc_title <- function(doc, title, subtitle = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle)) doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  doc
}

add_section_table <- function(doc, data) {
  if (!is.data.frame(data) || nrow(data) == 0) return(doc)

  ft <- flextable::flextable(data) |>
    style_flextable_osyr()

  # body_add_flextable est exportée par flextable, pas par officer.
  flextable::body_add_flextable(doc, value = ft)
}

add_figure_if_exists <- function(doc, path, title, caption = NULL, width = 6.4) {
  if (is.na(path) || !file.exists(path)) return(doc)
  doc <- officer::body_add_par(doc, title, style = "heading 3")
  doc <- officer::body_add_img(doc, src = path, width = width, height = width * 0.56)
  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }
  doc
}

add_points_a_rediger <- function(doc, section_name) {
  doc <- officer::body_add_par(doc, "Points à rédiger", style = "heading 2")
  officer::body_add_par(
    doc,
    paste(
      "Rédiger ici les principaux résultats de la section",
      paste0("'", section_name, "'"),
      "en distinguant les constats descriptifs, les associations statistiques et les limites d'interprétation."
    ),
    style = "Normal"
  )
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

doc <- officer::body_add_par(doc, paste0("Date de génération : ", format(Sys.Date(), "%d/%m/%Y")), style = "Normal")
doc <- officer::body_add_par(doc, osyr_method_note(), style = "Normal")

doc <- officer::body_add_par(doc, "Plan du rapport", style = "heading 1")
doc <- add_section_table(doc, plan_rapport |> dplyr::select(section, bloc, objectif))

doc <- officer::body_add_par(doc, "Couverture analytique", style = "heading 1")
doc <- add_section_table(
  doc,
  coverage_table |>
    dplyr::select(dplyr::any_of(c("section", "bloc", "n_figures_disponibles", "statut_couverture")))
)

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
      doc <- add_figure_if_exists(doc, figs$path[i], figs$titre[i], figs$caption[i])
    }
  }

  doc <- add_points_a_rediger(doc, sec_info$bloc)
}

# Annexes.
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
