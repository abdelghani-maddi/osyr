# =============================================================================
# SCRIPT 05 — PRODUIRE LE RAPPORT FINAL OSYR
# Version 2026-09-21 v5
# =============================================================================
# Génère deux documents :
#   1) un rapport principal structuré et rédigé à partir des sorties du workflow ;
#   2) une annexe graphique contenant les figures complémentaires.
#
# Le rapport principal ne reprend plus les éléments de suivi interne
# ("couverture analytique", "points à rédiger", catalogue technique, etc.).
# Les diagnostics restent disponibles dans outputs_osyr_rapport_final/tables/.
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

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

if (!file.exists("R/osyr_report_text.R")) stop("Fichier manquant : R/osyr_report_text.R")
source("R/osyr_report_text.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "figures"))

# -----------------------------------------------------------------------------
# 1. Analyses finales et catalogues
# -----------------------------------------------------------------------------

catalog_final_path <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")

if (!file.exists(catalog_final_path)) {
  if (!file.exists("R/osyr_final_analyses.R")) {
    stop("Fichier manquant : R/osyr_final_analyses.R")
  }
  source("R/osyr_final_analyses.R")
}

plan_rapport <- osyr_final_plan_registry()
safe_write_csv(plan_rapport, file.path(dirs$report, "tables", "plan_rapport_final.csv"))

figure_catalog <- if (file.exists(catalog_final_path)) {
  readr::read_csv(catalog_final_path, show_col_types = FALSE)
} else {
  build_figure_catalog()
}

if (!"caption" %in% names(figure_catalog)) {
  figure_catalog$caption <- "Source : enquête OSYR, données pondérées."
}
if (!"source_dir" %in% names(figure_catalog)) figure_catalog$source_dir <- "rapport_final"
if (!"priorite" %in% names(figure_catalog)) figure_catalog$priorite <- 9

figure_catalog <- figure_catalog |>
  dplyr::mutate(
    section = as.integer(section),
    path = as.character(path),
    caption = dplyr::case_when(
      stringr::str_detect(caption, "Figure issue des sorties") ~ "Source : enquête OSYR, données pondérées.",
      TRUE ~ as.character(caption)
    ),
    available = !is.na(path) & file.exists(path)
  ) |>
  dplyr::filter(available) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"))

# Le corps du rapport privilégie les figures produites spécifiquement pour le
# rapport final, puis les figures de priorité 1. Le reste est placé en annexe.
main_figures <- figure_catalog |>
  dplyr::group_by(section) |>
  dplyr::arrange(
    dplyr::desc(source_dir == "rapport_final"),
    priorite,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 3) |>
  dplyr::ungroup()

appendix_figures <- figure_catalog |>
  dplyr::anti_join(main_figures |> dplyr::select(section, file), by = c("section", "file"))

safe_write_csv(main_figures, file.path(dirs$report, "tables", "figures_rapport_principal.csv"))
safe_write_csv(appendix_figures, file.path(dirs$report, "tables", "figures_annexe.csv"))

# -----------------------------------------------------------------------------
# 2. Helpers Word
# -----------------------------------------------------------------------------

add_section_table <- function(doc, data, max_rows = 10) {
  if (!is.data.frame(data) || nrow(data) == 0) return(doc)

  data <- data |> dplyr::slice_head(n = max_rows)

  ft <- flextable::flextable(data) |>
    style_flextable_osyr()

  flextable::body_add_flextable(doc, value = ft)
}

add_text_paragraphs <- function(doc, paragraphs) {
  if (length(paragraphs) == 0) return(doc)
  for (txt in paragraphs) {
    if (!is.na(txt) && nzchar(txt)) {
      doc <- officer::body_add_par(doc, txt, style = "Normal")
    }
  }
  doc
}

figure_dimensions <- function(title, file) {
  dense <- stringr::str_detect(
    stringr::str_to_lower(paste(title, file)),
    "heatmap|discipline|carte|profils|q5|q8"
  )
  if (dense) c(width = 6.45, height = 4.55) else c(width = 6.25, height = 3.85)
}

add_figure_if_exists <- function(doc, path, title, caption = NULL) {
  if (is.na(path) || !file.exists(path)) return(doc)

  dims <- figure_dimensions(title, basename(path))

  doc <- officer::body_add_par(doc, title, style = "heading 3")
  doc <- officer::body_add_img(
    doc,
    src = path,
    width = unname(dims["width"]),
    height = unname(dims["height"])
  )

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }

  doc
}

add_cover <- function(doc) {
  doc <- officer::body_add_par(doc, "OSYR", style = "heading 1")
  doc <- officer::body_add_par(doc, "Rapport d'analyse de l'enquête auprès des doctorants", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    "Science ouverte, formations, connaissances, pratiques, intentions et perceptions",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    paste0("Version générée le ", format(Sys.Date(), "%d/%m/%Y")),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, " ", style = "Normal")
  doc <- officer::body_add_par(
    doc,
    "Ce document présente les résultats issus du plan de dépouillement final. Les analyses détaillées et les diagnostics méthodologiques sont conservés dans les sorties du workflow et dans l'annexe graphique.",
    style = "Normal"
  )
  officer::body_add_break(doc)
}

# -----------------------------------------------------------------------------
# 3. Rapport principal
# -----------------------------------------------------------------------------

doc <- officer::read_docx()
doc <- add_cover(doc)

# Résumé
summary_lines <- executive_summary_text(plan_rapport)
doc <- officer::body_add_par(doc, "Résumé des principaux résultats", style = "heading 1")
doc <- add_text_paragraphs(doc, summary_lines)

# Méthode
doc <- officer::body_add_par(doc, "Méthode et principes d'analyse", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les résultats reposent sur les données pondérées de l'enquête OSYR.",
    "Les analyses combinent descriptifs, comparaisons entre groupes, analyses par discipline et année de thèse, ainsi que des modèles ajustés et des tests de sensibilité.",
    "Les résultats avec et sans pondération, ainsi que les regroupements disciplinaires détaillés et agrégés, sont comparés lorsque cela est pertinent."
  ),
  style = "Normal"
)
doc <- officer::body_add_par(doc, osyr_method_note(), style = "Normal")

# Sections de résultats
for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, sec_info$bloc, style = "heading 1")
  doc <- officer::body_add_par(doc, report_section_intro(sec, sec_info), style = "Normal")

  # Résultats factuels
  section_text <- section_summary_text(sec)
  if (length(section_text) > 0) {
    doc <- officer::body_add_par(doc, "Principaux résultats", style = "heading 2")
    doc <- add_text_paragraphs(doc, section_text)
  }

  # Tableau synthétique
  section_table <- section_summary_table(sec)
  if (is.data.frame(section_table) && nrow(section_table) > 0) {
    doc <- officer::body_add_par(doc, "Tableau de synthèse", style = "heading 2")
    doc <- add_section_table(doc, section_table, max_rows = 10)
  }

  # Figures principales uniquement
  figs <- main_figures |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(dplyr::desc(source_dir == "rapport_final"), priorite, titre)

  if (nrow(figs) > 0) {
    doc <- officer::body_add_par(doc, "Figures", style = "heading 2")

    for (i in seq_len(nrow(figs))) {
      doc <- add_figure_if_exists(
        doc,
        figs$path[i],
        figs$titre[i],
        figs$caption[i]
      )
    }
  }
}

# Conclusion
doc <- officer::body_add_break(doc)
doc <- officer::body_add_par(doc, "Conclusion", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste(
    "L'enquête met en évidence une familiarisation importante des doctorants avec plusieurs dimensions de la science ouverte, mais également des écarts persistants entre connaissance, usage et mise en pratique.",
    "Les différences associées à l'exposition aux dispositifs de formation doivent être interprétées en tenant compte des disciplines, de l'année de thèse et des différences de composition entre groupes.",
    "Les analyses de robustesse permettent de distinguer les résultats relativement stables de ceux qui demeurent sensibles aux choix de pondération ou de spécification."
  ),
  style = "Normal"
)
doc <- officer::body_add_par(doc, osyr_method_note(), style = "Normal")

out_docx <- file.path(dirs$report, "rapport_final_osyr.docx")
print(doc, target = out_docx)

# -----------------------------------------------------------------------------
# 4. Annexe graphique séparée
# -----------------------------------------------------------------------------

annex <- officer::read_docx()
annex <- officer::body_add_par(annex, "OSYR — Annexe graphique", style = "heading 1")
annex <- officer::body_add_par(
  annex,
  "Figures complémentaires produites par le workflow et non retenues dans le corps principal du rapport.",
  style = "Normal"
)

for (sec in sort(unique(appendix_figures$section))) {
  sec_name <- plan_rapport$bloc[match(sec, plan_rapport$section)][1]
  annex <- officer::body_add_break(annex)
  annex <- officer::body_add_par(annex, sec_name, style = "heading 1")

  figs <- appendix_figures |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(priorite, titre)

  for (i in seq_len(nrow(figs))) {
    annex <- add_figure_if_exists(annex, figs$path[i], figs$titre[i], figs$caption[i])
  }
}

annex_path <- file.path(dirs$report, "annexe_graphique_osyr.docx")
print(annex, target = annex_path)

message("Rapport final généré : ", normalizePath(out_docx, mustWork = FALSE))
message("Annexe graphique générée : ", normalizePath(annex_path, mustWork = FALSE))
message("Figures dans le rapport principal : ", nrow(main_figures))
message("Figures en annexe : ", nrow(appendix_figures))
