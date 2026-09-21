# =============================================================================
# Fonctions de style et de cadrage pour la phase de production finale OSYR
# =============================================================================
# Ce fichier centralise les couleurs, les libellés, les chemins et les fonctions
# utilisées par les scripts de rapport final et de présentation finale.
#
# Les couleurs reprennent le gabarit OSYR :
# - marron : #998A5B
# - vert principal : #7FB680
# - beige : #FEFAD4
# =============================================================================

osyr_colors <- function() {
  c(
    brown = "#998A5B",
    green = "#7FB680",
    beige = "#FEFAD4",
    dark_green = "#2F4A35",
    pale_green = "#EAF5EC",
    grey = "#667085",
    light_grey = "#F2F4F7",
    white = "#FFFFFF",
    black = "#1F2933"
  )
}

osyr_dirs <- function() {
  list(
    final = "outputs_osyr_v2_final",
    complements = "outputs_osyr_v2_complements_30062026",
    report = "outputs_osyr_rapport_final",
    ppt = "outputs_osyr_presentation_finale"
  )
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

safe_write_csv <- function(x, path) {
  ensure_dir(dirname(path))
  readr::write_csv(x, path)
  invisible(path)
}

existing_path <- function(...) {
  paths <- c(...)
  paths[file.exists(paths)][1]
}

has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0 && ncol(x) > 0
}

osyr_theme <- function(base_size = 11) {
  cols <- osyr_colors()
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", color = cols[["dark_green"]], size = base_size + 5),
      plot.subtitle = ggplot2::element_text(color = cols[["grey"]], size = base_size + 1),
      plot.caption = ggplot2::element_text(color = cols[["grey"]], size = base_size - 2, hjust = 0),
      axis.title = ggplot2::element_text(color = cols[["black"]]),
      axis.text = ggplot2::element_text(color = cols[["black"]]),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E5E7EB", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = cols[["pale_green"]], color = NA),
      strip.text = ggplot2::element_text(face = "bold", color = cols[["dark_green"]])
    )
}

style_flextable_osyr <- function(ft) {
  cols <- osyr_colors()
  ft |>
    flextable::theme_vanilla() |>
    flextable::bg(part = "header", bg = cols[["green"]]) |>
    flextable::color(part = "header", color = "white") |>
    flextable::bold(part = "header") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}

# -----------------------------------------------------------------------------
# Trame du rapport final
# -----------------------------------------------------------------------------

osyr_final_plan_registry <- function() {
  tibble::tribble(
    ~section, ~bloc, ~objectif, ~questions_principales, ~sorties_attendues,
    1, "Parcours de formation",
    "Décrire l'exposition aux formations et dispositifs de science ouverte.",
    "Part des formés et non formés ; types de dispositifs Q8 ; organisateurs Q9 ; nombre de formations Q10 ; évaluation Q11 ; focus non formés, distanciel et autoformation.",
    "Tables descriptives, graphiques Q8/Q9/Q10/Q11, croisements par année et discipline.",
    2, "Connaissances",
    "Mesurer le niveau et le type de connaissances déclarées.",
    "Connaissance et usage Q5 ; différences selon formation, volume, type de dispositif, année, discipline et environnement ; représentations spontanées Q1.",
    "Heatmaps Q5, gap connaissance-usage, analyses des trois mots, croisements avec formation et discipline.",
    3, "Pratiques",
    "Comparer les pratiques de recherche et les usages effectifs de la science ouverte.",
    "Pratiques Q4 ; écart entre connaissances et usages Q5 ; facteurs associés aux pratiques ; liens avec année, discipline, environnement et formation.",
    "Descriptifs Q4/Q5, modèles ajustés, graphiques par discipline et année.",
    4, "Intentions et attitudes",
    "Étudier les intentions déclarées, les hésitations et les profils d'adhésion ou de distance.",
    "Intentions Q13 ; raisons de non-adoption Q14 ; cohérence Q13/Q15 ; place des non formés et des 'je ne sais pas'.",
    "Scores, profils, tableaux de réponses, modèles associatifs, analyses des discordances.",
    5, "Perceptions",
    "Documenter les représentations positives, négatives et ambivalentes de la science ouverte.",
    "Perceptions Q15 ; environnement Q12 ; lien avec pratiques Q5, formation, discipline, année et mots initiaux.",
    "Tableaux Q12/Q15, figures d'accord/désaccord, modèles et croisements.",
    6, "Profils et analyses transversales",
    "Identifier des configurations de répondants et des résultats utiles pour l'interprétation finale.",
    "Profils de doctorants ; autoformés ; non formés ; cumul formation + environnement + pratiques ; analyses factorielles ou classification si les effectifs le permettent.",
    "Profils, CAH éventuelle, synthèse de robustesse, annexes méthodologiques.",
    7, "Précautions méthodologiques",
    "Expliciter les limites de l'enquête et éviter les interprétations causales.",
    "Désirabilité sociale ; biais de réponse ; déclaratif ; corrélations non causales ; différences disciplinaires ; formations obligatoires.",
    "Encadré méthodologique, diagnostics, pondérations, tests de sensibilité."
  )
}

# -----------------------------------------------------------------------------
# Catalogue des figures attendues dans le rapport final
# -----------------------------------------------------------------------------

osyr_figure_catalog <- function() {
  tibble::tribble(
    ~section, ~bloc, ~titre, ~file, ~source_dir, ~priorite,
    1, "Parcours de formation", "Distribution détaillée des dispositifs déclarés", "02b_distribution_dispositifs_q8_detail.png", "final", 1,
    1, "Parcours de formation", "Dispositifs Q8 selon la langue du questionnaire", "02c_distribution_dispositifs_q8_detail_par_langue.png", "final", 2,
    1, "Parcours de formation", "Dispositifs Q8 par discipline détaillée", "02d_heatmap_dispositifs_q8_par_discipline_detail.png", "final", 2,
    1, "Parcours de formation", "MOOC et autoformation par discipline détaillée", "02e_mooc_autoformation_par_discipline_detail.png", "final", 3,
    2, "Connaissances", "Connaissance des notions de science ouverte par discipline détaillée", "14b_heatmap_q5_connaissance_par_discipline_detail.png", "final", 1,
    2, "Connaissances", "Gap connaissance-usage par discipline détaillée", "17a_gap_connaissance_usage_par_discipline_detail.png", "final", 1,
    2, "Connaissances", "Gap connaissance-usage par famille d'objets", "17b_gap_par_famille_objet_discipline_detail.png", "final", 2,
    3, "Pratiques", "Usage des outils de science ouverte par discipline détaillée", "15b_heatmap_q5_usage_par_discipline_detail.png", "final", 1,
    3, "Pratiques", "Connaissance et usage Q5 par discipline détaillée", "16c_scores_q5_par_discipline_detail_exposition.png", "final", 2,
    4, "Intentions et attitudes", "Robustesse des effets associés aux dispositifs", "01_robustesse_scores_pondere_non_pondere.png", "complements", 1,
    4, "Intentions et attitudes", "Items qui portent les écarts exposés/non exposés", "03_items_top_effets_fdr.png", "complements", 2,
    5, "Perceptions", "Écarts exposés/non exposés par discipline détaillée", "16b_scores_ecarts_par_discipline_detail_exposition.png", "final", 1,
    6, "Profils et analyses transversales", "Plus grands écarts de scores par discipline", "16d_top_ecarts_scores_discipline_detail.png", "final", 1,
    6, "Profils et analyses transversales", "Carte de robustesse des coefficients", "02_heatmap_robustesse_scores.png", "complements", 2,
    7, "Précautions méthodologiques", "Déséquilibres de composition entre exposés et non exposés", "04_balance_covariables_exposes_non_exposes.png", "complements", 1
  )
}

resolve_figure_path <- function(file, source_dir = c("final", "complements")) {
  dirs <- osyr_dirs()
  source_dir <- source_dir[1]
  base <- switch(
    source_dir,
    final = file.path(dirs$final, "figures"),
    complements = file.path(dirs$complements, "figures"),
    file.path(dirs$final, "figures")
  )
  path <- file.path(base, file)
  if (file.exists(path)) path else NA_character_
}

build_figure_catalog <- function() {
  catalog <- osyr_figure_catalog()
  catalog |>
    dplyr::mutate(
      path = purrr::map2_chr(file, source_dir, resolve_figure_path),
      available = !is.na(path) & file.exists(path)
    ) |>
    dplyr::arrange(section, priorite, titre)
}

# -----------------------------------------------------------------------------
# Titres sobres pour le rapport et la présentation
# -----------------------------------------------------------------------------

osyr_report_title <- function() "OSYR — Rapport d'analyse finale"
osyr_ppt_title <- function() "OSYR — Résultats de l'enquête doctorants"

osyr_method_note <- function() {
  paste(
    "Les résultats présentés sont descriptifs et associatifs.",
    "Les modèles ajustés ne permettent pas d'attribuer causalement les différences observées aux dispositifs de formation.",
    "Les réponses sont déclaratives et doivent être lues avec les précautions indiquées dans la section méthodologique."
  )
}
