# =============================================================================
# Polissage des figures du rapport final OSYR
# Version 2026-09-21
# =============================================================================
# Cette étape est exécutée après R/osyr_final_analyses.R.
# Elle ne recalcule pas les résultats de fond : elle reprend les tables produites
# par le workflow et régénère les figures principales dans un format plus lisible
# pour un rapport A4 et une présentation.
#
# Principes :
# - palette OSYR homogène ;
# - textes lisibles une fois la figure insérée dans Word ;
# - limitation des libellés inclinés ;
# - valeurs directement affichées lorsque la matrice reste de taille raisonnable ;
# - préférer dumbbells, dot plots et matrices annotées aux barres groupées ou
#   heatmaps trop denses ;
# - figures exploratoires très chargées conservées pour l'annexe.
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

pkgs <- c("tidyverse", "scales", "forcats", "fs")
missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

cols <- osyr_colors()
dirs <- osyr_dirs()

fig_dir <- file.path(dirs$report, "figures")
tab_dir <- file.path(dirs$report, "tables")
ensure_dir(fig_dir)

read_report_table <- function(name) {
  path <- file.path(tab_dir, paste0(name, ".csv"))
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

read_final_table <- function(name) {
  path <- file.path(dirs$final, "tables", paste0(name, ".csv"))
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pct_lab <- function(x, accuracy = 1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

save_polished <- function(plot, file, width = 12, height = 7.2) {
  ggplot2::ggsave(
    file.path(fig_dir, file),
    plot,
    width = width,
    height = height,
    dpi = 420,
    bg = "white"
  )
  invisible(file.path(fig_dir, file))
}

exposure_cols <- c(
  "Aucun dispositif" = cols[["brown"]],
  "Autoformation / autre seulement" = "#E6DDAA",
  "Dispositif organisé" = cols[["green"]]
)

two_group_cols <- c(
  "Aucun dispositif" = cols[["brown"]],
  "Dispositif organisé" = cols[["green"]]
)

profile_cols <- c(
  cols[["green"]],
  cols[["brown"]],
  cols[["dark_green"]],
  cols[["grey"]],
  "#B7CDB9"
)

matrix_percent_plot <- function(data, x, y, value, title, subtitle,
                                x_wrap = 18, y_wrap = 42,
                                show_text = TRUE, text_size = 3.0,
                                x_angle = 0) {
  data |>
    dplyr::mutate(
      .x = stringr::str_wrap(as.character(.data[[x]]), x_wrap),
      .y = stringr::str_wrap(as.character(.data[[y]]), y_wrap)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .x, y = .y, fill = .data[[value]])) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    {
      if (show_text) {
        ggplot2::geom_text(
          ggplot2::aes(label = pct_lab(.data[[value]], 1)),
          size = text_size,
          color = cols[["dark_green"]]
        )
      }
    } +
    ggplot2::scale_fill_gradient(
      low = "#F4F6F4",
      high = cols[["green"]],
      labels = scales::percent_format(accuracy = 1),
      na.value = "white"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      fill = "Part pondérée"
    ) +
    osyr_theme(base_size = 11.2) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = x_angle, hjust = ifelse(x_angle == 0, 0.5, 1)),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

bubble_percent_plot <- function(data, x, y, value, title, subtitle,
                                x_wrap = 18, y_wrap = 42, x_angle = 0) {
  data |>
    dplyr::mutate(
      .x = stringr::str_wrap(as.character(.data[[x]]), x_wrap),
      .y = stringr::str_wrap(as.character(.data[[y]]), y_wrap)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .x, y = .y, size = .data[[value]], fill = .data[[value]])) +
    ggplot2::geom_point(shape = 21, color = "white", stroke = 0.6, alpha = 0.95) +
    ggplot2::scale_size_area(max_size = 11, guide = "none") +
    ggplot2::scale_fill_gradient(
      low = "#DDE8DE",
      high = cols[["dark_green"]],
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      fill = "Part pondérée"
    ) +
    osyr_theme(base_size = 11.1) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = x_angle, hjust = ifelse(x_angle == 0, 0.5, 1)),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

dumbbell_plot <- function(data, label_col, group_col, value_col, title, subtitle,
                          y_wrap = 48, x_label = "Part pondérée") {
  d <- data |>
    dplyr::filter(.data[[group_col]] %in% names(two_group_cols)) |>
    dplyr::select(
      label = dplyr::all_of(label_col),
      group = dplyr::all_of(group_col),
      value = dplyr::all_of(value_col)
    ) |>
    dplyr::filter(!is.na(label), !is.na(value)) |>
    dplyr::mutate(label = stringr::str_wrap(as.character(label), y_wrap))

  order <- d |>
    dplyr::group_by(label) |>
    dplyr::summarise(order_value = max(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(order_value) |>
    dplyr::pull(label)

  d$label <- factor(d$label, levels = order)

  seg <- d |>
    tidyr::pivot_wider(names_from = group, values_from = value) |>
    dplyr::filter(
      !is.na(.data[["Aucun dispositif"]]),
      !is.na(.data[["Dispositif organisé"]])
    )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(
        x = .data[["Aucun dispositif"]],
        xend = .data[["Dispositif organisé"]],
        y = label,
        yend = label
      ),
      color = cols[["mid_grey"]],
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = d,
      ggplot2::aes(x = value, y = label, color = group),
      size = 3.2
    ) +
    ggplot2::scale_color_manual(values = two_group_cols) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.01, 0.08))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = NULL,
      color = NULL
    ) +
    osyr_theme(base_size = 11.2)
}

# -----------------------------------------------------------------------------
# 1. Parcours de formation
# -----------------------------------------------------------------------------

q8_year <- read_report_table("formation_dispositifs_q8_par_annee")
if (has_rows(q8_year) && all(c("group", "value", "pct") %in% names(q8_year))) {
  p <- bubble_percent_plot(
    q8_year,
    x = "group",
    y = "value",
    value = "pct",
    title = "Types de dispositifs selon l'année de thèse",
    subtitle = "Q8 est une question multiréponse ; la taille et la teinte représentent la part pondérée dans chaque année.",
    x_wrap = 14,
    y_wrap = 38,
    x_angle = 0
  )
  save_polished(p, "final_02_dispositifs_q8_par_annee.png", width = 11.8, height = 7.4)
}

q8_disc <- read_report_table("formation_dispositifs_q8_par_discipline")
if (has_rows(q8_disc) && all(c("group", "value", "pct") %in% names(q8_disc))) {
  p <- bubble_percent_plot(
    q8_disc,
    x = "group",
    y = "value",
    value = "pct",
    title = "Types de dispositifs selon la discipline",
    subtitle = "Part pondérée des répondants ayant déclaré chaque modalité Q8.",
    x_wrap = 16,
    y_wrap = 36,
    x_angle = 30
  )
  save_polished(p, "final_03_dispositifs_q8_par_discipline.png", width = 13.8, height = 8.0)
}

# -----------------------------------------------------------------------------
# 2. Connaissances
# -----------------------------------------------------------------------------

scores_expo <- read_report_table("scores_par_exposition")
if (has_rows(scores_expo) && all(c("group", "score", "mean_w") %in% names(scores_expo))) {
  score_labels <- c(
    score_q5_known_well = "Connaissance Q5",
    score_q5_used = "Usage Q5",
    score_q4_practices_research = "Pratiques Q4",
    score_q13_open_intentions = "Intentions Q13",
    score_q13_dont_know = "Je ne sais pas Q13",
    score_q12_incitation = "Incitation Q12",
    score_q12_frein = "Frein Q12",
    score_q15_agreement = "Accord Q15"
  )

  d <- scores_expo |>
    dplyr::mutate(score_label = dplyr::recode(score, !!!score_labels, .default = score))

  p <- dumbbell_plot(
    d,
    label_col = "score_label",
    group_col = "group",
    value_col = "mean_w",
    title = "Principaux scores selon l'exposition aux dispositifs",
    subtitle = "Comparaison pondérée entre répondants sans dispositif et répondants exposés à un dispositif organisé.",
    y_wrap = 30,
    x_label = "Score moyen pondéré"
  )
  save_polished(p, "final_11_scores_par_exposition.png", width = 11.5, height = 6.8)
}

q5_disc <- read_final_table("q5_by_discipline_detail")
if (has_rows(q5_disc) && all(c("discipline_detail", "item_label") %in% names(q5_disc))) {
  if ("pct_known_w" %in% names(q5_disc)) {
    top_known <- q5_disc |>
      dplyr::group_by(item_label) |>
      dplyr::summarise(overall = mean(pct_known_w, na.rm = TRUE), .groups = "drop") |>
      dplyr::slice_max(overall, n = 12, with_ties = FALSE) |>
      dplyr::pull(item_label)

    d <- q5_disc |>
      dplyr::filter(item_label %in% top_known)

    p <- bubble_percent_plot(
      d,
      x = "discipline_detail",
      y = "item_label",
      value = "pct_known_w",
      title = "Connaissance des outils de science ouverte par discipline",
      subtitle = "Douze notions parmi les plus connues ; part pondérée déclarant bien connaître chaque notion.",
      x_wrap = 16,
      y_wrap = 36,
      x_angle = 30
    )
    save_polished(p, "14b_heatmap_q5_connaissance_par_discipline_detail.png", width = 13.8, height = 8.4)
  }

  if ("pct_used_w" %in% names(q5_disc)) {
    top_used <- q5_disc |>
      dplyr::group_by(item_label) |>
      dplyr::summarise(overall = mean(pct_used_w, na.rm = TRUE), .groups = "drop") |>
      dplyr::slice_max(overall, n = 12, with_ties = FALSE) |>
      dplyr::pull(item_label)

    d <- q5_disc |>
      dplyr::filter(item_label %in% top_used)

    p <- bubble_percent_plot(
      d,
      x = "discipline_detail",
      y = "item_label",
      value = "pct_used_w",
      title = "Usage des outils de science ouverte par discipline",
      subtitle = "Douze outils parmi les plus utilisés ; part pondérée déclarant avoir déjà utilisé chaque outil.",
      x_wrap = 16,
      y_wrap = 36,
      x_angle = 30
    )
    save_polished(p, "15b_heatmap_q5_usage_par_discipline_detail.png", width = 13.8, height = 8.4)
  }
}

# -----------------------------------------------------------------------------
# 3. Pratiques
# -----------------------------------------------------------------------------

q5_year <- read_report_table("pratiques_q5_usages_par_annee")
if (has_rows(q5_year) && all(c("year", "item_label", "pct_used_w") %in% names(q5_year))) {
  top_items <- q5_year |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(overall = mean(pct_used_w, na.rm = TRUE), .groups = "drop") |>
    dplyr::slice_max(overall, n = 10, with_ties = FALSE) |>
    dplyr::pull(item_label)

  d <- q5_year |>
    dplyr::filter(item_label %in% top_items)

  p <- matrix_percent_plot(
    d,
    x = "year",
    y = "item_label",
    value = "pct_used_w",
    title = "Usages Q5 selon l'année de thèse",
    subtitle = "Dix outils parmi les plus utilisés ; les valeurs sont les parts pondérées dans chaque année.",
    x_wrap = 14,
    y_wrap = 40,
    show_text = TRUE,
    text_size = 3.1
  )
  save_polished(p, "final_21_usages_q5_par_annee.png", width = 10.8, height = 7.8)
}

# -----------------------------------------------------------------------------
# 4. Intentions et attitudes
# -----------------------------------------------------------------------------

q13 <- read_report_table("intentions_q13_par_exposition")
if (has_rows(q13) && all(c("exposure2", "item_label", "pct_yes_w") %in% names(q13))) {
  p <- dumbbell_plot(
    q13,
    label_col = "item_label",
    group_col = "exposure2",
    value_col = "pct_yes_w",
    title = "Intentions de pratiques ouvertes selon l'exposition",
    subtitle = "Part pondérée de réponses positives à chaque item Q13.",
    y_wrap = 44
  )
  save_polished(p, "final_30_intentions_q13_par_exposition.png", width = 11.8, height = 7.2)

  if ("pct_dk_w" %in% names(q13)) {
    p <- dumbbell_plot(
      q13,
      label_col = "item_label",
      group_col = "exposure2",
      value_col = "pct_dk_w",
      title = "Incertitudes déclarées sur les intentions",
      subtitle = "Part pondérée de réponses « je ne sais pas » à chaque item Q13.",
      y_wrap = 44
    )
    save_polished(p, "final_31_intentions_q13_je_ne_sais_pas.png", width = 11.8, height = 7.2)
  }
}

# -----------------------------------------------------------------------------
# 5. Perceptions
# -----------------------------------------------------------------------------

q15 <- read_report_table("perceptions_q15_par_exposition")
if (has_rows(q15) && all(c("exposure2", "item_label", "pct_agree_w") %in% names(q15))) {
  p <- dumbbell_plot(
    q15,
    label_col = "item_label",
    group_col = "exposure2",
    value_col = "pct_agree_w",
    title = "Perceptions de la science ouverte selon l'exposition",
    subtitle = "Part pondérée d'accord avec chaque affirmation Q15.",
    y_wrap = 48
  )
  save_polished(p, "final_40_perceptions_q15_par_exposition.png", width = 12.2, height = 7.8)
}

q12 <- read_report_table("perceptions_q12_environnement_par_exposition")
if (has_rows(q12) && all(c("exposure2", "item_label", "pct_incitation_w", "pct_frein_w") %in% names(q12))) {
  d <- q12 |>
    dplyr::select(exposure2, item_label, pct_incitation_w, pct_frein_w) |>
    tidyr::pivot_longer(
      cols = c(pct_incitation_w, pct_frein_w),
      names_to = "metric",
      values_to = "pct"
    ) |>
    dplyr::mutate(
      metric = dplyr::recode(
        metric,
        pct_incitation_w = "Incitation",
        pct_frein_w = "Frein"
      )
    )

  p <- d |>
    dplyr::mutate(
      exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé")),
      item_label = stringr::str_wrap(item_label, 46)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = exposure2, y = item_label, fill = pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_lab(pct, 1)),
      size = 3.1,
      color = cols[["dark_green"]]
    ) +
    ggplot2::facet_wrap(~ metric, nrow = 1) +
    ggplot2::scale_fill_gradient(
      low = "#F4F6F4",
      high = cols[["green"]],
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(
      title = "Environnement perçu : incitations et freins",
      subtitle = "Q12 selon l'exposition aux dispositifs ; valeurs pondérées affichées dans les cases.",
      x = NULL,
      y = NULL,
      fill = "Part pondérée"
    ) +
    osyr_theme(base_size = 11.1) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")

  save_polished(p, "final_41_environnement_q12_par_exposition.png", width = 11.8, height = 7.0)
}

# -----------------------------------------------------------------------------
# 6. Profils et analyses transversales
# -----------------------------------------------------------------------------

profile_coord <- read_report_table("profils_coordonnees")
if (has_rows(profile_coord) && all(c("profile", "dim1", "dim2") %in% names(profile_coord))) {
  centers <- profile_coord |>
    dplyr::group_by(profile) |>
    dplyr::summarise(
      dim1 = mean(dim1, na.rm = TRUE),
      dim2 = mean(dim2, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(profile_coord, ggplot2::aes(x = dim1, y = dim2, color = profile)) +
    ggplot2::geom_point(alpha = 0.18, size = 1.3) +
    ggplot2::stat_ellipse(linewidth = 0.8, alpha = 0.85, show.legend = FALSE) +
    ggplot2::geom_point(
      data = centers,
      ggplot2::aes(x = dim1, y = dim2, color = profile),
      size = 4.2,
      stroke = 0.6
    ) +
    ggplot2::geom_label(
      data = centers,
      ggplot2::aes(x = dim1, y = dim2, label = paste0(profile, " (n=", n, ")"), color = profile),
      fill = "white",
      label.size = 0,
      fontface = "bold",
      size = 3.3,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = profile_cols) +
    ggplot2::labs(
      title = "Profils exploratoires de répondants",
      subtitle = "Projection sur les deux premiers axes de l'ACP ; les ellipses et centroïdes résument chaque groupe.",
      x = "Axe 1",
      y = "Axe 2",
      color = NULL
    ) +
    osyr_theme(base_size = 11.2) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#EEF0F2", linewidth = 0.3))

  save_polished(p, "final_50_profils_acp_scores.png", width = 11.3, height = 7.3)
}

profile_means <- read_report_table("profils_moyennes_scores")
if (has_rows(profile_means) && all(c("profile", "score_label", "mean_w") %in% names(profile_means))) {
  p <- matrix_percent_plot(
    profile_means,
    x = "profile",
    y = "score_label",
    value = "mean_w",
    title = "Caractérisation des profils exploratoires",
    subtitle = "Moyennes pondérées des principaux scores dans chaque profil.",
    x_wrap = 14,
    y_wrap = 34,
    show_text = TRUE,
    text_size = 3.1
  )
  save_polished(p, "final_51_profils_moyennes_scores.png", width = 9.6, height = 6.5)
}

auto <- read_report_table("profils_non_formes_autoformes_scores")
if (has_rows(auto) && all(c("exposure3", "score_label", "mean_w") %in% names(auto))) {
  p <- auto |>
    dplyr::mutate(
      score_label = forcats::fct_reorder(stringr::str_wrap(score_label, 32), mean_w, .fun = max),
      exposure3 = factor(
        exposure3,
        levels = c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = mean_w, y = score_label, color = exposure3)) +
    ggplot2::geom_point(size = 3.2, position = ggplot2::position_dodge(width = 0.55)) +
    ggplot2::scale_color_manual(values = exposure_cols) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.01, 0.06))
    ) +
    ggplot2::labs(
      title = "Non formés, autoformés et exposés à un dispositif organisé",
      subtitle = "Comparaison des scores moyens pondérés ; l'autoformation est traitée séparément.",
      x = "Score moyen pondéré",
      y = NULL,
      color = NULL
    ) +
    osyr_theme(base_size = 11.2)

  save_polished(p, "final_52_focus_non_formes_autoformes.png", width = 11.4, height = 6.8)
}

# -----------------------------------------------------------------------------
# 7. Robustesse : une figure publiable remplace le diagnostic de couverture
# -----------------------------------------------------------------------------

robust_path <- file.path(dirs$complements, "tables", "score_robustness_summary.csv")
robust <- if (file.exists(robust_path)) readr::read_csv(robust_path, show_col_types = FALSE) else tibble::tibble()

if (has_rows(robust) && all(c("outcome_label", "median_estimate_pp", "min_estimate_pp", "max_estimate_pp", "conclusion") %in% names(robust))) {
  robust <- robust |>
    dplyr::mutate(
      outcome_label = forcats::fct_reorder(
        stringr::str_wrap(outcome_label, 38),
        median_estimate_pp
      ),
      conclusion_simple = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(conclusion), "positive stable") ~ "Association positive stable",
        stringr::str_detect(stringr::str_to_lower(conclusion), "négative stable|negative stable") ~ "Association négative stable",
        stringr::str_detect(stringr::str_to_lower(conclusion), "sensible") ~ "Résultat sensible",
        TRUE ~ "Signe stable, incertitude"
      )
    )

  robust_cols <- c(
    "Association positive stable" = cols[["green"]],
    "Association négative stable" = cols[["brown"]],
    "Signe stable, incertitude" = cols[["grey"]],
    "Résultat sensible" = "#B54708"
  )

  p <- ggplot2::ggplot(
    robust,
    ggplot2::aes(x = median_estimate_pp, y = outcome_label, color = conclusion_simple)
  ) +
    ggplot2::geom_vline(xintercept = 0, color = cols[["mid_grey"]], linewidth = 0.7) +
    ggplot2::geom_segment(
      ggplot2::aes(x = min_estimate_pp, xend = max_estimate_pp, yend = outcome_label),
      linewidth = 1.1,
      color = cols[["mid_grey"]]
    ) +
    ggplot2::geom_point(size = 3.4) +
    ggplot2::scale_color_manual(values = robust_cols) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1, decimal.mark = ","), " pts")
    ) +
    ggplot2::labs(
      title = "Robustesse des associations avec l'exposition aux dispositifs",
      subtitle = "Médiane et amplitude des estimations selon les principales spécifications testées.",
      x = "Écart ajusté : dispositif organisé moins aucun dispositif",
      y = NULL,
      color = NULL
    ) +
    osyr_theme(base_size = 11.2)

  new_file <- "final_61_robustesse_associations.png"
  save_polished(p, new_file, width = 11.8, height = 7.0)

  catalog_path <- file.path(tab_dir, "catalogue_figures_finales.csv")
  if (file.exists(catalog_path)) {
    cat <- readr::read_csv(catalog_path, show_col_types = FALSE)

    cat <- cat |>
      dplyr::filter(file != new_file) |>
      dplyr::bind_rows(
        tibble::tibble(
          section = 7L,
          bloc = "Précautions méthodologiques",
          titre = "Robustesse des associations avec l'exposition aux dispositifs",
          caption = "Les segments représentent l'amplitude des estimations obtenues selon les principales spécifications testées.",
          file = new_file,
          path = file.path(fig_dir, new_file),
          source_dir = "rapport_final",
          priorite = 1L,
          available = TRUE
        )
      ) |>
      dplyr::arrange(section, priorite, titre)

    readr::write_csv(cat, catalog_path)
  }
}

message("Polissage des figures terminé : ", normalizePath(fig_dir, mustWork = FALSE))
