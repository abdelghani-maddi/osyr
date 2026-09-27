# =============================================================================
# Figures destinées au rapport et à la présentation OSYR
# Version 2026-09-27
# =============================================================================
# Cette étape reprend les tables calculées par le workflow. Elle ne modifie pas
# les résultats statistiques. Les graphiques sont conçus pour rester lisibles
# après réduction à la largeur utile d'une page Word.
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
  p <- file.path(tab_dir, paste0(name, ".csv"))
  if (!file.exists(p)) return(tibble::tibble())
  readr::read_csv(p, show_col_types = FALSE)
}

read_final_table <- function(name) {
  p <- file.path(dirs$final, "tables", paste0(name, ".csv"))
  if (!file.exists(p)) return(tibble::tibble())
  readr::read_csv(p, show_col_types = FALSE)
}

read_complement_table <- function(name, subdir = "tables") {
  p <- file.path(dirs$complements, subdir, paste0(name, ".csv"))
  if (!file.exists(p)) return(tibble::tibble())
  readr::read_csv(p, show_col_types = FALSE)
}

pct_lab <- function(x, accuracy = 1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

save_polished <- function(plot, file, width = 12.8, height = 8.0) {
  ggplot2::ggsave(
    filename = file.path(fig_dir, file),
    plot = plot,
    width = width,
    height = height,
    dpi = 420,
    bg = "white"
  )
  invisible(file.path(fig_dir, file))
}

clean_plot_theme <- function(base_size = 14) {
  osyr_theme(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = base_size - 1),
      axis.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.justification = "center",
      plot.margin = ggplot2::margin(16, 26, 16, 16)
    )
}

exposure_cols <- c(
  "Aucun dispositif" = cols[["brown"]],
  "Autoformation / autre seulement" = "#D9CF9B",
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

short_discipline <- function(x) {
  x0 <- stringr::str_to_lower(as.character(x))
  dplyr::case_when(
    stringr::str_detect(x0, "math") ~ "Mathématiques",
    stringr::str_detect(x0, "phys") ~ "Physique",
    stringr::str_detect(x0, "terre|univers|espace") ~ "Terre, univers\net espace",
    stringr::str_detect(x0, "chim") ~ "Chimie",
    stringr::str_detect(x0, "biolog|médec|medec|sant") ~ "Biologie, médecine\net santé",
    stringr::str_detect(x0, "humaines|humanit") ~ "Sciences humaines\net humanités",
    stringr::str_detect(x0, "société|societe") ~ "Sciences\nde la société",
    stringr::str_detect(x0, "ingénieur|ingenieur") ~ "Sciences pour\nl'ingénieur",
    stringr::str_detect(x0, "information|communication|stic|numérique|numerique") ~ "STIC",
    stringr::str_detect(x0, "agron|écolog|ecolog") ~ "Agronomie\net écologie",
    TRUE ~ stringr::str_wrap(as.character(x), 18)
  )
}

annotated_matrix <- function(data, x, y, value, x_lab = NULL, y_lab = NULL,
                             x_wrap = 16, y_wrap = 40, x_angle = 0,
                             show_values = TRUE, base_size = 13.5) {
  d <- data |>
    dplyr::mutate(
      .x = stringr::str_wrap(as.character(.data[[x]]), x_wrap),
      .y = stringr::str_wrap(as.character(.data[[y]]), y_wrap)
    )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y, fill = .data[[value]])) +
    ggplot2::geom_tile(color = "white", linewidth = 0.9) +
    ggplot2::scale_fill_gradient(
      low = "#F2F5F2",
      high = cols[["dark_green"]],
      labels = scales::percent_format(accuracy = 1),
      na.value = "white"
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, fill = "Part pondérée") +
    clean_plot_theme(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = x_angle,
        hjust = ifelse(x_angle == 0, 0.5, 1),
        vjust = ifelse(x_angle == 0, 0.5, 1),
        size = base_size - 1.3
      ),
      axis.text.y = ggplot2::element_text(size = base_size - 1.1),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = base_size - 2)
    )

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = pct_lab(.data[[value]], 1)),
      size = 4.0,
      color = cols[["black"]]
    )
  }

  p
}

dumbbell_plot <- function(data, label_col, group_col, value_col,
                          y_wrap = 46, x_label = "Part pondérée",
                          base_size = 14, show_values = TRUE) {
  d <- data |>
    dplyr::filter(.data[[group_col]] %in% names(two_group_cols)) |>
    dplyr::transmute(
      label = stringr::str_wrap(as.character(.data[[label_col]]), y_wrap),
      group = as.character(.data[[group_col]]),
      value = as.numeric(.data[[value_col]])
    ) |>
    dplyr::filter(!is.na(label), !is.na(value))

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

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(
        x = .data[["Aucun dispositif"]],
        xend = .data[["Dispositif organisé"]],
        y = label,
        yend = label
      ),
      color = "#C9CED3",
      linewidth = 1.25
    ) +
    ggplot2::geom_point(
      data = d,
      ggplot2::aes(x = value, y = label, color = group),
      size = 4.3
    ) +
    ggplot2::scale_color_manual(values = two_group_cols) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::labs(x = x_label, y = NULL, color = NULL) +
    clean_plot_theme(base_size = base_size)

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        data = d,
        ggplot2::aes(
          x = value,
          y = label,
          label = pct_lab(value, 1),
          color = group,
          vjust = ifelse(group == "Aucun dispositif", 1.7, -0.9)
        ),
        size = 3.8,
        show.legend = FALSE
      )
  }

  p
}

# -----------------------------------------------------------------------------
# 1. Parcours de formation
# -----------------------------------------------------------------------------

expo_year <- read_report_table("formation_exposition_par_annee")
if (has_rows(expo_year) && all(c("group", "value", "pct") %in% names(expo_year))) {
  p <- expo_year |>
    dplyr::mutate(
      group = factor(as.character(group), levels = unique(as.character(group))),
      value = factor(
        as.character(value),
        levels = c("Dispositif organisé", "Autoformation / autre seulement", "Aucun dispositif")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = pct, y = group, fill = value)) +
    ggplot2::geom_col(width = 0.68, color = "white", linewidth = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = dplyr::if_else(pct >= 0.08, pct_lab(pct, 1), "")),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 4.0,
      color = cols[["black"]]
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = exposure_cols, drop = TRUE) +
    ggplot2::labs(x = "Part pondérée", y = NULL, fill = NULL) +
    clean_plot_theme(base_size = 14.5)

  save_polished(p, "final_01_exposition_par_annee.png", width = 12.8, height = 6.8)
}

q8_year <- read_report_table("formation_dispositifs_q8_par_annee")
if (has_rows(q8_year) && all(c("group", "value", "pct") %in% names(q8_year))) {
  d <- q8_year |>
    dplyr::mutate(group = factor(as.character(group), levels = unique(as.character(group))))
  p <- annotated_matrix(
    d, "group", "value", "pct",
    y_wrap = 38, show_values = TRUE, base_size = 13.5
  )
  save_polished(p, "final_02_dispositifs_q8_par_annee.png", width = 12.8, height = 8.0)
}

q8_disc <- read_report_table("formation_dispositifs_q8_par_discipline")
if (has_rows(q8_disc) && all(c("group", "value", "pct") %in% names(q8_disc))) {
  d <- q8_disc |>
    dplyr::mutate(group_short = short_discipline(group))
  p <- annotated_matrix(
    d, "group_short", "value", "pct",
    x_wrap = 15, y_wrap = 35, x_angle = 0,
    show_values = TRUE, base_size = 12.8
  )
  save_polished(p, "final_03_dispositifs_q8_par_discipline.png", width = 15.5, height = 8.8)
}

# -----------------------------------------------------------------------------
# 2. Connaissances
# -----------------------------------------------------------------------------

q5_items <- read_report_table("connaissances_q5_items_gap")
if (has_rows(q5_items) && all(c("item_label", "pct_known_w", "pct_used_w") %in% names(q5_items))) {
  d <- q5_items |>
    dplyr::select(item_label, pct_known_w, pct_used_w) |>
    tidyr::pivot_longer(
      cols = c(pct_known_w, pct_used_w),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric = dplyr::recode(
        metric,
        pct_known_w = "Connaît bien",
        pct_used_w = "A déjà utilisé"
      )
    )

  q5_cols <- c(
    "Connaît bien" = cols[["green"]],
    "A déjà utilisé" = cols[["brown"]]
  )

  d2 <- d |>
    dplyr::transmute(
      label = stringr::str_wrap(item_label, 48),
      group = metric,
      value = value
    )

  ord <- d2 |>
    dplyr::group_by(label) |>
    dplyr::summarise(v = max(value), .groups = "drop") |>
    dplyr::arrange(v) |>
    dplyr::pull(label)
  d2$label <- factor(d2$label, levels = ord)

  seg <- d2 |>
    tidyr::pivot_wider(names_from = group, values_from = value)

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(x = .data[["A déjà utilisé"]], xend = .data[["Connaît bien"]], y = label, yend = label),
      color = "#C9CED3", linewidth = 1.15
    ) +
    ggplot2::geom_point(data = d2, ggplot2::aes(x = value, y = label, color = group), size = 4.0) +
    ggplot2::scale_color_manual(values = q5_cols) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 0.90),
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::labs(x = "Part pondérée", y = NULL, color = NULL) +
    clean_plot_theme(base_size = 13.5)

  save_polished(p, "final_10_q5_connaissance_usage_items.png", width = 13.8, height = 10.0)
}

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
    d, "score_label", "group", "mean_w",
    y_wrap = 30, x_label = "Score moyen pondéré", base_size = 14
  )
  save_polished(p, "final_11_scores_par_exposition.png", width = 12.8, height = 7.5)
}

score_year <- read_report_table("scores_connaissance_usage_intentions_par_annee")
if (has_rows(score_year) && all(c("group", "score", "mean_w") %in% names(score_year))) {
  labels <- c(
    score_q5_known_well = "Connaissance Q5",
    score_q5_used = "Usage Q5",
    score_q13_open_intentions = "Intentions Q13"
  )
  d <- score_year |>
    dplyr::mutate(
      score_label = dplyr::recode(score, !!!labels, .default = score),
      group = factor(as.character(group), levels = unique(as.character(group)))
    )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = group, y = mean_w, group = score_label, color = score_label)) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::geom_point(size = 4.0) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_lab(mean_w, 1)),
      vjust = -1.0, size = 3.8, show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.05, 0.12))
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Connaissance Q5" = cols[["green"]],
        "Usage Q5" = cols[["brown"]],
        "Intentions Q13" = cols[["dark_green"]]
      )
    ) +
    ggplot2::labs(x = NULL, y = "Score moyen pondéré", color = NULL) +
    clean_plot_theme(base_size = 14)

  save_polished(p, "final_12_scores_par_annee.png", width = 12.8, height = 7.0)
}

q5_disc <- read_final_table("q5_by_discipline_detail")
if (has_rows(q5_disc) && all(c("discipline_detail", "item_label") %in% names(q5_disc))) {
  if ("pct_known_w" %in% names(q5_disc)) {
    top_known <- q5_disc |>
      dplyr::group_by(item_label) |>
      dplyr::summarise(overall = mean(pct_known_w, na.rm = TRUE), .groups = "drop") |>
      dplyr::slice_max(overall, n = 10, with_ties = FALSE) |>
      dplyr::pull(item_label)

    d <- q5_disc |>
      dplyr::filter(item_label %in% top_known) |>
      dplyr::mutate(discipline_short = short_discipline(discipline_detail))

    p <- annotated_matrix(
      d, "discipline_short", "item_label", "pct_known_w",
      y_wrap = 34, show_values = FALSE, base_size = 12.8
    )
    save_polished(p, "14b_heatmap_q5_connaissance_par_discipline_detail.png", width = 15.5, height = 9.0)
  }

  if ("pct_used_w" %in% names(q5_disc)) {
    top_used <- q5_disc |>
      dplyr::group_by(item_label) |>
      dplyr::summarise(overall = mean(pct_used_w, na.rm = TRUE), .groups = "drop") |>
      dplyr::slice_max(overall, n = 10, with_ties = FALSE) |>
      dplyr::pull(item_label)

    d <- q5_disc |>
      dplyr::filter(item_label %in% top_used) |>
      dplyr::mutate(discipline_short = short_discipline(discipline_detail))

    p <- annotated_matrix(
      d, "discipline_short", "item_label", "pct_used_w",
      y_wrap = 34, show_values = FALSE, base_size = 12.8
    )
    save_polished(p, "15b_heatmap_q5_usage_par_discipline_detail.png", width = 15.5, height = 9.0)
  }
}

# -----------------------------------------------------------------------------
# 3. Pratiques
# -----------------------------------------------------------------------------

q4 <- read_report_table("pratiques_q4_items")
if (has_rows(q4) && all(c("item_label", "pct_positive_w") %in% names(q4))) {
  d <- q4 |>
    dplyr::mutate(
      item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 52), pct_positive_w)
    )

  xmax <- min(1, max(d$pct_positive_w, na.rm = TRUE) * 1.16)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = pct_positive_w, y = item_label)) +
    ggplot2::geom_col(fill = cols[["green"]], width = 0.64) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_lab(pct_positive_w, 1)),
      hjust = -0.18, size = 4.2, color = cols[["dark_green"]]
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, xmax),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Part pondérée", y = NULL) +
    clean_plot_theme(base_size = 14.5)

  save_polished(p, "final_20_pratiques_q4_items.png", width = 12.8, height = 7.2)
}

q5_year <- read_report_table("pratiques_q5_usages_par_annee")
if (has_rows(q5_year) && all(c("year", "item_label", "pct_used_w") %in% names(q5_year))) {
  top_items <- q5_year |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(overall = mean(pct_used_w, na.rm = TRUE), .groups = "drop") |>
    dplyr::slice_max(overall, n = 10, with_ties = FALSE) |>
    dplyr::pull(item_label)

  d <- q5_year |>
    dplyr::filter(item_label %in% top_items) |>
    dplyr::mutate(year = factor(as.character(year), levels = unique(as.character(year))))

  p <- annotated_matrix(
    d, "year", "item_label", "pct_used_w",
    y_wrap = 38, show_values = TRUE, base_size = 13.2
  )
  save_polished(p, "final_21_usages_q5_par_annee.png", width = 12.8, height = 8.5)
}

# -----------------------------------------------------------------------------
# 4. Intentions et attitudes
# -----------------------------------------------------------------------------

q13 <- read_report_table("intentions_q13_par_exposition")
if (has_rows(q13) && all(c("exposure2", "item_label", "pct_yes_w") %in% names(q13))) {
  p <- dumbbell_plot(
    q13, "item_label", "exposure2", "pct_yes_w",
    y_wrap = 44, base_size = 14.5
  )
  save_polished(p, "final_30_intentions_q13_par_exposition.png", width = 12.8, height = 7.2)

  if ("pct_dk_w" %in% names(q13)) {
    p <- dumbbell_plot(
      q13, "item_label", "exposure2", "pct_dk_w",
      y_wrap = 44, base_size = 14.5
    )
    save_polished(p, "final_31_intentions_q13_je_ne_sais_pas.png", width = 12.8, height = 7.2)
  }
}

intent_grid <- read_report_table("intentions_selon_connaissance_usage")
if (has_rows(intent_grid) && all(c("knowledge_band", "usage_band", "intentions_w") %in% names(intent_grid))) {
  d <- intent_grid |>
    dplyr::mutate(
      usage_label = paste0("Quartile ", usage_band),
      knowledge_label = paste0("Quartile ", knowledge_band)
    )
  p <- annotated_matrix(
    d, "usage_label", "knowledge_label", "intentions_w",
    x_lab = "Usage Q5", y_lab = "Connaissance Q5",
    show_values = TRUE, base_size = 14
  )
  save_polished(p, "final_32_intentions_selon_connaissance_usage.png", width = 10.5, height = 7.5)
}

# -----------------------------------------------------------------------------
# 5. Perceptions
# -----------------------------------------------------------------------------

q15 <- read_report_table("perceptions_q15_par_exposition")
if (has_rows(q15) && all(c("exposure2", "item_label", "pct_agree_w") %in% names(q15))) {
  p <- dumbbell_plot(
    q15, "item_label", "exposure2", "pct_agree_w",
    y_wrap = 48, base_size = 14
  )
  save_polished(p, "final_40_perceptions_q15_par_exposition.png", width = 13.2, height = 8.2)
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
      ),
      item_label = stringr::str_wrap(item_label, 46),
      exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé"))
    )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = exposure2, y = item_label, fill = pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = pct_lab(pct, 1)), size = 4.0, color = cols[["black"]]) +
    ggplot2::facet_wrap(~ metric, nrow = 1) +
    ggplot2::scale_fill_gradient(
      low = "#F2F5F2", high = cols[["dark_green"]],
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Part pondérée") +
    clean_plot_theme(base_size = 13.5) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      strip.text = ggplot2::element_text(size = 13, face = "bold")
    )

  save_polished(p, "final_41_environnement_q12_par_exposition.png", width = 12.8, height = 7.8)
}

env_grid <- read_report_table("perceptions_q15_selon_q12")
if (has_rows(env_grid) && all(c("incitation_band", "frein_band", "agreement_w") %in% names(env_grid))) {
  d <- env_grid |>
    dplyr::mutate(
      frein_label = paste0("Quartile ", frein_band),
      incitation_label = paste0("Quartile ", incitation_band)
    )
  p <- annotated_matrix(
    d, "frein_label", "incitation_label", "agreement_w",
    x_lab = "Freins perçus", y_lab = "Incitations perçues",
    show_values = TRUE, base_size = 14
  )
  save_polished(p, "final_42_perceptions_selon_environnement.png", width = 10.5, height = 7.5)
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
    ggplot2::geom_point(alpha = 0.12, size = 1.3) +
    ggplot2::stat_ellipse(linewidth = 1.0, alpha = 0.9, show.legend = FALSE) +
    ggplot2::geom_point(
      data = centers,
      ggplot2::aes(x = dim1, y = dim2, color = profile),
      size = 5.0, show.legend = FALSE
    ) +
    ggplot2::geom_label(
      data = centers,
      ggplot2::aes(label = paste0(profile, "\n(n = ", n, ")"), color = profile),
      fill = "white", label.size = 0, fontface = "bold",
      size = 4.0, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = profile_cols) +
    ggplot2::labs(x = "Axe 1", y = "Axe 2", color = NULL) +
    clean_plot_theme(base_size = 14) +
    ggplot2::theme(legend.position = "none")

  save_polished(p, "final_50_profils_acp_scores.png", width = 12.0, height = 8.0)
}

profile_means <- read_report_table("profils_moyennes_scores")
if (has_rows(profile_means) && all(c("profile", "score_label", "mean_w") %in% names(profile_means))) {
  p <- annotated_matrix(
    profile_means, "profile", "score_label", "mean_w",
    y_wrap = 32, show_values = TRUE, base_size = 14
  )
  save_polished(p, "final_51_profils_moyennes_scores.png", width = 10.5, height = 7.5)
}

auto <- read_report_table("profils_non_formes_autoformes_scores")
if (has_rows(auto) && all(c("exposure3", "score_label", "mean_w") %in% names(auto))) {
  d <- auto |>
    dplyr::mutate(
      score_label = forcats::fct_reorder(stringr::str_wrap(score_label, 32), mean_w, .fun = max),
      exposure3 = factor(
        exposure3,
        levels = c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé")
      )
    )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = mean_w, y = score_label, color = exposure3)) +
    ggplot2::geom_point(size = 4.3, position = ggplot2::position_dodge(width = 0.58)) +
    ggplot2::scale_color_manual(values = exposure_cols) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.02, 0.08))
    ) +
    ggplot2::labs(x = "Score moyen pondéré", y = NULL, color = NULL) +
    clean_plot_theme(base_size = 14)

  save_polished(p, "final_52_focus_non_formes_autoformes.png", width = 12.8, height = 7.5)
}

# -----------------------------------------------------------------------------
# 7. Robustesse et composition des groupes
# -----------------------------------------------------------------------------

robust <- read_complement_table("score_robustness_summary")
if (has_rows(robust) && all(c("outcome_label", "median_estimate_pp", "min_estimate_pp", "max_estimate_pp", "conclusion") %in% names(robust))) {
  d <- robust |>
    dplyr::mutate(
      outcome_label = forcats::fct_reorder(stringr::str_wrap(outcome_label, 42), median_estimate_pp),
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

  p <- ggplot2::ggplot(d, ggplot2::aes(x = median_estimate_pp, y = outcome_label, color = conclusion_simple)) +
    ggplot2::geom_vline(xintercept = 0, color = cols[["mid_grey"]], linewidth = 0.8) +
    ggplot2::geom_segment(
      ggplot2::aes(x = min_estimate_pp, xend = max_estimate_pp, yend = outcome_label),
      linewidth = 1.5, color = "#C9CED3"
    ) +
    ggplot2::geom_point(size = 4.5) +
    ggplot2::scale_color_manual(values = robust_cols) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 0.1, decimal.mark = ","), " pts"),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      x = "Écart ajusté : dispositif organisé - aucun dispositif",
      y = NULL, color = NULL
    ) +
    clean_plot_theme(base_size = 14)

  save_polished(p, "final_61_robustesse_associations.png", width = 12.8, height = 7.8)
}

balance <- read_complement_table("covariate_balance_exposed_nonexposed", subdir = "methodology")
if (has_rows(balance) && all(c("covariate", "modality", "standardized_difference", "imbalance_flag") %in% names(balance))) {
  pretty_covariate <- function(cov, mod) {
    x <- dplyr::case_when(
      cov == "year_code" ~ "Année de thèse",
      cov == "discipline_code" ~ "Discipline (code)",
      cov == "language_group" ~ paste0("Langue : ", mod),
      cov == "discipline_detail" ~ paste0("Discipline : ", short_discipline(mod)),
      cov == "institution" ~ paste0("Établissement : ", mod),
      TRUE ~ paste0(cov, " : ", mod)
    )
    stringr::str_wrap(x, 42)
  }

  d <- balance |>
    dplyr::filter(!is.na(standardized_difference)) |>
    dplyr::mutate(
      label = pretty_covariate(covariate, modality),
      abs_diff = abs(standardized_difference),
      label = forcats::fct_reorder(label, standardized_difference)
    ) |>
    dplyr::slice_max(abs_diff, n = 16, with_ties = FALSE)

  bal_cols <- c(
    "Équilibre acceptable" = cols[["grey"]],
    "Déséquilibre modéré" = cols[["brown"]],
    "Fort déséquilibre" = cols[["dark_green"]]
  )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = standardized_difference, y = label, fill = imbalance_flag)) +
    ggplot2::geom_vline(xintercept = 0, color = "#98A2B3", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = c(-0.10, 0.10), color = "#D0D5DD", linetype = "dashed", linewidth = 0.55) +
    ggplot2::geom_vline(xintercept = c(-0.20, 0.20), color = "#D0D5DD", linetype = "dotted", linewidth = 0.55) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::scale_fill_manual(values = bal_cols) +
    ggplot2::labs(
      x = "Différence : exposés - non exposés",
      y = NULL, fill = NULL
    ) +
    clean_plot_theme(base_size = 13.2)

  save_polished(p, "final_62_balance_covariables.png", width = 13.2, height = 9.0)
}

# -----------------------------------------------------------------------------
# Catalogue : la figure de couverture reste un diagnostic interne.
# -----------------------------------------------------------------------------

catalog_path <- file.path(tab_dir, "catalogue_figures_finales.csv")
if (file.exists(catalog_path)) {
  cat <- readr::read_csv(catalog_path, show_col_types = FALSE)

  cat <- cat |>
    dplyr::filter(!file %in% c("final_61_robustesse_associations.png", "final_62_balance_covariables.png")) |>
    dplyr::bind_rows(
      tibble::tibble(
        section = c(7L, 7L),
        bloc = c("Précautions méthodologiques", "Précautions méthodologiques"),
        titre = c(
          "Robustesse des associations avec l'exposition aux dispositifs",
          "Composition des groupes exposés et non exposés"
        ),
        caption = c(
          "Médiane et amplitude des estimations obtenues selon les principales spécifications.",
          "Différences de composition pondérées entre répondants exposés et non exposés ; les seuils à 0,10 et 0,20 servent de repères descriptifs."
        ),
        file = c("final_61_robustesse_associations.png", "final_62_balance_covariables.png"),
        path = c(
          file.path(fig_dir, "final_61_robustesse_associations.png"),
          file.path(fig_dir, "final_62_balance_covariables.png")
        ),
        source_dir = c("rapport_final", "rapport_final"),
        priorite = c(1L, 2L),
        available = c(
          file.exists(file.path(fig_dir, "final_61_robustesse_associations.png")),
          file.exists(file.path(fig_dir, "final_62_balance_covariables.png"))
        )
      )
    ) |>
    dplyr::mutate(
      priorite = dplyr::if_else(file == "final_60_couverture_plan_depouillement.png", 99L, as.integer(priorite))
    ) |>
    dplyr::arrange(section, priorite, titre)

  readr::write_csv(cat, catalog_path)
}

message("Figures du rapport régénérées : ", normalizePath(fig_dir, mustWork = FALSE))
