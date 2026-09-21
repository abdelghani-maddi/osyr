# =============================================================================
# Analyses et figures pour la production finale OSYR
# =============================================================================
# Ce fichier est appelé par les scripts 05 et 06.
# Il part des sorties des scripts 01 et 03 et produit une couche d'analyses
# supplémentaires alignée sur le plan de dépouillement de septembre 2026.
#
# Les figures produites ici ne remplacent pas les figures exploratoires : elles
# servent à alimenter le rapport final et la présentation finale.
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

pkgs <- c("tidyverse", "fs", "scales", "forcats", "stringi", "ggrepel", "cluster")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) {
  stop("Fichier manquant : R/osyr_style.R")
}
source("R/osyr_style.R")

cols <- osyr_colors()
dirs <- osyr_dirs()

ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "figures"))
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "diagnostics"))

# -----------------------------------------------------------------------------
# Helpers généraux
# -----------------------------------------------------------------------------

clean_chr <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower(locale = "fr") |>
    stringr::str_squish()
}

safe_pct <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(as.numeric(x[ok]) * as.numeric(w[ok]), na.rm = TRUE) / sum(as.numeric(w[ok]), na.rm = TRUE)
}

w_prop <- function(x, w) w_mean(as.numeric(x), w)

safe_max <- function(x, multiplier = 1.15, floor = 0.05, ceiling = 1) {
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(m) || is.na(m)) return(floor)
  min(ceiling, max(floor, m * multiplier))
}

standardize_exposure <- function(x) {
  dplyr::case_when(
    stringr::str_detect(clean_chr(x), "aucun") ~ "Aucun dispositif",
    stringr::str_detect(clean_chr(x), "auto") ~ "Autoformation / autre seulement",
    stringr::str_detect(clean_chr(x), "organise|organis") ~ "Dispositif organisé",
    TRUE ~ as.character(x)
  )
}

save_plot_final <- function(plot, file, section, bloc, titre, caption,
                            width = 12, height = 7.2, priority = 1) {
  path <- file.path(dirs$report, "figures", file)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 340, bg = "white")
  tibble::tibble(
    section = section,
    bloc = bloc,
    titre = titre,
    caption = caption,
    file = file,
    path = path,
    source_dir = "rapport_final",
    priorite = priority,
    available = file.exists(path)
  )
}

copy_existing_figures <- function() {
  catalog <- build_figure_catalog()
  if (!has_rows(catalog)) return(tibble::tibble())

  catalog_available <- catalog |>
    dplyr::filter(available) |>
    dplyr::mutate(
      report_path = file.path(dirs$report, "figures", file)
    )

  if (nrow(catalog_available) > 0) {
    purrr::walk2(
      catalog_available$path,
      catalog_available$report_path,
      ~ fs::file_copy(.x, .y, overwrite = TRUE)
    )
  }

  catalog_available |>
    dplyr::transmute(
      section, bloc, titre,
      caption = paste0("Figure issue des sorties ", source_dir, "."),
      file,
      path = report_path,
      source_dir,
      priorite,
      available = file.exists(report_path)
    )
}

weighted_distribution <- function(data, group_var, value_var, weight_var = ".weight") {
  if (!all(c(group_var, value_var, weight_var) %in% names(data))) return(tibble::tibble())

  data |>
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[value_var]]), !is.na(.data[[weight_var]])) |>
    dplyr::group_by(group = .data[[group_var]], value = .data[[value_var]]) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      pct = weighted_n / sum(weighted_n, na.rm = TRUE),
      pct_label = safe_pct(pct)
    ) |>
    dplyr::ungroup()
}

weighted_score_by_group <- function(data, group_var, score_vars, weight_var = ".weight") {
  score_vars <- score_vars[score_vars %in% names(data)]
  if (!group_var %in% names(data) || length(score_vars) == 0) return(tibble::tibble())

  data |>
    dplyr::select(dplyr::all_of(c(group_var, weight_var, score_vars))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(score_vars), names_to = "score", values_to = "value") |>
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(value)) |>
    dplyr::group_by(group = .data[[group_var]], score) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_w = w_mean(value, .data[[weight_var]]),
      .groups = "drop"
    )
}

multiresponse_by_group <- function(long_df, group_var, label_var = "device_label", weight_var = ".weight") {
  if (!has_rows(long_df) || !all(c(group_var, label_var, weight_var, "respondent_id") %in% names(long_df))) {
    return(tibble::tibble())
  }

  denom <- long_df |>
    dplyr::distinct(respondent_id, .data[[group_var]], .data[[weight_var]]) |>
    dplyr::filter(!is.na(.data[[group_var]])) |>
    dplyr::group_by(group = .data[[group_var]]) |>
    dplyr::summarise(total_weight = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop")

  long_df |>
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[label_var]])) |>
    dplyr::group_by(group = .data[[group_var]], value = .data[[label_var]]) |>
    dplyr::summarise(
      n = dplyr::n_distinct(respondent_id),
      weighted_n = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(denom, by = "group") |>
    dplyr::mutate(
      pct = weighted_n / total_weight,
      pct_label = safe_pct(pct)
    )
}

# -----------------------------------------------------------------------------
# Lecture des données et sorties existantes
# -----------------------------------------------------------------------------

main_rds <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.rds")
main_csv <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.csv")

if (file.exists(main_rds)) {
  df <- readRDS(main_rds)
} else if (file.exists(main_csv)) {
  df <- readr::read_csv(main_csv, show_col_types = FALSE)
} else {
  stop("Base nettoyée introuvable. Lancez d'abord le script 01.")
}

if (!".weight" %in% names(df)) df$.weight <- 1
if (!"weight_none" %in% names(df)) df$weight_none <- 1

if ("exposure2" %in% names(df)) df$exposure2 <- standardize_exposure(df$exposure2)
if ("exposure3" %in% names(df)) df$exposure3 <- standardize_exposure(df$exposure3)

q4_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q4_long.csv"))
q5_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q5_long.csv"))
q8_devices_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q8_devices_long.csv"))
q11_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q11_long.csv"))
q12_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q12_long.csv"))
q13_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q13_long.csv"))
q15_long <- safe_read_csv(file.path(dirs$final, "data_clean", "q15_long.csv"))

for (nm in c("q4_long", "q5_long", "q8_devices_long", "q11_long", "q12_long", "q13_long", "q15_long")) {
  x <- get(nm)
  if (has_rows(x) && "exposure2" %in% names(x)) x$exposure2 <- standardize_exposure(x$exposure2)
  if (has_rows(x) && "exposure3" %in% names(x)) x$exposure3 <- standardize_exposure(x$exposure3)
  assign(nm, x)
}

figure_log <- list(copy_existing_figures())

# -----------------------------------------------------------------------------
# 1. Parcours de formation
# -----------------------------------------------------------------------------

if ("exposure3" %in% names(df) && "year" %in% names(df)) {
  exposure_year <- weighted_distribution(df, "year", "exposure3")
  safe_write_csv(exposure_year, file.path(dirs$report, "tables", "formation_exposition_par_annee.csv"))

  if (has_rows(exposure_year)) {
    p <- exposure_year |>
      dplyr::mutate(group = forcats::fct_inorder(as.character(group))) |>
      ggplot2::ggplot(ggplot2::aes(x = pct, y = group, fill = value)) +
      ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.35) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c(
        "Aucun dispositif" = cols[["brown"]],
        "Autoformation / autre seulement" = cols[["beige"]],
        "Dispositif organisé" = cols[["green"]]
      ), drop = TRUE) +
      ggplot2::labs(
        title = "Exposition aux dispositifs selon l'année de thèse",
        subtitle = "Répartition pondérée dans chaque année.",
        x = "Part pondérée", y = NULL,
        caption = "Source : enquête OSYR."
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_01_exposition_par_annee.png", 1, "Parcours de formation",
      "Exposition aux dispositifs selon l'année de thèse",
      "Répartition pondérée entre absence de dispositif, autoformation et dispositif organisé.",
      width = 12, height = 6.8, priority = 1
    )
  }
}

if (has_rows(q8_devices_long) && "year" %in% names(q8_devices_long)) {
  q8_year <- multiresponse_by_group(q8_devices_long, "year", "device_label")
  safe_write_csv(q8_year, file.path(dirs$report, "tables", "formation_dispositifs_q8_par_annee.csv"))

  if (has_rows(q8_year)) {
    p <- q8_year |>
      dplyr::mutate(
        group = forcats::fct_inorder(as.character(group)),
        value = stringr::str_wrap(as.character(value), 36)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = value, y = group, fill = pct)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Types de dispositifs selon l'année de thèse",
        subtitle = "Question multiréponse Q8 ; part pondérée des répondants de chaque année.",
        x = NULL, y = NULL, fill = "Part"
      ) +
      osyr_theme(base_size = 9.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_02_dispositifs_q8_par_annee.png", 1, "Parcours de formation",
      "Types de dispositifs selon l'année de thèse",
      "Vue détaillée des dispositifs déclarés, sans agrégation.",
      width = 13.5, height = 7.8, priority = 2
    )
  }
}

if (has_rows(q8_devices_long) && "discipline_detail" %in% names(q8_devices_long)) {
  q8_disc <- multiresponse_by_group(q8_devices_long, "discipline_detail", "device_label")
  safe_write_csv(q8_disc, file.path(dirs$report, "tables", "formation_dispositifs_q8_par_discipline.csv"))

  if (has_rows(q8_disc)) {
    p <- q8_disc |>
      dplyr::mutate(
        group = stringr::str_wrap(as.character(group), 30),
        value = stringr::str_wrap(as.character(value), 35)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = value, y = group, fill = pct)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Types de dispositifs selon la discipline détaillée",
        subtitle = "Question multiréponse Q8 ; part pondérée des répondants de chaque discipline.",
        x = NULL, y = NULL, fill = "Part"
      ) +
      osyr_theme(base_size = 9.4) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_03_dispositifs_q8_par_discipline.png", 1, "Parcours de formation",
      "Types de dispositifs selon la discipline détaillée",
      "Permet de distinguer les dispositifs organisés, les formats distanciels et l'autoformation.",
      width = 14.5, height = 8.4, priority = 2
    )
  }
}

if ("training_intensity" %in% names(df) && "exposure3" %in% names(df)) {
  intensity <- weighted_distribution(df, "exposure3", "training_intensity")
  safe_write_csv(intensity, file.path(dirs$report, "tables", "formation_volume_par_exposition.csv"))

  if (has_rows(intensity)) {
    p <- intensity |>
      dplyr::mutate(value = stringr::str_wrap(as.character(value), 28)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct, y = value, fill = group)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c(
        "Aucun dispositif" = cols[["brown"]],
        "Autoformation / autre seulement" = cols[["beige"]],
        "Dispositif organisé" = cols[["green"]]
      ), drop = TRUE) +
      ggplot2::labs(
        title = "Volume déclaré de formation ou d'actions suivies",
        subtitle = "Distribution pondérée selon le type d'exposition.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_04_volume_formation_par_exposition.png", 1, "Parcours de formation",
      "Volume déclaré de formation ou d'actions suivies",
      "Figure de cadrage sur l'intensité d'exposition aux dispositifs.",
      width = 12, height = 6.8, priority = 3
    )
  }
}

if (has_rows(q11_long) && "agree" %in% names(q11_long)) {
  q11_summary <- q11_long |>
    dplyr::filter(!is.na(item_label)) |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(
      n = dplyr::n(),
      pct_agree_w = w_prop(agree, .weight),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(pct_agree_w))
  safe_write_csv(q11_summary, file.path(dirs$report, "tables", "formation_evaluation_q11.csv"))

  if (has_rows(q11_summary)) {
    p <- q11_summary |>
      dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 48), pct_agree_w)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_agree_w, y = item_label)) +
      ggplot2::geom_col(fill = cols[["green"]], width = 0.68) +
      ggplot2::geom_text(ggplot2::aes(label = safe_pct(pct_agree_w)), hjust = -0.1, size = 3.4, color = cols[["dark_green"]]) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, safe_max(q11_summary$pct_agree_w))) +
      ggplot2::labs(
        title = "Évaluation déclarée des formations",
        subtitle = "Part pondérée des répondants en accord avec chaque affirmation Q11.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_05_evaluation_formations_q11.png", 1, "Parcours de formation",
      "Évaluation déclarée des formations",
      "Appréciation des formations par les répondants concernés.",
      width = 12, height = 6.8, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 2. Connaissances
# -----------------------------------------------------------------------------

if (has_rows(q5_long) && all(c("known_well", "used") %in% names(q5_long))) {
  q5_items <- q5_long |>
    dplyr::filter(!is.na(item_label)) |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(
      pct_known_w = w_prop(known_well, .weight),
      pct_used_w = w_prop(used, .weight),
      gap_pp = 100 * (pct_known_w - pct_used_w),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(gap_pp))
  safe_write_csv(q5_items, file.path(dirs$report, "tables", "connaissances_q5_items_gap.csv"))

  if (has_rows(q5_items)) {
    q5_items_long <- q5_items |>
      dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 45), gap_pp)) |>
      dplyr::select(item_label, pct_known_w, pct_used_w) |>
      tidyr::pivot_longer(cols = c(pct_known_w, pct_used_w), names_to = "metric", values_to = "pct") |>
      dplyr::mutate(metric = dplyr::recode(metric, pct_known_w = "Connaît bien", pct_used_w = "A déjà utilisé"))

    p <- q5_items_long |>
      ggplot2::ggplot(ggplot2::aes(x = pct, y = item_label, color = metric)) +
      ggplot2::geom_line(ggplot2::aes(group = item_label), color = "#D0D5DD", linewidth = 0.7) +
      ggplot2::geom_point(size = 2.8) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_color_manual(values = c("Connaît bien" = cols[["green"]], "A déjà utilisé" = cols[["brown"]])) +
      ggplot2::labs(
        title = "Connaissance et usage des outils de science ouverte",
        subtitle = "Écart entre les notions bien connues et les outils déjà utilisés.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme(base_size = 10)

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_10_q5_connaissance_usage_items.png", 2, "Connaissances",
      "Connaissance et usage des outils de science ouverte",
      "Dumbbell chart item par item : connaissance déclarée et usage déclaré.",
      width = 13, height = 8.8, priority = 1
    )
  }
}

score_vars <- c(
  "score_q5_known_well", "score_q5_used", "score_q4_practices_research",
  "score_q13_open_intentions", "score_q13_dont_know",
  "score_q12_incitation", "score_q12_frein", "score_q15_agreement"
)
score_labels <- c(
  score_q5_known_well = "Connaissance Q5",
  score_q5_used = "Usage Q5",
  score_q4_practices_research = "Pratiques Q4",
  score_q13_open_intentions = "Intentions Q13",
  score_q13_dont_know = "NSP Q13",
  score_q12_incitation = "Incitation Q12",
  score_q12_frein = "Frein Q12",
  score_q15_agreement = "Accord Q15"
)

if ("exposure2" %in% names(df)) {
  score_expo <- weighted_score_by_group(df, "exposure2", score_vars)
  safe_write_csv(score_expo, file.path(dirs$report, "tables", "scores_par_exposition.csv"))

  if (has_rows(score_expo)) {
    p <- score_expo |>
      dplyr::mutate(
        score_label = dplyr::recode(score, !!!score_labels, .default = score),
        score_label = forcats::fct_reorder(score_label, mean_w)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = mean_w, y = score_label, fill = group)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c("Aucun dispositif" = cols[["brown"]], "Dispositif organisé" = cols[["green"]]), drop = TRUE) +
      ggplot2::labs(
        title = "Scores synthétiques selon l'exposition aux dispositifs",
        subtitle = "Comparaison pondérée entre répondants sans dispositif et répondants exposés à un dispositif organisé.",
        x = "Moyenne pondérée", y = NULL
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_11_scores_par_exposition.png", 2, "Connaissances",
      "Scores synthétiques selon l'exposition aux dispositifs",
      "Vue transversale des principaux scores utilisés dans le rapport.",
      width = 12.5, height = 7.2, priority = 2
    )
  }
}

if ("year" %in% names(df)) {
  score_year <- weighted_score_by_group(df, "year", c("score_q5_known_well", "score_q5_used", "score_q13_open_intentions"))
  safe_write_csv(score_year, file.path(dirs$report, "tables", "scores_connaissance_usage_intentions_par_annee.csv"))

  if (has_rows(score_year)) {
    p <- score_year |>
      dplyr::mutate(
        score_label = dplyr::recode(score, !!!score_labels, .default = score),
        group = forcats::fct_inorder(as.character(group))
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = group, y = mean_w, group = score_label, color = score_label)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2.4) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_color_manual(values = c("Connaissance Q5" = cols[["green"]], "Usage Q5" = cols[["brown"]], "Intentions Q13" = cols[["dark_green"]]), drop = TRUE) +
      ggplot2::labs(
        title = "Connaissance, usage et intentions selon l'année de thèse",
        subtitle = "Évolution descriptive des scores moyens pondérés.",
        x = NULL, y = "Moyenne pondérée"
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_12_scores_par_annee.png", 2, "Connaissances",
      "Connaissance, usage et intentions selon l'année de thèse",
      "Figure utile pour distinguer effet d'avancement doctoral et exposition aux dispositifs.",
      width = 12.5, height = 6.8, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 3. Pratiques
# -----------------------------------------------------------------------------

if (has_rows(q4_long) && "positive" %in% names(q4_long)) {
  q4_items <- q4_long |>
    dplyr::filter(!is.na(item_label)) |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(pct_positive_w = w_prop(positive, .weight), n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(pct_positive_w))
  safe_write_csv(q4_items, file.path(dirs$report, "tables", "pratiques_q4_items.csv"))

  if (has_rows(q4_items)) {
    p <- q4_items |>
      dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 48), pct_positive_w)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_positive_w, y = item_label)) +
      ggplot2::geom_col(fill = cols[["green"]], width = 0.68) +
      ggplot2::geom_text(ggplot2::aes(label = safe_pct(pct_positive_w)), hjust = -0.1, size = 3.2, color = cols[["dark_green"]]) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, safe_max(q4_items$pct_positive_w))) +
      ggplot2::labs(
        title = "Pratiques de recherche déjà réalisées",
        subtitle = "Items Q4 ; part pondérée des répondants concernés.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme(base_size = 10)

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_20_pratiques_q4_items.png", 3, "Pratiques",
      "Pratiques de recherche déjà réalisées",
      "État des pratiques de recherche hors ou en amont de la science ouverte.",
      width = 12.5, height = 7.8, priority = 1
    )
  }
}

if (has_rows(q5_long) && "used" %in% names(q5_long) && "year" %in% names(q5_long)) {
  q5_used_year <- q5_long |>
    dplyr::filter(!is.na(year), !is.na(item_label)) |>
    dplyr::group_by(year, item_label) |>
    dplyr::summarise(pct_used_w = w_prop(used, .weight), .groups = "drop")
  safe_write_csv(q5_used_year, file.path(dirs$report, "tables", "pratiques_q5_usages_par_annee.csv"))

  if (has_rows(q5_used_year)) {
    p <- q5_used_year |>
      dplyr::mutate(
        year = forcats::fct_inorder(as.character(year)),
        item_label = stringr::str_wrap(item_label, 35)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = item_label, y = year, fill = pct_used_w)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Usages Q5 selon l'année de thèse",
        subtitle = "Part pondérée déclarant avoir déjà utilisé chaque outil ou notion.",
        x = NULL, y = NULL, fill = "Usage"
      ) +
      osyr_theme(base_size = 9.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_21_usages_q5_par_annee.png", 3, "Pratiques",
      "Usages Q5 selon l'année de thèse",
      "Permet de repérer les usages qui apparaissent surtout avec l'avancement doctoral.",
      width = 14, height = 7.4, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 4. Intentions et attitudes
# -----------------------------------------------------------------------------

if (has_rows(q13_long) && "yes" %in% names(q13_long)) {
  q13_exposure <- q13_long |>
    dplyr::filter(!is.na(exposure2), !is.na(item_label)) |>
    dplyr::group_by(exposure2, item_label) |>
    dplyr::summarise(
      pct_yes_w = w_prop(yes, .weight),
      pct_dk_w = if ("dont_know" %in% names(q13_long)) w_prop(dont_know, .weight) else NA_real_,
      .groups = "drop"
    )
  safe_write_csv(q13_exposure, file.path(dirs$report, "tables", "intentions_q13_par_exposition.csv"))

  if (has_rows(q13_exposure)) {
    p <- q13_exposure |>
      dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 42), pct_yes_w, .fun = max)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_yes_w, y = item_label, fill = exposure2)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c("Aucun dispositif" = cols[["brown"]], "Dispositif organisé" = cols[["green"]]), drop = TRUE) +
      ggplot2::labs(
        title = "Intentions de pratiques ouvertes selon l'exposition",
        subtitle = "Items Q13 ; part pondérée de réponses positives.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme(base_size = 10)

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_30_intentions_q13_par_exposition.png", 4, "Intentions et attitudes",
      "Intentions de pratiques ouvertes selon l'exposition",
      "Compare les intentions déclarées des répondants exposés et non exposés à un dispositif organisé.",
      width = 13, height = 8.2, priority = 1
    )
  }

  if ("dont_know" %in% names(q13_long)) {
    q13_dk <- q13_exposure |>
      dplyr::filter(!is.na(pct_dk_w))

    if (has_rows(q13_dk)) {
      p <- q13_dk |>
        dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 42), pct_dk_w, .fun = max)) |>
        ggplot2::ggplot(ggplot2::aes(x = pct_dk_w, y = item_label, fill = exposure2)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
        ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::scale_fill_manual(values = c("Aucun dispositif" = cols[["brown"]], "Dispositif organisé" = cols[["green"]]), drop = TRUE) +
        ggplot2::labs(
          title = "Incertitudes déclarées sur les intentions",
          subtitle = "Items Q13 ; part pondérée de réponses 'je ne sais pas'.",
          x = "Part pondérée", y = NULL
        ) +
        osyr_theme(base_size = 10)

      figure_log[[length(figure_log) + 1]] <- save_plot_final(
        p, "final_31_intentions_q13_je_ne_sais_pas.png", 4, "Intentions et attitudes",
        "Incertitudes déclarées sur les intentions",
        "Met en évidence les pratiques pour lesquelles l'intention est plus fragile ou moins stabilisée.",
        width = 13, height = 8.2, priority = 2
      )
    }
  }
}

if (all(c("score_q5_known_well", "score_q5_used", "score_q13_open_intentions") %in% names(df))) {
  intentions_grad <- df |>
    dplyr::mutate(
      knowledge_band = dplyr::ntile(score_q5_known_well, 4),
      usage_band = dplyr::ntile(score_q5_used, 4)
    ) |>
    dplyr::filter(!is.na(knowledge_band), !is.na(usage_band), !is.na(score_q13_open_intentions)) |>
    dplyr::group_by(knowledge_band, usage_band) |>
    dplyr::summarise(
      n = dplyr::n(),
      intentions_w = w_mean(score_q13_open_intentions, .weight),
      .groups = "drop"
    )
  safe_write_csv(intentions_grad, file.path(dirs$report, "tables", "intentions_selon_connaissance_usage.csv"))

  if (has_rows(intentions_grad)) {
    p <- intentions_grad |>
      ggplot2::ggplot(ggplot2::aes(x = factor(usage_band), y = factor(knowledge_band), fill = intentions_w)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = safe_pct(intentions_w, accuracy = 1)), color = cols[["dark_green"]], size = 3.3) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Intentions selon les niveaux de connaissance et d'usage",
        subtitle = "Quartiles de connaissance Q5 et d'usage Q5 ; score moyen d'intentions Q13.",
        x = "Usage Q5, du plus faible au plus élevé", y = "Connaissance Q5, du plus faible au plus élevé", fill = "Intentions"
      ) +
      osyr_theme(base_size = 10.5) +
      ggplot2::theme(legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_32_intentions_selon_connaissance_usage.png", 4, "Intentions et attitudes",
      "Intentions selon les niveaux de connaissance et d'usage",
      "Analyse de cohérence entre connaissances, usages et intentions déclarées.",
      width = 10.5, height = 7.2, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 5. Perceptions
# -----------------------------------------------------------------------------

if (has_rows(q15_long) && "agree" %in% names(q15_long)) {
  q15_exposure <- q15_long |>
    dplyr::filter(!is.na(exposure2), !is.na(item_label)) |>
    dplyr::group_by(exposure2, item_label) |>
    dplyr::summarise(pct_agree_w = w_prop(agree, .weight), .groups = "drop")
  safe_write_csv(q15_exposure, file.path(dirs$report, "tables", "perceptions_q15_par_exposition.csv"))

  if (has_rows(q15_exposure)) {
    p <- q15_exposure |>
      dplyr::mutate(item_label = forcats::fct_reorder(stringr::str_wrap(item_label, 45), pct_agree_w, .fun = max)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_agree_w, y = item_label, fill = exposure2)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c("Aucun dispositif" = cols[["brown"]], "Dispositif organisé" = cols[["green"]]), drop = TRUE) +
      ggplot2::labs(
        title = "Perceptions de la science ouverte selon l'exposition",
        subtitle = "Items Q15 ; part pondérée d'accord.",
        x = "Part pondérée", y = NULL
      ) +
      osyr_theme(base_size = 10)

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_40_perceptions_q15_par_exposition.png", 5, "Perceptions",
      "Perceptions de la science ouverte selon l'exposition",
      "Permet de distinguer les représentations partagées et celles associées à l'exposition aux dispositifs.",
      width = 13, height = 8.5, priority = 1
    )
  }
}

if (has_rows(q12_long) && all(c("incitation", "frein") %in% names(q12_long))) {
  q12_exposure <- q12_long |>
    dplyr::filter(!is.na(exposure2), !is.na(item_label)) |>
    dplyr::group_by(exposure2, item_label) |>
    dplyr::summarise(
      pct_incitation_w = w_prop(incitation, .weight),
      pct_frein_w = w_prop(frein, .weight),
      .groups = "drop"
    )
  safe_write_csv(q12_exposure, file.path(dirs$report, "tables", "perceptions_q12_environnement_par_exposition.csv"))

  if (has_rows(q12_exposure)) {
    q12_plot <- q12_exposure |>
      dplyr::select(exposure2, item_label, pct_incitation_w, pct_frein_w) |>
      tidyr::pivot_longer(cols = c(pct_incitation_w, pct_frein_w), names_to = "metric", values_to = "pct") |>
      dplyr::mutate(
        metric = dplyr::recode(metric, pct_incitation_w = "Incitation", pct_frein_w = "Frein"),
        item_label = stringr::str_wrap(item_label, 40)
      )

    p <- q12_plot |>
      ggplot2::ggplot(ggplot2::aes(x = item_label, y = exposure2, fill = pct)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      ggplot2::facet_wrap(~ metric) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Environnement perçu : incitations et freins",
        subtitle = "Items Q12 selon l'exposition aux dispositifs.",
        x = NULL, y = NULL, fill = "Part"
      ) +
      osyr_theme(base_size = 9.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_41_environnement_q12_par_exposition.png", 5, "Perceptions",
      "Environnement perçu : incitations et freins",
      "Lecture croisée des dimensions environnementales perçues comme favorables ou défavorables.",
      width = 14, height = 7.5, priority = 2
    )
  }
}

if (all(c("score_q12_incitation", "score_q12_frein", "score_q15_agreement") %in% names(df))) {
  env_grid <- df |>
    dplyr::mutate(
      incitation_band = dplyr::ntile(score_q12_incitation, 4),
      frein_band = dplyr::ntile(score_q12_frein, 4)
    ) |>
    dplyr::filter(!is.na(incitation_band), !is.na(frein_band), !is.na(score_q15_agreement)) |>
    dplyr::group_by(incitation_band, frein_band) |>
    dplyr::summarise(agreement_w = w_mean(score_q15_agreement, .weight), n = dplyr::n(), .groups = "drop")
  safe_write_csv(env_grid, file.path(dirs$report, "tables", "perceptions_q15_selon_q12.csv"))

  if (has_rows(env_grid)) {
    p <- env_grid |>
      ggplot2::ggplot(ggplot2::aes(x = factor(frein_band), y = factor(incitation_band), fill = agreement_w)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = safe_pct(agreement_w, accuracy = 1)), color = cols[["dark_green"]], size = 3.2) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Perceptions selon l'environnement perçu",
        subtitle = "Quartiles d'incitation et de frein Q12 ; accord moyen Q15.",
        x = "Freins perçus", y = "Incitations perçues", fill = "Accord Q15"
      ) +
      osyr_theme(base_size = 10.5) +
      ggplot2::theme(legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_42_perceptions_selon_environnement.png", 5, "Perceptions",
      "Perceptions selon l'environnement perçu",
      "Analyse des liens entre environnement perçu et accord avec les affirmations sur la science ouverte.",
      width = 10.5, height = 7.2, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 6. Profils et analyses transversales
# -----------------------------------------------------------------------------

profile_scores <- score_vars[score_vars %in% names(df)]
profile_scores <- profile_scores[!profile_scores %in% c("score_q13_dont_know")]

if (length(profile_scores) >= 4) {
  profile_df <- df |>
    dplyr::select(respondent_id, .weight, dplyr::all_of(profile_scores), dplyr::any_of(c("exposure3", "year", "discipline_detail", "language_group"))) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(profile_scores), ~ !is.na(.x)))

  if (nrow(profile_df) >= 50) {
    x <- profile_df |>
      dplyr::select(dplyr::all_of(profile_scores)) |>
      scale()

    pca <- stats::prcomp(x, center = TRUE, scale. = TRUE)
    k <- min(4, max(2, floor(nrow(profile_df) / 50)))
    km <- stats::kmeans(x, centers = k, nstart = 50)

    profile_coord <- profile_df |>
      dplyr::mutate(
        profile = paste0("Profil ", km$cluster),
        dim1 = pca$x[, 1],
        dim2 = pca$x[, 2]
      )

    safe_write_csv(profile_coord, file.path(dirs$report, "tables", "profils_coordonnees.csv"))

    profile_means <- profile_coord |>
      dplyr::select(profile, .weight, dplyr::all_of(profile_scores)) |>
      tidyr::pivot_longer(cols = dplyr::all_of(profile_scores), names_to = "score", values_to = "value") |>
      dplyr::group_by(profile, score) |>
      dplyr::summarise(mean_w = w_mean(value, .weight), .groups = "drop") |>
      dplyr::mutate(score_label = dplyr::recode(score, !!!score_labels, .default = score))

    safe_write_csv(profile_means, file.path(dirs$report, "tables", "profils_moyennes_scores.csv"))

    p <- profile_coord |>
      ggplot2::ggplot(ggplot2::aes(x = dim1, y = dim2, color = profile)) +
      ggplot2::geom_point(alpha = 0.55, size = 1.8) +
      ggplot2::scale_color_manual(values = c(cols[["green"]], cols[["brown"]], cols[["dark_green"]], cols[["beige"]], cols[["grey"]])) +
      ggplot2::labs(
        title = "Profils de répondants selon les scores de science ouverte",
        subtitle = "Projection ACP et classification exploratoire.",
        x = "Axe 1", y = "Axe 2"
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_50_profils_acp_scores.png", 6, "Profils et analyses transversales",
      "Profils de répondants selon les scores de science ouverte",
      "Analyse exploratoire destinée à repérer des configurations de connaissances, usages, intentions et perceptions.",
      width = 11.5, height = 7.2, priority = 1
    )

    p <- profile_means |>
      dplyr::mutate(score_label = stringr::str_wrap(score_label, 30)) |>
      ggplot2::ggplot(ggplot2::aes(x = score_label, y = profile, fill = mean_w)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.35) +
      ggplot2::geom_text(ggplot2::aes(label = safe_pct(mean_w, accuracy = 1)), size = 3, color = cols[["dark_green"]]) +
      ggplot2::scale_fill_gradient(low = cols[["light_grey"]], high = cols[["green"]], labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Caractérisation des profils de répondants",
        subtitle = "Moyennes pondérées des scores dans chaque profil exploratoire.",
        x = NULL, y = NULL, fill = "Score"
      ) +
      osyr_theme(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "right")

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_51_profils_moyennes_scores.png", 6, "Profils et analyses transversales",
      "Caractérisation des profils de répondants",
      "Tableau graphique des scores moyens par profil exploratoire.",
      width = 12.5, height = 6.8, priority = 1
    )
  }
}

if ("exposure3" %in% names(df)) {
  focus_auto_non <- df |>
    dplyr::filter(exposure3 %in% c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé")) |>
    dplyr::select(exposure3, .weight, dplyr::any_of(score_vars)) |>
    tidyr::pivot_longer(cols = dplyr::any_of(score_vars), names_to = "score", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(exposure3, score) |>
    dplyr::summarise(mean_w = w_mean(value, .weight), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(score_label = dplyr::recode(score, !!!score_labels, .default = score))

  safe_write_csv(focus_auto_non, file.path(dirs$report, "tables", "profils_non_formes_autoformes_scores.csv"))

  if (has_rows(focus_auto_non)) {
    p <- focus_auto_non |>
      dplyr::mutate(score_label = forcats::fct_reorder(stringr::str_wrap(score_label, 30), mean_w, .fun = max)) |>
      ggplot2::ggplot(ggplot2::aes(x = mean_w, y = score_label, fill = exposure3)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.74), width = 0.62) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c(
        "Aucun dispositif" = cols[["brown"]],
        "Autoformation / autre seulement" = cols[["beige"]],
        "Dispositif organisé" = cols[["green"]]
      ), drop = TRUE) +
      ggplot2::labs(
        title = "Non formés, autoformés et exposés à un dispositif organisé",
        subtitle = "Comparaison des scores moyens pondérés.",
        x = "Moyenne pondérée", y = NULL
      ) +
      osyr_theme()

    figure_log[[length(figure_log) + 1]] <- save_plot_final(
      p, "final_52_focus_non_formes_autoformes.png", 6, "Profils et analyses transversales",
      "Non formés, autoformés et exposés à un dispositif organisé",
      "Focus sur deux groupes explicitement mentionnés dans le plan de dépouillement.",
      width = 13, height = 7.5, priority = 2
    )
  }
}

# -----------------------------------------------------------------------------
# 7. Précautions méthodologiques
# -----------------------------------------------------------------------------

coverage <- osyr_final_plan_registry() |>
  dplyr::select(section, bloc, objectif, questions_principales) |>
  dplyr::left_join(
    dplyr::bind_rows(figure_log) |>
      dplyr::filter(available) |>
      dplyr::count(section, name = "n_figures_disponibles"),
    by = "section"
  ) |>
  dplyr::mutate(
    n_figures_disponibles = tidyr::replace_na(n_figures_disponibles, 0L),
    statut_couverture = dplyr::case_when(
      n_figures_disponibles >= 5 ~ "Couverture forte",
      n_figures_disponibles >= 3 ~ "Couverture correcte",
      n_figures_disponibles >= 1 ~ "Couverture partielle",
      TRUE ~ "À compléter"
    )
  )

safe_write_csv(coverage, file.path(dirs$report, "tables", "couverture_plan_de_depouillement.csv"))

if (has_rows(coverage)) {
  p <- coverage |>
    dplyr::mutate(bloc = forcats::fct_reorder(bloc, section)) |>
    ggplot2::ggplot(ggplot2::aes(x = n_figures_disponibles, y = bloc, fill = statut_couverture)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::scale_fill_manual(values = c(
      "Couverture forte" = cols[["green"]],
      "Couverture correcte" = cols[["dark_green"]],
      "Couverture partielle" = cols[["brown"]],
      "À compléter" = cols[["grey"]]
    ), drop = TRUE) +
    ggplot2::labs(
      title = "Couverture du plan de dépouillement par les sorties disponibles",
      subtitle = "Nombre de figures mobilisables par section du rapport final.",
      x = "Nombre de figures", y = NULL
    ) +
    osyr_theme()

  figure_log[[length(figure_log) + 1]] <- save_plot_final(
    p, "final_60_couverture_plan_depouillement.png", 7, "Précautions méthodologiques",
    "Couverture du plan de dépouillement par les sorties disponibles",
    "Diagnostic de production : il permet de vérifier les sections encore trop peu couvertes.",
    width = 11.5, height = 6.8, priority = 1
  )
}

# -----------------------------------------------------------------------------
# Catalogue consolidé des figures finales
# -----------------------------------------------------------------------------

figure_catalog_final <- dplyr::bind_rows(figure_log) |>
  dplyr::filter(!is.na(file)) |>
  dplyr::distinct(section, bloc, titre, file, .keep_all = TRUE) |>
  dplyr::mutate(
    available = file.exists(path),
    section = as.integer(section)
  ) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog_final, file.path(dirs$report, "tables", "catalogue_figures_finales.csv"))

missing_final <- figure_catalog_final |>
  dplyr::filter(!available)

safe_write_csv(missing_final, file.path(dirs$report, "tables", "figures_finales_manquantes.csv"))

message("Analyses finales préparées : ", normalizePath(dirs$report, mustWork = FALSE))
message("Figures disponibles : ", sum(figure_catalog_final$available, na.rm = TRUE))
