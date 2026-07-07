# =============================================================================
# SCRIPT 02 — GÉNÉRATION DU RAPPORT WORD OSYR
# =============================================================================
# Rôle dans le workflow
#   Ce script lit les sorties de `outputs_osyr_v2_final/` produites par le
#   script 01 et génère un rapport Word commenté, modifiable et partageable.
#
# À lancer seul si le script 01 a déjà été exécuté :
#   source("02_generer_rapport_word.R")
#
# Sortie principale :
#   outputs_osyr_v2_rapport_word/rapport_commenté_OSYR_V2_v4_analytique_approfondi.docx
# =============================================================================

# =============================================================================
# OSYR — Génération d'un rapport Word commenté
# Version : 2026-06-19 — v4 analytique approfondie
#
# À lancer APRÈS le workflow d'analyse final :
#   source("01_analyse_osyr_base_et_modeles.R")
#
# Ce script :
#   1) lit les sorties déjà produites dans outputs_osyr_v2_final/ ;
#   2) régénère quelques graphiques plus lisibles pour le rapport Word ;
#   3) construit un rapport Word commenté avec figures, tableaux et interprétation ;
#   4) exporte un .docx prêt à relire/compléter.
#   5) ajoute une lecture analytique plus développée et un atlas de figures complémentaires.
#
# Important :
#   - Les analyses restent descriptives/associatives, non causales.
#   - La variable langue du questionnaire est interprétée prudemment comme proxy
#     de profil international, et non comme mesure de nationalité.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------

required_packages <- c(
  "tidyverse", "readr", "stringr", "forcats", "scales", "glue",
  "officer", "flextable", "fs"
)

missing_packages <- required_packages[
  !purrr::map_lgl(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Chemins
# -----------------------------------------------------------------------------

analysis_dir <- "outputs_osyr_v2_final"
tables_dir   <- file.path(analysis_dir, "tables")
models_dir   <- file.path(analysis_dir, "models")
text_dir     <- file.path(analysis_dir, "text_analysis")
figures_dir  <- file.path(analysis_dir, "figures")

report_dir   <- "outputs_osyr_v2_rapport_word"
report_fig_dir <- file.path(report_dir, "figures_retravaillees")
report_docx  <- file.path(report_dir, "rapport_commenté_OSYR_V2_v4_analytique_approfondi.docx")

# Option importante :
# TRUE  = le rapport Word utilise en priorité les figures originales du workflow final,
#         celles que vous préférez esthétiquement.
# FALSE = le rapport utilise en priorité les figures retravaillées générées par ce script.
use_original_figures_first <- TRUE

# Option secondaire :
# Les figures retravaillées peuvent rester utiles comme tests, mais elles ne seront
# pas utilisées en priorité si use_original_figures_first = TRUE.
generate_reworked_figures <- FALSE

# Version v4 :
# - inclut une lecture analytique plus longue ;
# - ajoute les figures transversales, les tableaux de synthèse et une annexe graphique ;
# - insère automatiquement les PNG disponibles dans outputs_osyr_v2_final/figures
#   qui n'ont pas déjà été utilisés dans le corps du rapport.
include_extra_figure_atlas <- TRUE
max_extra_figures <- 80

fs::dir_create(report_dir)
fs::dir_create(report_fig_dir)

if (!dir.exists(analysis_dir)) {
  stop(
    "Le dossier ", analysis_dir, " est introuvable.\n",
    "Lancez d'abord le workflow final d'analyse, puis relancez ce script."
  )
}

# -----------------------------------------------------------------------------
# 2. Fonctions utilitaires
# -----------------------------------------------------------------------------

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    warning("Fichier introuvable : ", path)
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

tbl <- function(name, subdir = "tables") {
  read_csv_safe(file.path(analysis_dir, subdir, paste0(name, ".csv")))
}

fig <- function(name) {
  file.path(figures_dir, name)
}

report_fig <- function(name) {
  file.path(report_fig_dir, name)
}

first_existing <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(NA_character_)
  paths[1]
}

# Choix des figures pour le rapport :
# par défaut, on privilégie les figures originales du workflow final, car elles sont
# plus cohérentes avec les sorties validées. Les figures retravaillées ne sont
# utilisées qu'en secours, ou si use_original_figures_first = FALSE.
choose_figure <- function(original, reworked = NULL) {
  original_paths <- purrr::map_chr(original, fig)
  reworked_paths <- if (is.null(reworked)) character(0) else purrr::map_chr(reworked, report_fig)

  if (isTRUE(use_original_figures_first)) {
    first_existing(c(original_paths, reworked_paths))
  } else {
    first_existing(c(reworked_paths, original_paths))
  }
}

# Recherche souple d'une figure quand le nom exact peut varier selon la version
# du workflow principal.
find_figure_by_pattern <- function(candidates = character(), patterns = character()) {
  candidate_paths <- purrr::map_chr(candidates, fig)
  hit <- first_existing(candidate_paths)
  if (!is.na(hit)) return(hit)

  if (!dir.exists(figures_dir)) return(NA_character_)

  all_png <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(all_png) == 0) return(NA_character_)

  base <- basename(all_png)
  for (pat in patterns) {
    idx <- stringr::str_detect(stringr::str_to_lower(base), stringr::str_to_lower(pat))
    if (any(idx, na.rm = TRUE)) return(all_png[which(idx)[1]])
  }

  NA_character_
}

fmt_pct <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "NA", scales::percent(x, accuracy = accuracy, decimal.mark = ","))
}

fmt_pp <- function(x, accuracy = 0.1) {
  ifelse(
    is.na(x),
    "NA",
    paste0(ifelse(x >= 0, "+", ""), scales::number(x, accuracy = accuracy, decimal.mark = ","), " pts")
  )
}

get_value <- function(data, filter_col, filter_value, value_col) {
  if (nrow(data) == 0) return(NA_real_)
  if (!all(c(filter_col, value_col) %in% names(data))) return(NA_real_)
  x <- data |>
    dplyr::filter(.data[[filter_col]] == filter_value) |>
    dplyr::pull(.data[[value_col]])
  if (length(x) == 0) NA_real_ else x[1]
}

clean_label <- function(x, width = 55) {
  x |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish() |>
    stringr::str_wrap(width = width)
}

# Normalise les sorties de modèles.
# Selon la version exacte du workflow, certains fichiers contiennent déjà
# estimate_pp / conf_low_pp / conf_high_pp ; d'autres contiennent seulement
# estimate / conf.low / conf.high. Cette fonction rend les noms homogènes.
normalize_model_effects <- function(df) {
  if (nrow(df) == 0) return(df)

  if (!"outcome" %in% names(df)) {
    df$outcome <- NA_character_
  }

  if (!"score_label" %in% names(df)) {
    df$score_label <- if ("outcome" %in% names(df)) as.character(df$outcome) else NA_character_
  }

  if (!"score_label_clean" %in% names(df)) {
    df$score_label_clean <- as.character(df$score_label)
  }

  if (!"estimate_pp" %in% names(df)) {
    df$estimate_pp <- if ("estimate" %in% names(df)) 100 * df$estimate else NA_real_
  }

  if (!"conf_low_pp" %in% names(df)) {
    df$conf_low_pp <- if ("conf.low" %in% names(df)) 100 * df$conf.low else NA_real_
  }

  if (!"conf_high_pp" %in% names(df)) {
    df$conf_high_pp <- if ("conf.high" %in% names(df)) 100 * df$conf.high else NA_real_
  }

  if (!"p.value" %in% names(df)) {
    df$p.value <- NA_real_
  }

  df
}

normalize_interactions <- function(df) {
  df <- normalize_model_effects(df)

  if (nrow(df) == 0) return(df)

  if (!"term" %in% names(df)) {
    df$term <- NA_character_
  }

  if (!"interaction_label" %in% names(df)) {
    df$interaction_label <- as.character(df$term)
  }

  if (!"evidence" %in% names(df)) {
    df$evidence <- NA_character_
  }

  df
}

# Palette cohérente avec les figures précédentes.
osyr_cols <- c(
  navy   = "#17324D",
  teal   = "#2A9D8F",
  coral  = "#E76F51",
  orange = "#F4A261",
  blue   = "#3B82F6",
  grey   = "#98A2B3",
  light  = "#F2F4F7"
)

exposure_cols <- c(
  "Aucun dispositif" = osyr_cols["coral"],
  "Dispositif organisé" = osyr_cols["teal"],
  "Autoformation / autre seulement" = osyr_cols["orange"]
)

theme_report <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 5,
        color = osyr_cols["navy"],
        lineheight = 1.05
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size + 1,
        color = "#475467",
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 2,
        color = "#667085",
        hjust = 0,
        margin = ggplot2::margin(t = 12)
      ),
      axis.text = ggplot2::element_text(color = "#344054"),
      axis.title = ggplot2::element_text(color = "#344054"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#EAECF0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

save_report_plot <- function(plot, filename, width = 12, height = 7.5) {
  path <- report_fig(filename)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = 340,
    bg = "white"
  )
  invisible(path)
}

# -----------------------------------------------------------------------------
# 3. Lecture des sorties utiles
# -----------------------------------------------------------------------------

sample_year       <- tbl("sample_year")
sample_language   <- tbl("sample_language")
sample_exposure   <- tbl("sample_exposure3")
cross_lang        <- tbl("cross_exposure_by_language")
cross_year        <- tbl("cross_exposure_by_year")
cross_disc        <- tbl("cross_exposure_by_discipline")

q5_known          <- tbl("q5_known_well_overall")
q5_used           <- tbl("q5_used_overall")
q13_intentions    <- tbl("q13_intentions_yes_overall")
q15_agreement     <- tbl("q15_agreement_overall")

q5_known_diff     <- tbl("q5_known_diff_exposure")
q5_used_diff      <- tbl("q5_used_diff_exposure")
score_diff        <- tbl("score_diff_exposure2")

model_effects     <- tbl("score_models_exposure_effects", subdir = "models") |> normalize_model_effects()
model_summary     <- tbl("score_models_exposure_effects_summary", subdir = "models")
model_interactions <- tbl("score_models_interaction_terms", subdir = "models") |> normalize_interactions()
model_predictions <- tbl("predictions_interactions_score_q5_known_well", subdir = "models")

q3_concepts       <- tbl("q3_concepts_overall", subdir = "text_analysis")
# Ces tables peuvent manquer si le workflow principal a été interrompu avant
# l'analyse textuelle. Le rapport s'appuie alors simplement sur les figures déjà
# présentes, si elles existent.
q3_diff_exposure  <- tbl("q3_concepts_diff_exposure2", subdir = "text_analysis")
q3_diff_language  <- tbl("q3_concepts_diff_language", subdir = "text_analysis")
figure_captions   <- tbl("figure_captions")

# -----------------------------------------------------------------------------
# 4. Graphiques retravaillés pour le rapport Word
# -----------------------------------------------------------------------------
if (generate_reworked_figures) {
# L'idée est de produire une version plus lisible des figures les plus difficiles :
# - éviter les lignes reliant des modalités catégorielles ;
# - filtrer les interactions pour ne garder que les signaux vraiment lisibles ;
# - privilégier les points et intervalles de confiance ;
# - utiliser des titres plus interprétatifs.

# 4.1 Exposition organisée par langue : version simple et lisible.
if (nrow(cross_lang) > 0 && all(c("row_category", "col_category", "pct_row") %in% names(cross_lang))) {
  exposure_lang_simple <- cross_lang |>
    dplyr::filter(col_category == "Dispositif organisé") |>
    dplyr::mutate(
      row_category = forcats::fct_reorder(row_category, pct_row),
      label = fmt_pct(pct_row, accuracy = 1)
    )

  p_expo_lang <- exposure_lang_simple |>
    ggplot2::ggplot(ggplot2::aes(x = pct_row, y = row_category)) +
    ggplot2::geom_col(fill = osyr_cols["teal"], width = 0.62) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      hjust = -0.15,
      size = 4,
      color = osyr_cols["navy"],
      fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, min(1, max(exposure_lang_simple$pct_row, na.rm = TRUE) * 1.18))
    ) +
    ggplot2::labs(
      title = "Les répondants au questionnaire anglais sont plus souvent exposés",
      subtitle = "Part pondérée ayant suivi au moins un dispositif organisé de science ouverte.",
      x = "Part exposée à un dispositif organisé",
      y = NULL,
      caption = "Lecture : la langue du questionnaire est un proxy prudent de profil international, pas une mesure de nationalité."
    ) +
    theme_report(base_size = 13)

  save_report_plot(p_expo_lang, "R01_exposition_organisee_par_langue.png", width = 10, height = 5.2)
}

# 4.2 Effets ajustés : version sobre, lisible dans Word.
if (nrow(model_effects) > 0) {
  model_plot <- model_effects |>
    dplyr::mutate(
      score_label_clean = dplyr::coalesce(
        as.character(score_label_clean),
        as.character(score_label),
        as.character(outcome)
      ),
      estimate_pp = estimate_pp,
      conf_low_pp = conf_low_pp,
      conf_high_pp = conf_high_pp,
      group = dplyr::case_when(
        !is.na(p.value) & p.value < 0.05 & conf_low_pp > 0 ~ "Association positive claire",
        !is.na(p.value) & p.value < 0.05 & conf_high_pp < 0 ~ "Association négative claire",
        TRUE ~ "Pas d'association claire"
      ),
      label = fmt_pp(estimate_pp, accuracy = 0.1),
      score_label_plot = clean_label(score_label_clean, width = 42),
      score_label_plot = forcats::fct_reorder(score_label_plot, estimate_pp)
    )

  model_cols <- c(
    "Association positive claire" = osyr_cols["teal"],
    "Association négative claire" = osyr_cols["coral"],
    "Pas d'association claire" = osyr_cols["grey"]
  )

  p_model_clean <- model_plot |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = score_label_plot, color = group)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.5) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
      height = 0.17,
      linewidth = 1
    ) +
    ggplot2::geom_point(size = 3.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      hjust = dplyr::if_else(model_plot$estimate_pp >= 0, -0.12, 1.12),
      size = 3.5,
      color = osyr_cols["navy"],
      fontface = "bold"
    ) +
    ggplot2::scale_color_manual(values = model_cols, drop = TRUE) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, " pts"),
      expand = ggplot2::expansion(mult = c(0.10, 0.16))
    ) +
    ggplot2::labs(
      title = "Ce qui reste associé à l'exposition après ajustement",
      subtitle = "Différences ajustées en points de pourcentage entre exposés et non exposés.",
      x = "Différence ajustée",
      y = NULL,
      caption = "Lecture : les barres indiquent les intervalles de confiance à 95 %. Les effets gris ne permettent pas de conclure à une association claire."
    ) +
    theme_report(base_size = 12.5) +
    ggplot2::coord_cartesian(clip = "off")

  save_report_plot(p_model_clean, "R02_effets_ajustes_lisibles.png", width = 12.5, height = 7.3)
}

# 4.3 Prédictions ajustées : remplacer les lignes par des points décalés.
# Les lignes sont souvent moins appropriées ici, car année/discipline/langue sont
# des modalités catégorielles. Les points + intervalles sont plus honnêtes et lisibles.
plot_prediction_points <- function(predictions, moderator, filename, title, subtitle, horizontal = TRUE) {
  if (nrow(predictions) == 0 || !moderator %in% names(predictions)) return(invisible(NULL))

  tab <- predictions |>
    dplyr::filter(moderator == !!moderator) |>
    dplyr::filter(!is.na(.data[[moderator]]), !is.na(exposure2), !is.na(fit)) |>
    dplyr::mutate(
      moderator_value = .data[[moderator]],
      exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé")),
      fit_label = scales::percent(fit, accuracy = 1, decimal.mark = ",")
    )

  if (nrow(tab) == 0) return(invisible(NULL))

  dodge <- ggplot2::position_dodge(width = 0.52)

  if (horizontal) {
    tab <- tab |>
      dplyr::mutate(moderator_value = forcats::fct_rev(factor(moderator_value)))

    p <- tab |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = fit,
          y = moderator_value,
          color = exposure2
        )
      ) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        height = 0.22,
        linewidth = 0.9,
        position = dodge,
        alpha = 0.8
      ) +
      ggplot2::geom_point(size = 3.4, position = dodge) +
      ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, min(1, max(tab$conf.high, na.rm = TRUE) * 1.12))
      ) +
      ggplot2::scale_color_manual(values = exposure_cols[c("Aucun dispositif", "Dispositif organisé")]) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = "Score Q5 prédit",
        y = NULL,
        caption = "Lecture : points = valeurs ajustées prédites ; barres = intervalles de confiance à 95 %. Les contrôles sont fixés à leur modalité la plus fréquente."
      ) +
      theme_report(base_size = 12.5)
  } else {
    p <- tab |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = moderator_value,
          y = fit,
          color = exposure2
        )
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        linewidth = 0.9,
        position = dodge,
        alpha = 0.8
      ) +
      ggplot2::geom_point(size = 3.4, position = dodge) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, min(1, max(tab$conf.high, na.rm = TRUE) * 1.12))
      ) +
      ggplot2::scale_color_manual(values = exposure_cols[c("Aucun dispositif", "Dispositif organisé")]) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = NULL,
        y = "Score Q5 prédit",
        caption = "Lecture : points = valeurs ajustées prédites ; barres = intervalles de confiance à 95 %. Les contrôles sont fixés à leur modalité la plus fréquente."
      ) +
      theme_report(base_size = 12.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18, hjust = 1))
  }

  save_report_plot(p, filename, width = 11.5, height = ifelse(horizontal, 6.5, 6.3))
}

plot_prediction_points(
  model_predictions,
  "year",
  "R03_prediction_connaissance_annee_points.png",
  "La connaissance ajustée reste plus élevée chez les exposés",
  "Score de connaissance Q5 selon l'année de thèse et l'exposition.",
  horizontal = FALSE
)

plot_prediction_points(
  model_predictions,
  "discipline_broad",
  "R04_prediction_connaissance_discipline_points.png",
  "Un écart exposés/non exposés visible dans plusieurs disciplines",
  "Score de connaissance Q5 selon le domaine disciplinaire et l'exposition.",
  horizontal = TRUE
)

plot_prediction_points(
  model_predictions,
  "language_group",
  "R05_prediction_connaissance_langue_points.png",
  "La langue du questionnaire nuance les niveaux de connaissance",
  "Score de connaissance Q5 selon la langue du questionnaire et l'exposition.",
  horizontal = TRUE
)

# 4.4 Interactions : figure allégée.
# L'ancienne version était informative mais trop dense. On garde seulement les
# interactions statistiquement claires ; si aucune n'existe, on produit une
# figure de note indiquant qu'il faut rester prudent.
if (nrow(model_interactions) > 0) {
  inter <- model_interactions |>
    dplyr::mutate(
      estimate_pp = estimate_pp,
      conf_low_pp = conf_low_pp,
      conf_high_pp = conf_high_pp,
      interaction_label = dplyr::coalesce(as.character(interaction_label), as.character(term)),
      label = fmt_pp(estimate_pp, accuracy = 0.1),
      evidence_clean = dplyr::case_when(
        p.value < 0.05 & conf_low_pp > 0 ~ "Interaction positive claire",
        p.value < 0.05 & conf_high_pp < 0 ~ "Interaction négative claire",
        TRUE ~ "Interaction incertaine"
      )
    ) |>
    dplyr::filter(evidence_clean != "Interaction incertaine") |>
    dplyr::arrange(dplyr::desc(abs(estimate_pp))) |>
    dplyr::slice_head(n = 10)

  if (nrow(inter) > 0) {
    inter_cols <- c(
      "Interaction positive claire" = osyr_cols["teal"],
      "Interaction négative claire" = osyr_cols["coral"]
    )

    inter <- inter |>
      dplyr::mutate(
        label_plot = paste0(
          clean_label(score_label, 34),
          "\n",
          clean_label(interaction_label, 46)
        ),
        label_plot = forcats::fct_reorder(label_plot, estimate_pp)
      )

    p_inter_clean <- inter |>
      ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = label_plot, color = evidence_clean)) +
      ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.5) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
        height = 0.16,
        linewidth = 0.9
      ) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        hjust = dplyr::if_else(inter$estimate_pp >= 0, -0.12, 1.12),
        size = 3.1,
        color = osyr_cols["navy"],
        fontface = "bold"
      ) +
      ggplot2::scale_color_manual(values = inter_cols, drop = TRUE) +
      ggplot2::scale_x_continuous(
        labels = function(x) paste0(x, " pts"),
        expand = ggplot2::expansion(mult = c(0.15, 0.18))
      ) +
      ggplot2::labs(
        title = "Les rares interactions lisibles doivent rester exploratoires",
        subtitle = "Termes d'interaction statistiquement clairs uniquement.",
        x = "Différence additionnelle associée au groupe",
        y = NULL,
        caption = "Lecture : un terme d'interaction indique si l'écart exposés/non exposés est plus fort ou plus faible dans un groupe donné que dans le groupe de référence."
      ) +
      theme_report(base_size = 11.5) +
      ggplot2::coord_cartesian(clip = "off")

    save_report_plot(p_inter_clean, "R06_interactions_claires_lisibles.png", width = 13, height = 7.6)
  } else {
    p_inter_note <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0,
        y = 0,
        label = "Aucune interaction suffisamment claire à représenter simplement.\nLes variations par année, discipline et langue doivent être lues comme exploratoires.",
        size = 5,
        color = osyr_cols["navy"],
        fontface = "bold",
        lineheight = 1.15
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = "Interactions : pas de signal simple et robuste",
        subtitle = "Les modèles d'interaction sont conservés dans les tableaux techniques."
      ) +
      theme_report(base_size = 13)

    save_report_plot(p_inter_note, "R06_interactions_claires_lisibles.png", width = 10, height = 4.5)
  }
}


} # fin generate_reworked_figures

# -----------------------------------------------------------------------------
# 5. Préparation des indicateurs textuels
# -----------------------------------------------------------------------------

kpi_exposed <- get_value(sample_exposure, "category", "Dispositif organisé", "pct_w")
kpi_no_device <- get_value(sample_exposure, "category", "Aucun dispositif", "pct_w")
kpi_auto <- get_value(sample_exposure, "category", "Autoformation / autre seulement", "pct_w")
kpi_english <- get_value(sample_language, "category", "Questionnaire en anglais", "pct_w")

known_effect <- model_effects |>
  dplyr::filter(stringr::str_detect(dplyr::coalesce(score_label_clean, score_label, outcome), "Notions et outils bien connus")) |>
  dplyr::pull(estimate_pp)
known_effect <- if (length(known_effect) == 0) NA_real_ else known_effect[1]

used_effect <- model_effects |>
  dplyr::filter(stringr::str_detect(dplyr::coalesce(score_label_clean, score_label, outcome), "déjà utilisés")) |>
  dplyr::pull(estimate_pp)
used_effect <- if (length(used_effect) == 0) NA_real_ else used_effect[1]

incitation_effect <- model_effects |>
  dplyr::filter(stringr::str_detect(dplyr::coalesce(score_label_clean, score_label, outcome), "incitatif")) |>
  dplyr::pull(estimate_pp)
incitation_effect <- if (length(incitation_effect) == 0) NA_real_ else incitation_effect[1]

# Table modèle pour Word.
model_table_word <- model_effects |>
  dplyr::mutate(
    dimension = dplyr::coalesce(as.character(score_label_clean), as.character(score_label), as.character(outcome)),
    estimate_pp = estimate_pp,
    conf_low_pp = conf_low_pp,
    conf_high_pp = conf_high_pp,
    effet = fmt_pp(estimate_pp, 0.1),
    ic95 = paste0("[", fmt_pp(conf_low_pp, 0.1), " ; ", fmt_pp(conf_high_pp, 0.1), "]"),
    interpretation = dplyr::case_when(
      p.value < 0.05 & conf_low_pp > 0 & estimate_pp >= 8 ~ "Association positive forte",
      p.value < 0.05 & conf_low_pp > 0 ~ "Association positive modérée/faible",
      p.value < 0.05 & conf_high_pp < 0 ~ "Association négative claire",
      TRUE ~ "Pas d'association claire"
    )
  ) |>
  dplyr::select(Dimension = dimension, `Effet ajusté` = effet, `IC 95 %` = ic95, Interprétation = interpretation)

# Top connaissances et usages.
top_known_word <- q5_known_diff |>
  dplyr::filter(!is.na(diff_pp)) |>
  dplyr::arrange(dplyr::desc(diff_pp)) |>
  dplyr::slice_head(n = 6) |>
  dplyr::transmute(
    Notion = stringr::str_replace_all(item_label, "\\n", " "),
    `Écart exposés - non exposés` = fmt_pp(diff_pp, 0.1)
  )

top_used_word <- q5_used_diff |>
  dplyr::filter(!is.na(diff_pp)) |>
  dplyr::arrange(dplyr::desc(diff_pp)) |>
  dplyr::slice_head(n = 6) |>
  dplyr::transmute(
    Notion = stringr::str_replace_all(item_label, "\\n", " "),
    `Écart exposés - non exposés` = fmt_pp(diff_pp, 0.1)
  )


# Tables analytiques supplémentaires pour la version approfondie.
pick_col <- function(data, candidates) {
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

make_rank_table <- function(data, label_candidates, value_candidates, label_name, value_name, n = 10, percent = TRUE) {
  if (nrow(data) == 0) return(tibble::tibble())
  label_col <- pick_col(data, label_candidates)
  value_col <- pick_col(data, value_candidates)
  if (is.na(label_col) || is.na(value_col)) return(tibble::tibble())

  out <- data |>
    dplyr::mutate(
      .label = stringr::str_replace_all(as.character(.data[[label_col]]), "\\n", " "),
      .value = as.numeric(.data[[value_col]])
    ) |>
    dplyr::filter(!is.na(.label), !is.na(.value)) |>
    dplyr::arrange(dplyr::desc(.value)) |>
    dplyr::slice_head(n = n)

  if (isTRUE(percent)) {
    out |>
      dplyr::transmute(
        !!label_name := .label,
        !!value_name := fmt_pct(.value, 1)
      )
  } else {
    out |>
      dplyr::transmute(
        !!label_name := .label,
        !!value_name := fmt_pp(.value, 0.1)
      )
  }
}

make_gap_table <- function(known, used, n = 10) {
  if (nrow(known) == 0 || nrow(used) == 0) return(tibble::tibble())

  label_k <- pick_col(known, c("item_label", "label", "item", "notion", "question"))
  value_k <- pick_col(known, c("pct_w", "pct", "prop_w", "percentage", "mean_w"))
  label_u <- pick_col(used, c("item_label", "label", "item", "notion", "question"))
  value_u <- pick_col(used, c("pct_w", "pct", "prop_w", "percentage", "mean_w"))

  if (any(is.na(c(label_k, value_k, label_u, value_u)))) return(tibble::tibble())

  k <- known |>
    dplyr::transmute(
      notion = stringr::str_replace_all(as.character(.data[[label_k]]), "\\n", " "),
      known = as.numeric(.data[[value_k]])
    )

  u <- used |>
    dplyr::transmute(
      notion = stringr::str_replace_all(as.character(.data[[label_u]]), "\\n", " "),
      used = as.numeric(.data[[value_u]])
    )

  dplyr::inner_join(k, u, by = "notion") |>
    dplyr::mutate(
      gap = known - used,
      known_lab = fmt_pct(known, 1),
      used_lab = fmt_pct(used, 1),
      gap_lab = fmt_pp(100 * gap, 0.1)
    ) |>
    dplyr::arrange(dplyr::desc(gap)) |>
    dplyr::slice_head(n = n) |>
    dplyr::transmute(
      Notion = notion,
      `Bien connue` = known_lab,
      `Déjà utilisée` = used_lab,
      `Écart connaissance-usage` = gap_lab
    )
}

make_cross_exposure_table <- function(data, n = Inf) {
  if (nrow(data) == 0 || !all(c("row_category", "col_category", "pct_row") %in% names(data))) {
    return(tibble::tibble())
  }

  data |>
    dplyr::mutate(
      row_category = as.character(row_category),
      col_category = as.character(col_category),
      value = fmt_pct(pct_row, 1)
    ) |>
    dplyr::select(Groupe = row_category, Type = col_category, value) |>
    tidyr::pivot_wider(names_from = Type, values_from = value) |>
    dplyr::slice_head(n = n)
}

make_diff_table <- function(data, n = 10, title_col = "item_label") {
  if (nrow(data) == 0 || !"diff_pp" %in% names(data)) return(tibble::tibble())
  label_col <- if (title_col %in% names(data)) title_col else pick_col(data, c("score_label", "item_label", "label", "item", "notion", "question"))
  if (is.na(label_col)) return(tibble::tibble())

  data |>
    dplyr::mutate(
      label = stringr::str_replace_all(as.character(.data[[label_col]]), "\\n", " "),
      diff_pp = as.numeric(diff_pp)
    ) |>
    dplyr::filter(!is.na(label), !is.na(diff_pp)) |>
    dplyr::arrange(dplyr::desc(abs(diff_pp))) |>
    dplyr::slice_head(n = n) |>
    dplyr::transmute(
      Dimension = label,
      `Écart exposés - non exposés` = fmt_pp(diff_pp, 0.1),
      Lecture = dplyr::if_else(diff_pp >= 0, "Plus élevé chez les exposés", "Plus élevé chez les non exposés")
    )
}

top_known_overall_word <- make_rank_table(
  q5_known,
  c("item_label", "label", "item", "notion", "question"),
  c("pct_w", "pct", "prop_w", "percentage", "mean_w"),
  "Notion ou outil",
  "Part déclarant bien connaître",
  n = 12
)

top_used_overall_word <- make_rank_table(
  q5_used,
  c("item_label", "label", "item", "notion", "question"),
  c("pct_w", "pct", "prop_w", "percentage", "mean_w"),
  "Notion ou outil",
  "Part déclarant avoir déjà utilisé",
  n = 12
)

gap_known_used_word <- make_gap_table(q5_known, q5_used, n = 12)

exposure_year_word <- make_cross_exposure_table(cross_year)
exposure_disc_word <- make_cross_exposure_table(cross_disc)
exposure_lang_word <- make_cross_exposure_table(cross_lang)

intentions_diff_word <- make_diff_table(tbl("q13_intentions_diff_exposure"), n = 10)
representations_diff_word <- make_diff_table(tbl("q15_agreement_diff_exposure"), n = 10)

score_diff_word <- make_diff_table(score_diff, n = 12, title_col = "score_label")

q3_overall_word <- make_rank_table(
  q3_concepts,
  c("concept", "concept_label", "label", "item"),
  c("pct_w", "pct", "prop_w", "percentage", "share_w"),
  "Concept",
  "Part pondérée",
  n = 15
)

q3_diff_exposure_word <- make_diff_table(q3_diff_exposure, n = 15, title_col = "concept")
q3_diff_language_word <- make_diff_table(q3_diff_language, n = 15, title_col = "concept")

interaction_table_word <- model_interactions |>
  dplyr::mutate(
    label = dplyr::coalesce(
      as.character(interaction_label),
      as.character(term),
      as.character(score_label_clean),
      as.character(score_label),
      if ("outcome" %in% names(model_interactions)) as.character(outcome) else NA_character_
    ),
    estimate_pp = as.numeric(estimate_pp),
    conf_low_pp = as.numeric(conf_low_pp),
    conf_high_pp = as.numeric(conf_high_pp),
    clear = !is.na(conf_low_pp) & !is.na(conf_high_pp) & (conf_low_pp > 0 | conf_high_pp < 0)
  ) |>
  dplyr::arrange(dplyr::desc(clear), dplyr::desc(abs(estimate_pp))) |>
  dplyr::slice_head(n = 12) |>
  dplyr::transmute(
    Interaction = stringr::str_replace_all(label, "\\n", " "),
    `Effet additionnel` = fmt_pp(estimate_pp, 0.1),
    `IC 95 %` = paste0("[", fmt_pp(conf_low_pp, 0.1), " ; ", fmt_pp(conf_high_pp, 0.1), "]"),
    Lecture = dplyr::case_when(
      clear & estimate_pp > 0 ~ "Écart exposés/non exposés plus fort dans ce groupe",
      clear & estimate_pp < 0 ~ "Écart exposés/non exposés plus faible dans ce groupe",
      TRUE ~ "Interaction incertaine"
    )
  )


# -----------------------------------------------------------------------------
# 6. Fonctions pour écrire le document Word
# -----------------------------------------------------------------------------
# On évite volontairement les styles Word "heading 1" / "heading 2" car, selon
# le modèle Word installé localement, ils peuvent être automatiquement numérotés.
# C'est ce qui produisait des titres du type "3. 1. Structure...".
# Ici, les titres sont formatés manuellement pour garder un rendu stable.

doc_add_title <- function(doc, text) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        text,
        officer::fp_text(font.size = 22, bold = TRUE, color = osyr_cols[["navy"]])
      )
    )
  )
  officer::body_add_par(doc, "", style = "Normal")
}

doc_add_h1 <- function(doc, text) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        text,
        officer::fp_text(font.size = 17, bold = TRUE, color = osyr_cols[["navy"]])
      )
    )
  )
  officer::body_add_par(doc, "", style = "Normal")
}

doc_add_h2 <- function(doc, text) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        text,
        officer::fp_text(font.size = 13, bold = TRUE, color = osyr_cols[["navy"]])
      )
    )
  )
}

doc_add_p <- function(doc, text) {
  officer::body_add_par(doc, value = text, style = "Normal")
}

doc_add_note <- function(doc, text) {
  # Ancienne version : note dans un tableau flextable.
  # Problème : selon Word/LibreOffice, le tableau pouvait devenir très étroit.
  # Nouvelle version : paragraphe normal, robuste et lisible.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste0("À retenir — ", text),
        officer::fp_text(font.size = 10.5, italic = TRUE, color = osyr_cols[["navy"]])
      )
    )
  )
  officer::body_add_par(doc, "", style = "Normal")
}

doc_add_table <- function(doc, data, caption = NULL) {
  if (nrow(data) == 0) {
    doc <- doc_add_p(doc, "Tableau non disponible.")
    return(doc)
  }

  if (!is.null(caption)) {
    doc <- doc_add_p(doc, caption)
  }

  ft <- flextable::flextable(data)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 8.5, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#F2F4F7", part = "header")
  ft <- flextable::color(ft, color = osyr_cols[["navy"]], part = "header")
  ft <- flextable::padding(ft, padding = 4, part = "all")
  ft <- flextable::autofit(ft)

  # Si disponible dans la version de flextable installée, on contraint la largeur
  # pour éviter les tableaux qui débordent de la page.
  if ("fit_to_width" %in% getNamespaceExports("flextable")) {
    ft <- flextable::fit_to_width(ft, max_width = 6.5)
  }

  flextable::body_add_flextable(x = doc, value = ft)
}

used_figures <- character(0)

doc_add_img <- function(doc, path, caption = NULL, width = 6.6, height = 4.2, missing_note = TRUE) {
  if (is.na(path) || !file.exists(path)) {
    if (isTRUE(missing_note)) {
      doc <- doc_add_p(doc, "Figure non générée dans cette version du workflow.")
    }
    return(doc)
  }

  used_figures <<- unique(c(used_figures, normalizePath(path, mustWork = FALSE)))

  doc <- officer::body_add_img(doc, src = path, width = width, height = height)

  if (!is.null(caption)) {
    doc <- doc_add_p(doc, paste0("Lecture : ", caption))
  }

  doc
}

nice_figure_title <- function(path) {
  nm <- tools::file_path_sans_ext(basename(path))
  nm |>
    stringr::str_replace("^[0-9]+[a-z]?_", "") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_to_sentence()
}

caption_from_catalogue <- function(path) {
  if (nrow(figure_captions) == 0) return(nice_figure_title(path))

  file_col <- pick_col(figure_captions, c("file", "filename", "figure", "path", "name"))
  cap_col  <- pick_col(figure_captions, c("caption", "legend", "description", "lecture", "title"))

  if (is.na(file_col) || is.na(cap_col)) return(nice_figure_title(path))

  hit <- figure_captions |>
    dplyr::filter(basename(as.character(.data[[file_col]])) == basename(path)) |>
    dplyr::pull(.data[[cap_col]])

  if (length(hit) == 0 || is.na(hit[1])) nice_figure_title(path) else as.character(hit[1])
}

doc_add_captioned_figure <- function(doc, path, caption = NULL, width = 6.6, height = 4.2) {
  if (is.na(path) || !file.exists(path)) return(doc)
  if (is.null(caption)) caption <- caption_from_catalogue(path)
  doc_add_img(doc, path, caption = caption, width = width, height = height, missing_note = FALSE)
}

doc_add_table_if <- function(doc, data, caption = NULL) {
  if (nrow(data) == 0) return(doc)
  doc_add_table(doc, data, caption = caption)
}

# -----------------------------------------------------------------------------
# 7. Construction du rapport Word
# -----------------------------------------------------------------------------

doc <- officer::read_docx()

# Page de titre
doc <- doc_add_title(doc, "Enquête OSYR — Science ouverte chez les doctorants")
doc <- doc_add_p(doc, "Rapport analytique approfondi des premiers résultats")
doc <- doc_add_p(doc, paste0("Document généré automatiquement le ", format(Sys.Date(), "%d/%m/%Y"), "."))
doc <- doc_add_note(
  doc,
  paste(
    "Ce rapport est produit à partir des sorties du workflow OSYR V2 corrigé.",
    "Les résultats sont pondérés.",
    "Les modèles sont associatifs et ne doivent pas être interprétés comme des effets causaux.",
    "Cette version privilégie une lecture analytique développée : mécanismes, contrastes entre groupes, effets ajustés, interactions exploratoires et figures complémentaires."
  )
)
doc <- officer::body_add_break(doc)

# Résumé exécutif approfondi
doc <- doc_add_h1(doc, "Résumé exécutif approfondi")
doc <- doc_add_p(
  doc,
  glue::glue(
    "Le premier résultat structurant est le niveau élevé d'exposition : {fmt_pct(kpi_exposed)} des doctorants déclarent avoir été exposés à un dispositif organisé de science ouverte, contre {fmt_pct(kpi_no_device)} sans dispositif et {fmt_pct(kpi_auto)} relevant seulement de l'autoformation ou d'une autre forme d'exposition."
  )
)
doc <- doc_add_p(
  doc,
  glue::glue(
    "Le deuxième résultat est que l'exposition semble surtout agir sur la capacité pratique. Après ajustement, elle est associée à une hausse du score de connaissance de {fmt_pp(known_effect)}, à une hausse plus modérée du score d'usage de {fmt_pp(used_effect)}, et à une perception beaucoup plus incitative de l'environnement de recherche ({fmt_pp(incitation_effect)})."
  )
)
doc <- doc_add_p(
  doc,
  "Le troisième résultat est plus subtil : les intentions d'ouverture et les représentations générales de la science ouverte varient moins fortement avec l'exposition. Autrement dit, les dispositifs ne semblent pas principalement convertir les doctorants à la valeur morale ou scientifique de l'ouverture ; ils semblent plutôt rendre cette orientation plus opérationnelle."
)
doc <- doc_add_p(
  doc,
  "Le quatrième résultat concerne la stratification des objets de science ouverte. Les objets les plus familiers — archives ouvertes, identifiants chercheurs, plateformes de revues en accès ouvert, réseaux sociaux académiques — ne sont pas toujours ceux qui distinguent le plus les exposés des non exposés. Les écarts les plus forts portent souvent sur des objets plus institutionnalisés et plus techniques : voies de l'accès ouvert, principes FAIR, plans de gestion de données, outils d'aide aux PGD."
)
doc <- doc_add_p(
  doc,
  "Le cinquième résultat tient à l'hétérogénéité : l'année de thèse, la discipline et la langue du questionnaire ne sont pas de simples variables de contrôle. Elles structurent les occasions d'exposition, les horizons de pratique et les cadrages sémantiques de la science ouverte. Les interactions restent exploratoires, mais elles indiquent que l'effet des dispositifs peut varier selon les contextes."
)
doc <- doc_add_note(
  doc,
  "Message central : l'enjeu n'est pas seulement de sensibiliser à la science ouverte. Il est de transformer une adhésion déjà élevée en compétences, repères, usages et conditions concrètes de mise en pratique."
)

doc <- doc_add_h2(doc, "Lecture synthétique des résultats")
synthese_table <- tibble::tibble(
  Niveau = c(
    "Adhésion générale",
    "Connaissance opérationnelle",
    "Usage déclaré",
    "Environnement institutionnel",
    "Contraintes et risques",
    "Différences disciplinaires / langue"
  ),
  Résultat = c(
    "Très favorable dans l'ensemble ; peu différenciée par l'exposition.",
    "C'est le domaine où l'association avec les dispositifs est la plus nette.",
    "Progression réelle mais plus modeste que la connaissance.",
    "Les exposés perçoivent davantage leur environnement comme incitatif.",
    "Les signaux sont plus faibles ou moins clairement associés à l'exposition.",
    "Dimensions importantes pour nuancer l'interprétation et éviter une lecture homogénéisante."
  ),
  `Interprétation` = c(
    "La science ouverte est déjà une norme largement légitime.",
    "Les dispositifs fonctionnent comme instruments d'acculturation structurée.",
    "Le passage de la familiarité à la pratique reste partiel.",
    "Les formations peuvent rendre visibles des ressources, attentes ou opportunités institutionnelles.",
    "L'exposition ne suffit pas à lever les contraintes matérielles, disciplinaires ou évaluatives.",
    "Les dispositifs sont reçus dans des contextes différenciés."
  )
)
doc <- doc_add_table(doc, synthese_table, caption = "Synthèse interprétative des principaux résultats.")
doc <- officer::body_add_break(doc)

# Cadre d'analyse
doc <- doc_add_h1(doc, "Cadre d'analyse et précautions")
doc <- doc_add_p(
  doc,
  "Le rapport distingue quatre niveaux qui ne doivent pas être confondus : l'exposition à des dispositifs, la connaissance déclarée, l'usage déclaré, et les représentations ou intentions. Cette distinction est importante car un dispositif peut augmenter la familiarité avec des outils sans nécessairement produire immédiatement des usages, et encore moins modifier des valeurs déjà largement partagées."
)
doc <- doc_add_p(
  doc,
  "L'exposition à un dispositif organisé est interprétée comme un indicateur d'acculturation structurée. Elle peut correspondre à des formations, ateliers, modules ou dispositifs institutionnels. Elle ne mesure pas directement l'intensité, la qualité, la durée ou le contenu précis de l'exposition."
)
doc <- doc_add_p(
  doc,
  "Les modèles ajustés contrôlent les variables disponibles — année de thèse, discipline, établissement et langue du questionnaire — mais ils ne permettent pas d'inférer une causalité. Les doctorants exposés peuvent aussi être ceux qui ont déjà un intérêt plus fort pour la science ouverte, ou qui appartiennent à des environnements plus actifs sur ces questions."
)
doc <- doc_add_note(
  doc,
  "La bonne lecture est associative : les résultats identifient des configurations et des écarts robustes à certaines variables observées, pas des effets causaux démontrés."
)

# Structure
doc <- doc_add_h1(doc, "Structure de l'échantillon et exposition")
doc <- doc_add_p(
  doc,
  "La répartition pondérée par année de thèse est globalement équilibrée. C'est un point favorable pour l'analyse car l'année de thèse conditionne mécaniquement les opportunités d'exposition et de mise en pratique : plus la thèse avance, plus les occasions de publier, déposer des données, gérer du code, utiliser des identifiants ou préparer une diffusion augmentent."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("00_distribution_annee_these_controle.png"))),
  caption = "part pondérée des répondants selon l'année de thèse.",
  width = 6.5,
  height = 3.9
)
doc <- doc_add_table_if(doc, make_rank_table(sample_year, c("category", "year", "row_category"), c("pct_w", "pct", "prop_w"), "Année de thèse", "Part pondérée", n = 10), caption = "Répartition pondérée par année de thèse.")

doc <- doc_add_p(
  doc,
  glue::glue(
    "La langue du questionnaire introduit une autre dimension de lecture : {fmt_pct(kpi_english)} des répondants ont répondu en anglais. Cette variable est utile pour approcher prudemment des profils plus internationaux, sans l'assimiler à une nationalité ni à un statut administratif."
  )
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("01_langue_questionnaire.png"))),
  caption = "répartition pondérée des répondants selon la langue du questionnaire.",
  width = 6.5,
  height = 3.7
)

doc <- doc_add_h2(doc, "Intensité et formes d'exposition")
doc <- doc_add_p(
  doc,
  "La distribution de l'exposition montre que les dispositifs organisés occupent une place centrale. La catégorie « aucun dispositif » reste toutefois importante : elle constitue le groupe de comparaison principal pour mesurer ce que les dispositifs différencient effectivement."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("02_exposition_dispositifs.png", "02_exposition_aux_dispositifs.png", "02_exposition_globale.png"),
    c("exposition.*dispositif", "exposition.*globale", "dispositifs.*science")
  ),
  caption = "répartition pondérée des formes d'exposition aux dispositifs de science ouverte.",
  width = 6.5,
  height = 3.8
)
doc <- doc_add_table_if(doc, make_rank_table(sample_exposure, c("category", "exposure3", "label"), c("pct_w", "pct", "prop_w"), "Type d'exposition", "Part pondérée", n = 10), caption = "Répartition pondérée par type d'exposition.")

doc <- doc_add_h2(doc, "Exposition selon l'année de thèse")
doc <- doc_add_p(
  doc,
  "L'exposition par année de thèse permet de vérifier si les dispositifs touchent surtout les doctorants avancés ou s'ils interviennent dès le début de la trajectoire doctorale. Une exposition précoce est particulièrement importante pour les pratiques qui doivent être anticipées, comme la gestion des données, la documentation du code ou le choix des modalités de diffusion."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("03_exposition_par_annee.png", "03_exposition_selon_annee_these.png", "03_exposition_annee.png"),
    c("exposition.*année", "exposition.*annee", "annee.*exposition")
  ),
  caption = "chaque barre représente 100 % des répondants d'une même année ; on compare la composition des formes d'exposition.",
  width = 6.5,
  height = 4.2
)
doc <- doc_add_table_if(doc, exposure_year_word, caption = "Exposition aux dispositifs par année de thèse.")

doc <- doc_add_h2(doc, "Exposition selon le domaine disciplinaire")
doc <- doc_add_p(
  doc,
  "Les contrastes disciplinaires doivent être lus comme des différences d'environnement de formation et de pratiques scientifiques. Les besoins de science ouverte ne sont pas identiques entre disciplines : les enjeux de données, de code, de prépublication, d'archives ouvertes, de protocoles ou de valorisation ne se présentent pas de la même façon."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("04_exposition_par_discipline.png", "04_exposition_selon_discipline.png", "04_exposition_discipline.png"),
    c("exposition.*discipline", "discipline.*exposition", "domaine.*disciplinaire")
  ),
  caption = "part pondérée de chaque type d'exposition au sein de chaque grand domaine disciplinaire.",
  width = 6.5,
  height = 4.2
)
doc <- doc_add_table_if(doc, exposure_disc_word, caption = "Exposition aux dispositifs par domaine disciplinaire.")

doc <- doc_add_h2(doc, "Exposition selon la langue du questionnaire")
doc <- doc_add_p(
  doc,
  "La comparaison par langue du questionnaire est particulièrement utile parce qu'elle signale un possible effet de composition : les répondants au questionnaire anglais semblent plus souvent exposés à des dispositifs organisés. Cela peut refléter une attention institutionnelle particulière aux doctorants internationaux, une auto-sélection de profils plus sensibilisés, ou des différences d'accès à certaines formations."
)
doc <- doc_add_img(
  doc,
  choose_figure("05_exposition_par_langue.png", "R01_exposition_organisee_par_langue.png"),
  caption = "comparaison de la part de doctorants exposés à un dispositif organisé selon la langue du questionnaire.",
  width = 6.5,
  height = 3.7
)
doc <- doc_add_table_if(doc, exposure_lang_word, caption = "Exposition aux dispositifs selon la langue du questionnaire.")
doc <- officer::body_add_break(doc)

# Connaissances et usages
doc <- doc_add_h1(doc, "Connaissances, usages et zones d'acculturation")
doc <- doc_add_p(
  doc,
  "La connaissance des notions et outils est le premier lieu où les dispositifs semblent faire une différence. Mais il faut distinguer trois situations : les objets déjà très connus et souvent utilisés, les objets connus mais peu pratiqués, et les objets encore peu appropriés. Ces trois situations appellent des interventions différentes."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("06_q5_notions_bien_connues.png"))),
  caption = "part pondérée de doctorants déclarant bien connaître chaque notion ou outil.",
  width = 6.5,
  height = 5.1
)
doc <- doc_add_table_if(doc, top_known_overall_word, caption = "Notions et outils les plus souvent déclarés comme bien connus.")

doc <- doc_add_p(
  doc,
  "Le classement des usages donne une image plus exigeante que celui de la connaissance. Un objet peut être connu parce qu'il circule dans les discours institutionnels ou dans l'environnement doctoral, sans être réellement mobilisé dans la pratique de recherche. C'est pourquoi la comparaison connaissance/usage est centrale."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("07_q5_notions_deja_utilisees.png"), fig("07_q5_notions_utilisees.png"))),
  caption = "part pondérée de doctorants déclarant avoir déjà utilisé chaque notion ou outil.",
  width = 6.5,
  height = 5.1
)
doc <- doc_add_table_if(doc, top_used_overall_word, caption = "Notions et outils les plus souvent déclarés comme déjà utilisés.")

doc <- doc_add_h2(doc, "De la familiarité déclarée à l'usage")
doc <- doc_add_p(
  doc,
  "L'écart entre connaissance et usage est analytiquement très important. Les segments les plus longs identifient les notions connues sans être encore pleinement appropriées. Ces objets constituent des cibles prioritaires pour des formations opérationnelles : il ne s'agit plus seulement d'expliquer ce qu'ils sont, mais de montrer comment les mobiliser dans une situation concrète de recherche."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("06b_q5_ecart_connaissance_usage.png"))),
  caption = "plus le segment est long, plus la notion est connue sans être encore largement pratiquée.",
  width = 6.5,
  height = 4.8
)
doc <- doc_add_table_if(doc, gap_known_used_word, caption = "Notions où l'écart entre connaissance déclarée et usage déclaré est le plus élevé.")

doc <- doc_add_h2(doc, "Ce que l'exposition distingue dans la connaissance")
doc <- doc_add_p(
  doc,
  "Les écarts entre exposés et non exposés ne portent pas seulement sur des objets génériques de la science ouverte. Ils sont particulièrement marqués pour des notions institutionnalisées ou techniques — voies de l'accès ouvert, FAIR, plans de gestion de données, plateformes de revues — qui supposent souvent un apprentissage structuré."
)
doc <- doc_add_table_if(
  doc,
  top_known_word,
  caption = "Notions dont la connaissance distingue le plus les doctorants exposés aux dispositifs organisés."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("10_diff_q5_connaissance_exposition.png"))),
  caption = "valeurs positives = connaissance plus élevée parmi les exposés ; écarts descriptifs.",
  width = 6.5,
  height = 5.0
)

doc <- doc_add_h2(doc, "Ce que l'exposition distingue dans les usages")
doc <- doc_add_p(
  doc,
  "Les différences d'usage sont plus modestes que les différences de connaissance. C'est un résultat attendu : l'usage dépend non seulement de la connaissance, mais aussi du stade d'avancement de la thèse, de la discipline, de la disponibilité de données ou de code, des contraintes juridiques et des attentes des encadrants."
)
doc <- doc_add_table_if(
  doc,
  top_used_word,
  caption = "Usages dont la fréquence distingue le plus les doctorants exposés aux dispositifs organisés."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("11_diff_q5_usage_exposition.png"))),
  caption = "valeurs positives = usage plus fréquent parmi les exposés ; écarts descriptifs.",
  width = 6.5,
  height = 5.0
)

doc <- doc_add_h2(doc, "Connaissances selon l'année et la discipline")
doc <- doc_add_p(
  doc,
  "Les heatmaps permettent de repérer les objets qui progressent avec l'avancement dans la thèse et ceux qui restent fortement dépendants du domaine disciplinaire. Elles sont utiles pour passer d'un diagnostic général à des priorités de formation différenciées."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("14_heatmap_q5_connaissance_annee.png", "14_connaissance_notions_annee.png"),
    c("connaissance.*année", "connaissance.*annee", "heatmap.*annee")
  ),
  caption = "part pondérée déclarant bien connaître chaque notion ou outil selon l'année de thèse.",
  width = 6.5,
  height = 5.1
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("15_heatmap_q5_connaissance_discipline.png", "15_connaissance_notions_discipline.png"),
    c("connaissance.*discipline", "heatmap.*discipline")
  ),
  caption = "part pondérée déclarant bien connaître chaque notion ou outil selon le domaine disciplinaire.",
  width = 6.5,
  height = 5.1
)
doc <- officer::body_add_break(doc)

# Intentions et représentations
doc <- doc_add_h1(doc, "Intentions, représentations et limites de la conversion normative")
doc <- doc_add_p(
  doc,
  "Les intentions de diffusion ouverte sont fortes pour la thèse et les articles, mais plus limitées pour les données, le code, l'édition commerciale ou la valorisation économique. Cette hiérarchie montre que l'ouverture est plus immédiatement pensée pour les produits éditoriaux que pour les matériaux de recherche."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("08_q13_intentions_oui.png"))),
  caption = "part pondérée de réponses oui à chaque intention. Ces intentions ne sont pas des comportements observés.",
  width = 6.5,
  height = 4.2
)
doc <- doc_add_table_if(doc, make_rank_table(q13_intentions, c("item_label", "label", "item", "question"), c("pct_w", "pct", "prop_w", "percentage", "mean_w"), "Intention", "Part de réponses oui", n = 10), caption = "Intentions déclarées de pratiques ouvertes ou de valorisation.")

doc <- doc_add_p(
  doc,
  "Les représentations générales sont très favorables : reproductibilité, coopération et intégrité scientifique occupent les premiers niveaux d'accord. Mais cette adhésion coexiste avec une critique institutionnelle forte, notamment l'idée que la science ouverte n'est pas assez reconnue dans l'évaluation de la recherche."
)
doc <- doc_add_img(
  doc,
  first_existing(c(fig("09_q15_accord_affirmations.png"))),
  caption = "part pondérée de répondants plutôt d'accord ou tout à fait d'accord.",
  width = 6.5,
  height = 4.5
)
doc <- doc_add_table_if(doc, make_rank_table(q15_agreement, c("item_label", "label", "item", "question"), c("pct_w", "pct", "prop_w", "percentage", "mean_w"), "Affirmation", "Part d'accord", n = 10), caption = "Accord avec les affirmations sur la science ouverte.")

doc <- doc_add_h2(doc, "Écarts d'intentions et de représentations entre exposés et non exposés")
doc <- doc_add_p(
  doc,
  "Les écarts d'intentions et de représentations sont plus faibles que les écarts de connaissance. Cette asymétrie est l'un des résultats les plus importants : les dispositifs semblent moins produire l'adhésion que donner des repères et des capacités pour agir."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("12_diff_q13_intentions_exposition.png", "12_intentions_ecarts_exposition.png"),
    c("intentions.*exposition", "diff.*intentions")
  ),
  caption = "différences pondérées de réponses oui ; valeurs positives = intentions plus fréquentes chez les exposés.",
  width = 6.5,
  height = 4.4
)
doc <- doc_add_table_if(doc, intentions_diff_word, caption = "Écarts descriptifs d'intentions entre exposés et non exposés.")
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("13_diff_q15_representations_exposition.png", "13_representations_ecarts_exposition.png"),
    c("representations.*exposition", "représentations.*exposition", "diff.*represent")
  ),
  caption = "différences pondérées d'accord avec les affirmations ; valeurs positives = accord plus fréquent chez les exposés.",
  width = 6.5,
  height = 4.4
)
doc <- doc_add_table_if(doc, representations_diff_word, caption = "Écarts descriptifs de représentations entre exposés et non exposés.")
doc <- doc_add_note(
  doc,
  "Interprétation : la science ouverte apparaît déjà comme un horizon fortement légitime. Les formations ne sont donc pas seulement des dispositifs de sensibilisation ; elles jouent surtout un rôle d'opérationnalisation."
)
doc <- officer::body_add_break(doc)

# Scores synthétiques
doc <- doc_add_h1(doc, "Scores synthétiques : ce que l'exposition change vraiment")
doc <- doc_add_p(
  doc,
  "Les scores synthétiques permettent de regrouper les items en dimensions plus interprétables : connaissance, usage, activités déjà réalisées, intentions, bénéfices perçus, risques, contraintes, environnement incitatif ou environnement perçu comme un frein. Cette approche évite de raisonner uniquement item par item."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("16_diff_scores_exposition.png", "16_scores_diff_exposition.png", "16_ce_que_change_exposition.png"),
    c("score.*exposition", "change.*exposition", "diff.*scores")
  ),
  caption = "différence pondérée de scores moyens entre dispositif organisé et aucun dispositif.",
  width = 6.5,
  height = 4.7
)
doc <- doc_add_table_if(doc, score_diff_word, caption = "Écarts descriptifs de scores moyens entre exposés et non exposés.")

doc <- doc_add_h2(doc, "Scores par exposition, année et langue")
doc <- doc_add_p(
  doc,
  "Les heatmaps croisées permettent d'observer si l'écart exposés/non exposés se maintient à niveau d'avancement donné ou selon la langue du questionnaire. Elles complètent les modèles : elles donnent une lecture visuelle des profils, sans prétendre isoler un effet causal."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("17_heatmap_scores_exposition_annee.png", "17_scores_exposition_annee.png"),
    c("scores.*exposition.*annee", "scores.*exposition.*année", "heatmap.*scores.*annee")
  ),
  caption = "comparaison des scores entre exposés et non exposés à année de thèse donnée.",
  width = 6.5,
  height = 5.0
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("18_heatmap_scores_exposition_langue.png", "18_scores_exposition_langue.png"),
    c("scores.*exposition.*langue", "scores.*exposition.*language", "heatmap.*scores.*langue")
  ),
  caption = "comparaison des scores entre exposés et non exposés selon la langue du questionnaire.",
  width = 6.5,
  height = 5.0
)
doc <- officer::body_add_break(doc)

# Modèles
doc <- doc_add_h1(doc, "Résultats des modèles ajustés")
doc <- doc_add_p(
  doc,
  "Les modèles pondérés comparent les doctorants exposés à un dispositif organisé aux doctorants sans dispositif organisé, en tenant compte des variables disponibles. Les coefficients sont exprimés en points de pourcentage. Ils indiquent donc des différences ajustées moyennes."
)
doc <- doc_add_img(
  doc,
  choose_figure("19_modeles_scores_effet_exposition_highlevel.png", "R02_effets_ajustes_lisibles.png"),
  caption = "chaque point est une différence ajustée ; les barres sont les intervalles de confiance à 95 %.",
  width = 6.5,
  height = 4.5
)
doc <- doc_add_table_if(
  doc,
  model_table_word,
  caption = "Synthèse des associations ajustées entre exposition à un dispositif organisé et scores."
)
doc <- doc_add_p(
  doc,
  "La modélisation confirme la hiérarchie observée descriptivement. Les associations les plus nettes concernent l'environnement perçu comme incitatif et la connaissance des notions et outils. Les usages et activités progressent également, mais plus modestement. Les intentions, les bénéfices perçus, les risques et les contraintes ne présentent pas d'association ajustée claire."
)
doc <- doc_add_note(
  doc,
  "Lecture analytique : les dispositifs semblent surtout produire de la capacité et de la lisibilité institutionnelle. Ils ne suffisent pas à eux seuls à transformer les contraintes, les intentions ou les arbitrages pratiques."
)

doc <- doc_add_h2(doc, "Prédictions ajustées du score de connaissance")
doc <- doc_add_p(
  doc,
  "Les figures de prédictions ajustées représentent le score de connaissance Q5 attendu selon l'exposition et une variable de contexte, les autres contrôles étant fixés à leur modalité de référence ou à leur modalité la plus fréquente. Elles sont plus lisibles que des courbes continues car les modérateurs sont catégoriels."
)
doc <- doc_add_img(
  doc,
  choose_figure("20_prediction_connaissance_exposition_annee.png", "R03_prediction_connaissance_annee_points.png"),
  caption = "score de connaissance prédit selon année de thèse et exposition.",
  width = 6.5,
  height = 4.2
)
doc <- doc_add_img(
  doc,
  choose_figure("21_prediction_connaissance_exposition_discipline.png", "R04_prediction_connaissance_discipline_points.png"),
  caption = "score de connaissance prédit selon discipline et exposition.",
  width = 6.5,
  height = 4.2
)
doc <- doc_add_img(
  doc,
  choose_figure("22_prediction_connaissance_exposition_langue.png", "R05_prediction_connaissance_langue_points.png"),
  caption = "score de connaissance prédit selon langue du questionnaire et exposition.",
  width = 6.5,
  height = 3.8
)

doc <- doc_add_h2(doc, "Interactions exploratoires")
doc <- doc_add_p(
  doc,
  "Les interactions cherchent à déterminer si l'écart entre exposés et non exposés varie selon l'année, la discipline ou la langue du questionnaire. Cette partie doit rester exploratoire : les effectifs de certains sous-groupes peuvent être plus faibles, les intervalles plus larges, et les interactions sont plus fragiles que les effets principaux."
)
doc <- doc_add_img(
  doc,
  choose_figure("23b_modeles_interactions_synthese.png", "R06_interactions_claires_lisibles.png"),
  caption = "un terme d'interaction indique si l'écart exposés/non exposés est plus fort ou plus faible dans un groupe donné que dans le groupe de référence.",
  width = 6.5,
  height = 4.6
)
doc <- doc_add_table_if(doc, interaction_table_word, caption = "Principaux termes d'interaction à lire comme signaux exploratoires.")
doc <- officer::body_add_break(doc)

# Trois mots
doc <- doc_add_h1(doc, "Représentations spontanées : les trois mots associés à la science ouverte")
doc <- doc_add_p(
  doc,
  "L'analyse des trois mots complète les items fermés. Elle donne accès aux cadrages spontanés : ce que les doctorants associent immédiatement à la science ouverte, avant toute reformulation par des catégories d'enquête."
)
doc <- doc_add_table_if(doc, q3_overall_word, caption = "Concepts les plus fréquents dans les trois mots associés à la science ouverte.")

doc <- doc_add_h2(doc, "Cadrages associés à l'exposition")
doc <- doc_add_p(
  doc,
  "Les doctorants exposés mobilisent relativement davantage un vocabulaire institutionnalisé et outillé : données ouvertes, FAIR, publications, libre accès. Les non exposés mobilisent davantage certains registres généraux ou normatifs : intégrité, éthique, accès, liberté ou gratuité. Cette opposition suggère un déplacement du registre des valeurs vers celui des instruments."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("24_q3_concepts_diff_exposition.png", "24_q3_concepts_diff_exposure.png", "24_q3_concepts_diff_exposure2.png"),
    c("q3.*exposition", "q3.*exposure", "concept.*exposition", "concept.*exposure", "trois.*exposition")
  ),
  caption = "différence pondérée de présence des concepts dans les trois mots ; valeurs positives = concepts plus fréquents chez les exposés.",
  width = 6.5,
  height = 4.8
)
doc <- doc_add_table_if(doc, q3_diff_exposure_word, caption = "Concepts qui distinguent le plus les répondants exposés et non exposés.")

doc <- doc_add_h2(doc, "Cadrages associés à la langue du questionnaire")
doc <- doc_add_p(
  doc,
  "La langue du questionnaire fait aussi varier les cadrages sémantiques. Les réponses en anglais peuvent davantage insister sur l'ouverture, le bien commun ou la collaboration, tandis que les réponses en français peuvent mobiliser plus souvent des registres d'accès, de liberté, de transparence, de code ou de partage. Cette lecture doit rester prudente car la langue du questionnaire condense plusieurs dimensions : langue, profil international, socialisation institutionnelle et contexte disciplinaire."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("25_q3_concepts_diff_langue.png", "25_q3_concepts_diff_language.png"),
    c("q3.*langue", "q3.*language", "concept.*langue", "concept.*language", "trois.*langue")
  ),
  caption = "différence pondérée de présence des concepts : questionnaire anglais moins questionnaire français.",
  width = 6.5,
  height = 5.0
)
doc <- doc_add_table_if(doc, q3_diff_language_word, caption = "Concepts qui distinguent le plus les réponses au questionnaire anglais et français.")
doc <- officer::body_add_break(doc)

# Profils exploratoires
doc <- doc_add_h1(doc, "Profils exploratoires de doctorants")
doc <- doc_add_p(
  doc,
  "Le clustering, lorsqu'il est disponible, ne doit pas être interprété comme une typologie définitive. Il sert à repérer des configurations : profils très favorables mais peu outillés, profils fortement acculturés, profils contraints, profils davantage orientés vers les usages ou les pratiques."
)
doc <- doc_add_img(
  doc,
  find_figure_by_pattern(
    c("26_clusters_scores_heatmap.png", "26_profils_exploratoires.png", "26_kmeans_profils.png"),
    c("profil.*doctorants", "clusters", "kmeans", "profils.*exploratoires")
  ),
  caption = "profils obtenus par clustering sur les scores standardisés ; à lire comme aide à l'interprétation, non comme classification définitive.",
  width = 6.5,
  height = 4.8
)

# Conclusion analytique
doc <- officer::body_add_break(doc)
doc <- doc_add_h1(doc, "Conclusion analytique")
doc <- doc_add_p(
  doc,
  "Les résultats convergent vers une interprétation forte : les dispositifs organisés de science ouverte sont associés à une acculturation plus opérationnelle. Ils ne se traduisent pas d'abord par une adhésion accrue aux valeurs de la science ouverte, car cette adhésion est déjà élevée. Ils se traduisent plutôt par une meilleure connaissance des notions, un usage un peu plus fréquent de certains outils et une perception plus incitative de l'environnement."
)
doc <- doc_add_p(
  doc,
  "Cette conclusion a une conséquence importante pour l'action institutionnelle. Les formations ne devraient pas seulement présenter les grands principes de la science ouverte ; elles devraient être organisées autour de situations de recherche concrètes : où déposer, comment documenter, quel identifiant utiliser, comment choisir une licence, comment préparer un plan de gestion de données, comment rendre un code réutilisable, comment articuler ouverture et contraintes juridiques."
)
doc <- doc_add_p(
  doc,
  "Le passage de la connaissance à l'usage apparaît comme le principal point de friction. Les objets les moins appropriés ne sont pas forcément rejetés ; ils peuvent être connus sans que les doctorants disposent du temps, de l'encadrement, des incitations ou des ressources nécessaires pour les mettre en œuvre."
)
doc <- doc_add_p(
  doc,
  "Enfin, les différences disciplinaires et linguistiques invitent à éviter une politique de formation uniforme. Les besoins des doctorants en sciences humaines et sociales, en sciences du vivant, en sciences formelles ou en ingénierie ne sont pas identiques. De même, les répondants au questionnaire anglais semblent avoir un profil spécifique qui mérite d'être analysé comme configuration institutionnelle et non comme simple catégorie linguistique."
)
doc <- doc_add_note(
  doc,
  "Formulation possible pour la restitution : les dispositifs de science ouverte ne fabriquent pas seulement de l'adhésion ; ils transforment surtout une disposition favorable en capacités pratiques inégalement distribuées."
)

doc <- doc_add_h2(doc, "Pistes d'approfondissement")
pistes_table <- tibble::tibble(
  Axe = c(
    "Effet de l'exposition",
    "Passage connaissance → usage",
    "Différences disciplinaires",
    "Profil international",
    "Conditions institutionnelles",
    "Analyse textuelle"
  ),
  `Question analytique` = c(
    "Quels résultats restent associés aux dispositifs après ajustement ?",
    "Quelles notions sont connues sans être pratiquées ?",
    "Quels objets de science ouverte sont propres à certains domaines ?",
    "La langue du questionnaire signale-t-elle des trajectoires ou ressources spécifiques ?",
    "Quels freins persistent malgré l'exposition ?",
    "Quels cadrages spontanés accompagnent l'acculturation ?"
  ),
  `Usage possible` = c(
    "Argumenter sur le rôle des formations.",
    "Identifier les priorités pédagogiques.",
    "Adapter les formations par champ disciplinaire.",
    "Construire une offre plus inclusive.",
    "Relier science ouverte et politiques d'évaluation.",
    "Compléter les résultats quantitatifs par une lecture sémantique."
  )
)
doc <- doc_add_table(doc, pistes_table, caption = "Axes d'approfondissement pour la suite de l'analyse.")

# Atlas graphique complémentaire : insère toutes les figures disponibles non encore utilisées.
if (isTRUE(include_extra_figure_atlas) && dir.exists(figures_dir)) {
  all_figures <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  all_figures <- all_figures[order(basename(all_figures))]
  already <- normalizePath(used_figures, mustWork = FALSE)
  remaining <- all_figures[!normalizePath(all_figures, mustWork = FALSE) %in% already]
  remaining <- remaining[!stringr::str_detect(basename(remaining), "^tmp|test|debug")]

  if (length(remaining) > 0) {
    doc <- officer::body_add_break(doc)
    doc <- doc_add_h1(doc, "Atlas graphique complémentaire")
    doc <- doc_add_p(
      doc,
      glue::glue(
        "Cette section insère automatiquement les figures PNG disponibles dans le dossier {figures_dir} qui n'ont pas déjà été mobilisées dans le corps du rapport. Elle sert de réserve visuelle pour repérer des analyses utiles à commenter plus finement."
      )
    )

    for (p in head(remaining, max_extra_figures)) {
      doc <- doc_add_h2(doc, nice_figure_title(p))
      doc <- doc_add_captioned_figure(
        doc,
        p,
        caption = caption_from_catalogue(p),
        width = 6.5,
        height = 4.6
      )
    }
  }
}



# Annexe courte : précautions de lecture
doc <- officer::body_add_break(doc)
doc <- doc_add_h1(doc, "Précautions de lecture")
doc <- doc_add_p(doc, "Les résultats sont pondérés et doivent être lus comme des résultats descriptifs ou associatifs.")
doc <- doc_add_p(doc, "Les écarts descriptifs entre exposés et non exposés ne sont pas des effets causaux.")
doc <- doc_add_p(doc, "Les modèles ajustés contrôlent les variables disponibles, mais ne corrigent pas les différences non observées entre répondants.")
doc <- doc_add_p(doc, "La langue du questionnaire est utilisée comme un indicateur prudent de profil international ; elle ne mesure pas directement la nationalité.")
doc <- doc_add_p(doc, "Les analyses textuelles des trois mots dépendent du dictionnaire de concepts utilisé et doivent être interprétées comme une exploration des cadrages spontanés.")
doc <- doc_add_p(doc, "Les interactions doivent être lues comme des signaux exploratoires : elles indiquent où l'association entre exposition et scores pourrait varier, mais elles sont plus sensibles aux tailles de sous-groupes et au choix du modèle.")
doc <- doc_add_p(doc, "Les intentions déclarées ne doivent pas être assimilées à des comportements observés. Elles indiquent un horizon d'action possible, pas une pratique effective.")
doc <- doc_add_p(doc, "Le rapport est volontairement analytique : certains commentaires formulent des hypothèses interprétatives à discuter avec les collègues et à confronter à la connaissance du terrain.")

# -----------------------------------------------------------------------------
# 8. Export
# -----------------------------------------------------------------------------

print(doc, target = report_docx)

message("\nRapport Word généré : ", normalizePath(report_docx, mustWork = FALSE))
message("Figures retravaillées : ", normalizePath(report_fig_dir, mustWork = FALSE))
