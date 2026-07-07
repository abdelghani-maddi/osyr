# =============================================================================
# SCRIPT 03 — ANALYSES COMPLÉMENTAIRES OSYR
# Version v12.1 — 07/07/2026
#
# OBJECTIF
#   Compléter le script 01 par une couche de tests, robustesse et visualisations :
#   - comparer avec et sans pondération ;
#   - comparer discipline détaillée vs discipline agrégée ;
#   - tester les écarts item par item avec intervalles de confiance ;
#   - ajouter une correction FDR pour éviter la surinterprétation des nombreux tests ;
#   - produire des synthèses interprétables plutôt qu'un empilement de coefficients ;
#   - intégrer les nouvelles sorties du script 01 v14 : dispositifs Q8 détaillés,
#     MOOC/autoformation, gaps connaissance-usage, disciplines détaillées ;
#   - générer un export Excel consolidé, plus facile à partager ;
#   - corriger la fonction FDR pour les modèles Q8, dont les colonnes IC sont nommées *_approx.
#
# PRÉREQUIS
#   source("01_analyse_osyr_base_et_modeles.R")
#
# SORTIES
#   outputs_osyr_v2_complements_30062026/
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE,
  survey.lonely.psu = "adjust"
)

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
}

pkgs <- c(
  "tidyverse", "readr", "janitor", "survey", "broom", "scales",
  "forcats", "stringr", "stringi", "fs", "openxlsx", "ggrepel"
)

install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Chemins
# -----------------------------------------------------------------------------

main_out <- "outputs_osyr_v2_final"
main_data <- file.path(main_out, "data_clean")
main_tables <- file.path(main_out, "tables")
main_figures <- file.path(main_out, "figures")

out_root <- "outputs_osyr_v2_complements_30062026"
dir_tables <- file.path(out_root, "tables")
dir_figures <- file.path(out_root, "figures")
dir_models <- file.path(out_root, "models")
dir_method <- file.path(out_root, "methodology")
dir_exports <- file.path(out_root, "exports")
dir_text <- file.path(out_root, "text_analysis")

purrr::walk(
  c(out_root, dir_tables, dir_figures, dir_models, dir_method, dir_exports, dir_text),
  fs::dir_create
)

# -----------------------------------------------------------------------------
# 2. Fonctions utilitaires
# -----------------------------------------------------------------------------

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

write_csv_safe <- function(x, path) {
  fs::dir_create(dirname(path))
  readr::write_csv(x, path)
  invisible(x)
}

save_table <- function(x, name, dir = dir_tables) {
  write_csv_safe(x, file.path(dir, paste0(name, ".csv")))
}

save_model <- function(x, name) {
  write_csv_safe(x, file.path(dir_models, paste0(name, ".csv")))
}

save_method <- function(x, name) {
  write_csv_safe(x, file.path(dir_method, paste0(name, ".csv")))
}

save_text <- function(x, name) {
  write_csv_safe(x, file.path(dir_text, paste0(name, ".csv")))
}

existing_vars <- function(vars, data) {
  vars <- vars[!is.na(vars)]
  vars <- vars[vars != ""]
  vars <- unique(vars)
  vars[vars %in% names(data)]
}

has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0 && ncol(x) > 0
}

clean_ascii <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower(locale = "fr") |>
    stringr::str_squish()
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(as.numeric(x[ok]) * as.numeric(w[ok]), na.rm = TRUE) / sum(as.numeric(w[ok]), na.rm = TRUE)
}

w_prop <- function(x, w) w_mean(as.numeric(x), w)

fmt_pp <- function(x, accuracy = 0.1) {
  paste0(ifelse(x >= 0, "+", ""), scales::number(x, accuracy = accuracy, decimal.mark = ","), " pts")
}

safe_max <- function(x, default = 1) {
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(m) || is.na(m)) return(default)
  m
}

make_design <- function(data, weight_var) {
  d <- data
  if (!weight_var %in% names(d)) d[[weight_var]] <- 1
  d$.__weight__ <- suppressWarnings(as.numeric(d[[weight_var]]))
  d$.__weight__[is.na(d$.__weight__) | d$.__weight__ <= 0] <- 1
  survey::svydesign(ids = ~1, weights = ~.__weight__, data = d)
}

model_empty <- function() {
  tibble::tibble(
    outcome = character(),
    outcome_label = character(),
    model_type = character(),
    weight_var = character(),
    discipline_level = character(),
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    statistic = numeric(),
    p.value = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    estimate_pp = numeric(),
    conf_low_pp = numeric(),
    conf_high_pp = numeric(),
    n_model = integer(),
    controls = character()
  )
}

osyr_cols <- c(
  navy = "#17324D",
  teal = "#2A9D8F",
  coral = "#E76F51",
  orange = "#F4A261",
  blue = "#3B82F6",
  purple = "#7B2CBF",
  grey = "#667085",
  light = "#F2F4F7"
)

theme_osyr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 5,
        color = osyr_cols[["navy"]]
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size + 1,
        color = "#475467",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 2,
        color = "#667085",
        hjust = 0
      ),
      axis.text = ggplot2::element_text(color = "#344054"),
      axis.title = ggplot2::element_text(color = "#344054"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#EAECF0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", color = osyr_cols[["navy"]]),
      strip.background = ggplot2::element_rect(fill = "#F2F4F7", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

save_plot <- function(plot, filename, width = 12, height = 7.2) {
  ggplot2::ggsave(
    filename = file.path(dir_figures, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 340,
    bg = "white"
  )
  invisible(file.path(dir_figures, filename))
}

reorder_factor_by <- function(label, value, fun = max) {
  tmp <- tibble::tibble(label = as.character(label), value = as.numeric(value)) |>
    dplyr::filter(!is.na(label), !is.na(value)) |>
    dplyr::group_by(label) |>
    dplyr::summarise(order_value = fun(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(order_value)

  factor(as.character(label), levels = tmp$label)
}

standardize_exposure <- function(x) {
  dplyr::case_when(
    stringr::str_detect(clean_ascii(x), "aucun") ~ "Aucun dispositif",
    stringr::str_detect(clean_ascii(x), "organise|organis") ~ "Dispositif organisé",
    stringr::str_detect(clean_ascii(x), "auto") ~ "Autoformation / autre seulement",
    TRUE ~ as.character(x)
  )
}

add_fdr <- function(data, group_vars = c("outcome_label"),
                    conf_low_col = NULL,
                    conf_high_col = NULL) {
  # Version robuste : les modèles score/item utilisent conf_low_pp/conf_high_pp,
  # tandis que les modèles Q8 utilisent conf_low_pp_approx/conf_high_pp_approx.
  # On détecte automatiquement les colonnes disponibles pour éviter l'erreur :
  # object 'conf_low_pp' not found.
  if (!has_rows(data) || !"p.value" %in% names(data)) return(data)

  if (is.null(conf_low_col)) {
    conf_low_col <- dplyr::case_when(
      "conf_low_pp" %in% names(data) ~ "conf_low_pp",
      "conf_low_pp_approx" %in% names(data) ~ "conf_low_pp_approx",
      "conf.low" %in% names(data) ~ "conf.low",
      TRUE ~ NA_character_
    )
  }

  if (is.null(conf_high_col)) {
    conf_high_col <- dplyr::case_when(
      "conf_high_pp" %in% names(data) ~ "conf_high_pp",
      "conf_high_pp_approx" %in% names(data) ~ "conf_high_pp_approx",
      "conf.high" %in% names(data) ~ "conf.high",
      TRUE ~ NA_character_
    )
  }

  out <- data |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_vars))) |>
    dplyr::mutate(
      p_fdr = stats::p.adjust(p.value, method = "BH")
    ) |>
    dplyr::ungroup()

  if (!is.na(conf_low_col) && !is.na(conf_high_col) &&
      conf_low_col %in% names(out) && conf_high_col %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        .conf_low_for_evidence = .data[[conf_low_col]],
        .conf_high_for_evidence = .data[[conf_high_col]],
        evidence = dplyr::case_when(
          !is.na(.conf_low_for_evidence) & .conf_low_for_evidence > 0 & p_fdr < 0.05 ~ "Positif robuste FDR<0,05",
          !is.na(.conf_high_for_evidence) & .conf_high_for_evidence < 0 & p_fdr < 0.05 ~ "Négatif robuste FDR<0,05",
          !is.na(.conf_low_for_evidence) & .conf_low_for_evidence > 0 ~ "Positif non corrigé",
          !is.na(.conf_high_for_evidence) & .conf_high_for_evidence < 0 ~ "Négatif non corrigé",
          TRUE ~ "Écart incertain"
        )
      ) |>
      dplyr::select(-.conf_low_for_evidence, -.conf_high_for_evidence)
  } else {
    out <- out |>
      dplyr::mutate(
        evidence = dplyr::case_when(
          !is.na(p_fdr) & p_fdr < 0.05 ~ "Association robuste FDR<0,05",
          TRUE ~ "Écart incertain"
        )
      )
  }

  out
}

# -----------------------------------------------------------------------------
# 3. Lecture des sorties du script 01
# -----------------------------------------------------------------------------

rds_path <- file.path(main_data, "osyr_v2_corrigee_clean.rds")
csv_path <- file.path(main_data, "osyr_v2_corrigee_clean.csv")

if (file.exists(rds_path)) {
  message("Lecture : ", rds_path)
  df <- readRDS(rds_path)
} else if (file.exists(csv_path)) {
  message("Lecture : ", csv_path)
  df <- readr::read_csv(csv_path, show_col_types = FALSE)
} else {
  stop("Base enrichie introuvable. Lancez d'abord le script 01.")
}

q4_long <- read_csv_safe(file.path(main_data, "q4_long.csv"))
q5_long <- read_csv_safe(file.path(main_data, "q5_long.csv"))
q8_devices_long <- read_csv_safe(file.path(main_data, "q8_devices_long.csv"))
q12_long <- read_csv_safe(file.path(main_data, "q12_long.csv"))
q13_long <- read_csv_safe(file.path(main_data, "q13_long.csv"))
q15_long <- read_csv_safe(file.path(main_data, "q15_long.csv"))

if (!".weight" %in% names(df)) df$.weight <- 1
if (!"weight_none" %in% names(df)) df$weight_none <- 1
if (!"exposure2" %in% names(df)) stop("La variable exposure2 est absente. Relancer le script 01.")
if (!"discipline_detail" %in% names(df)) stop("La variable discipline_detail est absente. Relancer le script 01 v14.")

df <- df |>
  dplyr::mutate(
    exposure2 = standardize_exposure(exposure2),
    exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé")),
    exposure3 = if ("exposure3" %in% names(df)) standardize_exposure(exposure3) else NA_character_
  )

for (obj in c("q4_long", "q5_long", "q8_devices_long", "q12_long", "q13_long", "q15_long")) {
  x <- get(obj)
  if (has_rows(x) && "exposure2" %in% names(x)) {
    x <- x |>
      dplyr::mutate(
        exposure2 = standardize_exposure(exposure2),
        exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé"))
      )
    assign(obj, x)
  }
}

# Contrôle qualité des libellés de discipline.
discipline_label_quality_runtime <- df |>
  dplyr::distinct(discipline_code, discipline_detail) |>
  dplyr::mutate(
    label_is_fallback = stringr::str_detect(as.character(discipline_detail), "^Discipline code|^Code"),
    label_length = stringr::str_length(as.character(discipline_detail))
  )

save_method(discipline_label_quality_runtime, "discipline_label_quality_runtime_v12")

if (any(discipline_label_quality_runtime$label_is_fallback, na.rm = TRUE)) {
  stop(
    "Les libellés de discipline ne sont pas récupérés correctement : discipline_detail contient encore des libellés génériques.\n",
    "Relancez le script 01 v14 ou complétez data/q2_discipline_labels.csv."
  )
}

# -----------------------------------------------------------------------------
# 4. Registre méthodologique, variables et couverture analytique
# -----------------------------------------------------------------------------

analysis_registry <- tibble::tribble(
  ~bloc, ~question, ~sortie_principale, ~statut,
  "Pondération", "Les résultats changent-ils avec/sans poids ?", "models/score_tests_weighted_unweighted_discipline_detail_broad.csv", "implémenté",
  "Pondération", "Quels poids sont disponibles et utilisables ?", "methodology/weight_registry_v12.csv", "implémenté",
  "Discipline", "Les conclusions changent-elles avec discipline détaillée vs agrégée ?", "figures/02_sensibilite_discipline_detail_vs_agregee.png", "implémenté",
  "Items", "Quels items portent les écarts ?", "models/item_tests_weighted_unweighted_discipline_detail.csv", "implémenté",
  "Multiplicité", "Les résultats résistent-ils à une correction FDR ?", "models/item_tests_weighted_unweighted_discipline_detail_fdr.csv", "implémenté",
  "Composition", "Les exposés et non exposés diffèrent-ils en composition ?", "methodology/covariate_balance_exposed_nonexposed.csv", "implémenté",
  "Q8", "Quels dispositifs sont associés à la langue et aux disciplines ?", "tables/q8_device_models_summary.csv", "implémenté si Q8 disponible",
  "Q5", "Où le gap connaissance-usage est-il le plus fort ?", "tables/q5_gap_interpretive_summary.csv", "implémenté si Q5 disponible",
  "Synthèse", "Quels résultats sont les plus stables et interprétables ?", "tables/interpretive_findings_for_report.csv", "implémenté"
)

save_method(analysis_registry, "analysis_registry_v12")

# -----------------------------------------------------------------------------
# 5. Registre des tests et variables de pondération
# -----------------------------------------------------------------------------

weight_vars <- names(df)[
  stringr::str_detect(stringr::str_to_lower(names(df)), "poids|weight|pond") &
    purrr::map_lgl(df, is.numeric)
]

weight_vars <- unique(c("weight_none", ".weight", weight_vars))
weight_vars <- existing_vars(weight_vars, df)

weight_registry <- purrr::map_dfr(weight_vars, function(wv) {
  tibble::tibble(
    weight_var = wv,
    n_missing = sum(is.na(df[[wv]])),
    n_non_positive = sum(!is.na(df[[wv]]) & df[[wv]] <= 0),
    min_weight = min(df[[wv]], na.rm = TRUE),
    max_weight = max(df[[wv]], na.rm = TRUE),
    mean_weight = mean(df[[wv]], na.rm = TRUE),
    sd_weight = stats::sd(df[[wv]], na.rm = TRUE),
    cv_weight = sd_weight / mean_weight,
    sum_weight = sum(df[[wv]], na.rm = TRUE),
    interpretation = dplyr::case_when(
      wv == "weight_none" ~ "Analyse non pondérée",
      wv == ".weight" ~ "Pondération principale issue de Poids",
      TRUE ~ "Autre variable de pondération détectée"
    )
  )
})

save_method(weight_registry, "weight_registry_v12")

score_labels <- tibble::tribble(
  ~outcome, ~outcome_label, ~family,
  "score_q4_practices_research", "Pratiques de recherche déjà réalisées", "Pratiques",
  "score_q5_known_well", "Notions et outils bien connus", "Connaissance/usage",
  "score_q5_used", "Notions et outils déjà utilisés", "Connaissance/usage",
  "score_q13_open_intentions", "Intentions de pratiques ouvertes", "Intentions",
  "score_q13_dont_know", "Intentions : je ne sais pas", "Incertitude",
  "score_q12_incitation", "Environnement perçu comme incitatif", "Environnement",
  "score_q12_frein", "Environnement perçu comme un frein", "Environnement",
  "score_q15_agreement", "Accord avec les affirmations SO", "Représentations",
  "score_q15_benefits", "Bénéfices scientifiques perçus", "Représentations",
  "score_q15_constraints", "Contraintes institutionnelles/économiques", "Représentations",
  "score_q15_risks", "Risques individuels perçus", "Représentations"
) |>
  dplyr::filter(outcome %in% names(df))

core_outcomes <- score_labels$outcome

# -----------------------------------------------------------------------------
# 6. Modèles formels scores : avec/sans pondération, discipline détaillée/agrégée
# -----------------------------------------------------------------------------

fit_one_model <- function(data, outcome, outcome_label, weight_var, discipline_level = "detail") {
  if (!outcome %in% names(data)) return(model_empty())

  discipline_var <- if (discipline_level == "detail") "discipline_detail" else "discipline_broad"
  controls <- existing_vars(c("exposure2", "year", discipline_var, "language_group"), data)
  if (!"exposure2" %in% controls) return(model_empty())

  d <- data |>
    dplyr::select(dplyr::all_of(c(outcome, controls, weight_var))) |>
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(exposure2))

  if (nrow(d) < 30 || dplyr::n_distinct(d$exposure2, na.rm = TRUE) < 2) return(model_empty())

  controls2 <- controls[purrr::map_lgl(controls, ~ dplyr::n_distinct(d[[.x]], na.rm = TRUE) >= 2)]
  rhs <- paste(controls2, collapse = " + ")
  f <- stats::as.formula(paste(outcome, "~", rhs))

  if (weight_var == "weight_none") {
    mod <- tryCatch(stats::lm(f, data = d), error = function(e) NULL)
    model_type <- "unweighted_lm"
  } else {
    mod <- tryCatch(survey::svyglm(f, design = make_design(d, weight_var)), error = function(e) NULL)
    model_type <- "survey_weighted_lpm"
  }

  if (is.null(mod)) return(model_empty())

  broom::tidy(mod, conf.int = TRUE) |>
    dplyr::filter(term == "exposure2Dispositif organisé") |>
    dplyr::mutate(
      outcome = outcome,
      outcome_label = outcome_label,
      model_type = model_type,
      weight_var = weight_var,
      discipline_level = discipline_level,
      estimate_pp = 100 * estimate,
      conf_low_pp = 100 * conf.low,
      conf_high_pp = 100 * conf.high,
      n_model = nrow(d),
      controls = rhs
    )
}

score_weighted_unweighted_tests <- purrr::map_dfr(seq_len(nrow(score_labels)), function(i) {
  purrr::map_dfr(weight_vars, function(wv) {
    dplyr::bind_rows(
      fit_one_model(df, score_labels$outcome[i], score_labels$outcome_label[i], wv, "detail"),
      fit_one_model(df, score_labels$outcome[i], score_labels$outcome_label[i], wv, "broad")
    )
  })
}) |>
  dplyr::left_join(score_labels |> dplyr::select(outcome, family), by = "outcome") |>
  add_fdr(group_vars = c("model_type", "weight_var", "discipline_level"))

save_model(score_weighted_unweighted_tests, "score_tests_weighted_unweighted_discipline_detail_broad")
save_model(score_weighted_unweighted_tests, "score_tests_weighted_unweighted_discipline_detail_broad_fdr")

# Synthèse de robustesse score par score.
score_robustness_summary <- score_weighted_unweighted_tests |>
  dplyr::filter(weight_var %in% c("weight_none", ".weight")) |>
  dplyr::mutate(
    scenario = dplyr::case_when(
      weight_var == "weight_none" & discipline_level == "detail" ~ "Non pondéré + discipline détaillée",
      weight_var == "weight_none" & discipline_level == "broad" ~ "Non pondéré + discipline agrégée",
      weight_var == ".weight" & discipline_level == "detail" ~ "Pondéré + discipline détaillée",
      weight_var == ".weight" & discipline_level == "broad" ~ "Pondéré + discipline agrégée",
      TRUE ~ paste(weight_var, discipline_level)
    ),
    sign_estimate = dplyr::case_when(
      estimate_pp > 0 ~ "positive",
      estimate_pp < 0 ~ "negative",
      TRUE ~ "zero"
    )
  ) |>
  dplyr::group_by(outcome, outcome_label, family) |>
  dplyr::summarise(
    n_scenarios = dplyr::n(),
    min_estimate_pp = min(estimate_pp, na.rm = TRUE),
    max_estimate_pp = max(estimate_pp, na.rm = TRUE),
    median_estimate_pp = stats::median(estimate_pp, na.rm = TRUE),
    sign_stable = dplyr::n_distinct(sign_estimate[!is.na(sign_estimate)]) == 1,
    n_positive_ci = sum(conf_low_pp > 0, na.rm = TRUE),
    n_negative_ci = sum(conf_high_pp < 0, na.rm = TRUE),
    n_fdr_robust = sum(p_fdr < 0.05, na.rm = TRUE),
    conclusion = dplyr::case_when(
      sign_stable & n_positive_ci >= 2 & n_fdr_robust >= 1 ~ "Association positive stable",
      sign_stable & n_negative_ci >= 2 & n_fdr_robust >= 1 ~ "Association négative stable",
      sign_stable ~ "Signe stable mais incertitude statistique",
      TRUE ~ "Résultat sensible aux spécifications"
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(abs(median_estimate_pp)))

save_table(score_robustness_summary, "score_robustness_summary")

# -----------------------------------------------------------------------------
# 7. Tests item par item avec/sans pondération + FDR
# -----------------------------------------------------------------------------

fit_item_models <- function(long_df, outcome, bloc_label, weight_vars, discipline_level = "detail") {
  if (!has_rows(long_df) || !outcome %in% names(long_df)) return(model_empty())
  items <- unique(long_df$item)

  purrr::map_dfr(items, function(it) {
    d_item <- long_df |>
      dplyr::filter(item == it) |>
      dplyr::mutate(.outcome = as.numeric(.data[[outcome]]))

    item_lab <- dplyr::first(d_item$item_label)

    purrr::map_dfr(weight_vars, function(wv) {
      if (!wv %in% names(d_item)) return(model_empty())

      discipline_var <- if (discipline_level == "detail") "discipline_detail" else "discipline_broad"
      controls <- existing_vars(c("exposure2", "year", discipline_var, "language_group"), d_item)
      if (!"exposure2" %in% controls) return(model_empty())

      d <- d_item |>
        dplyr::select(.outcome, dplyr::all_of(c(controls, wv))) |>
        dplyr::filter(!is.na(.outcome), !is.na(exposure2))

      if (nrow(d) < 30 || dplyr::n_distinct(d$exposure2, na.rm = TRUE) < 2) return(model_empty())

      controls2 <- controls[purrr::map_lgl(controls, ~ dplyr::n_distinct(d[[.x]], na.rm = TRUE) >= 2)]
      rhs <- paste(controls2, collapse = " + ")
      f <- stats::as.formula(paste(".outcome ~", rhs))

      if (wv == "weight_none") {
        mod <- tryCatch(stats::lm(f, data = d), error = function(e) NULL)
        model_type <- "unweighted_lm"
      } else {
        mod <- tryCatch(survey::svyglm(f, design = make_design(d, wv)), error = function(e) NULL)
        model_type <- "survey_weighted_lpm"
      }

      if (is.null(mod)) return(model_empty())

      broom::tidy(mod, conf.int = TRUE) |>
        dplyr::filter(term == "exposure2Dispositif organisé") |>
        dplyr::mutate(
          outcome = outcome,
          outcome_label = bloc_label,
          item = it,
          item_label = item_lab,
          model_type = model_type,
          weight_var = wv,
          discipline_level = discipline_level,
          estimate_pp = 100 * estimate,
          conf_low_pp = 100 * conf.low,
          conf_high_pp = 100 * conf.high,
          n_model = nrow(d),
          controls = rhs
        )
    })
  })
}

item_weight_vars <- existing_vars(c("weight_none", ".weight"), q5_long)

item_tests <- dplyr::bind_rows(
  fit_item_models(q5_long, "known_well", "Q5 connaissance", item_weight_vars, "detail"),
  fit_item_models(q5_long, "used", "Q5 usage", item_weight_vars, "detail"),
  fit_item_models(q13_long, "yes", "Q13 intentions oui", existing_vars(c("weight_none", ".weight"), q13_long), "detail"),
  fit_item_models(q13_long, "dont_know", "Q13 je ne sais pas", existing_vars(c("weight_none", ".weight"), q13_long), "detail"),
  fit_item_models(q12_long, "incitation", "Q12 incitation", existing_vars(c("weight_none", ".weight"), q12_long), "detail"),
  fit_item_models(q12_long, "frein", "Q12 frein", existing_vars(c("weight_none", ".weight"), q12_long), "detail"),
  fit_item_models(q15_long, "agree", "Q15 accord", existing_vars(c("weight_none", ".weight"), q15_long), "detail")
) |>
  add_fdr(group_vars = c("outcome_label", "weight_var", "discipline_level"))

save_model(item_tests, "item_tests_weighted_unweighted_discipline_detail")
save_model(item_tests, "item_tests_weighted_unweighted_discipline_detail_fdr")

item_tests_summary <- item_tests |>
  dplyr::filter(weight_var == ".weight", discipline_level == "detail") |>
  dplyr::mutate(abs_estimate_pp = abs(estimate_pp)) |>
  dplyr::arrange(outcome_label, dplyr::desc(abs_estimate_pp)) |>
  dplyr::group_by(outcome_label) |>
  dplyr::slice_head(n = 15) |>
  dplyr::ungroup() |>
  dplyr::select(
    outcome_label, item, item_label, estimate_pp, conf_low_pp, conf_high_pp,
    p.value, p_fdr, evidence, n_model
  )

save_table(item_tests_summary, "item_tests_top15_by_bloc")

# -----------------------------------------------------------------------------
# 8. Balance des covariables exposés / non exposés
# -----------------------------------------------------------------------------

standardized_difference_numeric <- function(x, g, w) {
  g <- as.character(g)
  ok <- !is.na(x) & !is.na(g) & !is.na(w) & g %in% c("Aucun dispositif", "Dispositif organisé")
  if (!any(ok)) return(NA_real_)
  x <- as.numeric(x[ok]); g <- g[ok]; w <- as.numeric(w[ok])
  m0 <- w_mean(x[g == "Aucun dispositif"], w[g == "Aucun dispositif"])
  m1 <- w_mean(x[g == "Dispositif organisé"], w[g == "Dispositif organisé"])
  sd_pooled <- stats::sd(x, na.rm = TRUE)
  if (is.na(sd_pooled) || sd_pooled == 0) return(NA_real_)
  (m1 - m0) / sd_pooled
}

covariate_balance <- tibble::tibble()
balance_vars <- existing_vars(c("year_code", "discipline_code", "language_group", "discipline_detail", "institution"), df)

if (length(balance_vars) > 0) {
  covariate_balance <- purrr::map_dfr(balance_vars, function(v) {
    if (is.numeric(df[[v]])) {
      tibble::tibble(
        covariate = v,
        modality = "numeric",
        standardized_difference = standardized_difference_numeric(df[[v]], df$exposure2, df$.weight),
        type = "numeric"
      )
    } else {
      tab <- df |>
        dplyr::filter(!is.na(exposure2), !is.na(.data[[v]])) |>
        dplyr::group_by(exposure2, modality = .data[[v]]) |>
        dplyr::summarise(weighted_n = sum(.weight, na.rm = TRUE), .groups = "drop_last") |>
        dplyr::mutate(pct = weighted_n / sum(weighted_n, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::select(exposure2, modality, pct) |>
        tidyr::pivot_wider(names_from = exposure2, values_from = pct, values_fill = 0) |>
        janitor::clean_names()

      if (!all(c("aucun_dispositif", "dispositif_organise") %in% names(tab))) return(tibble::tibble())

      tab |>
        dplyr::mutate(
          covariate = v,
          standardized_difference = dispositif_organise - aucun_dispositif,
          abs_standardized_difference = abs(standardized_difference),
          type = "categorical_prop_diff"
        ) |>
        dplyr::select(covariate, modality, standardized_difference, abs_standardized_difference, type)
    }
  })
}

covariate_balance <- covariate_balance |>
  dplyr::mutate(
    imbalance_flag = dplyr::case_when(
      abs(standardized_difference) >= 0.20 ~ "Fort déséquilibre",
      abs(standardized_difference) >= 0.10 ~ "Déséquilibre modéré",
      TRUE ~ "Équilibre acceptable"
    )
  )

save_method(covariate_balance, "covariate_balance_exposed_nonexposed")

# -----------------------------------------------------------------------------
# 9. Analyses complémentaires avec sorties du script 01 v14
# -----------------------------------------------------------------------------

q5_by_discipline_detail <- read_csv_safe(file.path(main_tables, "q5_by_discipline_detail.csv"))
score_means_by_discipline_detail <- read_csv_safe(file.path(main_tables, "score_means_by_discipline_detail.csv"))
organized_by_discipline_detail_ci <- read_csv_safe(file.path(main_tables, "organized_exposure_by_discipline_detail_ci.csv"))
q8_device_distribution_detail <- read_csv_safe(file.path(main_tables, "q8_device_distribution_detail.csv"))
q8_device_distribution_detail_by_language <- read_csv_safe(file.path(main_tables, "q8_device_distribution_detail_by_language.csv"))
q8_device_distribution_detail_by_discipline <- read_csv_safe(file.path(main_tables, "q8_device_distribution_detail_by_discipline.csv"))

# 9.1 Effet de l'exposition dans chaque discipline.
discipline_detail_effects <- tibble::tibble()

if (all(c("score_q5_known_well", "score_q5_used") %in% names(df))) {
  discipline_detail_effects <- purrr::map_dfr(unique(as.character(df$discipline_detail)), function(disc) {
    d_disc <- df |> dplyr::filter(as.character(discipline_detail) == disc)
    if (nrow(d_disc) < 30 || dplyr::n_distinct(d_disc$exposure2, na.rm = TRUE) < 2) return(model_empty())

    dplyr::bind_rows(
      fit_one_model(d_disc, "score_q5_known_well", "Connaissance Q5", ".weight", "broad") |>
        dplyr::mutate(discipline_detail = disc),
      fit_one_model(d_disc, "score_q5_used", "Usage Q5", ".weight", "broad") |>
        dplyr::mutate(discipline_detail = disc),
      fit_one_model(d_disc, "score_q13_open_intentions", "Intentions Q13", ".weight", "broad") |>
        dplyr::mutate(discipline_detail = disc)
    )
  }) |>
    add_fdr(group_vars = c("outcome_label"))
}

save_model(discipline_detail_effects, "discipline_detail_specific_exposure_effects_fdr")

# 9.2 Modèles centrés dispositifs Q8 : présence de chaque modalité selon langue et discipline.
q8_device_models <- tibble::tibble()
q8_device_models_summary <- tibble::tibble()

if (has_rows(q8_devices_long) && all(c("respondent_id", "device_code", "device_label") %in% names(q8_devices_long))) {
  q8_presence <- q8_devices_long |>
    dplyr::distinct(respondent_id, device_code, device_label, device_type) |>
    dplyr::mutate(has_device = 1) |>
    tidyr::pivot_wider(
      id_cols = respondent_id,
      names_from = device_code,
      values_from = has_device,
      values_fill = 0,
      names_prefix = "q8_device_"
    )

  q8_meta <- q8_devices_long |>
    dplyr::distinct(device_code, device_label, device_type)

  df_q8_model <- df |>
    dplyr::left_join(q8_presence, by = "respondent_id")

  q8_device_vars <- names(df_q8_model) |> stringr::str_subset("^q8_device_")

  for (dv in q8_device_vars) {
    df_q8_model[[dv]][is.na(df_q8_model[[dv]])] <- 0
  }

  fit_device_model <- function(device_var) {
    if (!device_var %in% names(df_q8_model)) return(tibble::tibble())

    d <- df_q8_model |>
      dplyr::select(dplyr::all_of(c(device_var, ".weight", "language_group", "discipline_detail", "year"))) |>
      dplyr::filter(!is.na(.data[[device_var]]))

    if (nrow(d) < 30 || mean(d[[device_var]], na.rm = TRUE) <= 0.02) return(tibble::tibble())

    controls <- existing_vars(c("language_group", "discipline_detail", "year"), d)
    controls <- controls[purrr::map_lgl(controls, ~ dplyr::n_distinct(d[[.x]], na.rm = TRUE) >= 2)]

    if (length(controls) == 0) return(tibble::tibble())

    f <- stats::as.formula(paste(device_var, "~", paste(controls, collapse = " + ")))

    mod <- tryCatch(
      survey::svyglm(f, design = make_design(d, ".weight"), family = quasibinomial()),
      error = function(e) NULL
    )

    if (is.null(mod)) return(tibble::tibble())

    broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term != "(Intercept)") |>
      dplyr::mutate(
        device_var = device_var,
        device_code = suppressWarnings(as.numeric(stringr::str_remove(device_var, "^q8_device_"))),
        estimate_pp_approx = 100 * estimate,
        conf_low_pp_approx = 100 * conf.low,
        conf_high_pp_approx = 100 * conf.high,
        n_model = nrow(d)
      )
  }

  q8_device_models <- purrr::map_dfr(q8_device_vars, fit_device_model) |>
    dplyr::left_join(q8_meta, by = "device_code") |>
    add_fdr(group_vars = c("device_label"))

  save_model(q8_device_models, "q8_device_models_language_discipline_year")
}

if (has_rows(q8_device_models)) {
  q8_device_models_summary <- q8_device_models |>
    dplyr::filter(stringr::str_detect(term, "Questionnaire en anglais")) |>
    dplyr::mutate(abs_estimate_pp = abs(estimate_pp_approx)) |>
    dplyr::arrange(dplyr::desc(abs_estimate_pp)) |>
    dplyr::select(
      device_label, device_type, term, estimate_pp_approx, conf_low_pp_approx,
      conf_high_pp_approx, p.value, p_fdr, evidence, n_model
    )

  save_table(q8_device_models_summary, "q8_device_models_summary")
} else {
  q8_device_models_summary <- tibble::tibble(
    note = "Modèles Q8 non produits : q8_devices_long absent ou effectifs insuffisants."
  )
  save_table(q8_device_models_summary, "q8_device_models_summary")
}

# 9.3 Synthèse interprétative Q5 gap connaissance-usage.
q5_gap_interpretive_summary <- tibble::tibble()

if (has_rows(q5_by_discipline_detail)) {
  q5_gap_interpretive_summary <- q5_by_discipline_detail |>
    dplyr::group_by(item_label, item_family) |>
    dplyr::summarise(
      mean_known_w = mean(pct_known_w, na.rm = TRUE),
      mean_used_w = mean(pct_used_w, na.rm = TRUE),
      mean_gap_pp = mean(gap_pp, na.rm = TRUE),
      max_gap_pp = max(gap_pp, na.rm = TRUE),
      discipline_max_gap = as.character(discipline_detail[which.max(gap_pp)]),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_gap_pp))
}

save_table(q5_gap_interpretive_summary, "q5_gap_interpretive_summary")

# -----------------------------------------------------------------------------
# 10. Visualisations enrichies
# -----------------------------------------------------------------------------

# 10.1 Coefficient plot score : scénario pondération / discipline.
if (has_rows(score_weighted_unweighted_tests)) {
  p_score_tests <- score_weighted_unweighted_tests |>
    dplyr::filter(weight_var %in% c("weight_none", ".weight")) |>
    dplyr::mutate(
      scenario = dplyr::case_when(
        weight_var == "weight_none" & discipline_level == "detail" ~ "Sans poids + discipline détaillée",
        weight_var == "weight_none" & discipline_level == "broad" ~ "Sans poids + discipline agrégée",
        weight_var == ".weight" & discipline_level == "detail" ~ "Pondéré + discipline détaillée",
        weight_var == ".weight" & discipline_level == "broad" ~ "Pondéré + discipline agrégée",
        TRUE ~ paste(weight_var, discipline_level)
      ),
      outcome_label = reorder_factor_by(outcome_label, estimate_pp, max)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = outcome_label, color = scenario)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.45) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
      height = 0.15,
      alpha = 0.48,
      position = ggplot2::position_dodge(width = 0.65)
    ) +
    ggplot2::geom_point(size = 2.6, position = ggplot2::position_dodge(width = 0.65)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::labs(
      title = "Robustesse des effets associés aux dispositifs",
      subtitle = "Comparaison avec/sans pondération et discipline détaillée/agrégée.",
      x = "Différence ajustée : dispositif organisé moins aucun dispositif",
      y = NULL,
      caption = "Modèles associatifs, contrôlés par année, langue et discipline."
    ) +
    theme_osyr(base_size = 11)

  save_plot(p_score_tests, "01_robustesse_scores_pondere_non_pondere.png", width = 13.8, height = 7.8)
  save_plot(p_score_tests, "01_tests_scores_avec_sans_ponderation.png", width = 13.8, height = 7.8)
}

# 10.2 Heatmap de robustesse : valeurs des coefficients selon scénario.
if (has_rows(score_weighted_unweighted_tests)) {
  p_robust_heat <- score_weighted_unweighted_tests |>
    dplyr::filter(weight_var %in% c("weight_none", ".weight")) |>
    dplyr::mutate(
      scenario = dplyr::case_when(
        weight_var == "weight_none" & discipline_level == "detail" ~ "Sans poids\nDisc. détaillée",
        weight_var == "weight_none" & discipline_level == "broad" ~ "Sans poids\nDisc. agrégée",
        weight_var == ".weight" & discipline_level == "detail" ~ "Pondéré\nDisc. détaillée",
        weight_var == ".weight" & discipline_level == "broad" ~ "Pondéré\nDisc. agrégée",
        TRUE ~ paste(weight_var, discipline_level)
      ),
      outcome_label = stringr::str_wrap(outcome_label, 34)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = scenario, y = outcome_label, fill = estimate_pp)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(estimate_pp), " pts")),
      size = 2.8,
      color = "#17324D"
    ) +
    ggplot2::scale_fill_gradient2(
      low = osyr_cols[["coral"]],
      mid = "#F2F4F7",
      high = osyr_cols[["teal"]],
      midpoint = 0,
      labels = function(x) paste0(x, " pts"),
      na.value = "grey90"
    ) +
    ggplot2::labs(
      title = "Carte de robustesse des coefficients",
      subtitle = "Un résultat solide reste du même côté de zéro selon les spécifications.",
      x = NULL,
      y = NULL,
      fill = "Écart"
    ) +
    theme_osyr(base_size = 10.4) +
    ggplot2::theme(legend.position = "right")

  save_plot(p_robust_heat, "02_heatmap_robustesse_scores.png", width = 12.5, height = 7.8)
}

# 10.3 Résumé de robustesse des scores.
if (has_rows(score_robustness_summary)) {
  p_robust_summary <- score_robustness_summary |>
    dplyr::mutate(
      outcome_label = reorder_factor_by(stringr::str_wrap(outcome_label, 42), median_estimate_pp, max),
      conclusion = factor(
        conclusion,
        levels = c(
          "Association positive stable",
          "Association négative stable",
          "Signe stable mais incertitude statistique",
          "Résultat sensible aux spécifications"
        )
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = median_estimate_pp, y = outcome_label, color = conclusion)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054") +
    ggplot2::geom_segment(
      ggplot2::aes(x = min_estimate_pp, xend = max_estimate_pp, yend = outcome_label),
      color = "#98A2B3",
      linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::scale_color_manual(
      values = c(
        "Association positive stable" = osyr_cols[["teal"]],
        "Association négative stable" = osyr_cols[["coral"]],
        "Signe stable mais incertitude statistique" = osyr_cols[["orange"]],
        "Résultat sensible aux spécifications" = osyr_cols[["grey"]]
      ),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = "Quels résultats sont vraiment robustes ?",
      subtitle = "Médiane et amplitude des estimations selon les spécifications.",
      x = "Écart ajusté médian",
      y = NULL,
      color = NULL
    ) +
    theme_osyr(base_size = 10.8)

  save_plot(p_robust_summary, "02b_resume_robustesse_scores.png", width = 13, height = 7.6)
}

# 10.4 Items : top des effets avec FDR.
if (has_rows(item_tests)) {
  p_item_fdr <- item_tests |>
    dplyr::filter(weight_var == ".weight", discipline_level == "detail") |>
    dplyr::mutate(
      item_plot = stringr::str_wrap(item_label, 52),
      item_plot = reorder_factor_by(item_plot, abs(estimate_pp), max),
      fdr_ok = p_fdr < 0.05
    ) |>
    dplyr::group_by(outcome_label) |>
    dplyr::slice_max(order_by = abs(estimate_pp), n = 10, with_ties = FALSE) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = item_plot, color = fdr_ok)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
      height = 0.12,
      alpha = 0.45
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::facet_wrap(~ outcome_label, scales = "free_y") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = osyr_cols[["teal"]], "FALSE" = osyr_cols[["grey"]]),
      labels = c("FALSE" = "Non robuste FDR", "TRUE" = "FDR < 0,05")
    ) +
    ggplot2::labs(
      title = "Items qui portent les écarts exposés / non exposés",
      subtitle = "Modèles pondérés, discipline détaillée ; correction FDR par bloc.",
      x = "Écart ajusté",
      y = NULL,
      color = NULL
    ) +
    theme_osyr(base_size = 9.5)

  save_plot(p_item_fdr, "03_items_top_effets_fdr.png", width = 16, height = 11)
  save_plot(p_item_fdr, "03_tests_items_avec_sans_ponderation.png", width = 16, height = 11)
}

# 10.5 Balance des covariables.
if (has_rows(covariate_balance)) {
  p_balance <- covariate_balance |>
    dplyr::filter(type == "categorical_prop_diff") |>
    dplyr::mutate(
      label = paste(covariate, stringr::str_wrap(as.character(modality), 35), sep = " — "),
      label = reorder_factor_by(label, abs(standardized_difference), max)
    ) |>
    dplyr::slice_max(order_by = abs(standardized_difference), n = 30) |>
    ggplot2::ggplot(ggplot2::aes(x = standardized_difference * 100, y = label, fill = imbalance_flag)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054") +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::scale_fill_manual(
      values = c(
        "Fort déséquilibre" = osyr_cols[["coral"]],
        "Déséquilibre modéré" = osyr_cols[["orange"]],
        "Équilibre acceptable" = osyr_cols[["blue"]]
      ),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = "Déséquilibres de composition entre exposés et non exposés",
      subtitle = "Différences de proportions pondérées avant interprétation des écarts.",
      x = "Différence exposés - non exposés",
      y = NULL,
      fill = NULL,
      caption = "Ces déséquilibres invitent à lire les écarts comme associatifs et non causaux."
    ) +
    theme_osyr(base_size = 10.5)

  save_plot(p_balance, "04_balance_covariables_exposes_non_exposes.png", width = 13.5, height = 9)
}

# 10.6 Effet de l'exposition par discipline détaillée.
if (has_rows(discipline_detail_effects)) {
  p_disc_effects <- discipline_detail_effects |>
    dplyr::filter(weight_var == ".weight") |>
    dplyr::mutate(
      discipline_detail = stringr::str_wrap(as.character(discipline_detail), 35),
      discipline_detail = reorder_factor_by(discipline_detail, estimate_pp, max),
      fdr_ok = p_fdr < 0.05
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = discipline_detail, color = fdr_ok)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
      height = 0.15,
      alpha = 0.45
    ) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::facet_wrap(~ outcome_label) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = osyr_cols[["teal"]], "FALSE" = osyr_cols[["grey"]]),
      labels = c("FALSE" = "Non robuste FDR", "TRUE" = "FDR < 0,05")
    ) +
    ggplot2::labs(
      title = "Effet de l'exposition dans chaque discipline détaillée",
      subtitle = "Modèles pondérés séparés par discipline.",
      x = "Différence ajustée exposés - non exposés",
      y = NULL,
      color = NULL,
      caption = "À lire avec prudence : les IC sont larges dans les disciplines à faibles effectifs."
    ) +
    theme_osyr(base_size = 9.8)

  save_plot(p_disc_effects, "05_effets_exposition_par_discipline_detail.png", width = 14.5, height = 9)
}

# 10.7 Heatmap écarts exposés/non exposés par famille d'objets et discipline.
if (has_rows(q5_long) && "item_family" %in% names(q5_long)) {
  gap_family_disc <- q5_long |>
    dplyr::filter(!is.na(discipline_detail), !is.na(exposure2)) |>
    dplyr::group_by(discipline_detail, item_family, exposure2) |>
    dplyr::summarise(
      pct_known = w_prop(known_well, .weight),
      pct_used = w_prop(used, .weight),
      .groups = "drop"
    )

  gap_family_known <- gap_family_disc |>
    dplyr::select(discipline_detail, item_family, exposure2, pct_known) |>
    tidyr::pivot_wider(names_from = exposure2, values_from = pct_known) |>
    janitor::clean_names()

  gap_family_used <- gap_family_disc |>
    dplyr::select(discipline_detail, item_family, exposure2, pct_used) |>
    tidyr::pivot_wider(names_from = exposure2, values_from = pct_used) |>
    janitor::clean_names()

  if (all(c("aucun_dispositif", "dispositif_organise") %in% names(gap_family_known)) &&
      all(c("aucun_dispositif", "dispositif_organise") %in% names(gap_family_used))) {

    gap_family_combined <- dplyr::bind_rows(
      gap_family_known |>
        dplyr::mutate(metric = "Connaissance", diff_pp = 100 * (dispositif_organise - aucun_dispositif)) |>
        dplyr::select(discipline_detail, item_family, metric, diff_pp),
      gap_family_used |>
        dplyr::mutate(metric = "Usage", diff_pp = 100 * (dispositif_organise - aucun_dispositif)) |>
        dplyr::select(discipline_detail, item_family, metric, diff_pp)
    )

    save_table(gap_family_combined, "q5_family_gap_by_discipline_detail")

    p_gap_family <- gap_family_combined |>
      dplyr::mutate(
        discipline_detail = stringr::str_wrap(as.character(discipline_detail), 32),
        item_family = stringr::str_wrap(as.character(item_family), 28)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = item_family, y = discipline_detail, fill = diff_pp)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.25) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(diff_pp), " pts")), size = 2.4, color = "#17324D") +
      ggplot2::facet_wrap(~ metric) +
      ggplot2::scale_fill_gradient2(
        low = osyr_cols[["coral"]],
        mid = "#F2F4F7",
        high = osyr_cols[["teal"]],
        midpoint = 0,
        labels = function(x) paste0(x, " pts"),
        na.value = "grey90"
      ) +
      ggplot2::labs(
        title = "Où l'exposition fait-elle le plus de différence ?",
        subtitle = "Écarts exposés - non exposés par discipline détaillée et famille d'objets.",
        x = NULL,
        y = NULL,
        fill = "Écart"
      ) +
      theme_osyr(base_size = 9.5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "right")

    save_plot(p_gap_family, "06_heatmap_ecarts_exposition_familles_par_discipline_detail.png", width = 15, height = 9)
  }
}

# 10.8 Q8 : différences par langue et modélisation des dispositifs.
if (has_rows(q8_device_distribution_detail_by_language)) {
  q8_language_diff <- q8_device_distribution_detail_by_language |>
    dplyr::filter(language_group %in% c("Questionnaire en français", "Questionnaire en anglais")) |>
    dplyr::select(language_group, device_label, device_type, pct_respondents_w) |>
    tidyr::pivot_wider(names_from = language_group, values_from = pct_respondents_w) |>
    janitor::clean_names()

  if (all(c("questionnaire_en_francais", "questionnaire_en_anglais") %in% names(q8_language_diff))) {
    q8_language_diff <- q8_language_diff |>
      dplyr::mutate(
        diff_english_minus_french_pp = 100 * (questionnaire_en_anglais - questionnaire_en_francais),
        abs_diff_pp = abs(diff_english_minus_french_pp)
      ) |>
      dplyr::arrange(dplyr::desc(abs_diff_pp))

    save_table(q8_language_diff, "q8_device_language_differences")

    p_q8_language_diff <- q8_language_diff |>
      dplyr::mutate(device_label = forcats::fct_reorder(stringr::str_wrap(device_label, 42), diff_english_minus_french_pp)) |>
      ggplot2::ggplot(ggplot2::aes(x = diff_english_minus_french_pp, y = device_label, fill = diff_english_minus_french_pp > 0)) +
      ggplot2::geom_vline(xintercept = 0, color = "#344054") +
      ggplot2::geom_col(width = 0.68) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = osyr_cols[["teal"]], "FALSE" = osyr_cols[["coral"]]),
        labels = c("FALSE" = "Plus fréquent en français", "TRUE" = "Plus fréquent en anglais")
      ) +
      ggplot2::labs(
        title = "Quels dispositifs sont plus souvent déclarés dans le questionnaire anglais ?",
        subtitle = "Différence de part pondérée : anglais moins français.",
        x = "Différence en points de pourcentage",
        y = NULL,
        fill = NULL
      ) +
      theme_osyr(base_size = 10.8)

    save_plot(p_q8_language_diff, "07_dispositifs_q8_difference_langue.png", width = 13, height = 7.5)
  }
}

if (has_rows(q8_device_models_summary)) {
  p_q8_model_language <- q8_device_models_summary |>
    dplyr::mutate(
      device_label = forcats::fct_reorder(stringr::str_wrap(device_label, 42), estimate_pp_approx),
      fdr_ok = p_fdr < 0.05
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp_approx, y = device_label, color = fdr_ok)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp_approx, xmax = conf_high_pp_approx),
      height = 0.14,
      alpha = 0.5
    ) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = osyr_cols[["teal"]], "FALSE" = osyr_cols[["grey"]]),
      labels = c("FALSE" = "Non robuste FDR", "TRUE" = "FDR < 0,05")
    ) +
    ggplot2::labs(
      title = "Association entre langue du questionnaire et dispositifs Q8",
      subtitle = "Modèles pondérés, contrôlés par année et discipline détaillée.",
      x = "Association approximative en points",
      y = NULL,
      color = NULL,
      caption = "Modèles logistiques : l'échelle en points est une approximation descriptive."
    ) +
    theme_osyr(base_size = 10.6)

  save_plot(p_q8_model_language, "08_modeles_q8_langue_controles.png", width = 13, height = 7.5)
}

# -----------------------------------------------------------------------------
# 11. Synthèse automatique des résultats de tests
# -----------------------------------------------------------------------------

test_summary <- score_weighted_unweighted_tests |>
  dplyr::filter(weight_var %in% c("weight_none", ".weight"), discipline_level == "detail") |>
  dplyr::mutate(
    scenario = ifelse(weight_var == "weight_none", "Sans pondération", "Pondération principale"),
    conclusion = dplyr::case_when(
      conf_low_pp > 0 & p_fdr < 0.05 ~ "association positive robuste",
      conf_high_pp < 0 & p_fdr < 0.05 ~ "association négative robuste",
      conf_low_pp > 0 ~ "association positive non corrigée",
      conf_high_pp < 0 ~ "association négative non corrigée",
      TRUE ~ "écart non concluant"
    ),
    estimate_label = fmt_pp(estimate_pp),
    ci_label = paste0("[", fmt_pp(conf_low_pp), " ; ", fmt_pp(conf_high_pp), "]")
  ) |>
  dplyr::select(
    outcome_label, family, scenario, estimate_pp, conf_low_pp, conf_high_pp,
    estimate_label, ci_label, p.value, p_fdr, conclusion, n_model
  )

save_table(test_summary, "test_summary_scores_weighted_unweighted")

interpretive_findings_for_report <- score_robustness_summary |>
  dplyr::mutate(
    phrase = dplyr::case_when(
      conclusion == "Association positive stable" ~ paste0(
        outcome_label, " : écart positif stable associé à l'exposition, médiane ",
        fmt_pp(median_estimate_pp), "."
      ),
      conclusion == "Association négative stable" ~ paste0(
        outcome_label, " : écart négatif stable associé à l'exposition, médiane ",
        fmt_pp(median_estimate_pp), "."
      ),
      conclusion == "Signe stable mais incertitude statistique" ~ paste0(
        outcome_label, " : signe stable, mais l'incertitude reste importante."
      ),
      TRUE ~ paste0(
        outcome_label, " : résultat sensible aux spécifications, à interpréter avec prudence."
      )
    )
  ) |>
  dplyr::select(family, outcome_label, conclusion, median_estimate_pp, min_estimate_pp, max_estimate_pp, phrase)

save_table(interpretive_findings_for_report, "interpretive_findings_for_report")

# -----------------------------------------------------------------------------
# 12. Export Excel consolidé
# -----------------------------------------------------------------------------

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(name, data) {
    nm <- substr(name, 1, 31)
    openxlsx::addWorksheet(wb, nm)

    if (!has_rows(data)) {
      data <- tibble::tibble(note = "Table vide ou non produite dans cette exécution.")
    }

    openxlsx::writeData(wb, nm, data)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_len(max(1, ncol(data))), widths = "auto")
  }

  add_sheet("analysis_registry", analysis_registry)
  add_sheet("weights", weight_registry)
  add_sheet("score_tests", score_weighted_unweighted_tests)
  add_sheet("score_robustness", score_robustness_summary)
  add_sheet("item_tests", item_tests)
  add_sheet("item_top15", item_tests_summary)
  add_sheet("covariate_balance", covariate_balance)
  add_sheet("discipline_effects", discipline_detail_effects)
  add_sheet("test_summary", test_summary)
  add_sheet("interpretation", interpretive_findings_for_report)

  if (exists("gap_family_combined")) add_sheet("q5_family_gap", gap_family_combined)
  if (has_rows(q5_gap_interpretive_summary)) add_sheet("q5_gap_summary", q5_gap_interpretive_summary)
  if (exists("q8_language_diff")) add_sheet("q8_language_diff", q8_language_diff)
  if (has_rows(q8_device_models_summary)) add_sheet("q8_models", q8_device_models_summary)

  openxlsx::saveWorkbook(
    wb,
    file.path(dir_exports, "osyr_analyses_complementaires_v12_1_consolide.xlsx"),
    overwrite = TRUE
  )
}

# -----------------------------------------------------------------------------
# 13. README des sorties du script 03
# -----------------------------------------------------------------------------

readme <- c(
  "# Sorties du script 03 v12.1",
  "",
  "Ce script produit une couche complémentaire de tests, robustesse et visualisations.",
  "",
  "## Sorties principales",
  "",
  "- `models/score_tests_weighted_unweighted_discipline_detail_broad_fdr.csv` : modèles scores avec/sans pondération, discipline détaillée/agrégée, FDR.",
  "- `models/item_tests_weighted_unweighted_discipline_detail_fdr.csv` : tests item par item avec correction FDR.",
  "- `tables/score_robustness_summary.csv` : synthèse de robustesse des scores.",
  "- `tables/item_tests_top15_by_bloc.csv` : items les plus différenciants par bloc.",
  "- `tables/q8_device_language_differences.csv` : différences de dispositifs Q8 selon la langue.",
  "- `tables/q8_device_models_summary.csv` : modèles Q8 contrôlés par discipline et année.",
  "- `tables/interpretive_findings_for_report.csv` : phrases de synthèse prêtes à être relues pour le rapport.",
  "",
  "## Figures principales",
  "",
  "- `figures/01_robustesse_scores_pondere_non_pondere.png`",
  "- `figures/02_heatmap_robustesse_scores.png`",
  "- `figures/02b_resume_robustesse_scores.png`",
  "- `figures/03_items_top_effets_fdr.png`",
  "- `figures/04_balance_covariables_exposes_non_exposes.png`",
  "- `figures/05_effets_exposition_par_discipline_detail.png`",
  "- `figures/06_heatmap_ecarts_exposition_familles_par_discipline_detail.png`",
  "- `figures/07_dispositifs_q8_difference_langue.png`",
  "- `figures/08_modeles_q8_langue_controles.png`",
  "",
  "## Lecture méthodologique",
  "",
  "Les modèles sont associatifs. Ils ne permettent pas d'attribuer causalement les différences aux dispositifs.",
  "La comparaison pondéré/non pondéré sert à tester la robustesse.",
  "La correction FDR limite la surinterprétation des nombreux tests item par item.",
  "Les analyses Q8 restent descriptives et doivent être lues comme des associations."
)

writeLines(readme, file.path(out_root, "README_SCRIPT03_V12_1_ANALYSES_COMPLEMENTAIRES.md"))

sink(file.path(dir_method, "sessionInfo_script03_v12_1.txt"))
print(sessionInfo())
sink()

message("\nScript 03 v12.1 terminé.")
message("Figures : ", normalizePath(dir_figures, mustWork = FALSE))
message("Export Excel : ", normalizePath(file.path(dir_exports, "osyr_analyses_complementaires_v12_1_consolide.xlsx"), mustWork = FALSE))
