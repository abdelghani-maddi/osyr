# =============================================================================
# SCRIPT — COMPLÉTER ET AUDITER LE PLAN DE DÉPOUILLEMENT
# Version 2026-09-27
# =============================================================================
# RÔLE DANS LE WORKFLOW
#   Confronter les sorties déjà calculées au plan de dépouillement de septembre
#   2026 et produire les analyses manquantes lorsque les variables nécessaires
#   sont disponibles dans la base.
#
# PRINCIPES
#   - chaque bloc ci-dessous correspond à un point explicite du plan ;
#   - aucune variable absente n'est reconstruite par supposition ;
#   - les analyses multiréponses utilisent des dénominateurs au niveau répondant ;
#   - les modèles restent associatifs et utilisent uniquement .weight ;
#   - les analyses exploratoires (Q3, profils, CAH) sont identifiées comme telles.
#
# SORTIE DE CONTRÔLE
#   outputs_osyr_rapport_final/tables/couverture_plan_depouillement_detaillee.csv
#   Cette table distingue : analysé, partiel, non analysé et donnée externe requise.
# =============================================================================
options(scipen = 999, dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE)

pkgs <- c("tidyverse", "survey", "broom", "scales", "forcats", "cluster", "fs")
missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

dirs <- osyr_dirs()
cols <- osyr_colors()
tab_dir <- file.path(dirs$report, "tables")
fig_dir <- file.path(dirs$report, "figures")
ensure_dir(tab_dir)
ensure_dir(fig_dir)

read_clean <- function(name) {
  p <- file.path(dirs$final, "data_clean", name)
  if (!file.exists(p)) return(tibble::tibble())
  readr::read_csv(p, show_col_types = FALSE)
}

read_report <- function(name) {
  p <- file.path(tab_dir, paste0(name, ".csv"))
  if (!file.exists(p)) return(tibble::tibble())
  readr::read_csv(p, show_col_types = FALSE)
}

write_plan <- function(x, name) {
  readr::write_csv(x, file.path(tab_dir, paste0(name, ".csv")))
  invisible(x)
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(as.numeric(x[ok]) * as.numeric(w[ok]), na.rm = TRUE) / sum(as.numeric(w[ok]), na.rm = TRUE)
}

w_prop <- function(x, w) w_mean(as.numeric(x), w)

w_cor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  if (sum(ok) < 3) return(NA_real_)
  x <- as.numeric(x[ok]); y <- as.numeric(y[ok]); w <- as.numeric(w[ok])
  mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w)
  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  vx <- sum(w * (x - mx)^2) / sum(w)
  vy <- sum(w * (y - my)^2) / sum(w)
  if (vx <= 0 || vy <= 0) return(NA_real_)
  cov_xy / sqrt(vx * vy)
}

norm_text <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower(locale = "fr") |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

group_mean <- function(data, groups, value) {
  if (!has_rows(data) || !all(c(groups, value, ".weight") %in% names(data))) return(tibble::tibble())
  data |>
    dplyr::filter(!is.na(.data[[value]]), !is.na(.weight), .weight > 0) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(groups), ~ !is.na(.x))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.weight, na.rm = TRUE),
      mean_w = w_mean(.data[[value]], .weight),
      .groups = "drop"
    )
}

safe_save <- function(p, file, width = 12.8, height = 7.5) {
  ggplot2::ggsave(file.path(fig_dir, file), p, width = width, height = height, dpi = 420, bg = "white")
  file.path(fig_dir, file)
}

add_model_fdr <- function(x) {
  if (!has_rows(x) || !"p.value" %in% names(x)) return(x)
  group_vars <- if ("model" %in% names(x)) "model" else character()
  x |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_vars))) |>
    dplyr::mutate(
      p_fdr = dplyr::if_else(
        term == "(Intercept)",
        NA_real_,
        stats::p.adjust(dplyr::if_else(term == "(Intercept)", NA_real_, p.value), method = "BH")
      )
    ) |>
    dplyr::ungroup()
}

main_rds <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.rds")
main_csv <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.csv")
if (file.exists(main_rds)) {
  df <- readRDS(main_rds)
} else if (file.exists(main_csv)) {
  df <- readr::read_csv(main_csv, show_col_types = FALSE)
} else {
  stop("Base analytique absente. Relancer le script 01.")
}

if (!".weight" %in% names(df)) stop("La pondération .weight est absente.")
df$.weight <- suppressWarnings(as.numeric(df$.weight))

q3_long <- read_clean("q3_long.csv")
q4_long <- read_clean("q4_long.csv")
q5_long <- read_clean("q5_long.csv")
q8_long <- read_clean("q8_devices_long.csv")
q9_long <- read_clean("q9_long.csv")
q11_long <- read_clean("q11_long.csv")
q12_long <- read_clean("q12_long.csv")
q13_long <- read_clean("q13_long.csv")
q14_long <- read_clean("q14_long.csv")
q15_long <- read_clean("q15_long.csv")

# -----------------------------------------------------------------------------
# Variables respondent-level utiles aux croisements du plan
# -----------------------------------------------------------------------------

if (!"score_q11_training_evaluation" %in% names(df) && has_rows(q11_long) && "agree" %in% names(q11_long)) {
  q11_score <- q11_long |>
    dplyr::group_by(respondent_id) |>
    dplyr::summarise(score_q11_training_evaluation = mean(as.numeric(agree), na.rm = TRUE), .groups = "drop")
  q11_score$score_q11_training_evaluation[is.nan(q11_score$score_q11_training_evaluation)] <- NA_real_
  df <- df |> dplyr::left_join(q11_score, by = "respondent_id")
}

director_env <- tibble::tibble()
if (has_rows(q12_long) && all(c("respondent_id", "item_label", "response_num") %in% names(q12_long))) {
  director_env <- q12_long |>
    dplyr::filter(stringr::str_detect(norm_text(item_label), "directeur de these|direction de these")) |>
    dplyr::transmute(
      respondent_id,
      director_environment = dplyr::case_when(
        response_num %in% c(1, 2) ~ "Frein",
        response_num == 3 ~ "Neutre",
        response_num %in% c(4, 5) ~ "Incitation",
        response_num == 97 ~ "Je ne sais pas",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::distinct(respondent_id, .keep_all = TRUE)
  df <- df |> dplyr::left_join(director_env, by = "respondent_id")
}

# -----------------------------------------------------------------------------
# A. Parcours de formation — points 1 à 7 du plan : Q8, Q9, Q10, Q11 et profils
# -----------------------------------------------------------------------------

if ("exposure3" %in% names(df)) {
  formation_global <- df |>
    dplyr::filter(!is.na(exposure3), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(exposure3) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n))
  write_plan(formation_global, "plan_formation_exposition_globale")
}

if ("training_intensity" %in% names(df)) {
  q10_global <- df |>
    dplyr::filter(!is.na(training_intensity), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(training_intensity) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n))
  write_plan(q10_global, "plan_q10_distribution_globale")

  q10_year <- df |>
    dplyr::filter(!is.na(year), !is.na(training_intensity), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(year, training_intensity) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop_last") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n)) |>
    dplyr::ungroup()
  write_plan(q10_year, "plan_q10_par_annee")

  q10_disc <- df |>
    dplyr::filter(!is.na(discipline_detail), !is.na(training_intensity), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(discipline_detail, training_intensity) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop_last") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n)) |>
    dplyr::ungroup()
  write_plan(q10_disc, "plan_q10_par_discipline")
}

if (all(c("exposure3", "discipline_detail") %in% names(df))) {
  exposure_disc <- df |>
    dplyr::filter(!is.na(exposure3), !is.na(discipline_detail), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(discipline_detail, exposure3) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop_last") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n)) |>
    dplyr::ungroup()
  write_plan(exposure_disc, "plan_formation_exposition_par_discipline")
}

q9_available <- has_rows(q9_long) && all(c("respondent_id", "organizer_label", ".weight") %in% names(q9_long))
if (q9_available) {
  denom_q9 <- q9_long |>
    dplyr::distinct(respondent_id, .weight) |>
    dplyr::summarise(total_w = sum(.weight[!is.na(.weight) & .weight > 0], na.rm = TRUE)) |>
    dplyr::pull(total_w)

  q9_overall <- q9_long |>
    dplyr::filter(!is.na(organizer_label), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(organizer_label) |>
    dplyr::summarise(n = dplyr::n_distinct(respondent_id), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_respondents_w = weighted_n / denom_q9) |>
    dplyr::arrange(dplyr::desc(pct_respondents_w))
  write_plan(q9_overall, "plan_q9_organisateurs_global")

  q9_by_year <- q9_long |>
    dplyr::filter(!is.na(year), !is.na(organizer_label), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(year) |>
    dplyr::mutate(total_w = sum(.weight[!duplicated(respondent_id)])) |>
    dplyr::group_by(year, organizer_label) |>
    dplyr::summarise(n = dplyr::n_distinct(respondent_id), weighted_n = sum(.weight), total_w = max(total_w), .groups = "drop") |>
    dplyr::mutate(pct_respondents_w = weighted_n / total_w)
  write_plan(q9_by_year, "plan_q9_organisateurs_par_annee")

  q9_by_disc <- q9_long |>
    dplyr::filter(!is.na(discipline_detail), !is.na(organizer_label), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(discipline_detail) |>
    dplyr::mutate(total_w = sum(.weight[!duplicated(respondent_id)])) |>
    dplyr::group_by(discipline_detail, organizer_label) |>
    dplyr::summarise(n = dplyr::n_distinct(respondent_id), weighted_n = sum(.weight), total_w = max(total_w), .groups = "drop") |>
    dplyr::mutate(pct_respondents_w = weighted_n / total_w)
  write_plan(q9_by_disc, "plan_q9_organisateurs_par_discipline")

  if ("score_q11_training_evaluation" %in% names(df)) {
    q9_q11 <- q9_long |>
      dplyr::distinct(respondent_id, organizer_label) |>
      dplyr::left_join(df |> dplyr::select(respondent_id, .weight, score_q11_training_evaluation), by = "respondent_id") |>
      dplyr::filter(!is.na(score_q11_training_evaluation), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(organizer_label) |>
      dplyr::summarise(n = dplyr::n(), evaluation_w = w_mean(score_q11_training_evaluation, .weight), .groups = "drop")
    write_plan(q9_q11, "plan_q11_selon_organisateur_q9")
  }
}

if ("score_q11_training_evaluation" %in% names(df)) {
  if ("training_intensity" %in% names(df)) {
    write_plan(
      group_mean(df, c("training_intensity"), "score_q11_training_evaluation"),
      "plan_q11_selon_volume_q10"
    )
  }

  q8_modes <- c("q8_has_presentiel", "q8_has_distanciel", "q8_has_async", "q8_has_autoformation")
  q8_modes <- q8_modes[q8_modes %in% names(df)]
  if (length(q8_modes) > 0) {
    q11_modes <- purrr::map_dfr(q8_modes, function(v) {
      df |>
        dplyr::filter(.data[[v]] %in% TRUE, !is.na(score_q11_training_evaluation), !is.na(.weight), .weight > 0) |>
        dplyr::summarise(n = dplyr::n(), evaluation_w = w_mean(score_q11_training_evaluation, .weight)) |>
        dplyr::mutate(modality = v, .before = 1)
    })
    write_plan(q11_modes, "plan_q11_selon_modalite_q8")
  }

  if ("director_environment" %in% names(df)) {
    write_plan(
      group_mean(df, c("director_environment"), "score_q11_training_evaluation"),
      "plan_q11_selon_direction_these_q12"
    )
  }

  if (all(c("score_q12_incitation", "score_q12_frein") %in% names(df))) {
    q11_env <- df |>
      dplyr::filter(!is.na(score_q11_training_evaluation)) |>
      dplyr::mutate(
        incitation_quartile = dplyr::ntile(score_q12_incitation, 4),
        frein_quartile = dplyr::ntile(score_q12_frein, 4)
      )
    write_plan(group_mean(q11_env, c("incitation_quartile"), "score_q11_training_evaluation"), "plan_q11_selon_incitation_q12")
    write_plan(group_mean(q11_env, c("frein_quartile"), "score_q11_training_evaluation"), "plan_q11_selon_frein_q12")
  }
}

focus_scores <- c(
  "score_q4_practices_research", "score_q5_known_well", "score_q5_used",
  "score_q12_incitation", "score_q12_frein", "score_q13_open_intentions",
  "score_q13_dont_know", "score_q15_benefits", "score_q15_constraints", "score_q15_risks"
)
focus_scores <- focus_scores[focus_scores %in% names(df)]

if ("exposure3" %in% names(df) && length(focus_scores) > 0) {
  focus_training <- df |>
    dplyr::filter(exposure3 %in% c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé")) |>
    dplyr::select(exposure3, .weight, dplyr::all_of(focus_scores)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(focus_scores), names_to = "indicator", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(exposure3, indicator) |>
    dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
  write_plan(focus_training, "plan_focus_non_formes_autoformes_organises")

  composition_vars <- c("year", "discipline_detail", "director_environment")
  composition_vars <- composition_vars[composition_vars %in% names(df)]
  focus_composition <- purrr::map_dfr(composition_vars, function(v) {
    df |>
      dplyr::filter(
        exposure3 %in% c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé"),
        !is.na(.data[[v]]), !is.na(.weight), .weight > 0
      ) |>
      dplyr::group_by(exposure3, category = .data[[v]]) |>
      dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
      dplyr::mutate(pct_w = weighted_n / sum(weighted_n), variable = v, .before = 1) |>
      dplyr::ungroup()
  })
  write_plan(focus_composition, "plan_focus_non_formes_autoformes_composition")
}

mode_flags <- c("q8_has_presentiel", "q8_has_distanciel")
if (all(mode_flags %in% names(df)) && length(focus_scores) > 0) {
  df_modes <- df |>
    dplyr::mutate(
      training_mode = dplyr::case_when(
        q8_has_presentiel & q8_has_distanciel ~ "Présentiel et distanciel",
        q8_has_distanciel & !q8_has_presentiel ~ "Distanciel seulement",
        q8_has_presentiel & !q8_has_distanciel ~ "Présentiel seulement",
        TRUE ~ "Autre / aucun"
      )
    )
  mode_scores <- df_modes |>
    dplyr::select(training_mode, .weight, dplyr::all_of(focus_scores)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(focus_scores), names_to = "indicator", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(training_mode, indicator) |>
    dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
  write_plan(mode_scores, "plan_focus_presentiel_distanciel_scores")
}

# -----------------------------------------------------------------------------
# B. Connaissances — points 8 à 10 : familles Q5, facteurs associés, mots Q3 et liens Q4-Q5
# -----------------------------------------------------------------------------

q5_family_person <- tibble::tibble()
if (has_rows(q5_long) && all(c("respondent_id", "item_family", "known_well", "used") %in% names(q5_long))) {
  q5_family_person <- q5_long |>
    dplyr::filter(!is.na(item_family)) |>
    dplyr::group_by(respondent_id, item_family) |>
    dplyr::summarise(
      knowledge_family = mean(as.numeric(known_well), na.rm = TRUE),
      usage_family = mean(as.numeric(used), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      knowledge_family = dplyr::if_else(is.nan(knowledge_family), NA_real_, knowledge_family),
      usage_family = dplyr::if_else(is.nan(usage_family), NA_real_, usage_family)
    ) |>
    dplyr::left_join(
      df |> dplyr::select(
        respondent_id, .weight, dplyr::any_of(c(
          "exposure3", "training_intensity", "year", "discipline_detail", "discipline_broad",
          "director_environment", "q8_has_presentiel", "q8_has_distanciel"
        ))
      ),
      by = "respondent_id"
    )

  write_plan(
    q5_family_person |>
      dplyr::filter(!is.na(exposure3), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(exposure3, item_family) |>
      dplyr::summarise(
        n = dplyr::n(),
        knowledge_w = w_mean(knowledge_family, .weight),
        usage_w = w_mean(usage_family, .weight),
        .groups = "drop"
      ),
    "plan_q5_familles_par_exposition"
  )

  if ("training_intensity" %in% names(q5_family_person)) {
    write_plan(
      q5_family_person |>
        dplyr::filter(!is.na(training_intensity), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(training_intensity, item_family) |>
        dplyr::summarise(knowledge_w = w_mean(knowledge_family, .weight), usage_w = w_mean(usage_family, .weight), n = dplyr::n(), .groups = "drop"),
      "plan_q5_familles_par_volume_formation"
    )
  }

  if ("year" %in% names(q5_family_person)) {
    write_plan(
      q5_family_person |>
        dplyr::filter(!is.na(year), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(year, item_family) |>
        dplyr::summarise(knowledge_w = w_mean(knowledge_family, .weight), usage_w = w_mean(usage_family, .weight), n = dplyr::n(), .groups = "drop"),
      "plan_q5_familles_par_annee"
    )
  }

  if ("discipline_detail" %in% names(q5_family_person)) {
    write_plan(
      q5_family_person |>
        dplyr::filter(!is.na(discipline_detail), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(discipline_detail, item_family) |>
        dplyr::summarise(knowledge_w = w_mean(knowledge_family, .weight), usage_w = w_mean(usage_family, .weight), n = dplyr::n(), .groups = "drop"),
      "plan_q5_familles_par_discipline"
    )
  }

  if ("director_environment" %in% names(q5_family_person)) {
    write_plan(
      q5_family_person |>
        dplyr::filter(!is.na(director_environment), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(director_environment, item_family) |>
        dplyr::summarise(knowledge_w = w_mean(knowledge_family, .weight), usage_w = w_mean(usage_family, .weight), n = dplyr::n(), .groups = "drop"),
      "plan_q5_familles_selon_direction_these"
    )
  }

  if (all(c("q8_has_presentiel", "q8_has_distanciel") %in% names(q5_family_person))) {
    q5_mode <- q5_family_person |>
      dplyr::mutate(
        training_mode = dplyr::case_when(
          q8_has_presentiel & q8_has_distanciel ~ "Présentiel et distanciel",
          q8_has_distanciel & !q8_has_presentiel ~ "Distanciel seulement",
          q8_has_presentiel & !q8_has_distanciel ~ "Présentiel seulement",
          TRUE ~ "Autre / aucun"
        )
      ) |>
      dplyr::filter(!is.na(.weight), .weight > 0) |>
      dplyr::group_by(training_mode, item_family) |>
      dplyr::summarise(knowledge_w = w_mean(knowledge_family, .weight), usage_w = w_mean(usage_family, .weight), n = dplyr::n(), .groups = "drop")
    write_plan(q5_mode, "plan_q5_familles_par_presentiel_distanciel")
  }
}

q4_q5_rules <- tibble::tribble(
  ~link, ~q4_pattern, ~q5_pattern,
  "Article soumis -> accès ouvert", "soumis un article", "voie|archive ouverte|revue.*acces ouvert|plateforme.*revue",
  "Données produites -> gestion et partage", "produit ou collecte.*donnee", "fair|plan.*gestion|entrepot|plateforme.*plan.*gestion",
  "Code produit -> code ouvert", "produit du code|logiciel|script|macro", "plateforme.*code|archivage du code|software heritage",
  "Réutilisation de données -> licences et entrepôts", "reutilise.*donnee", "licence|creative commons|entrepot"
)

q4_q5_links <- tibble::tibble()
if (has_rows(q4_long) && has_rows(q5_long)) {
  q4_q5_links <- purrr::pmap_dfr(q4_q5_rules, function(link, q4_pattern, q5_pattern) {
    q4_one <- q4_long |>
      dplyr::filter(stringr::str_detect(norm_text(item_label), q4_pattern)) |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise(practice_done = any(positive %in% TRUE), .groups = "drop")

    q5_one <- q5_long |>
      dplyr::filter(stringr::str_detect(norm_text(item_label), q5_pattern)) |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise(
        relevant_knowledge = mean(as.numeric(known_well), na.rm = TRUE),
        relevant_usage = mean(as.numeric(used), na.rm = TRUE),
        .groups = "drop"
      )

    q4_one |>
      dplyr::inner_join(q5_one, by = "respondent_id") |>
      dplyr::left_join(df |> dplyr::select(respondent_id, .weight), by = "respondent_id") |>
      dplyr::filter(!is.na(.weight), .weight > 0) |>
      dplyr::group_by(practice_done) |>
      dplyr::summarise(
        n = dplyr::n(),
        knowledge_w = w_mean(relevant_knowledge, .weight),
        usage_w = w_mean(relevant_usage, .weight),
        .groups = "drop"
      ) |>
      dplyr::mutate(link = link, .before = 1)
  })
  write_plan(q4_q5_links, "plan_lien_q4_connaissances_usages_q5")
}

# -----------------------------------------------------------------------------
# C. Pratiques — facteurs associés aux usages Q5 et caractéristiques de formation
# -----------------------------------------------------------------------------

fit_weighted_model <- function(data, outcome, predictors, model_name) {
  vars <- unique(c(outcome, predictors, ".weight"))
  vars <- vars[vars %in% names(data)]
  predictors <- predictors[predictors %in% names(data)]
  if (!outcome %in% vars || length(predictors) == 0) return(tibble::tibble())

  d <- data |> dplyr::select(dplyr::all_of(vars))
  d <- d |> dplyr::filter(!is.na(.data[[outcome]]), !is.na(.weight), .weight > 0)
  predictors <- predictors[purrr::map_lgl(predictors, ~ dplyr::n_distinct(d[[.x]], na.rm = TRUE) >= 2)]
  if (length(predictors) == 0 || nrow(d) < 50) return(tibble::tibble())

  f <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  des <- survey::svydesign(ids = ~1, weights = ~.weight, data = d)
  mod <- tryCatch(survey::svyglm(f, design = des), error = function(e) NULL)
  if (is.null(mod)) return(tibble::tibble())

  broom::tidy(mod, conf.int = TRUE) |>
    dplyr::mutate(model = model_name, n_model = stats::nobs(mod), .before = 1)
}

practice_predictors <- c(
  "exposure2", "year", "discipline_detail", "language_group",
  "score_q4_practices_research", "score_q12_incitation", "score_q12_frein",
  "score_q15_benefits", "score_q15_constraints", "score_q15_risks", "q7_group"
)
practice_model <- fit_weighted_model(df, "score_q5_used", practice_predictors, "Usage Q5 - modèle élargi") |>
  add_model_fdr()
write_plan(practice_model, "plan_modele_pratiques_q5_elargi")

training_predictors <- c(
  "training_intensity", "q8_has_presentiel", "q8_has_distanciel",
  "score_q11_training_evaluation", "year", "discipline_detail", "language_group"
)
training_df <- if ("exposure3" %in% names(df)) df |> dplyr::filter(exposure3 != "Aucun dispositif") else df
training_model <- fit_weighted_model(training_df, "score_q5_used", training_predictors, "Usage Q5 - caractéristiques de formation") |>
  add_model_fdr()
write_plan(training_model, "plan_modele_pratiques_q5_caracteristiques_formation")

if ("score_q11_training_evaluation" %in% names(df) && "score_q5_used" %in% names(df)) {
  q11_q5 <- df |>
    dplyr::filter(!is.na(score_q11_training_evaluation), !is.na(score_q5_used), !is.na(.weight), .weight > 0) |>
    dplyr::mutate(q11_quartile = dplyr::ntile(score_q11_training_evaluation, 4)) |>
    dplyr::group_by(q11_quartile) |>
    dplyr::summarise(n = dplyr::n(), usage_q5_w = w_mean(score_q5_used, .weight), .groups = "drop")
  write_plan(q11_q5, "plan_q11_evaluation_et_pratique_reelle")
}

# -----------------------------------------------------------------------------
# D. Intentions et attitudes — Q13, Q14, discordances et cumul des conditions favorables
# -----------------------------------------------------------------------------

corr_vars <- c(
  "score_q4_practices_research", "score_q5_known_well", "score_q5_used",
  "score_q13_open_intentions", "score_q12_incitation", "score_q12_frein",
  "score_q15_benefits", "score_q15_constraints", "score_q15_risks"
)
corr_vars <- corr_vars[corr_vars %in% names(df)]
if (length(corr_vars) >= 2) {
  corr_long <- tidyr::expand_grid(var1 = corr_vars, var2 = corr_vars) |>
    dplyr::rowwise() |>
    dplyr::mutate(correlation_w = w_cor(df[[var1]], df[[var2]], df$.weight)) |>
    dplyr::ungroup()
  write_plan(corr_long, "plan_correlations_pratiques_intentions_perceptions")
}

if (all(c("score_q5_used", "score_q13_open_intentions") %in% names(df))) {
  med_use <- stats::median(df$score_q5_used, na.rm = TRUE)
  med_int <- stats::median(df$score_q13_open_intentions, na.rm = TRUE)
  typology <- df |>
    dplyr::filter(!is.na(score_q5_used), !is.na(score_q13_open_intentions), !is.na(.weight), .weight > 0) |>
    dplyr::mutate(
      practice_intention_profile = dplyr::case_when(
        score_q5_used >= med_use & score_q13_open_intentions >= med_int ~ "Usage élevé / intentions élevées",
        score_q5_used < med_use & score_q13_open_intentions >= med_int ~ "Usage faible / intentions élevées",
        score_q5_used >= med_use & score_q13_open_intentions < med_int ~ "Usage élevé / intentions faibles",
        TRUE ~ "Usage faible / intentions faibles"
      )
    )

  typology_summary <- typology |>
    dplyr::group_by(practice_intention_profile) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n))
  write_plan(typology_summary, "plan_profils_pratiques_intentions")

  typology_characteristics <- typology |>
    dplyr::select(
      practice_intention_profile, .weight, dplyr::any_of(c(
        "year", "discipline_broad", "exposure3", "q7_group",
        "score_q15_benefits", "score_q15_constraints", "score_q15_risks"
      ))
    )
  write_plan(typology_characteristics, "plan_profils_pratiques_intentions_individus")
}

q14_available <- has_rows(q14_long) && all(c("reason_label", ".weight") %in% names(q14_long))
if (q14_available) {
  denom_q14 <- q14_long |>
    dplyr::filter(reason_code != 97, !is.na(.weight), .weight > 0) |>
    dplyr::distinct(respondent_id, intention_index, .weight) |>
    dplyr::group_by(intention_index) |>
    dplyr::summarise(total_w = sum(.weight), .groups = "drop")

  q14_summary <- q14_long |>
    dplyr::filter(reason_code != 97, !is.na(.weight), .weight > 0) |>
    dplyr::group_by(intention_index, intention_label, reason_code, reason_label) |>
    dplyr::summarise(n = dplyr::n_distinct(respondent_id), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::left_join(denom_q14, by = "intention_index") |>
    dplyr::mutate(pct_respondents_w = weighted_n / total_w) |>
    dplyr::arrange(intention_index, dplyr::desc(pct_respondents_w))
  write_plan(q14_summary, "plan_q14_raisons_non_adoption")

  q14_global_denom <- q14_long |>
    dplyr::filter(reason_code != 97, !is.na(.weight), .weight > 0) |>
    dplyr::distinct(respondent_id, .weight) |>
    dplyr::summarise(total_w = sum(.weight), n_respondents = dplyr::n())

  q14_global <- q14_long |>
    dplyr::filter(reason_code != 97, !is.na(.weight), .weight > 0) |>
    dplyr::distinct(respondent_id, reason_label, .weight) |>
    dplyr::group_by(reason_label) |>
    dplyr::summarise(
      n_respondents = dplyr::n_distinct(respondent_id),
      weighted_respondents = sum(.weight),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      total_w = q14_global_denom$total_w,
      pct_respondents_w = weighted_respondents / total_w
    ) |>
    dplyr::arrange(dplyr::desc(pct_respondents_w))

  write_plan(q14_global, "plan_q14_raisons_global_respondants")

  if ("exposure3" %in% names(q14_long)) {
    q14_den_exp <- q14_long |>
      dplyr::filter(reason_code != 97, !is.na(exposure3), !is.na(.weight), .weight > 0) |>
      dplyr::distinct(exposure3, respondent_id, .weight) |>
      dplyr::group_by(exposure3) |>
      dplyr::summarise(total_w = sum(.weight), .groups = "drop")

    q14_exposure <- q14_long |>
      dplyr::filter(reason_code != 97, !is.na(exposure3), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(exposure3, reason_label) |>
      dplyr::summarise(n = dplyr::n_distinct(respondent_id), weighted_n = sum(.weight), .groups = "drop") |>
      dplyr::left_join(q14_den_exp, by = "exposure3") |>
      dplyr::mutate(pct_respondents_w = weighted_n / total_w)
    write_plan(q14_exposure, "plan_q14_raisons_par_exposition")
  }
}

if (all(c("score_q5_known_well", "score_q5_used", "score_q13_open_intentions") %in% names(df))) {
  intent_levels <- df |>
    dplyr::filter(!is.na(.weight), .weight > 0) |>
    dplyr::mutate(
      knowledge_quartile = dplyr::ntile(score_q5_known_well, 4),
      usage_quartile = dplyr::ntile(score_q5_used, 4),
      practice_quartile = if ("score_q4_practices_research" %in% names(df)) dplyr::ntile(score_q4_practices_research, 4) else NA_integer_
    )
  write_plan(group_mean(intent_levels, c("knowledge_quartile"), "score_q13_open_intentions"), "plan_intentions_selon_connaissance")
  write_plan(group_mean(intent_levels, c("usage_quartile"), "score_q13_open_intentions"), "plan_intentions_selon_usage")
  if ("score_q4_practices_research" %in% names(df)) {
    write_plan(group_mean(intent_levels, c("practice_quartile"), "score_q13_open_intentions"), "plan_intentions_selon_pratiques_q4")
  }
}

if ("score_q13_open_intentions" %in% names(df)) {
  for (g in c("year", "discipline_detail", "q7_group")) {
    if (g %in% names(df)) {
      write_plan(group_mean(df, c(g), "score_q13_open_intentions"), paste0("plan_intentions_par_", g))
    }
  }
  if ("score_q11_training_evaluation" %in% names(df)) {
    tmp <- df |> dplyr::filter(!is.na(score_q11_training_evaluation)) |> dplyr::mutate(q11_quartile = dplyr::ntile(score_q11_training_evaluation, 4))
    write_plan(group_mean(tmp, c("q11_quartile"), "score_q13_open_intentions"), "plan_intentions_selon_evaluation_q11")
  }
}

# Indice cumulatif descriptif demandé dans le plan.
if (all(c("score_q5_known_well", "score_q5_used", "score_q13_open_intentions") %in% names(df))) {
  k_med <- stats::median(df$score_q5_known_well, na.rm = TRUE)
  u_med <- stats::median(df$score_q5_used, na.rm = TRUE)
  cumulative <- df |>
    dplyr::mutate(
      high_training = if ("q10" %in% names(df)) suppressWarnings(as.numeric(q10)) %in% c(2, 3) else FALSE,
      director_support = if ("director_environment" %in% names(df)) director_environment == "Incitation" else FALSE,
      high_knowledge = score_q5_known_well >= k_med,
      high_usage = score_q5_used >= u_med,
      cumulative_support = rowSums(cbind(high_training, director_support, high_knowledge, high_usage), na.rm = TRUE)
    )
  cumulative_summary <- group_mean(cumulative, c("cumulative_support"), "score_q13_open_intentions")
  write_plan(cumulative_summary, "plan_cumul_formation_environnement_connaissance_usage_intentions")
}

# -----------------------------------------------------------------------------
# E. Perceptions — Q7, Q12 et dimensions positives, contraintes et risques de Q15
# -----------------------------------------------------------------------------

if ("q7_group" %in% names(df)) {
  q7_global <- df |>
    dplyr::filter(!is.na(q7_group), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(q7_group) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_w = weighted_n / sum(weighted_n))
  write_plan(q7_global, "plan_q7_connaissance_politique_etablissement")

  for (g in c("exposure3", "year", "discipline_detail")) {
    if (g %in% names(df)) {
      q7_cross <- df |>
        dplyr::filter(!is.na(q7_group), !is.na(.data[[g]]), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(.data[[g]], q7_group) |>
        dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(pct_w = weighted_n / sum(weighted_n)) |>
        dplyr::ungroup()
      write_plan(q7_cross, paste0("plan_q7_par_", g))
    }
  }
}

if (has_rows(q15_long) && all(c("item_label", "agree", "disagree", ".weight") %in% names(q15_long))) {
  q15_overall <- q15_long |>
    dplyr::filter(!is.na(item_label), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(item_label) |>
    dplyr::summarise(
      n = dplyr::n(),
      pct_agree_w = w_prop(agree, .weight),
      pct_disagree_w = w_prop(disagree, .weight),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(pct_agree_w))
  write_plan(q15_overall, "plan_q15_accord_desaccord_global")
}

perception_scores <- c("score_q15_benefits", "score_q15_constraints", "score_q15_risks")
perception_scores <- perception_scores[perception_scores %in% names(df)]
if (length(perception_scores) > 0) {
  groupings <- c("year", "discipline_detail", "exposure3", "q7_group")
  groupings <- groupings[groupings %in% names(df)]
  for (g in groupings) {
    tmp <- df |>
      dplyr::select(dplyr::all_of(c(g, ".weight", perception_scores))) |>
      tidyr::pivot_longer(cols = dplyr::all_of(perception_scores), names_to = "dimension", values_to = "value") |>
      dplyr::filter(!is.na(.data[[g]]), !is.na(value), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(.data[[g]], dimension) |>
      dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
    write_plan(tmp, paste0("plan_perceptions_q15_par_", g))
  }

  if (all(c("score_q4_practices_research", "score_q5_used", "score_q12_incitation") %in% names(df))) {
    d2 <- df |>
      dplyr::mutate(
        q4_band = dplyr::ntile(score_q4_practices_research, 4),
        q5_usage_band = dplyr::ntile(score_q5_used, 4),
        q12_incitation_band = dplyr::ntile(score_q12_incitation, 4)
      )
    for (g in c("q4_band", "q5_usage_band", "q12_incitation_band")) {
      tmp <- d2 |>
        dplyr::select(dplyr::all_of(c(g, ".weight", perception_scores))) |>
        tidyr::pivot_longer(cols = dplyr::all_of(perception_scores), names_to = "dimension", values_to = "value") |>
        dplyr::filter(!is.na(.data[[g]]), !is.na(value), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(.data[[g]], dimension) |>
        dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
      write_plan(tmp, paste0("plan_perceptions_q15_selon_", g))
    }
  }
}

# Plateformes d'accès non officielles (dont Sci-Hub est l'exemple habituel dans
# le plan) : on utilise uniquement l'item Q5 correspondant, sans inférer quel
# service précis a été utilisé.
if (has_rows(q5_long) && length(perception_scores) > 0) {
  unofficial <- q5_long |>
    dplyr::filter(stringr::str_detect(norm_text(item_label), "plateforme.*acces non officielle|acces non officiel")) |>
    dplyr::group_by(respondent_id) |>
    dplyr::summarise(unofficial_platform_used = any(used %in% TRUE), .groups = "drop")

  if (nrow(unofficial) > 0) {
    d_unofficial <- df |>
      dplyr::left_join(unofficial, by = "respondent_id") |>
      dplyr::mutate(unofficial_platform_used = tidyr::replace_na(unofficial_platform_used, FALSE))

    tmp <- d_unofficial |>
      dplyr::select(unofficial_platform_used, .weight, dplyr::all_of(perception_scores)) |>
      tidyr::pivot_longer(cols = dplyr::all_of(perception_scores), names_to = "dimension", values_to = "value") |>
      dplyr::filter(!is.na(value), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(unofficial_platform_used, dimension) |>
      dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
    write_plan(tmp, "plan_perceptions_selon_usage_plateformes_non_officielles")
  }
}

perception_predictors <- c(
  "exposure2", "year", "discipline_detail", "score_q4_practices_research",
  "score_q5_used", "score_q12_incitation", "score_q12_frein", "q7_group", "language_group"
)
perception_models <- purrr::map_dfr(perception_scores, function(outcome) {
  fit_weighted_model(df, outcome, perception_predictors, paste0("Perception - ", outcome))
}) |>
  add_model_fdr()
write_plan(perception_models, "plan_modeles_perceptions_q15")

# -----------------------------------------------------------------------------
# F. Mots spontanés Q3 — analyse lexicale exploratoire et dictionnaire explicite
# -----------------------------------------------------------------------------

q3_available <- has_rows(q3_long) && all(c("respondent_id", "word_normalized", ".weight") %in% names(q3_long))
if (q3_available) {
  q3_cat <- q3_long |>
    dplyr::mutate(
      word_category = dplyr::case_when(
        stringr::str_detect(word_normalized, "hal|orcid|zenodo|github|gitlab|sci hub|software heritage|arxiv|openedition") ~ "Outil ou infrastructure",
        stringr::str_detect(word_normalized, "donnee|data|fair|acces ouvert|open access|publication|archive|code|logiciel|licence|partage|reproduct|transparen|collabor|science citoyenne") ~ "Notion ou pratique",
        stringr::str_detect(word_normalized, "utile|important|necessaire|positif|bien|progres|opportun") ~ "Jugement positif",
        stringr::str_detect(word_normalized, "contrainte|complex|inutile|risque|cout|cher|plagiat|obligation|impose|peur") ~ "Jugement critique ou risque",
        TRUE ~ "Non classé"
      )
    )

  q3_freq <- q3_cat |>
    dplyr::filter(!is.na(word_normalized), word_normalized != "") |>
    dplyr::group_by(word_normalized) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(weighted_n))
  write_plan(q3_freq, "plan_q3_mots_frequences")

  q3_categories <- q3_cat |>
    dplyr::filter(!is.na(.weight), .weight > 0) |>
    dplyr::group_by(word_category) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight), .groups = "drop") |>
    dplyr::mutate(pct_mentions_w = weighted_n / sum(weighted_n))
  write_plan(q3_categories, "plan_q3_categories_dictionnaire_exploratoire")

  if ("exposure3" %in% names(q3_cat)) {
    q3_exposure <- q3_cat |>
      dplyr::filter(!is.na(exposure3), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(exposure3, word_category) |>
      dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
      dplyr::mutate(pct_mentions_w = weighted_n / sum(weighted_n)) |>
      dplyr::ungroup()
    write_plan(q3_exposure, "plan_q3_categories_par_exposition")
  }

  if ("training_intensity" %in% names(q3_cat)) {
    q3_volume <- q3_cat |>
      dplyr::filter(!is.na(training_intensity), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(training_intensity, word_category) |>
      dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
      dplyr::mutate(pct_mentions_w = weighted_n / sum(weighted_n)) |>
      dplyr::ungroup()
    write_plan(q3_volume, "plan_q3_categories_par_volume_formation")
  }

  if ("discipline_detail" %in% names(q3_cat)) {
    q3_disc <- q3_cat |>
      dplyr::filter(!is.na(discipline_detail), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(discipline_detail, word_category) |>
      dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
      dplyr::mutate(pct_mentions_w = weighted_n / sum(weighted_n)) |>
      dplyr::ungroup()
    write_plan(q3_disc, "plan_q3_categories_par_discipline")
  }

  if (all(c("q8_has_presentiel", "q8_has_distanciel") %in% names(q3_cat))) {
    q3_mode <- q3_cat |>
      dplyr::mutate(
        training_mode = dplyr::case_when(
          q8_has_presentiel & q8_has_distanciel ~ "Présentiel et distanciel",
          q8_has_distanciel & !q8_has_presentiel ~ "Distanciel seulement",
          q8_has_presentiel & !q8_has_distanciel ~ "Présentiel seulement",
          TRUE ~ "Autre / aucun"
        )
      ) |>
      dplyr::filter(!is.na(.weight), .weight > 0) |>
      dplyr::group_by(training_mode, word_category) |>
      dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
      dplyr::mutate(pct_mentions_w = weighted_n / sum(weighted_n)) |>
      dplyr::ungroup()
    write_plan(q3_mode, "plan_q3_categories_par_presentiel_distanciel")
  }

  if (q9_available) {
    q3_by_respondent <- q3_cat |>
      dplyr::select(respondent_id, word_category, .weight) |>
      dplyr::filter(!is.na(word_category), !is.na(.weight), .weight > 0) |>
      dplyr::distinct(respondent_id, word_category, .keep_all = TRUE)

    q9_by_respondent <- q9_long |>
      dplyr::filter(!is.na(organizer_label)) |>
      dplyr::distinct(respondent_id, organizer_label) |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise(organizers = list(organizer_label), .groups = "drop")

    q3_q9_pairs <- q3_by_respondent |>
      dplyr::inner_join(q9_by_respondent, by = "respondent_id") |>
      tidyr::unnest_longer(organizers, values_to = "organizer_label") |>
      dplyr::distinct(respondent_id, organizer_label, word_category, .keep_all = TRUE)

    q3_q9_denominators <- q3_q9_pairs |>
      dplyr::distinct(respondent_id, organizer_label, .weight) |>
      dplyr::group_by(organizer_label) |>
      dplyr::summarise(total_respondent_weight = sum(.weight), .groups = "drop")

    q3_q9 <- q3_q9_pairs |>
      dplyr::group_by(organizer_label, word_category) |>
      dplyr::summarise(
        n_respondents = dplyr::n_distinct(respondent_id),
        weighted_respondents = sum(.weight),
        .groups = "drop"
      ) |>
      dplyr::left_join(q3_q9_denominators, by = "organizer_label") |>
      dplyr::mutate(
        pct_respondents_w = weighted_respondents / total_respondent_weight
      ) |>
      dplyr::arrange(organizer_label, dplyr::desc(pct_respondents_w))

    write_plan(q3_q9, "plan_q3_categories_par_organisateur_q9")
  }

  score_links <- c("score_q5_known_well", "score_q5_used", "score_q13_open_intentions", "score_q15_benefits", "score_q15_constraints", "score_q15_risks")
  score_links <- score_links[score_links %in% names(df)]
  if (length(score_links) > 0) {
    q3_scores <- q3_cat |>
      dplyr::distinct(respondent_id, word_category) |>
      dplyr::left_join(df |> dplyr::select(respondent_id, .weight, dplyr::all_of(score_links)), by = "respondent_id") |>
      tidyr::pivot_longer(cols = dplyr::all_of(score_links), names_to = "indicator", values_to = "value") |>
      dplyr::filter(!is.na(value), !is.na(.weight), .weight > 0) |>
      dplyr::group_by(word_category, indicator) |>
      dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
    write_plan(q3_scores, "plan_q3_categories_et_scores")
  }

  q3_coherence <- q3_cat |>
    dplyr::group_by(respondent_id) |>
    dplyr::summarise(
      n_words = dplyr::n(),
      n_categories = dplyr::n_distinct(word_category[word_category != "Non classé"]),
      same_category = n_words >= 2 & n_categories == 1,
      .groups = "drop"
    )
  write_plan(q3_coherence, "plan_q3_coherence_trois_mots")
}

# -----------------------------------------------------------------------------
# G. Profils — classification hiérarchique exploratoire et caractérisation
# -----------------------------------------------------------------------------

cah_scores <- c(
  "score_q5_known_well", "score_q5_used", "score_q4_practices_research",
  "score_q13_open_intentions", "score_q12_incitation", "score_q12_frein",
  "score_q15_benefits", "score_q15_constraints", "score_q15_risks"
)
cah_scores <- cah_scores[cah_scores %in% names(df)]

if (length(cah_scores) >= 5) {
  cah_df <- df |>
    dplyr::select(respondent_id, .weight, dplyr::all_of(cah_scores), dplyr::any_of(c("year", "discipline_detail", "exposure3"))) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cah_scores), ~ !is.na(.x)))

  if (nrow(cah_df) >= 80) {
    x <- scale(cah_df |> dplyr::select(dplyr::all_of(cah_scores)))
    dmat <- stats::dist(x)
    hc <- stats::hclust(dmat, method = "ward.D2")
    k_grid <- 2:min(6, nrow(cah_df) - 1)
    sil <- purrr::map_dfr(k_grid, function(k) {
      cl <- stats::cutree(hc, k = k)
      tibble::tibble(k = k, silhouette = mean(cluster::silhouette(cl, dmat)[, "sil_width"]))
    })
    best_k <- sil$k[which.max(sil$silhouette)]
    cah_df$cah_profile <- paste0("CAH ", stats::cutree(hc, k = best_k))

    write_plan(sil, "plan_cah_choix_nombre_classes")
    write_plan(cah_df, "plan_cah_assignation_profils")

    cah_means <- cah_df |>
      dplyr::select(cah_profile, .weight, dplyr::all_of(cah_scores)) |>
      tidyr::pivot_longer(cols = dplyr::all_of(cah_scores), names_to = "indicator", values_to = "value") |>
      dplyr::group_by(cah_profile, indicator) |>
      dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
    write_plan(cah_means, "plan_cah_caracterisation_scores")

    cah_group_vars <- c("year", "discipline_detail", "exposure3")
    cah_group_vars <- cah_group_vars[cah_group_vars %in% names(cah_df)]
    cah_characteristics <- purrr::map_dfr(cah_group_vars, function(v) {
      cah_df |>
        dplyr::filter(!is.na(.data[[v]]), !is.na(.weight), .weight > 0) |>
        dplyr::group_by(cah_profile, category = .data[[v]]) |>
        dplyr::summarise(weighted_n = sum(.weight), n = dplyr::n(), .groups = "drop_last") |>
        dplyr::mutate(pct_w = weighted_n / sum(weighted_n), variable = v, .before = 1) |>
        dplyr::ungroup()
    })
    write_plan(cah_characteristics, "plan_cah_caracteristiques_annee_discipline_formation")
  }
}

# -----------------------------------------------------------------------------
# H. Figures complémentaires dérivées des analyses du plan
# -----------------------------------------------------------------------------

new_figs <- list()

if (exists("q9_overall") && has_rows(q9_overall)) {
  p <- q9_overall |>
    dplyr::mutate(organizer_label = forcats::fct_reorder(stringr::str_wrap(organizer_label, 40), pct_respondents_w)) |>
    ggplot2::ggplot(ggplot2::aes(x = pct_respondents_w, y = organizer_label)) +
    ggplot2::geom_col(fill = cols[["green"]], width = 0.64) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct_respondents_w, accuracy = 1, decimal.mark = ",")), hjust = -0.15, size = 4) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(x = "Part pondérée des répondants concernés", y = NULL) +
    osyr_theme(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank())
  f <- "plan_01_q9_organisateurs.png"
  safe_save(p, f, 12.5, 6.5)
  new_figs[[length(new_figs) + 1]] <- tibble::tibble(section = 1L, bloc = "Parcours de formation", titre = "Organisateurs des formations et actions", caption = "Q9 est une question multiréponse ; les pourcentages sont calculés parmi les répondants ayant renseigné au moins un organisateur.", file = f, path = file.path(fig_dir, f), source_dir = "rapport_final", priorite = 1L, available = TRUE)
}

if (exists("q14_global") && has_rows(q14_global)) {
  d <- q14_global |>
    dplyr::transmute(
      reason_label = forcats::fct_reorder(stringr::str_wrap(reason_label, 42), pct_respondents_w),
      pct = pct_respondents_w
    )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = pct, y = reason_label)) +
    ggplot2::geom_col(fill = cols[["brown"]], width = 0.64) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct, accuracy = 1, decimal.mark = ",")), hjust = -0.15, size = 4) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(x = "Part pondérée", y = NULL) +
    osyr_theme(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank())
  f <- "plan_10_q14_raisons_non_adoption.png"
  safe_save(p, f, 12.8, 7.2)
  new_figs[[length(new_figs) + 1]] <- tibble::tibble(section = 4L, bloc = "Intentions et attitudes", titre = "Raisons déclarées de non-adoption des pratiques", caption = "Q14 est une question multiréponse ; chaque motif est rapporté aux répondants ayant déclaré au moins une raison de non-adoption.", file = f, path = file.path(fig_dir, f), source_dir = "rapport_final", priorite = 1L, available = TRUE)
}

if (exists("q15_overall") && has_rows(q15_overall)) {
  d <- q15_overall |>
    dplyr::select(item_label, pct_agree_w, pct_disagree_w) |>
    tidyr::pivot_longer(cols = c(pct_agree_w, pct_disagree_w), names_to = "response", values_to = "pct") |>
    dplyr::mutate(
      response = dplyr::recode(response, pct_agree_w = "Accord", pct_disagree_w = "Désaccord"),
      item_label = stringr::str_wrap(item_label, 48)
    )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = pct, y = forcats::fct_reorder(item_label, pct, .fun = max), color = response)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_color_manual(values = c("Accord" = cols[["green"]], "Désaccord" = cols[["brown"]])) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "Part pondérée", y = NULL, color = NULL) +
    osyr_theme(base_size = 13.5) +
    ggplot2::theme(plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank())
  f <- "plan_12_q15_accord_desaccord.png"
  safe_save(p, f, 13.2, 8.2)
  new_figs[[length(new_figs) + 1]] <- tibble::tibble(section = 5L, bloc = "Perceptions", titre = "Accord et désaccord avec les affirmations sur la science ouverte", caption = "Part d'accord et de désaccord avec chaque affirmation Q15.", file = f, path = file.path(fig_dir, f), source_dir = "rapport_final", priorite = 1L, available = TRUE)
}

if (exists("cumulative_summary") && has_rows(cumulative_summary)) {
  p <- ggplot2::ggplot(cumulative_summary, ggplot2::aes(x = factor(cumulative_support), y = mean_w, group = 1)) +
    ggplot2::geom_line(color = cols[["dark_green"]], linewidth = 1.2) +
    ggplot2::geom_point(color = cols[["green"]], size = 4.5) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(mean_w, accuracy = 1, decimal.mark = ",")), vjust = -1, size = 4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(x = "Nombre de conditions réunies (0 à 4)", y = "Score moyen d'intentions Q13") +
    osyr_theme(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank(), legend.position = "none")
  f <- "plan_13_cumul_et_intentions.png"
  safe_save(p, f, 10.8, 6.7)
  new_figs[[length(new_figs) + 1]] <- tibble::tibble(section = 4L, bloc = "Intentions et attitudes", titre = "Cumul des conditions favorables et intentions", caption = "Indice descriptif combinant volume de formation, environnement de thèse favorable, niveau de connaissance et niveau d'usage.", file = f, path = file.path(fig_dir, f), source_dir = "rapport_final", priorite = 3L, available = TRUE)
}

# -----------------------------------------------------------------------------
# I. Audit point par point de la couverture du plan de dépouillement
# -----------------------------------------------------------------------------

status_if <- function(condition, yes = "Analysé", no = "Non analysé") if (isTRUE(condition)) yes else no

coverage_detail <- tibble::tribble(
  ~bloc, ~plan_ref, ~point, ~statut, ~sortie, ~note,
  "Parcours de formation", "1", "Part des formés / non formés", "Analysé", "plan_formation_exposition_globale.csv ; formation_exposition_par_annee.csv ; plan_formation_exposition_par_discipline.csv", "Global, année de thèse et discipline.",
  "Parcours de formation", "2", "Types de dispositifs Q8", "Analysé", "formation_dispositifs_q8_par_annee.csv ; formation_dispositifs_q8_par_discipline.csv", "Détail Q8, année, discipline, langue et formats présentiel/distanciel.",
  "Parcours de formation", "3", "Organisateurs Q9", status_if(q9_available), "plan_q9_organisateurs_global.csv", ifelse(q9_available, "Global, année, discipline et lien avec Q11.", "Les colonnes Q9 n'ont pas été détectées dans la base relancée."),
  "Parcours de formation", "4", "Nombre de formations Q10", status_if("training_intensity" %in% names(df)), "plan_q10_distribution_globale.csv ; plan_q10_par_annee.csv ; plan_q10_par_discipline.csv", "Global, année et discipline.",
  "Parcours de formation", "5", "Q11 croisé avec Q8, Q10, Q12 et Q9", ifelse("score_q11_training_evaluation" %in% names(df), "Analysé", "Partiel"), "plan_q11_selon_modalite_q8.csv ; plan_q11_selon_volume_q10.csv ; plan_q11_selon_incitation_q12.csv ; plan_q11_selon_frein_q12.csv ; plan_q11_selon_organisateur_q9.csv", "Le croisement Q9 est conditionnel à la disponibilité de Q9.",
  "Parcours de formation", "Focus", "Non formés, distanciel et autoformés", "Analysé", "plan_focus_non_formes_autoformes_organises.csv ; plan_focus_presentiel_distanciel_scores.csv ; plan_focus_non_formes_autoformes_composition.csv", "Comparaison sur connaissances, usages, pratiques, intentions, environnement, perceptions et composition.",
  "Parcours de formation", "6", "Comparer la part formée aux données des collèges doctoraux", "Donnée externe requise", "", "Impossible à produire à partir de l'enquête seule.",
  "Parcours de formation", "7", "Formation, attitudes et mots spontanés", ifelse(q3_available, "Analysé - dictionnaire exploratoire", "Partiel"), "plan_q3_categories_par_exposition.csv ; plan_correlations_pratiques_intentions_perceptions.csv", "La partie lexicale exige une validation humaine du dictionnaire.",
  "Connaissances", "8", "Niveau de connaissance et usage", "Analysé", "connaissances_q5_items_gap.csv", "Item par item et scores synthétiques.",
  "Connaissances", "9", "Types de connaissances et facteurs associés", "Analysé", "plan_q5_familles_par_exposition.csv ; plan_q5_familles_par_volume_formation.csv ; plan_q5_familles_par_presentiel_distanciel.csv ; plan_q5_familles_par_annee.csv ; plan_q5_familles_par_discipline.csv ; plan_q5_familles_selon_direction_these.csv", "Familles Q5 ; le classement heuristique reste à valider item par item.",
  "Connaissances", "10", "Représentations spontanées / trois mots", ifelse(q3_available, "Analysé - exploratoire", "Non analysé"), "plan_q3_mots_frequences.csv ; plan_q3_categories_dictionnaire_exploratoire.csv ; plan_q3_coherence_trois_mots.csv", "Le plan mentionne Q1 mais les commentaires du document renvoient à Q3 ; le workflow utilise Q3.",
  "Connaissances", "Lien Q4-Q5", "Lier connaissances des outils aux pratiques de recherche déjà réalisées", status_if(has_rows(q4_q5_links)), "plan_lien_q4_connaissances_usages_q5.csv", "Liens ciblés articles, données, code et réutilisation de données.",
  "Pratiques", "1", "État des pratiques Q4", "Analysé", "pratiques_q4_items.csv", "Descriptif pondéré item par item.",
  "Pratiques", "2", "Écart connaissance-pratique Q5", "Analysé", "connaissances_q5_items_gap.csv", "Gap connaissance-usage item par item et par discipline.",
  "Pratiques", "3", "Facteurs associés aux pratiques", "Analysé", "plan_modele_pratiques_q5_elargi.csv ; plan_modele_pratiques_q5_caracteristiques_formation.csv", "Ajout environnement, Q4, perceptions, Q7, volume et formats de formation.",
  "Pratiques", "Q6", "Outils déjà utilisés et pourquoi faire", "À vérifier dans le questionnaire", "", "Le plan mentionne Q6 mais les sorties et la DATAMAP documentée ne permettent pas d'identifier ici une batterie Q6 équivalente sans ambiguïté.",
  "Pratiques", "Q11", "Évaluation de la formation et pratique réelle", "Analysé", "plan_q11_evaluation_et_pratique_reelle.csv", "Usage Q5 selon quartiles du score Q11.",
  "Intentions et attitudes", "Profils", "Profils pratiques-intentions et caractéristiques", "Analysé", "plan_profils_pratiques_intentions.csv ; plan_cah_assignation_profils.csv", "Typologie descriptive plus CAH exploratoire ; K-means existant conservé.",
  "Intentions et attitudes", "Q13-Q15", "Cohérence ou discordance attitudes-intentions", "Analysé", "plan_correlations_pratiques_intentions_perceptions.csv", "Corrélations pondérées entre scores.",
  "Intentions et attitudes", "Q14", "Raisons de non-adoption", status_if(q14_available), "plan_q14_raisons_non_adoption.csv ; plan_q14_raisons_par_exposition.csv", ifelse(q14_available, "Raisons par intention et par exposition.", "Les colonnes Q14 n'ont pas été détectées dans la base relancée."),
  "Intentions et attitudes", "Q13", "Non formés et je ne sais pas", "Analysé", "intentions_q13_par_exposition.csv ; plan_focus_non_formes_autoformes_organises.csv", "Oui/non/je ne sais pas distingués.",
  "Intentions et attitudes", "Q12-Q13-Q15", "Selon année, discipline, Q11, Q5 et Q7", "Analysé", "plan_intentions_par_year.csv ; plan_intentions_par_discipline_detail.csv ; plan_intentions_selon_evaluation_q11.csv ; plan_intentions_selon_connaissance.csv ; plan_intentions_selon_usage.csv ; plan_intentions_par_q7_group.csv", "Les mots Q3 restent conditionnels à Q3.",
  "Intentions et attitudes", "Cumul", "Formation + direction + connaissances + usages vers intentions", "Analysé", "plan_cumul_formation_environnement_connaissance_usage_intentions.csv", "Indice descriptif de cumul, sans lecture causale.",
  "Perceptions", "Q15", "Perceptions selon année, discipline, Q4, Q5, Q12 et formation", "Analysé", "plan_perceptions_q15_par_year.csv ; plan_perceptions_q15_par_discipline_detail.csv ; plan_perceptions_q15_selon_q4_band.csv ; plan_perceptions_q15_selon_q5_usage_band.csv ; plan_perceptions_q15_selon_q12_incitation_band.csv ; plan_perceptions_q15_par_exposure3.csv", "Les dimensions bénéfices, contraintes et risques sont séparées.",
  "Perceptions", "Positif / négatif", "Regrouper perceptions positives et négatives", "Analysé", "scores Q15 benefits / constraints / risks ; plan_q15_accord_desaccord_global.csv", "Q15 n'est plus résumé par un unique score d'accord pour l'interprétation substantielle.",
  "Perceptions", "Déterminants", "Facteurs associés aux perceptions", "Analysé", "plan_modeles_perceptions_q15.csv", "Modèles pondérés associatifs.",
  "Perceptions", "Accords / désaccords", "Aspects recueillant le plus d'accord ou de désaccord", "Analysé", "plan_q15_accord_desaccord_global.csv", "Classement descriptif pondéré.",
  "Perceptions", "Sci-Hub", "Perception et usage des plateformes d'accès non officielles", "Analysé avec prudence", "plan_perceptions_selon_usage_plateformes_non_officielles.csv", "L'item Q5 ne permet pas d'affirmer quel service précis a été utilisé ; le tableau reste au niveau plateformes non officielles.",
  "Précautions méthodologiques", "Biais", "Désirabilité, intentions non observables, causalité, discipline et formations obligatoires", "Documenté", "rapport et document méthodologique", "À conserver dans l'interprétation ; certaines informations externes ne sont pas testables dans la base.",
  "Autres approches", "CAH", "CAH connaissances + usages + perceptions", ifelse(exists("cah_df") && has_rows(cah_df), "Analysé - exploratoire", "Non analysé"), "plan_cah_choix_nombre_classes.csv ; plan_cah_assignation_profils.csv ; plan_cah_caracterisation_scores.csv ; plan_cah_caracteristiques_annee_discipline_formation.csv", "Nombre de classes choisi par silhouette parmi 2 à 6.",
  "Autres approches", "Q7", "Connaissance de la politique d'établissement", status_if("q7_group" %in% names(df)), "plan_q7_connaissance_politique_etablissement.csv", "Global et croisements avec exposition, année et discipline.",
  "Autres approches", "Q9", "Qui forme et lien avec pratiques, intentions et perceptions", ifelse(q9_available, "Partiel", "Non analysé"), "plan_q9_organisateurs_global.csv ; plan_q11_selon_organisateur_q9.csv", "Le lien Q9 avec Q5/Q13/Q15 est ajouté si Q9 est disponible."
)

# Q9 : effets descriptifs sur scores si la variable existe.
if (q9_available && length(focus_scores) > 0) {
  q9_scores <- q9_long |>
    dplyr::distinct(respondent_id, organizer_label) |>
    dplyr::left_join(df |> dplyr::select(respondent_id, .weight, dplyr::all_of(focus_scores)), by = "respondent_id") |>
    tidyr::pivot_longer(cols = dplyr::all_of(focus_scores), names_to = "indicator", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(.weight), .weight > 0) |>
    dplyr::group_by(organizer_label, indicator) |>
    dplyr::summarise(n = dplyr::n(), mean_w = w_mean(value, .weight), .groups = "drop")
  write_plan(q9_scores, "plan_q9_organisateurs_et_scores")
  coverage_detail <- coverage_detail |>
    dplyr::mutate(
      statut = dplyr::if_else(bloc == "Autres approches" & plan_ref == "Q9", "Analysé", statut),
      sortie = dplyr::if_else(bloc == "Autres approches" & plan_ref == "Q9", "plan_q9_organisateurs_et_scores.csv", sortie)
    )
}

write_plan(coverage_detail, "couverture_plan_depouillement_detaillee")

# Ajouter les figures nouvelles au catalogue sans supprimer les sorties existantes.
catalog_path <- file.path(tab_dir, "catalogue_figures_finales.csv")
if (length(new_figs) > 0 && file.exists(catalog_path)) {
  cat <- readr::read_csv(catalog_path, show_col_types = FALSE)
  add <- dplyr::bind_rows(new_figs)
  cat <- cat |>
    dplyr::filter(!file %in% add$file) |>
    dplyr::bind_rows(add) |>
    dplyr::arrange(section, priorite, titre)
  readr::write_csv(cat, catalog_path)
}

message("Analyse du plan de dépouillement terminée : ", normalizePath(tab_dir, mustWork = FALSE))
message("Matrice détaillée : ", normalizePath(file.path(tab_dir, "couverture_plan_depouillement_detaillee.csv"), mustWork = FALSE))