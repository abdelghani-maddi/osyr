# =============================================================================
# SCRIPT 01 — ANALYSE PRINCIPALE OSYR
# =============================================================================

# Version v3 commentée — 30/06/2026
#
# Ce script est volontairement autonome : il part des deux fichiers fournis
# par OpinionWay et construit toutes les sorties de base utilisées ensuite
# par le rapport Word, les analyses complémentaires et la présentation PPT.
#
# Ordre logique :
#   1. fonctions utilitaires ;
#   2. lecture base + datamap ;
#   3. recodages robustes ;
#   4. contrôles qualité ;
#   5. descriptifs pondérés ;
#   6. tables longues des batteries ;
#   7. scores synthétiques ;
#   8. modèles ajustés ;
#   9. analyse textuelle Q3 ;
#  10. exports.
#
# Les analyses sont descriptives / associatives. Les écarts exposés/non exposés
# ne doivent pas être interprétés comme des effets causaux des formations.
# =============================================================================
# Rôle dans le workflow
#   Ce script est le socle de l'analyse. Il lit la base corrigée et la datamap,
#   nettoie les variables, construit les variables analytiques, produit les
#   descriptifs, les figures, les scores, les modèles ajustés et l'analyse
#   textuelle des trois mots associés à la science ouverte.
#
# À lancer seul si besoin :
#   source("01_analyse_osyr_base_et_modeles.R")
#
# À lancer avec tout le workflow :
#   source("00_lancer_workflow_complet.R")
#
# Données attendues :
#   data/BJ30232 - BDD V2.csv
#   data/BJ30232 - DATAMAP V2.xlsx
#
# Sortie principale :
#   outputs_osyr_v2_final/
#
# Note d'interprétation :
#   Les résultats sont descriptifs et associatifs. Ils ne doivent pas être
#   présentés comme des effets causaux des formations ou dispositifs.
# =============================================================================

# =============================================================================
# OSYR — Workflow final V2 corrigée
# Auteur : Abdelghani Maddi / projet OSYR
# Version : 2026-06-19 — finale, esthétique, commentée et interprétable
#
# Objectif
#   Analyse complète de la base corrigée : BJ30232 - BDD V2.csv
#   + BJ30232 - DATAMAP V2.xlsx
#
# Ce script produit :
#   1) une base analytique propre, documentée et robuste aux accents/encodages ;
#   2) des contrôles qualité et des vérifications de cohérence ;
#   3) des descriptifs pondérés ;
#   4) des comparaisons fines entre doctorants exposés et non exposés ;
#   5) des nuances par année de thèse, discipline, établissement et langue du questionnaire ;
#   6) des scores synthétiques et des modèles ajustés ;
#   7) des interactions exposition × année, exposition × discipline, exposition × langue ;
#   8) une analyse textuelle profonde des trois mots associés à la science ouverte ;
#   9) des graphiques haute résolution, plus lisibles et plus éditoriaux.
#
# Remarques importantes
#   - Les analyses sont pondérées par la variable Poids.
#   - Les comparaisons sont descriptives et associatives, pas causales.
#   - La variable RESPONDENT_LANGUAGE indique la langue du questionnaire.
#     Elle peut être utilisée comme proxy prudent d'un profil international,
#     mais ne doit pas être interprétée comme une nationalité.
#   - L'année de thèse est recodée à partir du code numérique Q1, et non
#     à partir des libellés de la datamap, afin d'éviter les problèmes
#     d'encodage qui faisaient disparaître certaines années dans les graphiques.
# =============================================================================

# =============================================================================
# 0. Préparation
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE,
  survey.lonely.psu = "adjust"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    message("Packages manquants : ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

pkgs <- c(
  "tidyverse", "readxl", "janitor", "survey", "srvyr", "broom",
  "scales", "forcats", "stringi", "tidytext", "stopwords",
  "igraph", "ggraph", "ggrepel", "patchwork", "openxlsx", "glue",
  "rlang"
)

install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# Chemins : le script peut être placé à la racine du projet RStudio.
data_dir <- "data"
out_dir  <- "outputs_osyr_v2_final"

bdd_file <- file.path(data_dir, "BJ30232 - BDD V2.csv")
map_file <- file.path(data_dir, "BJ30232 - DATAMAP V2.xlsx")

# Fallback si les fichiers sont dans le dossier courant.
if (!file.exists(bdd_file)) bdd_file <- "BJ30232 - BDD V2.csv"
if (!file.exists(map_file)) map_file <- "BJ30232 - DATAMAP V2.xlsx"

stopifnot(file.exists(bdd_file))
stopifnot(file.exists(map_file))

dirs <- file.path(
  out_dir,
  c("data_clean", "tables", "figures", "models", "text_analysis", "diagnostics")
)
purrr::walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. Fonctions générales
# =============================================================================

fix_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u0092||’", "'")
  x <- stringr::str_replace_all(x, "\u0091|‘", "'")
  x <- stringr::str_replace_all(x, "\u0093|“", "\"")
  x <- stringr::str_replace_all(x, "\u0094|”", "\"")
  x <- stringr::str_replace_all(x, "\u0085|…", "…")
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

short_label <- function(x) {
  x <- fix_text(x)
  x <- stringr::str_replace(x, "^.*\\?\\s*-\\s*", "")
  x <- stringr::str_replace(x, "^.*…\\s*-\\s*", "")
  x <- stringr::str_replace(x, "^.*\\.\\.\\.\\s*-\\s*", "")
  stringr::str_squish(x)
}

clean_mention <- function(x) {
  x |>
    fix_text() |>
    stringr::str_to_lower(locale = "fr") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[’'`´]", " ") |>
    stringr::str_replace_all("[^[:alnum:]\\s\\-]", " ") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish()
}

w_sum <- function(x, w) {
  sum(x * w, na.rm = TRUE)
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_prop <- function(condition, w) {
  ok <- !is.na(condition) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(as.numeric(condition[ok]) * w[ok]) / sum(w[ok])
}

safe_pct <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

pp <- function(x) round(100 * x, 1)

row_prop_codes <- function(data, vars, yes_codes, no_codes = NULL) {
  if (length(vars) == 0) return(rep(NA_real_, nrow(data)))
  mat <- data[, vars, drop = TRUE]
  mat <- as.data.frame(lapply(mat, function(x) {
    dplyr::case_when(
      x %in% yes_codes ~ 1,
      !is.null(no_codes) & x %in% no_codes ~ 0,
      is.null(no_codes) & !is.na(x) & !(x %in% yes_codes) ~ 0,
      TRUE ~ NA_real_
    )
  }))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

# Thème graphique OSYR : lisible, calme, présentable.
osyr_palette <- c(
  navy = "#17324D",
  blue = "#3A86FF",
  cyan = "#4CC9F0",
  teal = "#2A9D8F",
  green = "#6A994E",
  sand = "#E9C46A",
  orange = "#F4A261",
  coral = "#E76F51",
  rose = "#D45087",
  purple = "#7B2CBF",
  grey = "#667085",
  light = "#F7F9FB"
)

# Important : on utilise unname() pour éviter que les noms internes
# du vecteur osyr_palette perturbent scale_*_manual(). Sans cela,
# ggplot peut afficher les couleurs par défaut en gris.
exposure_colors <- c(
  "Aucun dispositif" = unname(osyr_palette["coral"]),
  "Autoformation / autre seulement" = unname(osyr_palette["orange"]),
  "Dispositif organisé" = unname(osyr_palette["teal"]),
  "Indéterminé" = "#B8C0CC"
)

language_colors <- c(
  "Questionnaire en français" = unname(osyr_palette["navy"]),
  "Questionnaire en anglais" = unname(osyr_palette["cyan"]),
  "Langue non renseignée" = "#B8C0CC"
)

score_colors <- c(
  "Activités de recherche déjà réalisées" = unname(osyr_palette["blue"]),
  "Bénéfices scientifiques perçus" = unname(osyr_palette["green"]),
  "Contraintes institutionnelles/économiques perçues" = unname(osyr_palette["orange"]),
  "Environnement perçu comme incitatif" = unname(osyr_palette["teal"]),
  "Environnement perçu comme un frein" = unname(osyr_palette["rose"]),
  "Intentions de pratiques ouvertes" = unname(osyr_palette["purple"]),
  "Notions et outils bien connus" = unname(osyr_palette["teal"]),
  "Notions et outils déjà utilisés" = unname(osyr_palette["blue"]),
  "Risques individuels perçus" = unname(osyr_palette["coral"])
)

theme_osyr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 6, color = osyr_palette["navy"], lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(size = base_size + 1.5, color = "#475467", margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(size = base_size - 2, color = "#667085", hjust = 0, margin = ggplot2::margin(t = 12)),
      axis.title = ggplot2::element_text(color = "#344054"),
      axis.text = ggplot2::element_text(color = "#344054"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#EAECF0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(face = "bold", color = osyr_palette["navy"], size = base_size),
      strip.background = ggplot2::element_rect(fill = "#F2F4F7", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

save_plot <- function(plot, filename, width = 12, height = 8) {
  ggplot2::ggsave(
    filename = file.path(out_dir, "figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 340,
    bg = "white"
  )
}

write_table <- function(x, name, subdir = "tables") {
  readr::write_csv(x, file.path(out_dir, subdir, paste0(name, ".csv")))
  invisible(x)
}

write_model <- function(x, name) {
  readr::write_csv(x, file.path(out_dir, "models", paste0(name, ".csv")))
  invisible(x)
}

weighted_frequency <- function(data, var, weight = ".weight") {
  data |>
    dplyr::mutate(category = .data[[var]]) |>
    dplyr::filter(!is.na(category), !is.na(.data[[weight]])) |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.data[[weight]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_w = weighted_n / sum(weighted_n),
      pct_w_label = safe_pct(pct_w)
    ) |>
    dplyr::arrange(dplyr::desc(pct_w))
}

cross_weighted <- function(data, row_var, col_var, weight = ".weight") {
  data |>
    dplyr::mutate(
      row_category = .data[[row_var]],
      col_category = .data[[col_var]]
    ) |>
    dplyr::filter(!is.na(row_category), !is.na(col_category), !is.na(.data[[weight]])) |>
    dplyr::group_by(row_category, col_category) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.data[[weight]], na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      pct_row = weighted_n / sum(weighted_n),
      pct_row_label = safe_pct(pct_row)
    ) |>
    dplyr::ungroup()
}

# Retire les modalités quasi nulles avant représentation.
# Utile pour éviter des légendes trop lourdes, par exemple une modalité
# "Indéterminé" présente dans les niveaux du facteur mais absente du graphique.
drop_small_modalities <- function(df, var_modality, var_value, threshold = 0.005) {
  df |>
    dplyr::group_by(.data[[var_modality]]) |>
    dplyr::mutate(max_value = max(.data[[var_value]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(max_value >= threshold) |>
    dplyr::select(-max_value)
}

# Petit dictionnaire d'aide à l'interprétation des coefficients.
model_interpretation <- function(estimate_pp, conf_low_pp, conf_high_pp, p_value) {
  dplyr::case_when(
    !is.na(p_value) & p_value < 0.05 & conf_low_pp > 0 & estimate_pp >= 8 ~ "Association positive forte",
    !is.na(p_value) & p_value < 0.05 & conf_low_pp > 0 & estimate_pp >= 3 ~ "Association positive modérée",
    !is.na(p_value) & p_value < 0.05 & conf_low_pp > 0 ~ "Association positive faible",
    !is.na(p_value) & p_value < 0.05 & conf_high_pp < 0 ~ "Association négative claire",
    TRUE ~ "Pas d'association claire"
  )
}


make_item_long <- function(data, vars, value_name, item_map, context_vars) {
  data |>
    dplyr::select(dplyr::any_of(context_vars), dplyr::all_of(vars)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "item",
      values_to = value_name
    ) |>
    dplyr::left_join(item_map, by = "item") |>
    dplyr::mutate(
      item_label = dplyr::coalesce(item_label, item),
      item_label = stringr::str_wrap(item_label, width = 62)
    )
}

extract_multi <- function(data, base, context_vars) {
  # Transforme les questions multiples Q8_M1, Q8_M2... en format long.
  # base doit être en noms nettoyés : "q8", "q14_2", "q6_5", etc.
  cols <- names(data) |> stringr::str_subset(paste0("^", base, "_m\\d+$"))
  other_cols <- names(data) |> stringr::str_subset(paste0("^o_", base, "_m\\d+$"))

  if (length(cols) == 0) return(tibble::tibble())

  long <- data |>
    dplyr::select(dplyr::any_of(context_vars), dplyr::all_of(cols), dplyr::any_of(other_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      names_to = "slot",
      values_to = "code"
    ) |>
    dplyr::mutate(
      base = base,
      slot_number = readr::parse_number(slot),
      code = as.numeric(code)
    )

  if (length(other_cols) > 0) {
    other_long <- data |>
      dplyr::select(dplyr::any_of(context_vars), dplyr::all_of(other_cols)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(other_cols),
        names_to = "other_slot",
        values_to = "other_text"
      ) |>
      dplyr::mutate(
        slot_number = readr::parse_number(other_slot),
        other_text = fix_text(other_text)
      ) |>
      dplyr::select(dplyr::any_of(context_vars), slot_number, other_text)

    long <- long |>
      dplyr::left_join(other_long, by = c(context_vars, "slot_number"))
  } else {
    long <- long |> dplyr::mutate(other_text = NA_character_)
  }

  long |> dplyr::filter(!is.na(code))
}

# =============================================================================
# 2. Lecture de la base et de la datamap
# =============================================================================

message("Lecture de la base : ", bdd_file)
df_raw <- readr::read_delim(
  bdd_file,
  delim = ";",
  locale = readr::locale(encoding = "ISO-8859-1"),
  guess_max = 10000,
  show_col_types = FALSE
)

names(df_raw) <- janitor::make_clean_names(names(df_raw))
df_raw <- df_raw |> dplyr::mutate(dplyr::across(where(is.character), fix_text))

message("Lecture de la datamap : ", map_file)
datamap_raw <- readxl::read_excel(map_file, sheet = 1)
names(datamap_raw) <- janitor::make_clean_names(names(datamap_raw))
datamap_raw <- datamap_raw |> dplyr::mutate(dplyr::across(where(is.character), fix_text))

datamap_ff <- datamap_raw |>
  tidyr::fill(ident, type, name, label, .direction = "down") |>
  dplyr::mutate(
    name_clean = janitor::make_clean_names(name),
    label = fix_text(label),
    value = fix_text(value),
    item_label = short_label(label),
    code = suppressWarnings(as.numeric(code))
  )

question_map <- datamap_raw |>
  dplyr::filter(!is.na(name)) |>
  dplyr::transmute(
    ident,
    type,
    name,
    item = janitor::make_clean_names(name),
    question_label = fix_text(label),
    item_label = short_label(label)
  )

answer_labels <- datamap_ff |>
  dplyr::filter(!is.na(code), !is.na(value)) |>
  dplyr::transmute(
    base = name_clean,
    code = as.numeric(code),
    value = value
  )

item_map <- question_map |> dplyr::select(item, question_label, item_label)

readr::write_csv(question_map, file.path(out_dir, "data_clean", "question_map_v2.csv"))
readr::write_csv(answer_labels, file.path(out_dir, "data_clean", "answer_labels_v2.csv"))

# =============================================================================
# 3. Construction de la base analytique
# =============================================================================

weight_var <- "poids"
stopifnot(weight_var %in% names(df_raw))
stopifnot("respondent_language" %in% names(df_raw))
stopifnot(all(c("rs1", "q1", "q2") %in% names(df_raw)))

label_tbl <- function(base_name) {
  answer_labels |> dplyr::filter(base == base_name) |> dplyr::select(code, value)
}

label_from_map <- function(x, labels) {
  out <- labels$value[match(as.numeric(x), labels$code)]
  fix_text(out)
}

rs1_labels <- label_tbl("rs1")
q2_labels  <- label_tbl("q2")

q8_cols <- names(df_raw) |> stringr::str_subset("^q8_m\\d+$")
stopifnot(length(q8_cols) > 0)

# Recodage robuste de l'année : à partir de Q1 numérique et non du libellé.
# Cela évite le problème observé dans le graphique où seule la 1re année apparaissait.
df <- df_raw |>
  dplyr::mutate(
    respondent_id = dplyr::row_number(),
    .weight = as.numeric(.data[[weight_var]]),
    language_group = dplyr::case_when(
      respondent_language == "en" ~ "Questionnaire en anglais",
      respondent_language == "fr" ~ "Questionnaire en français",
      TRUE ~ "Langue non renseignée"
    ),
    institution = label_from_map(rs1, rs1_labels),
    year_code = as.numeric(q1),
    year = dplyr::case_when(
      year_code == 1 ~ "1re année",
      year_code == 2 ~ "2e année",
      year_code == 3 ~ "3e année",
      year_code == 4 ~ "4e année ou plus",
      year_code == 5 ~ "Thèse déjà soutenue",
      TRUE ~ NA_character_
    ),
    discipline_code = as.numeric(q2),
    discipline = label_from_map(q2, q2_labels),
    discipline_broad = dplyr::case_when(
      discipline_code %in% c(1, 2, 3, 4) ~ "Sciences formelles, physiques et chimiques",
      discipline_code %in% c(5, 10) ~ "Sciences du vivant, santé et environnement",
      discipline_code %in% c(6, 7) ~ "Sciences humaines et sociales",
      discipline_code %in% c(8, 9) ~ "Ingénierie, informatique et numérique",
      TRUE ~ "Autre / non classé"
    )
  )

q8_codes_by_row <- df |>
  dplyr::select(dplyr::all_of(q8_cols)) |>
  purrr::pmap(function(...) {
    vals <- c(...)
    vals <- vals[!is.na(vals)]
    unique(as.numeric(vals))
  })

df <- df |>
  dplyr::mutate(
    q8_has_none = purrr::map_lgl(q8_codes_by_row, ~ 97 %in% .x),
    q8_has_organized = purrr::map_lgl(q8_codes_by_row, ~ any(.x %in% 1:5)),
    q8_has_self_or_other = purrr::map_lgl(q8_codes_by_row, ~ any(.x %in% c(6, 98))),
    q8_n_organized_types = purrr::map_int(q8_codes_by_row, ~ length(intersect(.x, 1:5))),
    exposure3 = dplyr::case_when(
      q8_has_none ~ "Aucun dispositif",
      q8_has_organized ~ "Dispositif organisé",
      q8_has_self_or_other ~ "Autoformation / autre seulement",
      TRUE ~ "Indéterminé"
    ),
    exposure2 = dplyr::case_when(
      exposure3 == "Aucun dispositif" ~ "Aucun dispositif",
      exposure3 == "Dispositif organisé" ~ "Dispositif organisé",
      TRUE ~ NA_character_
    ),
    training_intensity = dplyr::case_when(
      exposure3 == "Aucun dispositif" ~ "Aucun dispositif",
      q10 == 1 ~ "1 formation/action",
      q10 == 2 ~ "2 ou 3 formations/actions",
      q10 == 3 ~ "4 formations/actions ou plus",
      q10 == 97 ~ "Nombre inconnu",
      exposure3 == "Autoformation / autre seulement" ~ "Autoformation / autre seulement",
      TRUE ~ "Non renseigné"
    )
  ) |>
  dplyr::mutate(
    language_group = factor(language_group, levels = c("Questionnaire en français", "Questionnaire en anglais", "Langue non renseignée")),
    year = factor(year, levels = c("1re année", "2e année", "3e année", "4e année ou plus", "Thèse déjà soutenue")),
    exposure3 = factor(exposure3, levels = c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé", "Indéterminé")),
    exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé")),
    training_intensity = factor(
      training_intensity,
      levels = c("Aucun dispositif", "1 formation/action", "2 ou 3 formations/actions", "4 formations/actions ou plus", "Autoformation / autre seulement", "Nombre inconnu", "Non renseigné")
    ),
    discipline_broad = factor(
      discipline_broad,
      levels = c("Sciences humaines et sociales", "Sciences du vivant, santé et environnement", "Sciences formelles, physiques et chimiques", "Ingénierie, informatique et numérique", "Autre / non classé")
    )
  )

# Vérification immédiate de l'année, pour éviter toute disparition silencieuse.
year_check <- df |>
  dplyr::count(year_code, year, name = "n") |>
  dplyr::arrange(year_code)

write_table(year_check, "year_check_q1_codes", subdir = "diagnostics")

if (dplyr::n_distinct(stats::na.omit(df$year)) < 3) {
  warning("Attention : moins de trois modalités d'année détectées. Vérifier Q1 et year_check_q1_codes.csv")
}

# Sauvegarde de la base propre.
readr::write_csv(df, file.path(out_dir, "data_clean", "osyr_v2_corrigee_clean.csv"))
saveRDS(df, file.path(out_dir, "data_clean", "osyr_v2_corrigee_clean.rds"))

# =============================================================================
# 4. Contrôles qualité
# =============================================================================

quality_overview <- tibble::tibble(
  n_rows = nrow(df),
  n_cols = ncol(df),
  n_language_fr = sum(df$respondent_language == "fr", na.rm = TRUE),
  n_language_en = sum(df$respondent_language == "en", na.rm = TRUE),
  pct_language_en_unweighted = mean(df$respondent_language == "en", na.rm = TRUE),
  sum_weights = sum(df$.weight, na.rm = TRUE),
  min_weight = min(df$.weight, na.rm = TRUE),
  max_weight = max(df$.weight, na.rm = TRUE),
  mean_weight = mean(df$.weight, na.rm = TRUE),
  missing_weight = sum(is.na(df$.weight)),
  n_year_levels = dplyr::n_distinct(stats::na.omit(df$year)),
  n_exposure3_levels = dplyr::n_distinct(stats::na.omit(df$exposure3))
)

missing_by_variable <- df |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.x)))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "missing_rate") |>
  dplyr::arrange(dplyr::desc(missing_rate))

write_table(quality_overview, "quality_overview")
write_table(missing_by_variable, "missing_by_variable")

# Design d'enquête pondéré.
df_survey <- df |> dplyr::filter(!is.na(.weight), .weight > 0)
design <- survey::svydesign(ids = ~1, weights = ~.weight, data = df_survey)

# =============================================================================
# 5. Structure de l'échantillon et exposition
# =============================================================================

sample_language <- weighted_frequency(df, "language_group") |> write_table("sample_language")
sample_year <- weighted_frequency(df, "year") |> write_table("sample_year")
sample_institution <- weighted_frequency(df, "institution") |> write_table("sample_institution")
sample_discipline <- weighted_frequency(df, "discipline_broad") |> write_table("sample_discipline_broad")
sample_exposure <- weighted_frequency(df, "exposure3") |> write_table("sample_exposure3")
sample_intensity <- weighted_frequency(df, "training_intensity") |> write_table("sample_training_intensity")

# Graphique : année de thèse, pour vérifier que les quatre années apparaissent bien.
p_year <- sample_year |>
  dplyr::mutate(category = factor(category, levels = levels(df$year))) |>
  ggplot2::ggplot(ggplot2::aes(x = category, y = pct_w)) +
  ggplot2::geom_col(width = 0.68, fill = osyr_palette["navy"]) +
  ggplot2::geom_text(ggplot2::aes(label = pct_w_label), vjust = -0.45, color = osyr_palette["navy"], size = 4) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(sample_year$pct_w, na.rm = TRUE) * 1.22)) +
  ggplot2::labs(
    title = "Année de thèse des répondants",
    subtitle = "Répartition pondérée recodée directement à partir de Q1.",
    x = NULL,
    y = "Pourcentage pondéré",
    caption = "Source : enquête OSYR V2 corrigée, pondération Poids."
  ) +
  theme_osyr() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 12, hjust = 1))

save_plot(p_year, "00_distribution_annee_these_controle.png", width = 10.5, height = 5.8)

p_language <- sample_language |>
  dplyr::mutate(category = forcats::fct_reorder(category, pct_w)) |>
  ggplot2::ggplot(ggplot2::aes(x = category, y = pct_w, fill = category)) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_text(ggplot2::aes(label = pct_w_label), hjust = -0.12, color = osyr_palette["navy"], size = 4) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(sample_language$pct_w, na.rm = TRUE) * 1.18)) +
  ggplot2::scale_fill_manual(values = language_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Langue du questionnaire",
    subtitle = "La modalité anglaise est analysée comme un indicateur prudent de profil international.",
    x = NULL,
    y = "Pourcentage pondéré",
    caption = "Source : enquête OSYR V2 corrigée, pondération Poids."
  ) +
  theme_osyr() +
  ggplot2::theme(legend.position = "none")

save_plot(p_language, "01_langue_questionnaire.png", width = 10, height = 5.5)

p_exposure <- sample_exposure |>
  dplyr::filter(category != "Indéterminé") |>
  dplyr::mutate(category = forcats::fct_reorder(category, pct_w)) |>
  ggplot2::ggplot(ggplot2::aes(x = category, y = pct_w, fill = category)) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_text(ggplot2::aes(label = pct_w_label), hjust = -0.12, color = osyr_palette["navy"], size = 4) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(sample_exposure$pct_w, na.rm = TRUE) * 1.18)) +
  ggplot2::scale_fill_manual(values = exposure_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Exposition aux dispositifs de science ouverte",
    subtitle = "Classification construite à partir de Q8 : aucun dispositif, autoformation/autre seulement, dispositif organisé.",
    x = NULL,
    y = "Pourcentage pondéré",
    caption = "Source : enquête OSYR V2 corrigée, pondération Poids."
  ) +
  theme_osyr() +
  ggplot2::theme(legend.position = "none")

save_plot(p_exposure, "02_exposition_dispositifs.png", width = 10.5, height = 5.5)

# Croisements structurants.
cross_exposure_year <- cross_weighted(df, "year", "exposure3") |> write_table("cross_exposure_by_year")
cross_exposure_discipline <- cross_weighted(df, "discipline_broad", "exposure3") |> write_table("cross_exposure_by_discipline")
cross_exposure_language <- cross_weighted(df, "language_group", "exposure3") |> write_table("cross_exposure_by_language")
cross_exposure_institution <- cross_weighted(df, "institution", "exposure3") |> write_table("cross_exposure_by_institution")

p_exposure_year <- cross_exposure_year |>
  dplyr::filter(col_category != "Indéterminé") |>
  dplyr::mutate(row_category = factor(row_category, levels = levels(df$year))) |>
  ggplot2::ggplot(ggplot2::aes(x = row_category, y = pct_row, fill = col_category)) +
  ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.4) +
  ggplot2::geom_text(
    data = ~ dplyr::filter(.x, pct_row >= 0.08),
    ggplot2::aes(label = scales::percent(pct_row, accuracy = 1, decimal.mark = ",")),
    position = ggplot2::position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.6
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = exposure_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Exposition selon l'année de thèse",
    subtitle = "Répartition pondérée des types d'exposition dans chaque année. Recodage robuste à partir des codes Q1.",
    x = NULL,
    y = "Pourcentage pondéré",
    caption = "Lecture : chaque barre représente 100 % des répondants d'une même année de thèse."
  ) +
  theme_osyr() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10, hjust = 1))

save_plot(p_exposure_year, "03_exposition_par_annee_CORRIGE.png", width = 12, height = 6.5)

p_exposure_discipline <- cross_exposure_discipline |>
  dplyr::filter(col_category != "Indéterminé") |>
  ggplot2::ggplot(ggplot2::aes(x = row_category, y = pct_row, fill = col_category)) +
  ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.4) +
  ggplot2::geom_text(
    data = ~ dplyr::filter(.x, pct_row >= 0.08),
    ggplot2::aes(label = scales::percent(pct_row, accuracy = 1, decimal.mark = ",")),
    position = ggplot2::position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.3
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = exposure_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Exposition selon le domaine disciplinaire",
    subtitle = "Répartition pondérée des types d'exposition dans chaque grand domaine.",
    x = NULL,
    y = "Pourcentage pondéré"
  ) +
  theme_osyr()

save_plot(p_exposure_discipline, "04_exposition_par_discipline.png", width = 12, height = 6.8)

p_exposure_language <- cross_exposure_language |>
  dplyr::filter(col_category != "Indéterminé") |>
  ggplot2::ggplot(ggplot2::aes(x = row_category, y = pct_row, fill = col_category)) +
  ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.4) +
  ggplot2::geom_text(
    data = ~ dplyr::filter(.x, pct_row >= 0.08),
    ggplot2::aes(label = scales::percent(pct_row, accuracy = 1, decimal.mark = ",")),
    position = ggplot2::position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = exposure_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Exposition selon la langue du questionnaire",
    subtitle = "Comparaison entre répondants au questionnaire français et anglais.",
    x = NULL,
    y = "Pourcentage pondéré"
  ) +
  theme_osyr()

save_plot(p_exposure_language, "05_exposition_par_langue.png", width = 11.5, height = 5.7)

# =============================================================================
# 6. Tables longues pour les batteries de questions
# =============================================================================

context_vars <- c(
  "respondent_id", ".weight", "exposure3", "exposure2", "training_intensity",
  "year_code", "year", "discipline", "discipline_broad", "institution", "language_group"
)

q4_vars  <- names(df) |> stringr::str_subset("^q4_a\\d+$")
q5_vars  <- names(df) |> stringr::str_subset("^q5_a\\d+$")
q11_vars <- names(df) |> stringr::str_subset("^q11_a\\d+$")
q12_vars <- names(df) |> stringr::str_subset("^q12_a\\d+$")
q13_vars <- names(df) |> stringr::str_subset("^q13_a\\d+$")
q15_vars <- names(df) |> stringr::str_subset("^q15_a\\d+$")

q4_long <- make_item_long(df, q4_vars, "response", item_map, context_vars) |>
  dplyr::mutate(positive = response == 1, family = "Activités de recherche déjà réalisées")

q5_long <- make_item_long(df, q5_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    known_well = response %in% c(3, 4),
    used = response == 4,
    family = "Notions et outils de science ouverte"
  )

q11_long <- make_item_long(df, q11_vars, "response", item_map, context_vars) |>
  dplyr::mutate(agree = response %in% c(3, 4), family = "Effet perçu des formations")

q12_long <- make_item_long(df, q12_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    incitation = response %in% c(4, 5),
    frein = response %in% c(1, 2),
    neutral = response == 3,
    dont_know = response == 97,
    family = "Freins et incitations"
  )

q13_long <- make_item_long(df, q13_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    yes = response == 1,
    no = response == 2,
    dont_know = response == 97,
    open_intention_item = item %in% c("q13_a1", "q13_a2", "q13_a3", "q13_a5"),
    family = "Intentions de pratiques"
  )

q15_long <- make_item_long(df, q15_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    agree = response %in% c(4, 5),
    disagree = response %in% c(1, 2),
    statement_type = dplyr::case_when(
      item %in% c("q15_a2", "q15_a4", "q15_a6") ~ "Bénéfices scientifiques",
      item %in% c("q15_a1", "q15_a3") ~ "Risques individuels",
      item %in% c("q15_a5", "q15_a7") ~ "Contraintes institutionnelles / économiques",
      TRUE ~ "Autre"
    ),
    family = "Représentations de la science ouverte"
  )

# Sauvegarde des formats longs utiles.
readr::write_csv(q4_long, file.path(out_dir, "data_clean", "q4_long.csv"))
readr::write_csv(q5_long, file.path(out_dir, "data_clean", "q5_long.csv"))
readr::write_csv(q12_long, file.path(out_dir, "data_clean", "q12_long.csv"))
readr::write_csv(q13_long, file.path(out_dir, "data_clean", "q13_long.csv"))
readr::write_csv(q15_long, file.path(out_dir, "data_clean", "q15_long.csv"))

# =============================================================================
# 7. Descriptifs item par item
# =============================================================================

summarise_item_binary <- function(long_data, value_col, group_vars = character()) {
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)

  # Sécurité v2 : les libellés viennent de la datamap. Si la jointure avec la
  # datamap échoue pour certaines batteries, on ne bloque pas tout le workflow :
  # on recrée un libellé minimal à partir du nom de l'item.
  if (nrow(long_data) == 0 || !value_name %in% names(long_data)) {
    warning("Table longue vide ou colonne absente : ", value_name)
    return(tibble::tibble(
      item = character(),
      item_label = character(),
      n = integer(),
      weighted_n = numeric(),
      pct_w = numeric(),
      pct_w_label = character()
    ))
  }
  if (!"item" %in% names(long_data)) {
    long_data <- dplyr::mutate(long_data, item = paste0("item_", dplyr::row_number()))
  }
  if (!"item_label" %in% names(long_data)) {
    long_data <- dplyr::mutate(long_data, item_label = as.character(item))
  }

  group_cols <- unique(c(group_vars, "item", "item_label"))

  long_data |>
    dplyr::filter(!is.na(!!value_col), !is.na(.weight)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_cols))) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.weight, na.rm = TRUE),
      pct_w = sum(as.numeric(!!value_col) * .weight, na.rm = TRUE) / sum(.weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(pct_w_label = safe_pct(pct_w))
}

q4_overall <- summarise_item_binary(q4_long, positive) |> write_table("q4_activities_overall")
q5_known_overall <- summarise_item_binary(q5_long, known_well) |> write_table("q5_known_well_overall")
q5_used_overall <- summarise_item_binary(q5_long, used) |> write_table("q5_used_overall")
q12_incit_overall <- summarise_item_binary(q12_long, incitation) |> write_table("q12_incitation_overall")
q12_frein_overall <- summarise_item_binary(q12_long, frein) |> write_table("q12_frein_overall")
q13_yes_overall <- summarise_item_binary(q13_long, yes) |> write_table("q13_intentions_yes_overall")
q15_agree_overall <- summarise_item_binary(q15_long, agree) |> write_table("q15_agreement_overall")

plot_ranked_bar <- function(tab, filename, title, subtitle, fill = osyr_palette["navy"], width = 12, height = 7.5) {
  # Sécurité ajoutée dans le package v2 : selon les versions de datamap,
  # certaines tables intermédiaires peuvent ne pas contenir `item_label`.
  # Dans ce cas, on utilise `item`, puis `category`, puis un identifiant de ligne.
  # Cela évite l'arrêt du workflow sur une figure tout en gardant une trace claire.
  if (nrow(tab) == 0) {
    warning("Table vide pour la figure : ", filename)
    return(invisible(NULL))
  }
  if (!"pct_w" %in% names(tab)) {
    warning("Colonne pct_w absente pour la figure : ", filename)
    return(invisible(NULL))
  }
  if (!"item_label" %in% names(tab)) {
    if ("item" %in% names(tab)) {
      tab <- dplyr::mutate(tab, item_label = as.character(item))
    } else if ("category" %in% names(tab)) {
      tab <- dplyr::mutate(tab, item_label = as.character(category))
    } else {
      tab <- dplyr::mutate(tab, item_label = paste0("Item ", dplyr::row_number()))
    }
  }

  tab <- tab |>
    dplyr::filter(!is.na(pct_w)) |>
    dplyr::mutate(item_label = as.character(item_label))

  if (nrow(tab) == 0) {
    warning("Table sans valeur exploitable pour la figure : ", filename)
    return(invisible(NULL))
  }

  p <- tab |>
    dplyr::arrange(pct_w) |>
    dplyr::mutate(item_label = factor(item_label, levels = unique(item_label))) |>
    ggplot2::ggplot(ggplot2::aes(x = item_label, y = pct_w)) +
    ggplot2::geom_col(width = 0.68, fill = fill) +
    ggplot2::geom_text(
      ggplot2::aes(label = safe_pct(pct_w, accuracy = 1)),
      hjust = -0.10,
      color = osyr_palette["navy"],
      size = 3.8
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(tab$pct_w, na.rm = TRUE) * 1.22)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Pourcentage pondéré") +
    theme_osyr()
  save_plot(p, filename, width = width, height = height)
  invisible(p)
}

plot_ranked_bar(q5_known_overall, "06_q5_notions_bien_connues.png", "Notions et outils bien connus", "Part pondérée des doctorants déclarant connaître bien chaque notion ou outil.", fill = unname(osyr_palette["teal"]), height = 8.5)
plot_ranked_bar(q5_used_overall, "07_q5_notions_deja_utilisees.png", "Notions et outils déjà utilisés", "Part pondérée des doctorants déclarant avoir déjà utilisé chaque notion ou outil.", fill = unname(osyr_palette["blue"]), height = 8.5)
plot_ranked_bar(q13_yes_overall, "08_q13_intentions_oui.png", "Intentions de pratiques ouvertes ou de valorisation", "Part pondérée de réponses « oui » pour chaque intention.", fill = unname(osyr_palette["purple"]), height = 6.6)
plot_ranked_bar(q15_agree_overall, "09_q15_accord_affirmations.png", "Accord avec les affirmations sur la science ouverte", "Part pondérée de réponses « plutôt d'accord » ou « tout à fait d'accord ».", fill = unname(osyr_palette["navy"]), height = 7.2)

# Figure premium : écart entre connaissance et usage.
# Elle montre immédiatement les notions connues mais encore peu appropriées
# dans les pratiques.
#
# Correctif v3 : ne pas utiliser dplyr::if_else() pour tester l'existence
# d'une colonne. if_else() travaille ligne à ligne et attend des vecteurs de
# même longueur ; ici le test `"item_label" %in% names(...)` est un seul TRUE/FALSE.
# On prépare donc la table en amont avec un if/else classique, puis on poursuit
# le pipeline. Cela évite l'erreur :
#   Can't recycle `true` (size 15) to size 1.
q5_known_for_gap <- q5_known_overall
if (!"item_label" %in% names(q5_known_for_gap)) {
  if ("item" %in% names(q5_known_for_gap)) {
    q5_known_for_gap$item_label <- as.character(q5_known_for_gap$item)
  } else {
    q5_known_for_gap$item <- paste0("item_", seq_len(nrow(q5_known_for_gap)))
    q5_known_for_gap$item_label <- q5_known_for_gap$item
  }
}

q5_used_for_gap <- q5_used_overall
if (!"item" %in% names(q5_used_for_gap)) {
  q5_used_for_gap$item <- paste0("item_", seq_len(nrow(q5_used_for_gap)))
}

q5_known_used_gap <- q5_known_for_gap |>
  dplyr::mutate(item_label = as.character(item_label)) |>
  dplyr::select(item, item_label, pct_known = pct_w) |>
  dplyr::left_join(
    q5_used_for_gap |> dplyr::select(item, pct_used = pct_w),
    by = "item"
  ) |>
  dplyr::mutate(
    pct_used = dplyr::coalesce(pct_used, 0),
    gap_known_used = pct_known - pct_used,
    item_label = forcats::fct_reorder(item_label, gap_known_used)
  ) |>
  write_table("q5_gap_known_used")

p_q5_gap <- q5_known_used_gap |>
  ggplot2::ggplot(ggplot2::aes(y = item_label)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = pct_used, xend = pct_known, yend = item_label),
    linewidth = 1.25,
    color = "#CBD5E1"
  ) +
  ggplot2::geom_point(ggplot2::aes(x = pct_used), size = 3.2, color = unname(osyr_palette["blue"])) +
  ggplot2::geom_point(ggplot2::aes(x = pct_known), size = 3.2, color = unname(osyr_palette["teal"])) +
  ggplot2::geom_text(
    ggplot2::aes(x = pmax(pct_known, pct_used), label = paste0("+", round(100 * gap_known_used), " pts")),
    hjust = -0.12,
    size = 3.3,
    color = unname(osyr_palette["navy"])
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(q5_known_used_gap$pct_known, q5_known_used_gap$pct_used, na.rm = TRUE) * 1.18)
  ) +
  ggplot2::labs(
    title = "De la connaissance à l'usage : où se situent les écarts ?",
    subtitle = "Chaque segment relie la part qui connaît bien la notion et la part qui l'a déjà utilisée.",
    x = "Pourcentage pondéré",
    y = NULL,
    caption = "Bleu = déjà utilisé ; vert = bien connu. Plus le segment est long, plus la notion reste connue sans être encore pratiquée."
  ) +
  theme_osyr() +
  ggplot2::theme(legend.position = "none")

save_plot(p_q5_gap, "06b_q5_ecart_connaissance_usage.png", width = 13.5, height = 8.5)

# =============================================================================
# 8. Comparaisons exposés / non exposés item par item
# =============================================================================

compare_exposure_item <- function(long_data, value_col, min_n = 25) {
  value_col <- rlang::ensym(value_col)

  by_group <- long_data |>
    dplyr::filter(!is.na(exposure2), !is.na(!!value_col), !is.na(.weight)) |>
    dplyr::group_by(exposure2, item, item_label) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.weight, na.rm = TRUE),
      pct_w = sum(as.numeric(!!value_col) * .weight, na.rm = TRUE) / sum(.weight, na.rm = TRUE),
      .groups = "drop"
    )

  wide <- by_group |>
    dplyr::select(exposure2, item, item_label, n, weighted_n, pct_w) |>
    tidyr::pivot_wider(
      names_from = exposure2,
      values_from = c(n, weighted_n, pct_w),
      names_sep = "__"
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      diff_pp = 100 * (pct_w_dispositif_organise - pct_w_aucun_dispositif),
      ratio = pct_w_dispositif_organise / pct_w_aucun_dispositif,
      flag_small_cell = dplyr::if_any(dplyr::starts_with("n_"), ~ .x < min_n)
    ) |>
    dplyr::arrange(dplyr::desc(abs(diff_pp)))

  list(by_group = by_group, diff = wide)
}

q4_cmp <- compare_exposure_item(q4_long, positive)
q5_known_cmp <- compare_exposure_item(q5_long, known_well)
q5_used_cmp <- compare_exposure_item(q5_long, used)
q12_incit_cmp <- compare_exposure_item(q12_long, incitation)
q12_frein_cmp <- compare_exposure_item(q12_long, frein)
q13_yes_cmp <- compare_exposure_item(q13_long, yes)
q15_agree_cmp <- compare_exposure_item(q15_long, agree)

write_table(q4_cmp$by_group, "q4_by_exposure")
write_table(q4_cmp$diff, "q4_diff_exposure")
write_table(q5_known_cmp$by_group, "q5_known_by_exposure")
write_table(q5_known_cmp$diff, "q5_known_diff_exposure")
write_table(q5_used_cmp$by_group, "q5_used_by_exposure")
write_table(q5_used_cmp$diff, "q5_used_diff_exposure")
write_table(q12_incit_cmp$diff, "q12_incitation_diff_exposure")
write_table(q12_frein_cmp$diff, "q12_frein_diff_exposure")
write_table(q13_yes_cmp$diff, "q13_intentions_diff_exposure")
write_table(q15_agree_cmp$diff, "q15_agreement_diff_exposure")

plot_diff <- function(diff_tab, filename, title, subtitle, top_n = 15) {
  tab <- diff_tab |>
    dplyr::filter(!is.na(diff_pp)) |>
    dplyr::slice_max(abs(diff_pp), n = top_n) |>
    dplyr::mutate(
      item_label = stringr::str_wrap(item_label, 58),
      item_label = forcats::fct_reorder(item_label, diff_pp),
      direction = dplyr::if_else(diff_pp >= 0, "Plus élevé parmi les exposés", "Plus élevé parmi les non exposés")
    )

  p <- tab |>
    ggplot2::ggplot(ggplot2::aes(x = item_label, y = diff_pp, fill = direction)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_hline(yintercept = 0, color = "#344054", linewidth = 0.45) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(ifelse(diff_pp > 0, "+", ""), round(diff_pp, 1), " pts")),
      hjust = dplyr::if_else(tab$diff_pp >= 0, -0.08, 1.08),
      size = 3.5,
      color = osyr_palette["navy"]
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = c("Plus élevé parmi les exposés" = unname(osyr_palette["teal"]), "Plus élevé parmi les non exposés" = unname(osyr_palette["coral"]))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, " pts"), expand = ggplot2::expansion(mult = c(0.15, 0.18))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Différence en points de pourcentage") +
    theme_osyr()

  save_plot(p, filename, width = 13, height = 8)
}

plot_diff(q5_known_cmp$diff, "10_diff_q5_connaissance_exposition.png", "Ce qui distingue le plus les doctorants exposés", "Différences pondérées de connaissance entre dispositif organisé et aucun dispositif.")
plot_diff(q5_used_cmp$diff, "11_diff_q5_usage_exposition.png", "Usages associés à l'exposition aux dispositifs", "Différences pondérées d'usage déclaré entre dispositif organisé et aucun dispositif.")
plot_diff(q13_yes_cmp$diff, "12_diff_q13_intentions_exposition.png", "Intentions : écarts entre exposés et non exposés", "Différences pondérées de réponses « oui ».", top_n = 8)
plot_diff(q15_agree_cmp$diff, "13_diff_q15_representations_exposition.png", "Représentations : écarts entre exposés et non exposés", "Différences pondérées d'accord avec les affirmations.", top_n = 10)

# Heatmaps : exposition × groupe × scores.
plot_heatmap_group <- function(tab, filename, title, subtitle, x_lab = NULL) {
  p <- tab |>
    dplyr::mutate(
      pct_label = round(100 * pct_w, 0),
      item_label = stringr::str_wrap(item_label, 48)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = group, y = item_label, fill = pct_w)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(pct_label, "%")), size = 3.2, color = "#172A3A") +
    ggplot2::scale_fill_gradient(low = "#EEF6F9", high = osyr_palette["teal"], labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_lab, y = NULL, fill = NULL) +
    theme_osyr() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 18, hjust = 1))
  save_plot(p, filename, width = 12, height = 8)
}

q5_known_year <- q5_long |>
  dplyr::filter(!is.na(year), !is.na(known_well)) |>
  dplyr::group_by(group = year, item, item_label) |>
  dplyr::summarise(pct_w = w_prop(known_well, .weight), n = dplyr::n(), .groups = "drop")

plot_heatmap_group(q5_known_year, "14_heatmap_q5_connaissance_par_annee.png", "Connaissance des notions selon l'année de thèse", "Part pondérée déclarant bien connaître chaque notion ou outil.")

q5_known_disc <- q5_long |>
  dplyr::filter(!is.na(discipline_broad), !is.na(known_well)) |>
  dplyr::group_by(group = discipline_broad, item, item_label) |>
  dplyr::summarise(pct_w = w_prop(known_well, .weight), n = dplyr::n(), .groups = "drop")

plot_heatmap_group(q5_known_disc, "15_heatmap_q5_connaissance_par_discipline.png", "Connaissance des notions selon la discipline", "Part pondérée déclarant bien connaître chaque notion ou outil.")

# =============================================================================
# 9. Scores synthétiques
# =============================================================================

q13_open_vars <- intersect(q13_vars, c("q13_a1", "q13_a2", "q13_a3", "q13_a5"))
q15_benefit_vars <- intersect(q15_vars, c("q15_a2", "q15_a4", "q15_a6"))
q15_risk_vars <- intersect(q15_vars, c("q15_a1", "q15_a3"))
q15_constraint_vars <- intersect(q15_vars, c("q15_a5", "q15_a7"))

scores <- df |>
  dplyr::mutate(
    score_q4_activities = row_prop_codes(dplyr::cur_data_all(), q4_vars, yes_codes = 1, no_codes = 2),
    score_q5_known_well = row_prop_codes(dplyr::cur_data_all(), q5_vars, yes_codes = c(3, 4), no_codes = c(1, 2)),
    score_q5_used = row_prop_codes(dplyr::cur_data_all(), q5_vars, yes_codes = 4, no_codes = c(1, 2, 3)),
    score_q12_incitation = row_prop_codes(dplyr::cur_data_all(), q12_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3, 97)),
    score_q12_frein = row_prop_codes(dplyr::cur_data_all(), q12_vars, yes_codes = c(1, 2), no_codes = c(3, 4, 5, 97)),
    score_q13_open_intentions = row_prop_codes(dplyr::cur_data_all(), q13_open_vars, yes_codes = 1, no_codes = c(2, 97)),
    score_q15_benefits = row_prop_codes(dplyr::cur_data_all(), q15_benefit_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3)),
    score_q15_risks = row_prop_codes(dplyr::cur_data_all(), q15_risk_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3)),
    score_q15_constraints = row_prop_codes(dplyr::cur_data_all(), q15_constraint_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3))
  )

score_vars <- c(
  "score_q4_activities",
  "score_q5_known_well",
  "score_q5_used",
  "score_q12_incitation",
  "score_q12_frein",
  "score_q13_open_intentions",
  "score_q15_benefits",
  "score_q15_risks",
  "score_q15_constraints"
)

score_labels <- tibble::tribble(
  ~score, ~score_label,
  "score_q4_activities", "Activités de recherche déjà réalisées",
  "score_q5_known_well", "Notions et outils bien connus",
  "score_q5_used", "Notions et outils déjà utilisés",
  "score_q12_incitation", "Environnement perçu comme incitatif",
  "score_q12_frein", "Environnement perçu comme un frein",
  "score_q13_open_intentions", "Intentions de pratiques ouvertes",
  "score_q15_benefits", "Bénéfices scientifiques perçus",
  "score_q15_risks", "Risques individuels perçus",
  "score_q15_constraints", "Contraintes institutionnelles/économiques perçues"
)

score_summary_by <- function(data, group_vars) {
  data |>
    dplyr::select(dplyr::all_of(c(group_vars, ".weight", score_vars))) |>
    tidyr::pivot_longer(dplyr::all_of(score_vars), names_to = "score", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(.weight)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "score")))) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.weight, na.rm = TRUE),
      mean_w = w_mean(value, .weight),
      .groups = "drop"
    ) |>
    dplyr::left_join(score_labels, by = "score")
}

scores_by_exposure <- score_summary_by(scores, "exposure2") |> write_table("scores_by_exposure2")
scores_by_exposure_year <- score_summary_by(scores, c("exposure2", "year")) |> write_table("scores_by_exposure2_year")
scores_by_exposure_disc <- score_summary_by(scores, c("exposure2", "discipline_broad")) |> write_table("scores_by_exposure2_discipline")
scores_by_exposure_lang <- score_summary_by(scores, c("exposure2", "language_group")) |> write_table("scores_by_exposure2_language")
scores_by_intensity <- score_summary_by(scores, "training_intensity") |> write_table("scores_by_training_intensity")

# Différences de scores exposés vs non exposés.
score_diff_exposure <- scores_by_exposure |>
  dplyr::select(exposure2, score, score_label, mean_w, n, weighted_n) |>
  tidyr::pivot_wider(names_from = exposure2, values_from = c(mean_w, n, weighted_n), names_sep = "__") |>
  janitor::clean_names() |>
  dplyr::mutate(
    diff_pp = 100 * (mean_w_dispositif_organise - mean_w_aucun_dispositif)
  ) |>
  dplyr::arrange(dplyr::desc(abs(diff_pp))) |>
  write_table("score_diff_exposure2")

p_score_diff <- score_diff_exposure |>
  dplyr::mutate(
    score_label = forcats::fct_reorder(score_label, diff_pp),
    direction = dplyr::if_else(diff_pp >= 0, "Plus élevé parmi les exposés", "Plus élevé parmi les non exposés")
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = score_label, y = diff_pp, fill = direction)) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_hline(yintercept = 0, color = "#344054", linewidth = 0.45) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(ifelse(diff_pp > 0, "+", ""), round(diff_pp, 1), " pts")),
    hjust = dplyr::if_else(score_diff_exposure$diff_pp >= 0, -0.08, 1.08),
    size = 3.6,
    color = osyr_palette["navy"]
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_fill_manual(values = c("Plus élevé parmi les exposés" = unname(osyr_palette["teal"]), "Plus élevé parmi les non exposés" = unname(osyr_palette["coral"]))) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, " pts"), expand = ggplot2::expansion(mult = c(0.12, 0.18))) +
  ggplot2::labs(
    title = "Ce que change l'exposition aux dispositifs",
    subtitle = "Différence pondérée de scores moyens : dispositif organisé moins aucun dispositif.",
    x = NULL,
    y = "Différence en points de pourcentage"
  ) +
  theme_osyr()

save_plot(p_score_diff, "16_scores_difference_exposition.png", width = 12, height = 7.2)

# Figure synthèse premium : quatre messages en une page.
# Utile pour réunion de consortium ou diapositive de discussion.
make_kpi_tile <- function(value, title, subtitle, fill = unname(osyr_palette["navy"])) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = fill, alpha = 0.96) +
    ggplot2::annotate("text", x = 0.06, y = 0.68, label = value, hjust = 0, color = "white", size = 9, fontface = "bold") +
    ggplot2::annotate("text", x = 0.06, y = 0.38, label = title, hjust = 0, color = "white", size = 4.4, fontface = "bold") +
    ggplot2::annotate("text", x = 0.06, y = 0.18, label = subtitle, hjust = 0, color = "#F2F4F7", size = 3.3) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void()
}

kpi_exposed <- sample_exposure |>
  dplyr::filter(category == "Dispositif organisé") |>
  dplyr::pull(pct_w) |>
  (\(x) if (length(x) == 0) NA_real_ else x[1])()

kpi_english <- sample_language |>
  dplyr::filter(category == "Questionnaire en anglais") |>
  dplyr::pull(pct_w) |>
  (\(x) if (length(x) == 0) NA_real_ else x[1])()

kpi_known_effect <- score_diff_exposure |>
  dplyr::filter(score_label == "Notions et outils bien connus") |>
  dplyr::pull(diff_pp) |>
  (\(x) if (length(x) == 0) NA_real_ else x[1])()

kpi_context_effect <- score_diff_exposure |>
  dplyr::filter(score_label == "Environnement perçu comme incitatif") |>
  dplyr::pull(diff_pp) |>
  (\(x) if (length(x) == 0) NA_real_ else x[1])()

kpi1 <- make_kpi_tile(safe_pct(kpi_exposed, 0.1), "ont suivi un dispositif organisé", "Exposition construite à partir de Q8", unname(osyr_palette["teal"]))
kpi2 <- make_kpi_tile(safe_pct(kpi_english, 0.1), "ont répondu en anglais", "Proxy prudent d'un profil international", unname(osyr_palette["cyan"]))
kpi3 <- make_kpi_tile(paste0("+", round(kpi_known_effect, 1), " pts"), "sur les notions bien connues", "Écart brut : exposés − non exposés", unname(osyr_palette["blue"]))
kpi4 <- make_kpi_tile(paste0("+", round(kpi_context_effect, 1), " pts"), "sur l'environnement incitatif", "Écart brut : exposés − non exposés", unname(osyr_palette["purple"]))

p_dashboard <- (kpi1 | kpi2 | kpi3 | kpi4) /
  (p_score_diff + ggplot2::labs(title = "Lecture synthétique : ce que l'exposition change surtout")) +
  patchwork::plot_layout(heights = c(0.55, 1.45)) +
  patchwork::plot_annotation(
    title = "OSYR — premiers résultats structurants",
    subtitle = "Les dispositifs sont surtout associés aux connaissances et à la perception d'un environnement incitatif ; beaucoup moins aux intentions et aux attitudes générales.",
    caption = "Enquête OSYR V2 corrigée · résultats pondérés par Poids · comparaisons descriptives, non causales.",
    theme = theme_osyr(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 24, face = "bold", color = unname(osyr_palette["navy"])),
        plot.subtitle = ggplot2::element_text(size = 13, color = "#475467")
      )
  )

save_plot(p_dashboard, "00_dashboard_synthese_premium.png", width = 16, height = 10)

# Heatmap score × année × exposition.
plot_score_heatmap <- function(tab, group_var, filename, title, subtitle) {
  group_var <- rlang::ensym(group_var)
  p <- tab |>
    dplyr::filter(!is.na(exposure2), !is.na(!!group_var)) |>
    dplyr::mutate(
      facet_group = !!group_var,
      score_label = stringr::str_wrap(score_label, 38),
      label = paste0(round(100 * mean_w, 0), "%")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = exposure2, y = score_label, fill = mean_w)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.1, color = osyr_palette["navy"]) +
    ggplot2::facet_wrap(~ facet_group, nrow = 1) +
    ggplot2::scale_fill_gradient(low = "#F2F4F7", high = osyr_palette["teal"], labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = NULL) +
    theme_osyr(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 18, hjust = 1))
  save_plot(p, filename, width = 16, height = 7.5)
}

plot_score_heatmap(scores_by_exposure_year, year, "17_heatmap_scores_exposition_annee.png", "Scores par exposition et année de thèse", "Lecture horizontale : comparaison des exposés et non exposés à année de thèse donnée.")
plot_score_heatmap(scores_by_exposure_lang, language_group, "18_heatmap_scores_exposition_langue.png", "Scores par exposition et langue du questionnaire", "Comparaison des profils ayant répondu en français et en anglais.")

# =============================================================================
# 10. Modèles ajustés robustes
# =============================================================================

scores_model <- scores |>
  dplyr::filter(!is.na(exposure2)) |>
  dplyr::mutate(
    exposure2 = forcats::fct_drop(factor(exposure2)),
    year = forcats::fct_drop(factor(year)),
    discipline_broad = forcats::fct_drop(factor(discipline_broad)),
    language_group = forcats::fct_drop(factor(language_group)),
    institution_lump = forcats::fct_lump_min(factor(institution), min = 35, other_level = "Autres établissements") |>
      forcats::fct_drop()
  )

model_predictors <- c("exposure2", "year", "discipline_broad", "institution_lump", "language_group")

prepare_model_data <- function(data, outcome, predictors) {
  vars_needed <- c(outcome, predictors, ".weight")

  dat0 <- data |>
    dplyr::select(dplyr::any_of(vars_needed)) |>
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(.weight), .weight > 0)

  predictors <- predictors[predictors %in% names(dat0)]

  repeat {
    dat <- dat0 |> tidyr::drop_na(dplyr::all_of(predictors))

    dat <- dat |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(predictors),
          ~ if (is.factor(.x) || is.character(.x)) forcats::fct_drop(factor(.x)) else .x
        )
      )

    usable_predictors <- predictors[
      purrr::map_lgl(predictors, \(v) dplyr::n_distinct(dat[[v]], na.rm = TRUE) >= 2)
    ]

    if (identical(usable_predictors, predictors)) break
    predictors <- usable_predictors
    if (length(predictors) == 0) break
  }

  list(data = dat, predictors = predictors)
}

fit_score_model <- function(outcome) {
  prepared <- prepare_model_data(scores_model, outcome, model_predictors)
  dat <- prepared$data
  predictors <- prepared$predictors

  if (!"exposure2" %in% predictors || length(predictors) == 0 || nrow(dat) < 40) {
    return(tibble::tibble(
      outcome = outcome,
      model = "main_adjusted",
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      n_model = nrow(dat),
      predictors_used = paste(predictors, collapse = " + "),
      note = "Modèle non estimable : exposition ou prédicteurs insuffisamment variables."
    ))
  }

  rhs <- paste(predictors, collapse = " + ")
  f <- stats::as.formula(paste(outcome, "~", rhs))
  design_tmp <- survey::svydesign(ids = ~1, weights = ~.weight, data = dat)

  tryCatch({
    model <- survey::svyglm(f, design = design_tmp)
    broom::tidy(model, conf.int = TRUE) |>
      dplyr::mutate(
        outcome = outcome,
        model = "main_adjusted",
        n_model = nrow(dat),
        predictors_used = rhs,
        note = NA_character_
      )
  }, error = function(e) {
    tibble::tibble(
      outcome = outcome,
      model = "main_adjusted",
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      n_model = nrow(dat),
      predictors_used = rhs,
      note = paste("Erreur modèle :", conditionMessage(e))
    )
  })
}

score_models_main <- purrr::map_dfr(score_vars, fit_score_model) |>
  dplyr::left_join(score_labels, by = c("outcome" = "score")) |>
  dplyr::relocate(outcome, score_label, model, n_model, predictors_used, note)

write_model(score_models_main, "score_models_main_adjusted")

score_exposure_effects <- score_models_main |>
  dplyr::filter(is.na(note), stringr::str_detect(term, "^exposure2")) |>
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf_low_pp = 100 * conf.low,
    conf_high_pp = 100 * conf.high,
    score_label_clean = stringr::str_replace_all(score_label, "\\s+", " "),
    score_label_plot = stringr::str_wrap(score_label_clean, 42),
    association = model_interpretation(estimate_pp, conf_low_pp, conf_high_pp, p.value),
    association_group = dplyr::case_when(
      stringr::str_detect(association, "positive") ~ "Association positive claire",
      stringr::str_detect(association, "négative") ~ "Association négative claire",
      TRUE ~ "Pas d'association claire"
    ),
    label_est = dplyr::if_else(
      estimate_pp >= 0,
      paste0("+", round(estimate_pp, 1), " pts"),
      paste0(round(estimate_pp, 1), " pts")
    )
  ) |>
  dplyr::arrange(dplyr::desc(estimate_pp)) |>
  write_model("score_models_exposure_effects")

score_exposure_effects_summary <- score_exposure_effects |>
  dplyr::transmute(
    score = score_label_clean,
    effet_ajuste = estimate_pp,
    ic95_bas = conf_low_pp,
    ic95_haut = conf_high_pp,
    p_value = p.value,
    interpretation = association,
    lecture = dplyr::case_when(
      association_group == "Association positive claire" ~
        paste0("Les doctorants exposés à un dispositif organisé ont un score plus élevé de ",
               round(estimate_pp, 1), " points, à caractéristiques comparables."),
      association_group == "Association négative claire" ~
        paste0("Les doctorants exposés à un dispositif organisé ont un score plus faible de ",
               abs(round(estimate_pp, 1)), " points, à caractéristiques comparables."),
      TRUE ~
        "L'intervalle de confiance recoupe 0 : l'enquête ne permet pas de conclure à une association claire."
    )
  ) |>
  write_model("score_models_exposure_effects_summary")

# Sous-titre dynamique : il décrit les prédicteurs réellement conservés
# dans les modèles après contrôles de variance et valeurs manquantes.
predictors_used_main <- score_exposure_effects |>
  dplyr::filter(!is.na(predictors_used)) |>
  dplyr::pull(predictors_used) |>
  unique()

predictor_text <- function(x) {
  refs <- c()
  if (any(stringr::str_detect(x, "year"))) refs <- c(refs, "année de thèse")
  if (any(stringr::str_detect(x, "discipline_broad"))) refs <- c(refs, "discipline")
  if (any(stringr::str_detect(x, "institution_lump"))) refs <- c(refs, "établissement")
  if (any(stringr::str_detect(x, "language_group"))) refs <- c(refs, "langue du questionnaire")
  if (length(refs) == 0) return("les variables disponibles dans le modèle")
  paste(refs, collapse = ", ") |>
    stringr::str_replace(", ([^,]*)$", " et \\1")
}

model_subtitle <- paste0(
  "Différences ajustées en points de pourcentage, après prise en compte de ",
  predictor_text(predictors_used_main), "."
)

model_cols <- c(
  "Association positive claire" = unname(osyr_palette["teal"]),
  "Association négative claire" = unname(osyr_palette["coral"]),
  "Pas d'association claire" = "#98A2B3"
)

# Figure principale de modélisation :
# C'est la figure à utiliser dans une restitution. Elle montre le résultat
# central des modèles : ce qui reste associé à l'exposition après ajustement.
p_model_highlevel <- score_exposure_effects |>
  dplyr::mutate(score_label_plot = forcats::fct_reorder(score_label_plot, estimate_pp)) |>
  ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = score_label_plot, color = association_group)) +
  ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.55) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
    height = 0.18,
    linewidth = 1.05,
    alpha = 0.85
  ) +
  ggplot2::geom_point(size = 4.2) +
  ggplot2::geom_text(
    ggplot2::aes(label = label_est),
    hjust = dplyr::if_else(score_exposure_effects$estimate_pp >= 0, -0.12, 1.12),
    size = 3.7,
    color = unname(osyr_palette["navy"]),
    fontface = "bold"
  ) +
  ggplot2::scale_color_manual(values = model_cols, drop = TRUE) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, " pts"),
    expand = ggplot2::expansion(mult = c(0.10, 0.16))
  ) +
  ggplot2::labs(
    title = "Ce que change vraiment l'exposition à un dispositif organisé",
    subtitle = model_subtitle,
    x = "Différence ajustée en points de pourcentage",
    y = NULL,
    caption = paste(
      "Lecture : chaque point compare les doctorants exposés à un dispositif organisé aux doctorants sans dispositif organisé.",
      "Les barres indiquent les intervalles de confiance à 95 %. À droite de 0 = score plus élevé chez les exposés.",
      "Les effets en gris ne permettent pas de conclure à une association claire."
    )
  ) +
  theme_osyr(base_size = 13) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = 9.5, color = "#667085", hjust = 0)
  ) +
  ggplot2::coord_cartesian(clip = "off")

save_plot(p_model_highlevel, "19_modeles_scores_effet_exposition_highlevel.png", width = 13.5, height = 7.6)

# Version synthétique en "blocs" : utile dans un rapport pour distinguer
# les dimensions fortement, modérément ou faiblement associées aux dispositifs.
model_blocks <- score_exposure_effects |>
  dplyr::mutate(
    bloc = dplyr::case_when(
      stringr::str_detect(association, "forte") ~ "Effets robustes et importants",
      stringr::str_detect(association, "modérée|faible") ~ "Effets robustes mais plus modestes",
      TRUE ~ "Effets faibles ou incertains"
    ),
    bloc = factor(
      bloc,
      levels = c(
        "Effets robustes et importants",
        "Effets robustes mais plus modestes",
        "Effets faibles ou incertains"
      )
    ),
    score_label_plot = stringr::str_wrap(score_label_clean, 35),
    score_label_plot = forcats::fct_reorder(score_label_plot, estimate_pp)
  )

p_model_blocks <- model_blocks |>
  ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = score_label_plot, fill = association_group)) +
  ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.45) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_text(
    ggplot2::aes(label = label_est),
    hjust = dplyr::if_else(model_blocks$estimate_pp >= 0, -0.12, 1.12),
    size = 3.5,
    color = unname(osyr_palette["navy"]),
    fontface = "bold"
  ) +
  ggplot2::facet_grid(bloc ~ ., scales = "free_y", space = "free_y") +
  ggplot2::scale_fill_manual(values = model_cols, drop = TRUE) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, " pts"),
    expand = ggplot2::expansion(mult = c(0.12, 0.18))
  ) +
  ggplot2::labs(
    title = "Synthèse interprétative des effets ajustés",
    subtitle = "Les dispositifs se distinguent surtout par la connaissance, l'usage et l'environnement perçu comme incitatif.",
    x = "Différence ajustée en points de pourcentage",
    y = NULL,
    caption = "Lecture : valeurs positives = score plus élevé parmi les doctorants exposés à un dispositif organisé. Les effets sont associatifs et non causaux."
  ) +
  theme_osyr(base_size = 12.5) +
  ggplot2::theme(
    strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold"),
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::coord_cartesian(clip = "off")

save_plot(p_model_blocks, "19b_modeles_scores_synthese_blocs.png", width = 13.5, height = 8.8)

# Interactions : exposition × année, discipline, langue.
fit_interaction_model <- function(outcome, moderator) {
  controls <- c("discipline_broad", "institution_lump", "language_group", "year")
  controls <- setdiff(controls, moderator)
  predictors <- c("exposure2", moderator, paste0("exposure2:", moderator), controls)

  base_vars <- unique(c(outcome, "exposure2", moderator, controls, ".weight"))
  dat <- scores_model |>
    dplyr::select(dplyr::any_of(base_vars)) |>
    tidyr::drop_na() |>
    dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop))

  if (nrow(dat) < 80 || dplyr::n_distinct(dat$exposure2) < 2 || dplyr::n_distinct(dat[[moderator]]) < 2) {
    return(tibble::tibble(outcome = outcome, moderator = moderator, note = "Modèle interaction non estimable"))
  }

  # Retire les contrôles sans variance après drop_na.
  controls <- controls[purrr::map_lgl(controls, \(v) dplyr::n_distinct(dat[[v]], na.rm = TRUE) >= 2)]
  rhs <- paste(c("exposure2", moderator, paste0("exposure2:", moderator), controls), collapse = " + ")
  f <- stats::as.formula(paste(outcome, "~", rhs))
  des <- survey::svydesign(ids = ~1, weights = ~.weight, data = dat)

  tryCatch({
    broom::tidy(survey::svyglm(f, design = des), conf.int = TRUE) |>
      dplyr::mutate(outcome = outcome, moderator = moderator, n_model = nrow(dat), note = NA_character_)
  }, error = function(e) {
    tibble::tibble(outcome = outcome, moderator = moderator, note = conditionMessage(e))
  })
}

interaction_models <- tidyr::crossing(
  outcome = score_vars,
  moderator = c("year", "discipline_broad", "language_group")
) |>
  dplyr::mutate(res = purrr::map2(outcome, moderator, fit_interaction_model)) |>
  # Les tibble retournées par fit_interaction_model contiennent déjà outcome et moderator.
  # On retire donc les colonnes externes avant unnest pour éviter la duplication de noms.
  dplyr::select(-outcome, -moderator) |>
  tidyr::unnest(res, names_repair = "unique") |>
  dplyr::left_join(score_labels, by = c("outcome" = "score"))

write_model(interaction_models, "score_models_interactions")

# Représentation synthétique des interactions :
# l'objectif n'est pas de surinterpréter chaque coefficient, mais de repérer
# si l'association entre exposition et scores semble varier fortement selon
# l'année, la discipline ou la langue du questionnaire.
interaction_terms <- interaction_models |>
  dplyr::filter(is.na(note), stringr::str_detect(term, "^exposure2Dispositif organisé:")) |>
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf_low_pp = 100 * conf.low,
    conf_high_pp = 100 * conf.high,
    interaction_label = term |>
      stringr::str_replace("^exposure2Dispositif organisé:", "") |>
      stringr::str_replace("^year", "Année : ") |>
      stringr::str_replace("^discipline_broad", "Discipline : ") |>
      stringr::str_replace("^language_group", "Langue : ") |>
      stringr::str_wrap(width = 44),
    score_label_plot = stringr::str_wrap(score_label, 38),
    evidence = dplyr::case_when(
      p.value < 0.05 & conf_low_pp > 0 ~ "Interaction positive claire",
      p.value < 0.05 & conf_high_pp < 0 ~ "Interaction négative claire",
      TRUE ~ "Interaction incertaine"
    )
  ) |>
  write_model("score_models_interaction_terms")

interaction_terms_plot <- interaction_terms |>
  dplyr::arrange(p.value, dplyr::desc(abs(estimate_pp))) |>
  dplyr::slice_head(n = 18)

if (nrow(interaction_terms_plot) > 0) {
  interaction_cols <- c(
    "Interaction positive claire" = unname(osyr_palette["teal"]),
    "Interaction négative claire" = unname(osyr_palette["coral"]),
    "Interaction incertaine" = "#98A2B3"
  )

  p_interactions <- interaction_terms_plot |>
    dplyr::mutate(
      label_plot = paste0(score_label_plot, "\n", interaction_label),
      label_plot = forcats::fct_reorder(label_plot, estimate_pp),
      label_est = dplyr::if_else(
        estimate_pp >= 0,
        paste0("+", round(estimate_pp, 1), " pts"),
        paste0(round(estimate_pp, 1), " pts")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate_pp, y = label_plot, color = evidence)) +
    ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.45) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf_low_pp, xmax = conf_high_pp),
      height = 0.16,
      linewidth = 0.9,
      alpha = 0.85
    ) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_est),
      hjust = dplyr::if_else(interaction_terms_plot$estimate_pp >= 0, -0.10, 1.10),
      size = 3.1,
      color = unname(osyr_palette["navy"])
    ) +
    ggplot2::scale_color_manual(values = interaction_cols, drop = TRUE) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, " pts"),
      expand = ggplot2::expansion(mult = c(0.12, 0.18))
    ) +
    ggplot2::labs(
      title = "Où l'association avec les dispositifs varie-t-elle ?",
      subtitle = "Principaux termes d'interaction exposition × année, discipline ou langue du questionnaire.",
      x = "Différence additionnelle associée au groupe",
      y = NULL,
      caption = paste(
        "Lecture : un terme d'interaction indique si l'écart entre exposés et non exposés est plus fort ou plus faible",
        "dans un groupe donné que dans le groupe de référence. Ces résultats sont exploratoires."
      )
    ) +
    theme_osyr(base_size = 11.5) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")

  save_plot(p_interactions, "23b_modeles_interactions_synthese.png", width = 14.5, height = 9.5)
}


# Prédictions ajustées pour un score central : connaissance bien connue.
make_prediction_grid <- function(outcome, moderator) {
  controls <- c("discipline_broad", "institution_lump", "language_group", "year")
  controls <- setdiff(controls, moderator)
  base_vars <- unique(c(outcome, "exposure2", moderator, controls, ".weight"))

  dat <- scores_model |>
    dplyr::select(dplyr::any_of(base_vars)) |>
    tidyr::drop_na() |>
    dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop))

  if (nrow(dat) < 80) return(tibble())

  controls <- controls[purrr::map_lgl(controls, \(v) dplyr::n_distinct(dat[[v]], na.rm = TRUE) >= 2)]
  rhs <- paste(c("exposure2", moderator, paste0("exposure2:", moderator), controls), collapse = " + ")
  f <- stats::as.formula(paste(outcome, "~", rhs))
  des <- survey::svydesign(ids = ~1, weights = ~.weight, data = dat)

  model <- tryCatch(survey::svyglm(f, design = des), error = function(e) NULL)
  if (is.null(model)) return(tibble())

  grid <- tidyr::expand_grid(
    exposure2 = levels(dat$exposure2),
    moderator_value = levels(dat[[moderator]])
  )
  names(grid)[names(grid) == "moderator_value"] <- moderator

  for (ctrl in controls) {
    if (!ctrl %in% names(grid)) {
      ref <- names(sort(table(dat[[ctrl]]), decreasing = TRUE))[1]
      grid[[ctrl]] <- factor(ref, levels = levels(dat[[ctrl]]))
    }
  }
  grid$exposure2 <- factor(grid$exposure2, levels = levels(dat$exposure2))
  grid[[moderator]] <- factor(grid[[moderator]], levels = levels(dat[[moderator]]))

  # Selon les versions du package survey, predict.svyglm(..., se.fit = TRUE)
  # peut retourner soit une liste avec fit/se.fit, soit un vecteur de classe
  # svystat avec une variance attachée. Cette extraction robuste gère les deux cas.
  pred <- predict(model, newdata = grid, se.fit = TRUE)

  if (is.list(pred) && !is.null(pred$fit)) {
    fit <- as.numeric(pred$fit)
    se <- as.numeric(pred$se.fit)
  } else {
    fit <- as.numeric(pred)
    se <- tryCatch(
      as.numeric(survey::SE(pred)),
      error = function(e) {
        var_pred <- attr(pred, "var")
        if (!is.null(var_pred)) {
          sqrt(diag(as.matrix(var_pred)))
        } else {
          rep(NA_real_, length(fit))
        }
      }
    )
  }

  if (length(se) != length(fit)) {
    se <- rep(NA_real_, length(fit))
  }

  grid |>
    dplyr::mutate(
      outcome = outcome,
      moderator = moderator,
      fit = fit,
      se = se,
      conf.low = fit - 1.96 * se,
      conf.high = fit + 1.96 * se
    )
}

interaction_predictions <- purrr::map_dfr(
  c("year", "discipline_broad", "language_group"),
  \(m) make_prediction_grid("score_q5_known_well", m)
)

write_model(interaction_predictions, "predictions_interactions_score_q5_known_well")

plot_prediction <- function(preds, moderator, filename, title, subtitle) {
  tab <- preds |> dplyr::filter(moderator == !!moderator)
  if (nrow(tab) == 0) return(invisible(NULL))
  p <- tab |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[moderator]], y = fit, color = exposure2, group = exposure2)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.10, alpha = 0.65) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
    ggplot2::scale_color_manual(values = exposure_colors[c("Aucun dispositif", "Dispositif organisé")]) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Valeur prédite ajustée") +
    theme_osyr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18, hjust = 1))
  save_plot(p, filename, width = 13, height = 6.7)
}

plot_prediction(interaction_predictions, "year", "20_prediction_connaissance_exposition_annee.png", "Connaissance ajustée selon exposition et année", "Score Q5 prédit par le modèle, avec contrôles fixés à leur modalité la plus fréquente.")
plot_prediction(interaction_predictions, "discipline_broad", "21_prediction_connaissance_exposition_discipline.png", "Connaissance ajustée selon exposition et discipline", "Score Q5 prédit par le modèle, avec contrôles fixés à leur modalité la plus fréquente.")
plot_prediction(interaction_predictions, "language_group", "22_prediction_connaissance_exposition_langue.png", "Connaissance ajustée selon exposition et langue", "Score Q5 prédit par le modèle, avec contrôles fixés à leur modalité la plus fréquente.")

# =============================================================================
# 11. Sensibilité : pondération de propension simplifiée
# =============================================================================

propensity_data <- scores_model |>
  dplyr::filter(!is.na(exposure2)) |>
  dplyr::mutate(treat = as.integer(exposure2 == "Dispositif organisé")) |>
  dplyr::select(treat, .weight, year, discipline_broad, institution_lump, language_group, dplyr::all_of(score_vars)) |>
  tidyr::drop_na(treat, .weight, year, discipline_broad, institution_lump, language_group)

propensity_model <- tryCatch(
  stats::glm(treat ~ year + discipline_broad + institution_lump + language_group, data = propensity_data, family = binomial()),
  error = function(e) NULL
)

if (!is.null(propensity_model)) {
  propensity_data <- propensity_data |>
    dplyr::mutate(
      ps = pmin(pmax(stats::predict(propensity_model, type = "response"), 0.02), 0.98),
      ipw = dplyr::if_else(treat == 1, 1 / ps, 1 / (1 - ps)),
      combined_weight = .weight * ipw,
      exposure2 = factor(dplyr::if_else(treat == 1, "Dispositif organisé", "Aucun dispositif"), levels = c("Aucun dispositif", "Dispositif organisé"))
    )

  ps_summary <- propensity_data |>
    dplyr::group_by(exposure2) |>
    dplyr::summarise(
      n = dplyr::n(),
      ps_mean = mean(ps),
      ps_min = min(ps),
      ps_max = max(ps),
      ipw_mean = mean(ipw),
      ipw_p99 = stats::quantile(ipw, .99),
      .groups = "drop"
    )

  write_table(ps_summary, "propensity_score_diagnostics", subdir = "diagnostics")

  ipw_score_diff <- purrr::map_dfr(score_vars, function(s) {
    dat <- propensity_data |> dplyr::filter(!is.na(.data[[s]]))
    dat |>
      dplyr::group_by(exposure2) |>
      dplyr::summarise(mean_ipw = w_mean(.data[[s]], combined_weight), n = dplyr::n(), .groups = "drop") |>
      tidyr::pivot_wider(names_from = exposure2, values_from = c(mean_ipw, n), names_sep = "__") |>
      janitor::clean_names() |>
      dplyr::mutate(score = s, diff_pp_ipw = 100 * (mean_ipw_dispositif_organise - mean_ipw_aucun_dispositif))
  }) |>
    dplyr::left_join(score_labels, by = "score")

  write_table(ipw_score_diff, "score_diff_exposure_ipw_sensitivity")
}

# =============================================================================
# 12. Analyse des réponses multiples Q8, Q9, Q14, Q6
# =============================================================================

multi_context <- c("respondent_id", ".weight", "exposure3", "exposure2", "year", "discipline_broad", "institution", "language_group")

q8_long <- extract_multi(df, "q8", multi_context) |>
  dplyr::left_join(answer_labels |> dplyr::filter(base == "q8") |> dplyr::rename(choice_label = value), by = c("code" = "code")) |>
  write_table("q8_long", subdir = "data_clean")

q9_long <- extract_multi(df, "q9", multi_context) |>
  dplyr::left_join(answer_labels |> dplyr::filter(base == "q9") |> dplyr::rename(choice_label = value), by = c("code" = "code")) |>
  write_table("q9_long", subdir = "data_clean")

summarise_multi <- function(long_data, name) {
  if (nrow(long_data) == 0) return(tibble())
  long_data |>
    dplyr::filter(!is.na(choice_label)) |>
    dplyr::group_by(code, choice_label) |>
    dplyr::summarise(
      n_mentions = dplyr::n(),
      weighted_mentions = sum(.weight, na.rm = TRUE),
      n_respondents = dplyr::n_distinct(respondent_id),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(weighted_mentions)) |>
    write_table(name)
}

q8_multi_summary <- summarise_multi(q8_long, "q8_multi_summary")
q9_multi_summary <- summarise_multi(q9_long, "q9_multi_summary")

# Analyse des raisons Q14, si les variables sont présentes.
q14_bases <- names(df) |>
  stringr::str_subset("^q14_\\d+_m\\d+$") |>
  stringr::str_replace("_m\\d+$", "") |>
  unique()

q14_long_all <- purrr::map_dfr(q14_bases, \(b) extract_multi(df, b, multi_context))

if (nrow(q14_long_all) > 0) {
  q14_long_all <- q14_long_all |>
    dplyr::left_join(answer_labels |> dplyr::rename(choice_label = value), by = c("base" = "base", "code" = "code"))

  readr::write_csv(q14_long_all, file.path(out_dir, "data_clean", "q14_reasons_long.csv"))

  q14_reasons_summary <- q14_long_all |>
    dplyr::filter(!is.na(choice_label)) |>
    dplyr::group_by(base, choice_label) |>
    dplyr::summarise(
      n_mentions = dplyr::n(),
      weighted_mentions = sum(.weight, na.rm = TRUE),
      n_respondents = dplyr::n_distinct(respondent_id),
      .groups = "drop"
    ) |>
    dplyr::arrange(base, dplyr::desc(weighted_mentions))

  write_table(q14_reasons_summary, "q14_reasons_summary")
}

# =============================================================================
# 13. Analyse textuelle des trois mots Q3
# =============================================================================

q3_vars <- names(df) |> stringr::str_subset("^q3_a\\d+$")

q3_long <- df |>
  dplyr::select(dplyr::all_of(c(context_vars, q3_vars))) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(q3_vars),
    names_to = "slot",
    values_to = "mention"
  ) |>
  dplyr::mutate(
    slot_number = readr::parse_number(slot),
    mention = fix_text(mention),
    mention_norm = clean_mention(mention),
    slot_weight = dplyr::case_when(
      slot_number == 1 ~ 1.00,
      slot_number == 2 ~ 0.80,
      slot_number == 3 ~ 0.60,
      TRUE ~ 1.00
    ),
    text_weight = .weight * slot_weight
  ) |>
  dplyr::filter(!is.na(mention_norm), mention_norm != "")

readr::write_csv(q3_long, file.path(out_dir, "text_analysis", "q3_three_words_long.csv"))

q3_raw_mentions <- q3_long |>
  dplyr::group_by(mention_norm) |>
  dplyr::summarise(
    examples = paste(utils::head(unique(mention), 3), collapse = " | "),
    n_mentions = dplyr::n(),
    weighted_mentions = sum(text_weight, na.rm = TRUE),
    n_respondents = dplyr::n_distinct(respondent_id),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(weighted_mentions))

write_table(q3_raw_mentions, "q3_raw_mentions", subdir = "text_analysis")

# Dictionnaire conceptuel bilingue, volontairement extensible.
concept_dictionary <- tibble::tribble(
  ~concept, ~pattern,
  "Accès / accessibilité", "\\b(acces|accessible|accessibilite|ouvert|open|availability|available|disponible|disponibilite)\\b",
  "Partage / échange", "\\b(partage|partager|shared?|sharing|echange|echanges|mutualisation|diffusion commune|share)\\b",
  "Publications / libre accès", "\\b(publication|publications|article|articles|revue|journal|open access|libre acces|oa|preprint|preprints|archive ouverte|hal)\\b",
  "Gratuité / coûts", "\\b(gratuit|gratuite|gratuitement|free|cost|cout|couts|apc|payant|payer|frais)\\b",
  "Données ouvertes / FAIR", "\\b(donnee|donnees|data|dataset|datasets|fair|entrepot|repository|repositories|zenodo|recherche data|dmp|pgd)\\b",
  "Transparence / traçabilité", "\\b(transparence|transparent|trace|tracabilite|tracable|clarte|visibility|visible|visibilite)\\b",
  "Collaboration / coopération", "\\b(collaboration|collaboratif|cooperation|cooperatif|collectif|collective|network|reseau|communaute|community)\\b",
  "Reproductibilité / réutilisation", "\\b(reproductibilite|reproductible|replicable|replication|reutilisation|reuse|reusable|reutilisable|verification)\\b",
  "Intégrité / éthique", "\\b(integrite|ethique|ethic|ethical|honnetete|fiabilite|fiable|rigueur|qualite|trust|confiance)\\b",
  "Code / logiciels ouverts", "\\b(code|codes|logiciel|logiciels|software|script|scripts|github|gitlab|open source|opensource|libre)\\b",
  "Science citoyenne / société", "\\b(citoyen|citoyenne|citizens?|societe|societal|public|humanite|humanity|democratisation|democratique|inclusive|inclusion)\\b",
  "Communication / vulgarisation", "\\b(communication|communiquer|vulgarisation|dissemination|diffuser|diffusion|mediation|visibilite|outreach)\\b",
  "Liberté / autonomie", "\\b(liberte|libre|freedom|autonomie|independance|independent)\\b",
  "Connaissance comme bien commun", "\\b(connaissance|knowledge|savoir|science pour tous|bien commun|commons|commun)\\b",
  "Innovation / progrès", "\\b(innovation|progres|amelioration|improvement|advance|advancement|developpement|development)\\b",
  "Ouverture / esprit ouvert", "\\b(ouverture|openess|openness|ouvert|ouverte|ouvrir|open)\\b"
) |>
  dplyr::mutate(pattern = stringr::regex(pattern, ignore_case = TRUE))

concept_hits <- tidyr::crossing(
  q3_long |>
    dplyr::select(
      respondent_id, .weight, text_weight, exposure3, exposure2, training_intensity,
      year, year_code, discipline_broad, institution, language_group,
      slot_number, mention, mention_norm
    ),
  concept_dictionary
) |>
  dplyr::filter(stringr::str_detect(mention_norm, pattern)) |>
  dplyr::distinct(respondent_id, slot_number, mention_norm, concept, .keep_all = TRUE)

concept_presence <- concept_hits |>
  dplyr::distinct(
    respondent_id, concept, .weight, exposure3, exposure2, training_intensity,
    year, year_code, discipline_broad, institution, language_group
  )

concept_overall <- concept_presence |>
  dplyr::group_by(concept) |>
  dplyr::summarise(
    n_respondents = dplyr::n_distinct(respondent_id),
    weighted_respondents = sum(.weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pct_w = weighted_respondents / sum(df$.weight, na.rm = TRUE),
    pct_w_label = safe_pct(pct_w)
  ) |>
  dplyr::arrange(dplyr::desc(pct_w))

write_table(concept_overall, "q3_concepts_overall", subdir = "text_analysis")

concept_by_group <- function(group_var, filename) {
  respondent_universe <- df |>
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(.weight)) |>
    dplyr::group_by(group = .data[[group_var]]) |>
    dplyr::summarise(total_weight = sum(.weight, na.rm = TRUE), .groups = "drop")

  out <- concept_presence |>
    dplyr::filter(!is.na(.data[[group_var]])) |>
    dplyr::group_by(group = .data[[group_var]], concept) |>
    dplyr::summarise(
      n_respondents = dplyr::n_distinct(respondent_id),
      weighted_respondents = sum(.weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(respondent_universe, by = "group") |>
    dplyr::mutate(pct_w = weighted_respondents / total_weight) |>
    dplyr::arrange(group, dplyr::desc(pct_w))

  write_table(out, filename, subdir = "text_analysis")
  out
}

concept_by_exposure <- concept_by_group("exposure2", "q3_concepts_by_exposure2")
concept_by_year <- concept_by_group("year", "q3_concepts_by_year")
concept_by_disc <- concept_by_group("discipline_broad", "q3_concepts_by_discipline")
concept_by_lang <- concept_by_group("language_group", "q3_concepts_by_language")

plot_ranked_bar(concept_overall, "23_q3_concepts_overall.png", "À quoi fait penser la science ouverte ?", "Concepts repérés dans les trois mots cités par les doctorants. Un répondant peut contribuer à plusieurs concepts.", fill = osyr_palette["navy"], width = 12, height = 8)

# Concepts distinctifs exposés vs non exposés : différence en points.
concept_diff_exposure <- concept_by_exposure |>
  dplyr::filter(!is.na(group)) |>
  dplyr::select(group, concept, pct_w, n_respondents) |>
  tidyr::pivot_wider(names_from = group, values_from = c(pct_w, n_respondents), names_sep = "__") |>
  janitor::clean_names() |>
  dplyr::mutate(diff_pp = 100 * (pct_w_dispositif_organise - pct_w_aucun_dispositif)) |>
  dplyr::arrange(dplyr::desc(abs(diff_pp)))

write_table(concept_diff_exposure, "q3_concepts_diff_exposure2", subdir = "text_analysis")
plot_diff(concept_diff_exposure |> dplyr::rename(item_label = concept), "24_q3_concepts_diff_exposition.png", "Représentations plus fréquentes chez les exposés ou non exposés", "Différence pondérée de présence des concepts dans les trois mots.", top_n = 15)

# Analyse par langue du questionnaire.
concept_diff_language <- concept_by_lang |>
  dplyr::select(group, concept, pct_w, n_respondents) |>
  tidyr::pivot_wider(names_from = group, values_from = c(pct_w, n_respondents), names_sep = "__") |>
  janitor::clean_names() |>
  dplyr::mutate(diff_pp = 100 * (pct_w_questionnaire_en_anglais - pct_w_questionnaire_en_francais)) |>
  dplyr::arrange(dplyr::desc(abs(diff_pp)))

write_table(concept_diff_language, "q3_concepts_diff_language", subdir = "text_analysis")
plot_diff(concept_diff_language |> dplyr::rename(item_label = concept), "25_q3_concepts_diff_langue.png", "Science ouverte : ce que la langue du questionnaire fait varier", "Différence pondérée de présence des concepts : anglais moins français.", top_n = 15)

# Cooccurrences entre concepts, au niveau répondant.
concept_pairs <- concept_presence |>
  dplyr::select(respondent_id, concept, .weight) |>
  dplyr::distinct() |>
  dplyr::inner_join(
    concept_presence |> dplyr::select(respondent_id, concept2 = concept) |> dplyr::distinct(),
    by = "respondent_id"
  ) |>
  dplyr::filter(concept < concept2) |>
  dplyr::group_by(concept, concept2) |>
  dplyr::summarise(
    n_respondents = dplyr::n_distinct(respondent_id),
    weighted_respondents = sum(.weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(pct_w = weighted_respondents / sum(df$.weight, na.rm = TRUE)) |>
  dplyr::arrange(dplyr::desc(weighted_respondents))

write_table(concept_pairs, "q3_concept_cooccurrences", subdir = "text_analysis")

# Réseau de cooccurrence.
network_edges <- concept_pairs |>
  dplyr::filter(n_respondents >= 10) |>
  dplyr::slice_max(weighted_respondents, n = 45) |>
  dplyr::rename(from = concept, to = concept2, weight = weighted_respondents)

network_nodes <- concept_overall |>
  dplyr::filter(concept %in% unique(c(network_edges$from, network_edges$to))) |>
  dplyr::transmute(name = concept, pct_w = pct_w, n = n_respondents)

if (nrow(network_edges) > 0 && nrow(network_nodes) > 1) {
  g <- igraph::graph_from_data_frame(network_edges, vertices = network_nodes, directed = FALSE)
  igraph::V(g)$community <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)$membership

  p_network <- ggraph::ggraph(g, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(width = weight), alpha = 0.28, color = "#667085") +
    ggraph::geom_node_point(ggplot2::aes(size = pct_w, fill = factor(community)), shape = 21, color = "white", stroke = 0.8) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3.4, color = osyr_palette["navy"], family = "sans") +
    ggplot2::scale_edge_width(range = c(0.3, 2.2), guide = "none") +
    ggplot2::scale_size(range = c(4, 14), guide = "none") +
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    ggplot2::labs(
      title = "Réseau de cooccurrence des représentations de la science ouverte",
      subtitle = "Deux concepts sont reliés lorsqu'ils apparaissent dans les trois mots d'un même répondant.",
      caption = "Seules les cooccurrences les plus fréquentes sont affichées."
    ) +
    theme_osyr() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())

  save_plot(p_network, "26_q3_reseau_cooccurrence_concepts.png", width = 13, height = 9)
}

# Tokens libres : complément non dictionnaire.
stop_fr <- stopwords::stopwords("fr")
stop_en <- stopwords::stopwords("en")
custom_stop <- c(stop_fr, stop_en, "science", "ouverte", "open", "research", "recherche", "scientifique", "scientific") |>
  stringi::stri_trans_general("Latin-ASCII") |>
  unique()

token_freq <- q3_long |>
  dplyr::select(respondent_id, text_weight, exposure2, year, discipline_broad, language_group, mention_norm) |>
  tidytext::unnest_tokens(token, mention_norm) |>
  dplyr::filter(!token %in% custom_stop, stringr::str_length(token) > 2, !stringr::str_detect(token, "^\\d+$")) |>
  dplyr::group_by(token) |>
  dplyr::summarise(
    n = dplyr::n(),
    weighted_n = sum(text_weight, na.rm = TRUE),
    n_respondents = dplyr::n_distinct(respondent_id),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(weighted_n))

write_table(token_freq, "q3_token_frequency", subdir = "text_analysis")

# Diversité lexicale par groupe.
lexical_diversity <- q3_long |>
  dplyr::select(respondent_id, .weight, exposure2, year, discipline_broad, language_group, mention_norm) |>
  tidytext::unnest_tokens(token, mention_norm) |>
  dplyr::filter(!token %in% custom_stop, stringr::str_length(token) > 2, !stringr::str_detect(token, "^\\d+$")) |>
  dplyr::group_by(exposure2) |>
  dplyr::summarise(
    n_tokens = dplyr::n(),
    n_types = dplyr::n_distinct(token),
    type_token_ratio = n_types / n_tokens,
    .groups = "drop"
  )

write_table(lexical_diversity, "q3_lexical_diversity_by_exposure", subdir = "text_analysis")

# =============================================================================
# 14. Clustering exploratoire des profils
# =============================================================================

cluster_data <- scores |>
  dplyr::select(respondent_id, .weight, exposure3, exposure2, year, discipline_broad, language_group, dplyr::all_of(score_vars)) |>
  tidyr::drop_na(dplyr::all_of(score_vars))

if (nrow(cluster_data) >= 100) {
  set.seed(20260619)
  mat <- cluster_data |> dplyr::select(dplyr::all_of(score_vars)) |> scale()
  km <- stats::kmeans(mat, centers = 4, nstart = 50)

  clusters <- cluster_data |>
    dplyr::mutate(cluster = paste0("Profil ", km$cluster))

  cluster_profiles <- clusters |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(.weight, na.rm = TRUE),
      dplyr::across(dplyr::all_of(score_vars), ~ w_mean(.x, .weight), .names = "mean_{.col}"),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(dplyr::starts_with("mean_"), names_to = "score", values_to = "mean_w") |>
    dplyr::mutate(score = stringr::str_remove(score, "^mean_")) |>
    dplyr::left_join(score_labels, by = "score")

  cluster_composition <- clusters |>
    dplyr::group_by(cluster, exposure3, year, discipline_broad, language_group) |>
    dplyr::summarise(n = dplyr::n(), weighted_n = sum(.weight, na.rm = TRUE), .groups = "drop")

  write_table(cluster_profiles, "cluster_profiles_scores")
  write_table(cluster_composition, "cluster_composition")

  p_cluster <- cluster_profiles |>
    dplyr::mutate(score_label = stringr::str_wrap(score_label, 34)) |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, y = score_label, fill = mean_w)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * mean_w, 0), "%")), color = osyr_palette["navy"], size = 3.2) +
    ggplot2::scale_fill_gradient(low = "#F2F4F7", high = osyr_palette["purple"], labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Profils exploratoires de doctorants",
      subtitle = "Clustering k-means sur les scores standardisés. À interpréter comme aide à la typologie, non comme classification définitive.",
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_osyr() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  save_plot(p_cluster, "27_profils_exploratoires_clusters.png", width = 11, height = 7.2)
}


# =============================================================================
# 15. Légendes interprétatives des figures
# =============================================================================

figure_captions <- tibble::tribble(
  ~figure, ~titre_court, ~legende_interpretative,
  "00_distribution_annee_these_controle.png", "Année de thèse", "Part pondérée des répondants selon l'année de thèse. La répartition est globalement équilibrée, ce qui rend les comparaisons par niveau d'avancement plus solides.",
  "01_langue_questionnaire.png", "Langue du questionnaire", "Part pondérée des répondants selon la langue du questionnaire. La modalité anglaise est utilisée comme indicateur prudent d'un profil plus international, sans être assimilée à une nationalité.",
  "02_exposition_dispositifs.png", "Exposition aux dispositifs", "Répartition pondérée selon l'exposition aux dispositifs de science ouverte. La catégorie dispositif organisé désigne une exposition structurée ; autoformation/autre seulement désigne une exposition non institutionnalisée.",
  "03_exposition_par_annee_CORRIGE.png", "Exposition par année", "Part pondérée de chaque type d'exposition au sein de chaque année de thèse. Chaque barre représente 100 % des répondants d'une même année.",
  "04_exposition_par_discipline.png", "Exposition par discipline", "Part pondérée de chaque type d'exposition au sein de chaque grand domaine disciplinaire. Les différences doivent être lues comme des contrastes de structure et non comme un classement.",
  "05_exposition_par_langue.png", "Exposition par langue", "Comparaison pondérée de l'exposition entre répondants au questionnaire français et anglais. Le questionnaire anglais peut signaler un profil plus international, mais ne mesure pas directement la nationalité.",
  "06_q5_notions_bien_connues.png", "Notions bien connues", "Part pondérée de doctorants déclarant bien connaître chaque notion ou outil. Il s'agit d'une familiarité déclarée, et non d'une mesure objective de maîtrise.",
  "06b_q5_ecart_connaissance_usage.png", "Connaissance vers usage", "Chaque segment relie la part déclarant bien connaître une notion à la part déclarant l'avoir déjà utilisée. Plus le segment est long, plus la notion reste connue sans être encore largement pratiquée.",
  "07_q5_notions_deja_utilisees.png", "Notions déjà utilisées", "Part pondérée de doctorants déclarant avoir déjà utilisé chaque notion ou outil. La comparaison avec la connaissance permet de repérer les objets connus mais peu appropriés.",
  "08_q13_intentions_oui.png", "Intentions", "Part pondérée de réponses oui à différentes intentions de pratiques ouvertes ou de valorisation. Ces résultats mesurent des intentions déclarées, non des comportements observés.",
  "09_q15_accord_affirmations.png", "Représentations générales", "Part pondérée de répondants plutôt d'accord ou tout à fait d'accord avec chaque affirmation. Ces items renseignent les représentations générales de la science ouverte.",
  "10_diff_q5_connaissance_exposition.png", "Connaissance et exposition", "Différence pondérée entre exposés et non exposés. Une valeur positive indique une connaissance plus élevée parmi les exposés ; ces écarts sont descriptifs.",
  "11_diff_q5_usage_exposition.png", "Usage et exposition", "Différence pondérée d'usage déclaré entre exposés et non exposés. Une valeur positive indique un usage plus fréquent parmi les exposés ; ces écarts sont descriptifs.",
  "12_diff_q13_intentions_exposition.png", "Intentions et exposition", "Différence pondérée de réponses oui entre exposés et non exposés. Les faibles écarts suggèrent que l'exposition modifie peu les intentions déclarées.",
  "13_diff_q15_representations_exposition.png", "Représentations et exposition", "Différence pondérée d'accord entre exposés et non exposés. Les faibles écarts indiquent que les représentations générales sont largement partagées.",
  "14_heatmap_q5_connaissance_par_annee.png", "Connaissance par année", "Heatmap de la familiarité déclarée par année de thèse. Elle permet de repérer les notions qui progressent avec l'avancement dans la thèse.",
  "15_heatmap_q5_connaissance_par_discipline.png", "Connaissance par discipline", "Heatmap de la familiarité déclarée par grand domaine. Elle montre que les objets pertinents de science ouverte varient fortement selon les disciplines.",
  "16_scores_difference_exposition.png", "Scores et exposition", "Différence pondérée de scores moyens entre exposés et non exposés. Les écarts les plus forts concernent l'environnement perçu comme incitatif et la connaissance des notions.",
  "17_heatmap_scores_exposition_annee.png", "Scores par exposition et année", "Heatmap croisant scores, exposition et année de thèse. Lecture horizontale : comparer exposés et non exposés à année donnée.",
  "18_heatmap_scores_exposition_langue.png", "Scores par exposition et langue", "Heatmap croisant scores, exposition et langue du questionnaire. Elle aide à nuancer le profil des répondants anglophones.",
  "19_modeles_scores_effet_exposition_highlevel.png", "Modélisation principale", "Chaque point représente la différence ajustée moyenne entre doctorants exposés à un dispositif organisé et doctorants sans dispositif organisé. Les barres indiquent les intervalles de confiance à 95 %.",
  "19b_modeles_scores_synthese_blocs.png", "Synthèse des effets ajustés", "Classe les effets ajustés selon leur force et leur clarté statistique. Les dispositifs se distinguent surtout par connaissance, usage et environnement incitatif.",
  "20_prediction_connaissance_exposition_annee.png", "Prédictions par année", "Valeurs ajustées prédites du score de connaissance selon exposition et année. Les contrôles sont fixés à leur modalité la plus fréquente.",
  "21_prediction_connaissance_exposition_discipline.png", "Prédictions par discipline", "Valeurs ajustées prédites du score de connaissance selon exposition et discipline. À interpréter comme visualisation du modèle, non comme causalité.",
  "22_prediction_connaissance_exposition_langue.png", "Prédictions par langue", "Valeurs ajustées prédites du score de connaissance selon exposition et langue du questionnaire. À interpréter prudemment.",
  "23b_modeles_interactions_synthese.png", "Interactions exploratoires", "Principaux termes d'interaction exposition × année, discipline ou langue. Ils indiquent si l'écart exposés/non exposés varie selon certains groupes.",
  "24_q3_concepts_diff_exposition.png", "Trois mots et exposition", "Différence pondérée de présence des concepts dans les trois mots associés à la science ouverte. Les valeurs positives indiquent des concepts plus fréquents chez les exposés.",
  "25_q3_concepts_diff_langue.png", "Trois mots et langue", "Différence pondérée de présence des concepts selon la langue du questionnaire. La figure montre des cadrages sémantiques différents de la science ouverte.",
  "27_profils_exploratoires_clusters.png", "Profils exploratoires", "Profils obtenus par k-means sur scores standardisés. Les cases affichent les niveaux moyens observés ; cette figure est une aide à la typologie, pas une classification définitive."
)

write_table(figure_captions, "figure_captions")

# =============================================================================
# 16. Classeur Excel de synthèse
# =============================================================================

xlsx_path <- file.path(out_dir, "OSYR_V2_final_synthese_analyses.xlsx")
wb <- openxlsx::createWorkbook()

add_sheet <- function(wb, sheet_name, data) {
  sheet_name <- substr(sheet_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, data)
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::addFilter(wb, sheet_name, row = 1, cols = seq_len(ncol(data)))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
}

add_sheet(wb, "quality", quality_overview)
add_sheet(wb, "year_check", year_check)
add_sheet(wb, "sample_year", sample_year)
add_sheet(wb, "sample_language", sample_language)
add_sheet(wb, "sample_exposure", sample_exposure)
add_sheet(wb, "exposure_by_year", cross_exposure_year)
add_sheet(wb, "exposure_by_discipline", cross_exposure_discipline)
add_sheet(wb, "exposure_by_language", cross_exposure_language)
add_sheet(wb, "q5_known_overall", q5_known_overall)
add_sheet(wb, "q5_used_overall", q5_used_overall)
add_sheet(wb, "q13_intentions", q13_yes_overall)
add_sheet(wb, "q15_agreement", q15_agree_overall)
add_sheet(wb, "q5_diff_exposure", q5_known_cmp$diff)
add_sheet(wb, "score_diff_exposure", score_diff_exposure)
add_sheet(wb, "score_models", score_models_main)
add_sheet(wb, "model_exposure_effects", score_exposure_effects)
add_sheet(wb, "model_effects_summary", score_exposure_effects_summary)
add_sheet(wb, "q3_concepts", concept_overall)
add_sheet(wb, "q3_concepts_exposure", concept_by_exposure)
add_sheet(wb, "q3_concepts_language", concept_by_lang)
add_sheet(wb, "q3_cooccurrences", concept_pairs)
add_sheet(wb, "q3_raw_mentions", q3_raw_mentions |> dplyr::slice_head(n = 500))
add_sheet(wb, "figure_captions", figure_captions)
if (exists("interaction_terms")) add_sheet(wb, "interaction_terms", interaction_terms)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# =============================================================================
# 17. Résumé console
# =============================================================================

message("\nAnalyse OSYR V2 finale terminée.")
message("Sorties : ", normalizePath(out_dir, mustWork = FALSE))
message("Classeur Excel : ", normalizePath(xlsx_path, mustWork = FALSE))
message("Graphique de contrôle de l'année : ", normalizePath(file.path(out_dir, "figures", "00_distribution_annee_these_controle.png"), mustWork = FALSE))
message("Graphique corrigé exposition × année : ", normalizePath(file.path(out_dir, "figures", "03_exposition_par_annee_CORRIGE.png"), mustWork = FALSE))
message("Figure principale de modélisation : ", normalizePath(file.path(out_dir, "figures", "19_modeles_scores_effet_exposition_highlevel.png"), mustWork = FALSE))
message("Légendes interprétatives : ", normalizePath(file.path(out_dir, "tables", "figure_captions.csv"), mustWork = FALSE))
