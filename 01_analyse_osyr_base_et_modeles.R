# =============================================================================
# SCRIPT 01 — ANALYSE PRINCIPALE OSYR
# Version v14 — 07/07/2026
#
# OBJECTIF
#   Produire une base analytique propre ET intégrer dès le socle les sorties
#   demandées après la réunion WP2 :
#   - conserver toutes les disciplines détaillées, pas seulement les 4 agrégées ;
#   - récupérer robustement les libellés de disciplines depuis la datamap ;
#   - documenter explicitement les deux niveaux de discipline ;
#   - produire des figures pour toutes les disciplines détaillées ;
#   - produire les formats longs nécessaires aux tests du script 03 ;
#   - produire les premiers scores synthétiques ;
#   - produire une distribution détaillée des dispositifs Q8, sans agrégation ;
#   - corriger les visualisations Q8 par langue et scores par discipline ;
#   - ajouter des visualisations complémentaires robustes et éviter les figures vides.
#
# ENTRÉES
#   data/BJ30232 - BDD V2.csv
#   data/BJ30232 - DATAMAP V2.xlsx
#
# SORTIES
#   outputs_osyr_v2_final/
#     ├── data_clean/
#     ├── tables/
#     ├── figures/
#     ├── models/
#     ├── text_analysis/
#     └── diagnostics/
#
# NOTE MÉTHODOLOGIQUE
#   Les analyses sont descriptives et associatives. Les différences entre
#   doctorants exposés et non exposés ne sont pas des effets causaux (même si on peut être tenté ;) ).
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
  "tidyverse", "readxl", "janitor", "survey", "broom",
  "scales", "forcats", "stringi", "tidytext", "igraph",
  "ggraph", "ggrepel", "openxlsx", "glue", "fs"
)

install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Chemins
# -----------------------------------------------------------------------------

data_dir <- "data"
out_dir <- "outputs_osyr_v2_final"

bdd_file <- file.path(data_dir, "BJ30232 - BDD V2.csv")
map_file <- file.path(data_dir, "BJ30232 - DATAMAP V2.xlsx")

if (!file.exists(bdd_file)) bdd_file <- "BJ30232 - BDD V2.csv"
if (!file.exists(map_file)) map_file <- "BJ30232 - DATAMAP V2.xlsx"

stopifnot(file.exists(bdd_file))
stopifnot(file.exists(map_file))

dirs <- file.path(
  out_dir,
  c("data_clean", "tables", "figures", "models", "text_analysis", "diagnostics")
)
purrr::walk(dirs, fs::dir_create)

# -----------------------------------------------------------------------------
# 2. Fonctions générales
# -----------------------------------------------------------------------------

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

clean_ascii <- function(x) {
  x |>
    fix_text() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower(locale = "fr") |>
    stringr::str_squish()
}

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(as.numeric(x[ok]) * as.numeric(w[ok]), na.rm = TRUE) / sum(as.numeric(w[ok]), na.rm = TRUE)
}

w_prop <- function(condition, w) w_mean(as.numeric(condition), w)

safe_pct <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

write_table <- function(x, name, subdir = "tables") {
  readr::write_csv(x, file.path(out_dir, subdir, paste0(name, ".csv")))
  invisible(x)
}

write_model <- function(x, name) {
  readr::write_csv(x, file.path(out_dir, "models", paste0(name, ".csv")))
  invisible(x)
}

save_plot <- function(plot, filename, width = 12, height = 7.2) {
  ggplot2::ggsave(
    filename = file.path(out_dir, "figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 340,
    bg = "white"
  )
  invisible(file.path(out_dir, "figures", filename))
}

has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0 && ncol(x) > 0
}

# Sauvegarde uniquement si la table utilisée pour le graphique contient des lignes.
# Cela évite les figures blanches : titre + vide.
save_plot_if_data <- function(plot, data, filename, width = 12, height = 7.2, message_if_empty = NULL) {
  if (!has_rows(data)) {
    if (!is.null(message_if_empty)) warning(message_if_empty)
    return(invisible(NULL))
  }
  save_plot(plot, filename, width = width, height = height)
}

safe_max_pct <- function(x, multiplier = 1.18, floor = 0.05, ceiling = 1) {
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(m) || is.na(m)) return(floor)
  min(ceiling, max(floor, m * multiplier))
}

row_prop_codes <- function(data, vars, yes_codes, no_codes = NULL) {
  if (length(vars) == 0) return(rep(NA_real_, nrow(data)))
  mat <- data[, vars, drop = FALSE]
  mat <- as.data.frame(lapply(mat, function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    dplyr::case_when(
      x_num %in% yes_codes ~ 1,
      !is.null(no_codes) & x_num %in% no_codes ~ 0,
      is.null(no_codes) & !is.na(x_num) & !(x_num %in% yes_codes) ~ 0,
      TRUE ~ NA_real_
    )
  }))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

existing_vars <- function(vars, data) {
  vars <- vars[!is.na(vars)]
  vars <- vars[vars != ""]
  vars <- unique(vars)
  vars[vars %in% names(data)]
}

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

exposure_colors <- c(
  "Aucun dispositif" = unname(osyr_palette["coral"]),
  "Autoformation / autre seulement" = unname(osyr_palette["orange"]),
  "Dispositif organisé" = unname(osyr_palette["teal"]),
  "Indéterminé" = "#B8C0CC"
)

theme_osyr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 5,
        color = osyr_palette["navy"], lineheight = 1.05
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size + 1,
        color = "#475467",
        margin = ggplot2::margin(b = 12)
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
      strip.text = ggplot2::element_text(face = "bold", color = osyr_palette["navy"]),
      strip.background = ggplot2::element_rect(fill = "#F2F4F7", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# -----------------------------------------------------------------------------
# 3. Lecture base et datamap
# -----------------------------------------------------------------------------

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

write_table(question_map, "question_map_v2", subdir = "data_clean")
write_table(answer_labels, "answer_labels_v2", subdir = "data_clean")

# -----------------------------------------------------------------------------
# 3bis. Récupération robuste des libellés de modalités
# -----------------------------------------------------------------------------
# La datamap peut varier selon les exports : parfois les modalités Q2 sont bien
# sous base == "q2", parfois elles sont rattachées à un nom plus long, ou les
# colonnes s'appellent autrement. Les fonctions ci-dessous évitent de perdre les
# libellés des disciplines et prévoient un fichier manuel de secours.

label_tbl <- function(base_name) {
  answer_labels |>
    dplyr::filter(base == base_name) |>
    dplyr::select(code, value) |>
    dplyr::filter(!is.na(code), !is.na(value)) |>
    dplyr::distinct(code, value)
}

read_manual_labels <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())

  first_line <- readLines(path, n = 1, warn = FALSE)
  delim <- if (stringr::str_detect(first_line, ";")) ";" else ","

  manual <- readr::read_delim(path, delim = delim, show_col_types = FALSE)
  names(manual) <- janitor::make_clean_names(names(manual))

  code_col <- names(manual)[stringr::str_detect(names(manual), "^code$|q2|discipline_code|modalite")]
  label_col <- names(manual)[stringr::str_detect(names(manual), "label|libelle|discipline_detail|discipline|device|dispositif|formation|value|modalite")]

  # Éviter de prendre la même colonne pour code et label.
  label_col <- setdiff(label_col, code_col)

  if (length(code_col) == 0 || length(label_col) == 0) {
    stop(
      "Le fichier manuel ", path, " doit contenir une colonne de code et une colonne de libellé.\n",
      "Exemple attendu : code;discipline_detail"
    )
  }

  manual |>
    dplyr::transmute(
      code = suppressWarnings(as.numeric(.data[[code_col[1]]])),
      value = fix_text(.data[[label_col[1]]])
    ) |>
    dplyr::filter(!is.na(code), !is.na(value), value != "") |>
    dplyr::distinct(code, value)
}

extract_modal_labels <- function(base_name, observed_codes = NULL, semantic_hint = NULL, manual_file = NULL) {
  # 1. Fichier manuel prioritaire, si fourni.
  if (!is.null(manual_file) && file.exists(manual_file)) {
    out <- read_manual_labels(manual_file)
    attr(out, "source") <- paste0("manual:", manual_file)
    return(out)
  }

  observed_codes <- suppressWarnings(as.numeric(observed_codes))
  observed_codes <- sort(unique(observed_codes[!is.na(observed_codes)]))

  candidates <- list()

  # 2. Cas standard : base exactement égale au nom attendu.
  candidates[["base_exact"]] <- answer_labels |>
    dplyr::filter(base == base_name) |>
    dplyr::select(code, value)

  # 3. Cas plus souple : base commençant par q2 ou contenant le nom.
  candidates[["base_regex"]] <- answer_labels |>
    dplyr::filter(stringr::str_detect(base, paste0("^", base_name, "($|_)|", base_name))) |>
    dplyr::select(code, value)

  # 4. Cas sémantique : on cherche "discipline" dans name/label/value/ident.
  if (!is.null(semantic_hint)) {
    candidates[["semantic"]] <- datamap_ff |>
      dplyr::mutate(
        search_blob = clean_ascii(paste(ident, type, name, label, value, sep = " "))
      ) |>
      dplyr::filter(stringr::str_detect(search_blob, semantic_hint)) |>
      dplyr::transmute(code = suppressWarnings(as.numeric(code)), value = fix_text(value))
  }

  out <- dplyr::bind_rows(candidates, .id = "source") |>
    dplyr::filter(!is.na(code), !is.na(value), value != "") |>
    dplyr::mutate(
      value_clean = clean_ascii(value),
      code_chr = as.character(code)
    ) |>
    # Retirer les faux libellés qui ne sont que les codes.
    dplyr::filter(value_clean != code_chr) |>
    dplyr::select(source, code, value)

  if (length(observed_codes) > 0) {
    out <- out |> dplyr::filter(code %in% observed_codes)
  }

  # Si plusieurs candidats existent pour un même code, on garde le libellé le
  # plus informatif : source exacte d'abord, puis libellé le plus long.
  out <- out |>
    dplyr::mutate(
      source_priority = dplyr::case_when(
        source == "base_exact" ~ 1,
        source == "base_regex" ~ 2,
        source == "semantic" ~ 3,
        TRUE ~ 9
      ),
      label_length = stringr::str_length(value)
    ) |>
    dplyr::arrange(code, source_priority, dplyr::desc(label_length)) |>
    dplyr::group_by(code) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(code, value, source)

  attr(out, "source") <- if (nrow(out) > 0) paste(unique(out$source), collapse = "+") else "not_found"
  out
}

label_from_map <- function(x, labels, fallback_prefix = "Code") {
  x_num <- suppressWarnings(as.numeric(x))

  if (nrow(labels) == 0 || !all(c("code", "value") %in% names(labels))) {
    return(ifelse(is.na(x_num), NA_character_, paste(fallback_prefix, x_num)))
  }

  out <- labels$value[match(x_num, labels$code)]
  out <- fix_text(out)
  out <- ifelse(is.na(out) & !is.na(x_num), paste(fallback_prefix, x_num), out)
  out
}

validate_q2_labels <- function(q2_labels, observed_codes) {
  observed_codes <- sort(unique(suppressWarnings(as.numeric(observed_codes))))
  observed_codes <- observed_codes[!is.na(observed_codes)]

  diagnostics <- tibble::tibble(
    observed_code = observed_codes,
    recovered_label = q2_labels$value[match(observed_codes, q2_labels$code)],
    label_found = !is.na(recovered_label)
  )

  write_table(diagnostics, "q2_discipline_labels_diagnostics", subdir = "diagnostics")

  if (any(!diagnostics$label_found)) {
    template <- diagnostics |>
      dplyr::transmute(
        code = observed_code,
        discipline_detail = dplyr::coalesce(recovered_label, "")
      )

    write_table(template, "q2_discipline_labels_template_to_complete", subdir = "diagnostics")

    warning(
      "Certains libellés de discipline Q2 n'ont pas été récupérés automatiquement.\n",
      "Un modèle à compléter a été créé : outputs_osyr_v2_final/diagnostics/q2_discipline_labels_template_to_complete.csv\n",
      "Vous pouvez le copier en data/q2_discipline_labels.csv puis relancer le script 01."
    )
  }

  invisible(diagnostics)
}

# -----------------------------------------------------------------------------
# 4. Construction de la base analytique
# -----------------------------------------------------------------------------

stopifnot("poids" %in% names(df_raw))
stopifnot("respondent_language" %in% names(df_raw))
stopifnot(all(c("rs1", "q1", "q2") %in% names(df_raw)))

rs1_labels <- label_tbl("rs1")

# Récupération robuste des libellés de disciplines.
# Si l'auto-détection échoue, créer le fichier :
#   data/q2_discipline_labels.csv
# avec deux colonnes :
#   code;discipline_detail
q2_observed_codes <- sort(unique(suppressWarnings(as.numeric(df_raw$q2))))
q2_labels <- extract_modal_labels(
  base_name = "q2",
  observed_codes = q2_observed_codes,
  semantic_hint = "disciplin|discipline|field|domaine",
  manual_file = file.path(data_dir, "q2_discipline_labels.csv")
)

write_table(q2_labels, "q2_discipline_labels_used", subdir = "diagnostics")
validate_q2_labels(q2_labels, q2_observed_codes)

q8_cols <- names(df_raw) |> stringr::str_subset("^q8_m\\d+$")
stopifnot(length(q8_cols) > 0)

q8_codes_by_row <- df_raw |>
  dplyr::select(dplyr::all_of(q8_cols)) |>
  purrr::pmap(function(...) {
    vals <- c(...)
    vals <- vals[!is.na(vals)]
    unique(suppressWarnings(as.numeric(vals)))
  })

# Récupération robuste des libellés des dispositifs Q8.
# Priorité à un fichier manuel si besoin :
#   data/q8_device_labels.csv
# Format attendu :
#   code;device_label
q8_observed_codes <- sort(unique(unlist(q8_codes_by_row)))
q8_observed_codes <- q8_observed_codes[!is.na(q8_observed_codes)]

q8_labels <- extract_modal_labels(
  base_name = "q8",
  observed_codes = q8_observed_codes,
  semantic_hint = "dispositif|formation|mooc|autoformation|atelier|seminaire|module|stage|webinaire|presentiel|distanciel",
  manual_file = file.path(data_dir, "q8_device_labels.csv")
)

write_table(q8_labels, "q8_device_labels_used", subdir = "diagnostics")

q8_label_diagnostics <- tibble::tibble(
  observed_code = q8_observed_codes,
  recovered_label = q8_labels$value[match(q8_observed_codes, q8_labels$code)],
  label_found = !is.na(recovered_label)
)

write_table(q8_label_diagnostics, "q8_device_labels_diagnostics", subdir = "diagnostics")

if (any(!q8_label_diagnostics$label_found)) {
  q8_template <- q8_label_diagnostics |>
    dplyr::transmute(
      code = observed_code,
      device_label = dplyr::coalesce(recovered_label, "")
    )

  write_table(q8_template, "q8_device_labels_template_to_complete", subdir = "diagnostics")

  warning(
    "Certains libellés de dispositifs Q8 n'ont pas été récupérés automatiquement.\n",
    "Un modèle à compléter a été créé : outputs_osyr_v2_final/diagnostics/q8_device_labels_template_to_complete.csv\n",
    "Vous pouvez le copier en data/q8_device_labels.csv puis relancer le script 01."
  )
}

df <- df_raw |>
  dplyr::mutate(
    respondent_id = dplyr::row_number(),
    .weight = as.numeric(poids),
    weight_none = 1,

    language_group = dplyr::case_when(
      respondent_language == "en" ~ "Questionnaire en anglais",
      respondent_language == "fr" ~ "Questionnaire en français",
      TRUE ~ "Langue non renseignée"
    ),

    institution = label_from_map(rs1, rs1_labels, "Établissement code"),

    year_code = suppressWarnings(as.numeric(q1)),
    year = dplyr::case_when(
      year_code == 1 ~ "1re année",
      year_code == 2 ~ "2e année",
      year_code == 3 ~ "3e année",
      year_code == 4 ~ "4e année ou plus",
      year_code == 5 ~ "Thèse déjà soutenue",
      TRUE ~ NA_character_
    ),

    discipline_code = suppressWarnings(as.numeric(q2)),

    # Niveau détaillé : conserve toutes les disciplines de la datamap.
    discipline_detail = label_from_map(q2, q2_labels, "Discipline code"),

    # Niveau agrégé : utile pour les modèles plus stables, mais ne remplace pas
    # l'analyse détaillée.
    discipline_broad = dplyr::case_when(
      discipline_code %in% c(1, 2, 3, 4) ~ "Sciences formelles, physiques et chimiques",
      discipline_code %in% c(5, 10) ~ "Sciences du vivant, santé et environnement",
      discipline_code %in% c(6, 7) ~ "Sciences humaines et sociales",
      discipline_code %in% c(8, 9) ~ "Ingénierie, informatique et numérique",
      TRUE ~ "Autre / non classé"
    ),

    q8_has_none = purrr::map_lgl(q8_codes_by_row, ~ 97 %in% .x),
    q8_has_organized = purrr::map_lgl(q8_codes_by_row, ~ any(.x %in% 1:4)),
    q8_has_self_or_other = purrr::map_lgl(q8_codes_by_row, ~ any(.x %in% c(5, 6, 98))),
    q8_n_organized_types = purrr::map_int(q8_codes_by_row, ~ length(intersect(.x, 1:4))),

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
    exposure_organized = as.numeric(exposure3 == "Dispositif organisé"),

    training_intensity = dplyr::case_when(
      exposure3 == "Aucun dispositif" ~ "Aucun dispositif",
      "q10" %in% names(df_raw) & q10 == 1 ~ "1 formation/action",
      "q10" %in% names(df_raw) & q10 == 2 ~ "2 ou 3 formations/actions",
      "q10" %in% names(df_raw) & q10 == 3 ~ "4 formations/actions ou plus",
      "q10" %in% names(df_raw) & q10 == 97 ~ "Nombre inconnu",
      exposure3 == "Autoformation / autre seulement" ~ "Autoformation / autre seulement",
      TRUE ~ "Non renseigné"
    )
  ) |>
  dplyr::mutate(
    language_group = factor(language_group, levels = c("Questionnaire en français", "Questionnaire en anglais", "Langue non renseignée")),
    year = factor(year, levels = c("1re année", "2e année", "3e année", "4e année ou plus", "Thèse déjà soutenue")),
    exposure3 = factor(exposure3, levels = c("Aucun dispositif", "Autoformation / autre seulement", "Dispositif organisé", "Indéterminé")),
    exposure2 = factor(exposure2, levels = c("Aucun dispositif", "Dispositif organisé")),
    discipline_broad = factor(
      discipline_broad,
      levels = c(
        "Sciences humaines et sociales",
        "Sciences du vivant, santé et environnement",
        "Sciences formelles, physiques et chimiques",
        "Ingénierie, informatique et numérique",
        "Autre / non classé"
      )
    ),
    discipline_detail = factor(discipline_detail),
    training_intensity = factor(training_intensity)
  )

# -----------------------------------------------------------------------------
# 5. Formats longs des batteries
# -----------------------------------------------------------------------------

context_vars <- c(
  "respondent_id", ".weight", "weight_none", "exposure3", "exposure2",
  "exposure_organized", "training_intensity", "q8_n_organized_types",
  "year_code", "year", "discipline_code", "discipline_detail",
  "discipline_broad", "institution", "language_group"
)

# -----------------------------------------------------------------------------
# 5bis. Dispositifs Q8 détaillés, sans agrégation
# -----------------------------------------------------------------------------
# Cette table garde chaque modalité Q8 telle qu'elle est cochée.
# Important : le MOOC est traité comme autoformation / autre seulement, et non
# comme dispositif organisé, conformément à la correction méthodologique.
# La part calculée est une part de répondants ; comme Q8 est multiréponse,
# la somme des modalités peut dépasser 100 %, sauf pour "Aucun dispositif".

classify_q8_device <- function(code, label) {
  label_clean <- clean_ascii(label)

  dplyr::case_when(
    code == 97 | stringr::str_detect(label_clean, "aucun|none") ~ "Aucun dispositif",
    code %in% c(5, 6, 98) | stringr::str_detect(label_clean, "mooc|autoformation|auto formation|autre") ~ "Autoformation / MOOC / autre",
    code %in% 1:4 ~ "Dispositif organisé",
    stringr::str_detect(label_clean, "formation|atelier|seminaire|module|stage|webinaire|presentiel|distanciel") ~ "Dispositif organisé",
    TRUE ~ "Autre / à vérifier"
  )
}

q8_devices_long <- df |>
  dplyr::select(dplyr::any_of(context_vars), dplyr::all_of(q8_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(q8_cols),
    names_to = "q8_slot",
    values_to = "device_code"
  ) |>
  dplyr::mutate(device_code = suppressWarnings(as.numeric(device_code))) |>
  dplyr::filter(!is.na(device_code)) |>
  dplyr::left_join(
    q8_labels |>
      dplyr::select(device_code = code, device_label = value),
    by = "device_code"
  ) |>
  dplyr::mutate(
    device_label = dplyr::coalesce(device_label, paste0("Code ", device_code)),
    device_type = classify_q8_device(device_code, device_label)
  ) |>
  dplyr::distinct(respondent_id, device_code, .keep_all = TRUE)

readr::write_csv(q8_devices_long, file.path(out_dir, "data_clean", "q8_devices_long.csv"))

q8_device_distribution <- q8_devices_long |>
  dplyr::group_by(device_code, device_label, device_type) |>
  dplyr::summarise(
    n = dplyr::n_distinct(respondent_id),
    weighted_n = sum(.weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pct_respondents_w = weighted_n / sum(df$.weight, na.rm = TRUE),
    pct_respondents_w_label = safe_pct(pct_respondents_w)
  ) |>
  dplyr::arrange(dplyr::desc(pct_respondents_w))

write_table(q8_device_distribution, "q8_device_distribution_detail")

# Distribution par langue du questionnaire.
# Correction importante : le dénominateur doit être le poids total des répondants
# dans chaque groupe de langue, pas la somme à l'intérieur de chaque modalité Q8.
language_totals <- df |>
  dplyr::filter(!is.na(language_group)) |>
  dplyr::group_by(language_group) |>
  dplyr::summarise(total_weight_language = sum(.weight, na.rm = TRUE), .groups = "drop")

q8_device_distribution_by_language <- q8_devices_long |>
  dplyr::filter(!is.na(language_group)) |>
  dplyr::group_by(language_group, device_code, device_label, device_type) |>
  dplyr::summarise(
    n = dplyr::n_distinct(respondent_id),
    weighted_n = sum(.weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(language_totals, by = "language_group") |>
  dplyr::mutate(
    pct_respondents_w = weighted_n / total_weight_language,
    pct_respondents_w_label = safe_pct(pct_respondents_w)
  ) |>
  dplyr::arrange(language_group, dplyr::desc(pct_respondents_w))

write_table(q8_device_distribution_by_language, "q8_device_distribution_detail_by_language")

make_item_long <- function(data, vars, value_name, item_map, context_vars) {
  if (length(vars) == 0) return(tibble::tibble())
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
      item_label = stringr::str_squish(item_label)
    )
}

q4_vars <- names(df) |> stringr::str_subset("^q4_a\\d+$")
q5_vars <- names(df) |> stringr::str_subset("^q5_a\\d+$")
q11_vars <- names(df) |> stringr::str_subset("^q11_a\\d+$")
q12_vars <- names(df) |> stringr::str_subset("^q12_a\\d+$")
q13_vars <- names(df) |> stringr::str_subset("^q13_a\\d+$")
q15_vars <- names(df) |> stringr::str_subset("^q15_a\\d+$")

q4_long <- make_item_long(df, q4_vars, "response", item_map, context_vars) |>
  dplyr::mutate(positive = suppressWarnings(as.numeric(response)) == 1)

q5_long <- make_item_long(df, q5_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    response_num = suppressWarnings(as.numeric(response)),
    known_well = response_num %in% c(3, 4),
    used = response_num == 4,
    item_family = dplyr::case_when(
      stringr::str_detect(clean_ascii(item_label), "donnee|data|fair|pgd|gestion des donnees|entrepot") ~ "Données / FAIR / PGD",
      stringr::str_detect(clean_ascii(item_label), "code|logiciel|software|github|gitlab|software heritage") ~ "Code / logiciel",
      stringr::str_detect(clean_ascii(item_label), "archive|revue|publication|article|open access|acces ouvert|voie verte|voie doree|voie diamant") ~ "Publications / accès ouvert",
      stringr::str_detect(clean_ascii(item_label), "orcid|idhal|identifiant") ~ "Identifiants chercheurs",
      stringr::str_detect(clean_ascii(item_label), "creative commons|licence") ~ "Licences",
      TRUE ~ "Autres objets"
    )
  )

q11_long <- make_item_long(df, q11_vars, "response", item_map, context_vars) |>
  dplyr::mutate(response_num = suppressWarnings(as.numeric(response)), agree = response_num %in% c(3, 4))

q12_long <- make_item_long(df, q12_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    response_num = suppressWarnings(as.numeric(response)),
    incitation = response_num %in% c(4, 5),
    frein = response_num %in% c(1, 2)
  )

q13_long <- make_item_long(df, q13_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    response_num = suppressWarnings(as.numeric(response)),
    yes = response_num == 1,
    no = response_num == 2,
    dont_know = response_num %in% c(97, 99)
  )

q15_long <- make_item_long(df, q15_vars, "response", item_map, context_vars) |>
  dplyr::mutate(
    response_num = suppressWarnings(as.numeric(response)),
    agree = response_num %in% c(4, 5),
    disagree = response_num %in% c(1, 2)
  )

readr::write_csv(q4_long, file.path(out_dir, "data_clean", "q4_long.csv"))
readr::write_csv(q5_long, file.path(out_dir, "data_clean", "q5_long.csv"))
readr::write_csv(q11_long, file.path(out_dir, "data_clean", "q11_long.csv"))
readr::write_csv(q12_long, file.path(out_dir, "data_clean", "q12_long.csv"))
readr::write_csv(q13_long, file.path(out_dir, "data_clean", "q13_long.csv"))
readr::write_csv(q15_long, file.path(out_dir, "data_clean", "q15_long.csv"))

# -----------------------------------------------------------------------------
# 6. Scores synthétiques
# -----------------------------------------------------------------------------

df <- df |>
  dplyr::mutate(
    score_q4_practices_research = row_prop_codes(df, q4_vars, yes_codes = 1),
    score_q5_known_well = row_prop_codes(df, q5_vars, yes_codes = c(3, 4), no_codes = c(1, 2, 3, 4)),
    score_q5_used = row_prop_codes(df, q5_vars, yes_codes = 4, no_codes = c(1, 2, 3, 4)),
    score_q13_open_intentions = row_prop_codes(df, q13_vars, yes_codes = 1, no_codes = c(1, 2, 97, 99)),
    score_q13_dont_know = row_prop_codes(df, q13_vars, yes_codes = c(97, 99), no_codes = c(1, 2, 97, 99)),
    score_q12_incitation = row_prop_codes(df, q12_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3, 4, 5)),
    score_q12_frein = row_prop_codes(df, q12_vars, yes_codes = c(1, 2), no_codes = c(1, 2, 3, 4, 5)),
    score_q15_agreement = row_prop_codes(df, q15_vars, yes_codes = c(4, 5), no_codes = c(1, 2, 3, 4, 5)),
    # Alias conservés pour compatibilité avec le script 03.
    score_q15_benefits = score_q15_agreement,
    score_q15_constraints = NA_real_,
    score_q15_risks = NA_real_
  )

readr::write_csv(df, file.path(out_dir, "data_clean", "osyr_v2_corrigee_clean.csv"))
saveRDS(df, file.path(out_dir, "data_clean", "osyr_v2_corrigee_clean.rds"))

# -----------------------------------------------------------------------------
# 7. Contrôles qualité et documentation des disciplines
# -----------------------------------------------------------------------------

quality_overview <- tibble::tibble(
  n_rows = nrow(df),
  n_cols = ncol(df),
  sum_weights = sum(df$.weight, na.rm = TRUE),
  min_weight = min(df$.weight, na.rm = TRUE),
  max_weight = max(df$.weight, na.rm = TRUE),
  n_discipline_detail = dplyr::n_distinct(df$discipline_detail, na.rm = TRUE),
  n_discipline_broad = dplyr::n_distinct(df$discipline_broad, na.rm = TRUE),
  n_year_levels = dplyr::n_distinct(df$year, na.rm = TRUE),
  n_exposure3_levels = dplyr::n_distinct(df$exposure3, na.rm = TRUE)
)
write_table(quality_overview, "quality_overview")

discipline_mapping <- df |>
  dplyr::distinct(discipline_code, discipline_detail, discipline_broad) |>
  dplyr::arrange(discipline_code)
write_table(discipline_mapping, "discipline_mapping_detail_to_broad", subdir = "diagnostics")

# Contrôle explicite : si les disciplines apparaissent encore comme "Discipline code X",
# cela signifie que la datamap ne contenait pas les libellés attendus ou que le
# fichier manuel n'a pas été fourni.
discipline_label_quality <- discipline_mapping |>
  dplyr::mutate(
    label_is_fallback = stringr::str_detect(as.character(discipline_detail), "^Discipline code|^Code"),
    label_length = stringr::str_length(as.character(discipline_detail))
  )
write_table(discipline_label_quality, "discipline_label_quality", subdir = "diagnostics")

if (any(discipline_label_quality$label_is_fallback, na.rm = TRUE)) {
  warning(
    "Les libellés de certaines disciplines restent génériques. ",
    "Voir outputs_osyr_v2_final/diagnostics/discipline_label_quality.csv et ",
    "outputs_osyr_v2_final/diagnostics/q2_discipline_labels_template_to_complete.csv."
  )
}

# -----------------------------------------------------------------------------
# 8. Descriptifs pondérés
# -----------------------------------------------------------------------------

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
    dplyr::mutate(row_category = .data[[row_var]], col_category = .data[[col_var]]) |>
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

sample_year <- weighted_frequency(df, "year") |> write_table("sample_year")
sample_language <- weighted_frequency(df, "language_group") |> write_table("sample_language")
sample_exposure3 <- weighted_frequency(df, "exposure3") |> write_table("sample_exposure3")
sample_discipline_broad <- weighted_frequency(df, "discipline_broad") |> write_table("sample_discipline_broad")
sample_discipline_detail <- weighted_frequency(df, "discipline_detail") |> write_table("sample_discipline_detail")

cross_exposure_by_discipline_broad <- cross_weighted(df, "discipline_broad", "exposure3") |> write_table("cross_exposure_by_discipline_broad")
cross_exposure_by_discipline_detail <- cross_weighted(df, "discipline_detail", "exposure3") |> write_table("cross_exposure_by_discipline_detail")
cross_exposure_by_year <- cross_weighted(df, "year", "exposure3") |> write_table("cross_exposure_by_year")
cross_exposure_by_language <- cross_weighted(df, "language_group", "exposure3") |> write_table("cross_exposure_by_language")

# Distribution détaillée des dispositifs Q8, sans agrégation.
# Attention : Q8 est une question multiréponse ; les pourcentages représentent
# la part de répondants ayant coché chaque modalité et ne doivent pas être
# additionnés.
p_q8_devices_detail <- q8_device_distribution |>
  dplyr::mutate(
    device_label_plot = stringr::str_wrap(as.character(device_label), 55),
    device_label_plot = forcats::fct_reorder(device_label_plot, pct_respondents_w)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = pct_respondents_w, y = device_label_plot, fill = device_type)) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::geom_text(
    ggplot2::aes(label = pct_respondents_w_label),
    hjust = -0.10,
    size = 3.3,
    color = osyr_palette["navy"]
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, min(1, max(q8_device_distribution$pct_respondents_w, na.rm = TRUE) * 1.20))
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Dispositif organisé" = unname(osyr_palette["teal"]),
      "Autoformation / MOOC / autre" = unname(osyr_palette["orange"]),
      "Aucun dispositif" = unname(osyr_palette["coral"]),
      "Autre / à vérifier" = unname(osyr_palette["grey"])
    ),
    drop = TRUE
  ) +
  ggplot2::labs(
    title = "Distribution détaillée des dispositifs déclarés",
    subtitle = "Modalités Q8 non agrégées. Le MOOC est classé avec l'autoformation / autre.",
    x = "Part pondérée des répondants ayant coché la modalité",
    y = NULL,
    caption = "Question multiréponse : les pourcentages ne s'additionnent pas nécessairement à 100 %."
  ) +
  theme_osyr(base_size = 11)

save_plot(p_q8_devices_detail, "02b_distribution_dispositifs_q8_detail.png", width = 12.5, height = 7.5)

# Variante par langue du questionnaire, utile pour tester l'hypothèse selon
# laquelle les répondants en anglais cochent davantage certaines modalités
# comme le MOOC. Cette version corrige le calcul des dénominateurs : chaque
# barre représente la part des répondants d'un groupe de langue ayant coché
# la modalité.
if (dplyr::n_distinct(q8_device_distribution_by_language$language_group, na.rm = TRUE) > 1) {
  q8_language_plot_data <- q8_device_distribution_by_language |>
    dplyr::filter(language_group %in% c("Questionnaire en français", "Questionnaire en anglais")) |>
    dplyr::mutate(
      device_label_plot = stringr::str_wrap(as.character(device_label), 42),
      device_label_plot = forcats::fct_reorder(device_label_plot, pct_respondents_w, .fun = max)
    )

  p_q8_devices_language <- q8_language_plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = pct_respondents_w, y = device_label_plot, fill = language_group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.64) +
    ggplot2::geom_text(
      ggplot2::aes(label = pct_respondents_w_label),
      position = ggplot2::position_dodge(width = 0.75),
      hjust = -0.08,
      size = 3.1,
      color = osyr_palette["navy"]
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, min(1, max(q8_language_plot_data$pct_respondents_w, na.rm = TRUE) * 1.22))
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Questionnaire en français" = unname(osyr_palette["navy"]),
        "Questionnaire en anglais" = unname(osyr_palette["cyan"])
      ),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = "Dispositifs Q8 détaillés selon la langue du questionnaire",
      subtitle = "Part des répondants de chaque groupe ayant coché chaque modalité.",
      x = "Part pondérée dans chaque groupe de langue",
      y = NULL,
      caption = "Question multiréponse. La langue du questionnaire est un proxy, pas une variable d'identité."
    ) +
    theme_osyr(base_size = 10.5)

  save_plot(p_q8_devices_language, "02c_distribution_dispositifs_q8_detail_par_langue.png", width = 13.5, height = 8.2)
}

# -----------------------------------------------------------------------------
# 9. IC de l'exposition organisée par toutes les disciplines détaillées
# -----------------------------------------------------------------------------

df_survey <- df |> dplyr::filter(!is.na(.weight), .weight > 0)
design <- survey::svydesign(ids = ~1, weights = ~.weight, data = df_survey)

organized_by_discipline_detail_ci <- tryCatch({
  survey::svyby(
    ~exposure_organized,
    ~discipline_detail,
    design,
    survey::svymean,
    vartype = c("se", "ci"),
    na.rm = TRUE
  ) |>
    as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(pct_organized_w = exposure_organized) |>
    dplyr::arrange(dplyr::desc(pct_organized_w))
}, error = function(e) {
  warning("IC par discipline détaillée non générés : ", conditionMessage(e))
  tibble::tibble()
})
write_table(organized_by_discipline_detail_ci, "organized_exposure_by_discipline_detail_ci")

# -----------------------------------------------------------------------------
# 10. Descriptifs Q5 et scores par discipline détaillée
# -----------------------------------------------------------------------------

q5_by_discipline_detail <- q5_long |>
  dplyr::filter(!is.na(discipline_detail)) |>
  dplyr::group_by(discipline_detail, item, item_label, item_family) |>
  dplyr::summarise(
    n = dplyr::n(),
    pct_known_w = w_prop(known_well, .weight),
    pct_used_w = w_prop(used, .weight),
    gap_pp = 100 * (pct_known_w - pct_used_w),
    .groups = "drop"
  )
write_table(q5_by_discipline_detail, "q5_by_discipline_detail")

score_vars <- c(
  "score_q4_practices_research", "score_q5_known_well", "score_q5_used",
  "score_q13_open_intentions", "score_q13_dont_know",
  "score_q12_incitation", "score_q12_frein", "score_q15_agreement"
)
score_vars <- existing_vars(score_vars, df)

score_means_by_discipline_detail <- df |>
  dplyr::select(discipline_detail, exposure2, .weight, dplyr::all_of(score_vars)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(score_vars), names_to = "score", values_to = "value") |>
  dplyr::filter(!is.na(discipline_detail), !is.na(value)) |>
  dplyr::group_by(discipline_detail, exposure2, score) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_w = w_mean(value, .weight),
    .groups = "drop"
  )
write_table(score_means_by_discipline_detail, "score_means_by_discipline_detail")

# -----------------------------------------------------------------------------
# 11. Belles visualisations avec toutes les disciplines
# -----------------------------------------------------------------------------

# 11.1 Distribution détaillée des disciplines.
p_disc_detail <- sample_discipline_detail |>
  dplyr::mutate(category = forcats::fct_reorder(stringr::str_wrap(as.character(category), 45), pct_w)) |>
  ggplot2::ggplot(ggplot2::aes(x = pct_w, y = category)) +
  ggplot2::geom_col(fill = osyr_palette["navy"], width = 0.68) +
  ggplot2::geom_text(
    ggplot2::aes(label = pct_w_label),
    hjust = -0.12,
    size = 3.3,
    color = osyr_palette["navy"]
  ) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, min(1, max(sample_discipline_detail$pct_w, na.rm = TRUE) * 1.18))) +
  ggplot2::labs(
    title = "Toutes les disciplines détaillées des répondants",
    subtitle = "Répartition pondérée, sans agrégation en 4 grands domaines.",
    x = "Pourcentage pondéré",
    y = NULL,
    caption = "Source : enquête OSYR, pondération Poids."
  ) +
  theme_osyr(base_size = 11)
save_plot(p_disc_detail, "04a_distribution_discipline_detail.png", width = 12, height = 8.5)

# 11.2 Exposition par disciplines détaillées.
p_exposure_detail <- cross_exposure_by_discipline_detail |>
  dplyr::filter(col_category != "Indéterminé") |>
  dplyr::group_by(row_category) |>
  dplyr::mutate(order_val = pct_row[col_category == "Dispositif organisé"][1]) |>
  dplyr::ungroup() |>
  dplyr::mutate(row_category = forcats::fct_reorder(stringr::str_wrap(as.character(row_category), 45), order_val)) |>
  ggplot2::ggplot(ggplot2::aes(x = pct_row, y = row_category, fill = col_category)) +
  ggplot2::geom_col(width = 0.72, color = "white", linewidth = 0.35) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(values = exposure_colors, drop = TRUE) +
  ggplot2::labs(
    title = "Exposition aux dispositifs par discipline détaillée",
    subtitle = "Répartition pondérée dans chaque discipline, sans regroupement en 4 domaines.",
    x = "Pourcentage pondéré dans la discipline",
    y = NULL
  ) +
  theme_osyr(base_size = 10.8)
save_plot(p_exposure_detail, "04b_exposition_par_discipline_detail.png", width = 13.5, height = 9)

# 11.3 Exposition organisée avec IC par disciplines détaillées.
if (nrow(organized_by_discipline_detail_ci) > 0) {
  ci_cols <- names(organized_by_discipline_detail_ci)
  lower_col <- ci_cols[stringr::str_detect(ci_cols, "ci_l|ci_low|lower")]
  upper_col <- ci_cols[stringr::str_detect(ci_cols, "ci_u|ci_high|upper")]

  if (length(lower_col) > 0 && length(upper_col) > 0) {
    p_ci <- organized_by_discipline_detail_ci |>
      dplyr::mutate(discipline_detail = forcats::fct_reorder(stringr::str_wrap(as.character(discipline_detail), 45), pct_organized_w)) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_organized_w, y = discipline_detail)) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = .data[[lower_col[1]]], xmax = .data[[upper_col[1]]]),
        height = 0.18,
        color = "#98A2B3"
      ) +
      ggplot2::geom_point(size = 2.8, color = osyr_palette["teal"]) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Part exposée à un dispositif organisé par discipline détaillée",
        subtitle = "Estimations pondérées avec intervalles de confiance.",
        x = "Part pondérée exposée",
        y = NULL,
        caption = "Les IC peuvent être larges pour les disciplines à faibles effectifs."
      ) +
      theme_osyr(base_size = 10.8)
    save_plot(p_ci, "04c_exposition_organisee_par_discipline_detail_ci.png", width = 13, height = 8.5)
  }
}

# 11.4 Heatmap Q5 connaissance par disciplines détaillées.
p_q5_known_heat <- q5_by_discipline_detail |>
  dplyr::mutate(
    discipline_detail = stringr::str_wrap(as.character(discipline_detail), 32),
    item_label = stringr::str_wrap(item_label, 38)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = item_label, y = discipline_detail, fill = pct_known_w)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.25) +
  ggplot2::scale_fill_gradient(low = "#F2F4F7", high = osyr_palette["teal"], labels = scales::percent_format(accuracy = 1), na.value = "grey90") +
  ggplot2::labs(
    title = "Connaissance des notions de science ouverte par discipline détaillée",
    subtitle = "Part pondérée déclarant connaître bien chaque notion.",
    x = NULL,
    y = NULL,
    fill = "Connaissance"
  ) +
  theme_osyr(base_size = 9.5) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")
save_plot(p_q5_known_heat, "14b_heatmap_q5_connaissance_par_discipline_detail.png", width = 15, height = 9)

# 11.5 Heatmap Q5 usage par disciplines détaillées.
p_q5_used_heat <- q5_by_discipline_detail |>
  dplyr::mutate(
    discipline_detail = stringr::str_wrap(as.character(discipline_detail), 32),
    item_label = stringr::str_wrap(item_label, 38)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = item_label, y = discipline_detail, fill = pct_used_w)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.25) +
  ggplot2::scale_fill_gradient(low = "#F2F4F7", high = osyr_palette["blue"], labels = scales::percent_format(accuracy = 1), na.value = "grey90") +
  ggplot2::labs(
    title = "Usage des outils de science ouverte par discipline détaillée",
    subtitle = "Part pondérée déclarant avoir déjà utilisé chaque outil ou notion.",
    x = NULL,
    y = NULL,
    fill = "Usage"
  ) +
  theme_osyr(base_size = 9.5) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "right")
save_plot(p_q5_used_heat, "15b_heatmap_q5_usage_par_discipline_detail.png", width = 15, height = 9)

# 11.6 Scores par discipline détaillée et exposition.
# Version v14 : correction de la heatmap vide et ajout de sorties plus lisibles.
if (nrow(score_means_by_discipline_detail) > 0) {
  score_labels <- c(
    score_q4_practices_research = "Pratiques Q4",
    score_q5_known_well = "Connaissance Q5",
    score_q5_used = "Usage Q5",
    score_q13_open_intentions = "Intentions Q13",
    score_q13_dont_know = "NSP Q13",
    score_q12_incitation = "Incitation Q12",
    score_q12_frein = "Frein Q12",
    score_q15_agreement = "Accord Q15"
  )

  score_order <- c(
    "Connaissance Q5", "Usage Q5", "Pratiques Q4", "Intentions Q13",
    "Accord Q15", "Incitation Q12", "Frein Q12", "NSP Q13"
  )

  score_plot_data <- score_means_by_discipline_detail |>
    dplyr::filter(!is.na(exposure2)) |>
    dplyr::mutate(
      exposure2_clean = dplyr::case_when(
        stringr::str_detect(clean_ascii(exposure2), "aucun") ~ "Aucun dispositif",
        stringr::str_detect(clean_ascii(exposure2), "organise|organis") ~ "Dispositif organisé",
        TRUE ~ as.character(exposure2)
      ),
      score_label = dplyr::recode(score, !!!score_labels, .default = score)
    )

  # Correction v14 : éviter pivot_wider + janitor::clean_names(), qui pouvait
  # produire des colonnes inattendues et donc une heatmap vide. On fait deux
  # tables et une jointure explicite.
  score_none <- score_plot_data |>
    dplyr::filter(exposure2_clean == "Aucun dispositif") |>
    dplyr::select(discipline_detail, score, score_label, mean_none = mean_w, n_none = n)

  score_org <- score_plot_data |>
    dplyr::filter(exposure2_clean == "Dispositif organisé") |>
    dplyr::select(discipline_detail, score, score_label, mean_organized = mean_w, n_organized = n)

  score_gap_detail <- dplyr::full_join(
    score_none,
    score_org,
    by = c("discipline_detail", "score", "score_label")
  ) |>
    dplyr::mutate(
      diff_organized_vs_none_pp = 100 * (mean_organized - mean_none),
      abs_diff_pp = abs(diff_organized_vs_none_pp)
    ) |>
    dplyr::arrange(score_label, dplyr::desc(abs_diff_pp))

  write_table(score_gap_detail, "score_gap_by_discipline_detail_exposure")

  score_gap_plot <- score_gap_detail |>
    dplyr::filter(!is.na(diff_organized_vs_none_pp)) |>
    dplyr::mutate(
      discipline_detail = stringr::str_wrap(as.character(discipline_detail), 32),
      score_label = factor(score_label, levels = score_order)
    ) |>
    dplyr::filter(!is.na(score_label))

  if (nrow(score_gap_plot) > 0) {
    p_scores_gap <- score_gap_plot |>
      ggplot2::ggplot(ggplot2::aes(x = discipline_detail, y = score_label, fill = diff_organized_vs_none_pp)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.35) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(diff_organized_vs_none_pp), " pts")),
        size = 2.7,
        color = "#17324D"
      ) +
      ggplot2::scale_fill_gradient2(
        low = unname(osyr_palette["coral"]),
        mid = "#F2F4F7",
        high = unname(osyr_palette["teal"]),
        midpoint = 0,
        labels = function(x) paste0(x, " pts"),
        na.value = "grey90"
      ) +
      ggplot2::labs(
        title = "Écarts exposés / non exposés par discipline détaillée",
        subtitle = "Différence de score moyen pondéré : dispositif organisé moins aucun dispositif.",
        x = NULL,
        y = NULL,
        fill = "Écart",
        caption = "Lecture descriptive : les écarts ne sont pas interprétés causalement."
      ) +
      theme_osyr(base_size = 9.8) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
        legend.position = "right"
      )

    save_plot(p_scores_gap, "16b_scores_ecarts_par_discipline_detail_exposition.png", width = 15.5, height = 7.8)
    save_plot(p_scores_gap, "16b_scores_par_discipline_detail_exposition.png", width = 15.5, height = 7.8)
  } else {
    warning("La heatmap des écarts de scores est vide : vérifier score_means_by_discipline_detail.csv.")
  }

  # Figure compacte centrée Q5.
  q5_plot_data <- score_plot_data |>
    dplyr::filter(score %in% c("score_q5_known_well", "score_q5_used")) |>
    dplyr::mutate(
      discipline_detail = forcats::fct_reorder(stringr::str_wrap(as.character(discipline_detail), 34), mean_w, .fun = max),
      score_label = dplyr::recode(score, !!!score_labels, .default = score),
      exposure2_clean = factor(exposure2_clean, levels = c("Aucun dispositif", "Dispositif organisé"))
    )

  if (nrow(q5_plot_data) > 0) {
    p_scores_q5 <- q5_plot_data |>
      ggplot2::ggplot(ggplot2::aes(x = mean_w, y = discipline_detail, fill = exposure2_clean)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.64) +
      ggplot2::facet_wrap(~ score_label, ncol = 2) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(
        values = c(
          "Aucun dispositif" = unname(osyr_palette["coral"]),
          "Dispositif organisé" = unname(osyr_palette["teal"])
        ),
        drop = TRUE
      ) +
      ggplot2::labs(
        title = "Connaissance et usage Q5 par discipline détaillée",
        subtitle = "Comparaison entre doctorants sans dispositif et doctorants exposés à un dispositif organisé.",
        x = "Moyenne pondérée",
        y = NULL
      ) +
      theme_osyr(base_size = 10.2)

    save_plot(p_scores_q5, "16c_scores_q5_par_discipline_detail_exposition.png", width = 13.5, height = 8.7)
  }

  # Nouveau : lollipop des plus grands écarts, plus lisible qu'une heatmap complète.
  top_score_gaps <- score_gap_detail |>
    dplyr::filter(!is.na(diff_organized_vs_none_pp)) |>
    dplyr::mutate(
      label = paste0(as.character(discipline_detail), " — ", score_label),
      label = stringr::str_wrap(label, 56)
    ) |>
    dplyr::slice_max(order_by = abs_diff_pp, n = 22, with_ties = FALSE) |>
    dplyr::mutate(label = forcats::fct_reorder(label, diff_organized_vs_none_pp))

  if (nrow(top_score_gaps) > 0) {
    p_top_gaps <- top_score_gaps |>
      ggplot2::ggplot(ggplot2::aes(x = diff_organized_vs_none_pp, y = label)) +
      ggplot2::geom_vline(xintercept = 0, color = "#344054", linewidth = 0.45) +
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = diff_organized_vs_none_pp, yend = label),
        color = "#98A2B3",
        linewidth = 0.7
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = diff_organized_vs_none_pp > 0),
        size = 3
      ) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = unname(osyr_palette["teal"]), "FALSE" = unname(osyr_palette["coral"])),
        labels = c("FALSE" = "Plus faible chez les exposés", "TRUE" = "Plus élevé chez les exposés")
      ) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, " pts")) +
      ggplot2::labs(
        title = "Plus grands écarts exposés / non exposés",
        subtitle = "Top des différences absolues par discipline détaillée et indicateur.",
        x = "Écart en points de pourcentage",
        y = NULL,
        color = NULL
      ) +
      theme_osyr(base_size = 10.5)

    save_plot(p_top_gaps, "16d_top_ecarts_scores_discipline_detail.png", width = 13.5, height = 8.5)
  }
}

# -----------------------------------------------------------------------------
# 11bis. Visualisations complémentaires Q8 et Q5
# -----------------------------------------------------------------------------

# 11bis-1. Heatmap des dispositifs Q8 par discipline détaillée.
if (exists("q8_devices_long") && nrow(q8_devices_long) > 0) {
  discipline_totals <- df |>
    dplyr::filter(!is.na(discipline_detail)) |>
    dplyr::group_by(discipline_detail) |>
    dplyr::summarise(total_weight_discipline = sum(.weight, na.rm = TRUE), .groups = "drop")

  q8_device_by_discipline <- q8_devices_long |>
    dplyr::filter(!is.na(discipline_detail)) |>
    dplyr::group_by(discipline_detail, device_code, device_label, device_type) |>
    dplyr::summarise(
      n = dplyr::n_distinct(respondent_id),
      weighted_n = sum(.weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(discipline_totals, by = "discipline_detail") |>
    dplyr::mutate(
      pct_respondents_w = weighted_n / total_weight_discipline,
      pct_label = safe_pct(pct_respondents_w, accuracy = 1)
    ) |>
    dplyr::arrange(discipline_detail, dplyr::desc(pct_respondents_w))

  write_table(q8_device_by_discipline, "q8_device_distribution_detail_by_discipline")

  q8_device_heat <- q8_device_by_discipline |>
    dplyr::mutate(
      discipline_detail = stringr::str_wrap(as.character(discipline_detail), 30),
      device_label = stringr::str_wrap(as.character(device_label), 38)
    )

  if (nrow(q8_device_heat) > 0) {
    p_q8_device_heat <- q8_device_heat |>
      ggplot2::ggplot(ggplot2::aes(x = device_label, y = discipline_detail, fill = pct_respondents_w)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.30) +
      ggplot2::scale_fill_gradient(
        low = "#F2F4F7",
        high = unname(osyr_palette["teal"]),
        labels = scales::percent_format(accuracy = 1),
        na.value = "grey90"
      ) +
      ggplot2::labs(
        title = "Dispositifs Q8 détaillés par discipline",
        subtitle = "Part pondérée des répondants ayant coché chaque modalité dans chaque discipline.",
        x = NULL,
        y = NULL,
        fill = "Part",
        caption = "Question multiréponse : les pourcentages ne s'additionnent pas nécessairement à 100 %."
      ) +
      theme_osyr(base_size = 9.6) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
        legend.position = "right"
      )

    save_plot(p_q8_device_heat, "02d_heatmap_dispositifs_q8_par_discipline_detail.png", width = 14.5, height = 8.5)
  }

  # 11bis-2. Focus MOOC / autoformation par discipline.
  q8_auto_mooc_focus <- q8_device_by_discipline |>
    dplyr::filter(device_type == "Autoformation / MOOC / autre") |>
    dplyr::mutate(
      device_focus = dplyr::case_when(
        stringr::str_detect(clean_ascii(device_label), "mooc") ~ "MOOC / parcours asynchrone",
        stringr::str_detect(clean_ascii(device_label), "autoformation|documentation") ~ "Autoformation documentaire",
        TRUE ~ "Autre autoformation"
      )
    ) |>
    dplyr::group_by(discipline_detail, device_focus) |>
    dplyr::summarise(
      pct_respondents_w = sum(pct_respondents_w, na.rm = TRUE),
      .groups = "drop"
    )

  write_table(q8_auto_mooc_focus, "q8_autoformation_mooc_by_discipline")

  if (nrow(q8_auto_mooc_focus) > 0) {
    p_q8_auto <- q8_auto_mooc_focus |>
      dplyr::mutate(
        discipline_detail = forcats::fct_reorder(stringr::str_wrap(as.character(discipline_detail), 34), pct_respondents_w, .fun = max)
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = pct_respondents_w, y = discipline_detail, fill = device_focus)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.74), width = 0.64) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(
        values = c(
          "MOOC / parcours asynchrone" = unname(osyr_palette["orange"]),
          "Autoformation documentaire" = unname(osyr_palette["purple"]),
          "Autre autoformation" = unname(osyr_palette["grey"])
        ),
        drop = TRUE
      ) +
      ggplot2::labs(
        title = "MOOC et autoformation par discipline détaillée",
        subtitle = "Focus sur les modalités que l'on ne classe pas comme dispositifs organisés.",
        x = "Part pondérée des répondants",
        y = NULL
      ) +
      theme_osyr(base_size = 10.4)

    save_plot(p_q8_auto, "02e_mooc_autoformation_par_discipline_detail.png", width = 13.5, height = 8)
  }
}

# 11bis-3. Gap connaissance-usage par discipline détaillée.
if (exists("q5_by_discipline_detail") && nrow(q5_by_discipline_detail) > 0) {
  q5_gap_by_discipline <- q5_by_discipline_detail |>
    dplyr::group_by(discipline_detail) |>
    dplyr::summarise(
      pct_known_w = mean(pct_known_w, na.rm = TRUE),
      pct_used_w = mean(pct_used_w, na.rm = TRUE),
      gap_pp = mean(gap_pp, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(gap_pp))

  write_table(q5_gap_by_discipline, "q5_gap_knowledge_usage_by_discipline_detail")

  if (nrow(q5_gap_by_discipline) > 0) {
    p_gap_disc <- q5_gap_by_discipline |>
      dplyr::mutate(discipline_detail = forcats::fct_reorder(stringr::str_wrap(as.character(discipline_detail), 34), gap_pp)) |>
      ggplot2::ggplot(ggplot2::aes(x = gap_pp, y = discipline_detail)) +
      ggplot2::geom_col(fill = unname(osyr_palette["navy"]), width = 0.68) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(gap_pp), " pts")),
        hjust = -0.12,
        size = 3.2,
        color = osyr_palette["navy"]
      ) +
      ggplot2::scale_x_continuous(
        labels = function(x) paste0(x, " pts"),
        limits = c(0, safe_max_pct(q5_gap_by_discipline$gap_pp / 100, multiplier = 1.20, floor = 0.05, ceiling = 1) * 100)
      ) +
      ggplot2::labs(
        title = "Gap connaissance-usage par discipline détaillée",
        subtitle = "Écart moyen entre notions bien connues et outils déjà utilisés.",
        x = "Écart moyen en points de pourcentage",
        y = NULL
      ) +
      theme_osyr(base_size = 10.5)

    save_plot(p_gap_disc, "17a_gap_connaissance_usage_par_discipline_detail.png", width = 12.5, height = 7.5)
  }

  # Gap par famille d'objet et discipline.
  if ("item_family" %in% names(q5_by_discipline_detail)) {
    q5_gap_family_disc <- q5_by_discipline_detail |>
      dplyr::group_by(discipline_detail, item_family) |>
      dplyr::summarise(
        gap_pp = mean(gap_pp, na.rm = TRUE),
        pct_known_w = mean(pct_known_w, na.rm = TRUE),
        pct_used_w = mean(pct_used_w, na.rm = TRUE),
        .groups = "drop"
      )

    write_table(q5_gap_family_disc, "q5_gap_by_family_and_discipline_detail")

    if (nrow(q5_gap_family_disc) > 0) {
      p_gap_family <- q5_gap_family_disc |>
        dplyr::mutate(
          discipline_detail = stringr::str_wrap(as.character(discipline_detail), 30),
          item_family = stringr::str_wrap(as.character(item_family), 28)
        ) |>
        ggplot2::ggplot(ggplot2::aes(x = item_family, y = discipline_detail, fill = gap_pp)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.30) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(round(gap_pp), " pts")), size = 2.5, color = "#17324D") +
        ggplot2::scale_fill_gradient(
          low = "#F2F4F7",
          high = unname(osyr_palette["orange"]),
          labels = function(x) paste0(x, " pts"),
          na.value = "grey90"
        ) +
        ggplot2::labs(
          title = "Gap connaissance-usage par famille d'objets",
          subtitle = "Écart moyen en points par discipline détaillée et famille de notions/outils.",
          x = NULL,
          y = NULL,
          fill = "Gap"
        ) +
        theme_osyr(base_size = 9.8) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
          legend.position = "right"
        )

      save_plot(p_gap_family, "17b_gap_par_famille_objet_discipline_detail.png", width = 14, height = 8)
    }
  }
}

# -----------------------------------------------------------------------------
# 11ter. Correction optionnelle du graphique Q3 si les concepts existent

# -----------------------------------------------------------------------------
# Ce bloc n'est activé que si le script complémentaire a déjà produit la table
# q3_concept_framing.csv. Il évite les libellés génériques de type Item 1,
# Item 2, etc., dans les restitutions.
q3_concept_path <- file.path("outputs_osyr_v2_complements_30062026", "text_analysis", "q3_concept_framing.csv")
if (file.exists(q3_concept_path)) {
  q3_concept_framing <- readr::read_csv(q3_concept_path, show_col_types = FALSE)

  if (nrow(q3_concept_framing) > 0 && "concept" %in% names(q3_concept_framing)) {
    pct_col <- dplyr::case_when(
      "pct_w" %in% names(q3_concept_framing) ~ "pct_w",
      "pct" %in% names(q3_concept_framing) ~ "pct",
      TRUE ~ NA_character_
    )

    if (!is.na(pct_col)) {
      p_q3_concepts <- q3_concept_framing |>
        dplyr::mutate(
          concept_plot = forcats::fct_reorder(stringr::str_wrap(as.character(concept), 42), .data[[pct_col]])
        ) |>
        dplyr::arrange(dplyr::desc(.data[[pct_col]])) |>
        dplyr::slice_head(n = 16) |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[pct_col]], y = concept_plot)) +
        ggplot2::geom_col(fill = unname(osyr_palette["navy"]), width = 0.68) +
        ggplot2::geom_text(
          ggplot2::aes(label = safe_pct(.data[[pct_col]], accuracy = 1)),
          hjust = -0.10,
          size = 3.4,
          color = osyr_palette["navy"]
        ) +
        ggplot2::scale_x_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, min(1, max(q3_concept_framing[[pct_col]], na.rm = TRUE) * 1.20))
        ) +
        ggplot2::labs(
          title = "À quoi fait penser la science ouverte ?",
          subtitle = "Concepts repérés dans les trois mots cités par les doctorants.",
          x = "Pourcentage pondéré",
          y = NULL,
          caption = "Un répondant peut contribuer à plusieurs concepts."
        ) +
        theme_osyr(base_size = 11)

      save_plot(p_q3_concepts, "23_q3_concepts_overall.png", width = 12.5, height = 8)
    }
  }
}

# -----------------------------------------------------------------------------
# 12. Sortie de session et message final
# -----------------------------------------------------------------------------


sink(file.path(out_dir, "diagnostics", "sessionInfo_script01_v14.txt"))
print(sessionInfo())
sink()

message("\nScript 01 v14 terminé.")
message("Base enrichie : ", normalizePath(file.path(out_dir, "data_clean", "osyr_v2_corrigee_clean.rds"), mustWork = FALSE))
message("Figures disciplines détaillées et dispositifs détaillés : ", normalizePath(file.path(out_dir, "figures"), mustWork = FALSE))
