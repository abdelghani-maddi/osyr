# =============================================================================
# CONTRÔLES D'INTÉGRITÉ MÉTHODOLOGIQUE DU WORKFLOW OSYR
# Version 2026-09-27
# =============================================================================
# Rôle
#   Vérifier, après les analyses et avant la production des livrables, que les
#   conventions méthodologiques définies pour le dépouillement sont respectées.
#
# Position dans le workflow
#   01 -> 03 -> osyr_final_analyses -> osyr_plan_depouillement_analyses
#      -> CE SCRIPT -> osyr_figure_polish -> rapport / présentation
#
# Points du plan concernés
#   - transversal : pondération, dénominateurs, regroupements, valeurs manquantes ;
#   - parcours de formation : cohérence du classement Q8 ;
#   - connaissances / pratiques : cohérence des indicateurs Q5 ;
#   - intentions : codage exclusif oui / non / je ne sais pas de Q13 ;
#   - perceptions : cohérence des indicateurs Q12 et bornes des scores Q15 ;
#   - modèles : intervalles de confiance, valeurs p et correction FDR.
#
# Principe
#   Les contrôles de niveau "ERREUR" interrompent le workflow : produire un
#   rapport final avec une incohérence critique serait préférable à éviter.
#   Les contrôles de niveau "AVERTISSEMENT" sont exportés mais ne bloquent pas.
#
# Sortie
#   outputs_osyr_rapport_final/diagnostics/integrite_methodologique.csv
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

pkgs <- c("tidyverse", "janitor", "fs")
missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

dirs <- osyr_dirs()
diagnostic_dir <- file.path(dirs$report, "diagnostics")
ensure_dir(diagnostic_dir)

checks <- tibble::tibble()

add_check <- function(id, bloc, niveau, ok, detail, valeur = NA_character_) {
  checks <<- dplyr::bind_rows(
    checks,
    tibble::tibble(
      id = id,
      bloc = bloc,
      niveau = niveau,
      statut = ifelse(isTRUE(ok), "OK", niveau),
      valeur = as.character(valeur),
      detail = detail
    )
  )
  invisible(ok)
}

read_csv_if_exists <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

# -----------------------------------------------------------------------------
# 1. Base analytique et provenance de la pondération
# -----------------------------------------------------------------------------
# Plan : précautions méthodologiques / pondération.
# Attendu : une seule variable source "Poids", recodée en .weight ; aucun
# mécanisme de détection automatique d'autres colonnes comme poids d'enquête.

clean_rds <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.rds")
clean_csv <- file.path(dirs$final, "data_clean", "osyr_v2_corrigee_clean.csv")

if (file.exists(clean_rds)) {
  df <- readRDS(clean_rds)
} else if (file.exists(clean_csv)) {
  df <- readr::read_csv(clean_csv, show_col_types = FALSE)
} else {
  stop("Base analytique absente. Relancer le script 01.")
}

add_check(
  "base_analytiques",
  "Pondération",
  "ERREUR",
  nrow(df) > 0,
  "La base analytique doit contenir au moins une observation.",
  nrow(df)
)

bdd_file <- file.path("data", "BJ30232 - BDD V2.csv")
if (!file.exists(bdd_file)) bdd_file <- "BJ30232 - BDD V2.csv"

if (file.exists(bdd_file)) {
  raw_names <- readr::read_delim(
    bdd_file,
    delim = ";",
    locale = readr::locale(encoding = "ISO-8859-1"),
    n_max = 1,
    show_col_types = FALSE
  ) |>
    names() |>
    janitor::make_clean_names()

  n_poids_source <- sum(raw_names == "poids")

  add_check(
    "source_poids_unique",
    "Pondération",
    "ERREUR",
    n_poids_source == 1,
    "La base source doit comporter exactement une colonne de pondération nommée Poids.",
    n_poids_source
  )
} else {
  add_check(
    "source_poids_unique",
    "Pondération",
    "AVERTISSEMENT",
    FALSE,
    "La base source n'a pas été trouvée au moment du contrôle ; l'unicité de la colonne Poids n'a pas pu être vérifiée."
  )
}

add_check(
  "weight_presente",
  "Pondération",
  "ERREUR",
  ".weight" %in% names(df),
  "La base analytique doit contenir .weight, issue de la colonne Poids."
)

if (".weight" %in% names(df)) {
  w <- suppressWarnings(as.numeric(df$.weight))

  add_check(
    "weight_positive_disponible",
    "Pondération",
    "ERREUR",
    any(!is.na(w) & w > 0),
    "Au moins une observation doit disposer d'un poids strictement positif.",
    sum(!is.na(w) & w > 0)
  )

  add_check(
    "weight_invalides",
    "Pondération",
    "AVERTISSEMENT",
    sum(is.na(w) | w <= 0) == 0,
    "Les poids manquants, nuls ou négatifs sont exclus des analyses pondérées ; leur nombre est documenté ici.",
    sum(is.na(w) | w <= 0)
  )
}

add_check(
  "weight_none_technique",
  "Pondération",
  "ERREUR",
  !"weight_none" %in% names(df) || all(df$weight_none == 1, na.rm = TRUE),
  "weight_none, lorsqu'elle existe, doit être une constante égale à 1 utilisée uniquement pour les analyses non pondérées."
)

# -----------------------------------------------------------------------------
# 2. Bornes des scores et catégories analytiques
# -----------------------------------------------------------------------------
# Plan : transversal à Q4/Q5/Q11/Q12/Q13/Q15.
# Attendu : tous les scores construits comme proportions individuelles restent
# dans [0,1]. Les catégories d'exposition sont celles documentées dans la méthode.

score_vars <- names(df)[stringr::str_detect(names(df), "^score_")]

if (length(score_vars) > 0) {
  score_range <- purrr::map_dfr(score_vars, function(v) {
    x <- suppressWarnings(as.numeric(df[[v]]))
    tibble::tibble(
      variable = v,
      n_valid = sum(!is.na(x)),
      min = ifelse(any(!is.na(x)), min(x, na.rm = TRUE), NA_real_),
      max = ifelse(any(!is.na(x)), max(x, na.rm = TRUE), NA_real_),
      outside = sum(!is.na(x) & (x < 0 | x > 1))
    )
  })

  add_check(
    "scores_bornes",
    "Variables synthétiques",
    "ERREUR",
    all(score_range$outside == 0),
    "Les scores proportionnels doivent être compris entre 0 et 1.",
    sum(score_range$outside)
  )

  readr::write_csv(
    score_range,
    file.path(diagnostic_dir, "integrite_scores_bornes.csv")
  )
}

if ("exposure3" %in% names(df)) {
  allowed3 <- c(
    "Aucun dispositif",
    "Autoformation / autre seulement",
    "Dispositif organisé",
    "Indéterminé"
  )
  unexpected3 <- setdiff(unique(stats::na.omit(as.character(df$exposure3))), allowed3)

  add_check(
    "exposure3_modalites",
    "Parcours de formation",
    "ERREUR",
    length(unexpected3) == 0,
    "exposure3 ne doit contenir que les quatre modalités documentées.",
    paste(unexpected3, collapse = " | ")
  )
}

if (all(c("q8_inconsistent_none", "exposure3") %in% names(df))) {
  wrong_q8 <- sum(
    df$q8_inconsistent_none %in% TRUE &
      as.character(df$exposure3) != "Indéterminé",
    na.rm = TRUE
  )

  add_check(
    "q8_contradictions",
    "Parcours de formation",
    "ERREUR",
    wrong_q8 == 0,
    "Une réponse Q8 combinant « aucune » avec une autre modalité doit être classée Indéterminé.",
    wrong_q8
  )
}

# -----------------------------------------------------------------------------
# 3. Cohérence des batteries longues
# -----------------------------------------------------------------------------
# Plan :
#   - Q5 : connaissance / usage ;
#   - Q12 : incitations / freins ;
#   - Q13 : oui / non / je ne sais pas.
# Les indicateurs doivent être mutuellement cohérents et les non-réponses ne
# doivent pas devenir automatiquement FALSE.

q5_path <- file.path(dirs$final, "data_clean", "q5_long.csv")
q12_path <- file.path(dirs$final, "data_clean", "q12_long.csv")
q13_path <- file.path(dirs$final, "data_clean", "q13_long.csv")

q5 <- read_csv_if_exists(q5_path)
if (has_rows(q5) && all(c("known_well", "used") %in% names(q5))) {
  n_used_without_known <- q5 |>
    dplyr::filter(used %in% TRUE, !(known_well %in% TRUE)) |>
    nrow()

  add_check(
    "q5_usage_implique_connaissance",
    "Connaissances et pratiques",
    "ERREUR",
    n_used_without_known == 0,
    "Dans le codage Q5, « déjà utilisé » doit impliquer « bien connu ».",
    n_used_without_known
  )
}

q12 <- read_csv_if_exists(q12_path)
if (has_rows(q12) && all(c("incitation", "frein") %in% names(q12))) {
  both <- q12 |>
    dplyr::filter(incitation %in% TRUE, frein %in% TRUE) |>
    nrow()

  add_check(
    "q12_incitation_frein_exclusifs",
    "Perceptions / environnement",
    "ERREUR",
    both == 0,
    "Un même item Q12 ne peut être simultanément codé incitation et frein.",
    both
  )
}

q13 <- read_csv_if_exists(q13_path)
if (has_rows(q13) && all(c("response_num", "yes", "no", "dont_know") %in% names(q13))) {
  q13_valid <- q13 |>
    dplyr::filter(response_num %in% c(1, 2, 97)) |>
    dplyr::mutate(
      n_true = rowSums(cbind(yes %in% TRUE, no %in% TRUE, dont_know %in% TRUE))
    )

  add_check(
    "q13_modalites_exclusives",
    "Intentions",
    "ERREUR",
    all(q13_valid$n_true == 1),
    "Pour chaque réponse valide Q13, une et une seule modalité doit être vraie : oui, non ou je ne sais pas.",
    sum(q13_valid$n_true != 1)
  )
}

# -----------------------------------------------------------------------------
# 4. Cohérence des modèles et de l'inférence
# -----------------------------------------------------------------------------
# Plan : modèles ajustés, intervalles de confiance, FDR et robustesse.
# Les contrôles portent sur la cohérence numérique, pas sur l'interprétation.

model_files <- c(
  file.path(dirs$report, "tables", "plan_modele_pratiques_q5_elargi.csv"),
  file.path(dirs$report, "tables", "plan_modele_pratiques_q5_caracteristiques_formation.csv"),
  file.path(dirs$report, "tables", "plan_modeles_perceptions_q15.csv"),
  file.path(dirs$complements, "models", "score_tests_weighted_unweighted_discipline_detail_broad_fdr.csv"),
  file.path(dirs$complements, "models", "item_tests_weighted_unweighted_discipline_detail_fdr.csv")
)

model_files <- model_files[file.exists(model_files)]

for (path in model_files) {
  m <- readr::read_csv(path, show_col_types = FALSE)
  nm <- basename(path)

  estimate_col <- dplyr::case_when(
    "estimate" %in% names(m) ~ "estimate",
    "estimate_pp" %in% names(m) ~ "estimate_pp",
    "estimate_pp_approx" %in% names(m) ~ "estimate_pp_approx",
    TRUE ~ NA_character_
  )
  low_col <- dplyr::case_when(
    "conf.low" %in% names(m) ~ "conf.low",
    "conf_low_pp" %in% names(m) ~ "conf_low_pp",
    "conf_low_pp_approx" %in% names(m) ~ "conf_low_pp_approx",
    TRUE ~ NA_character_
  )
  high_col <- dplyr::case_when(
    "conf.high" %in% names(m) ~ "conf.high",
    "conf_high_pp" %in% names(m) ~ "conf_high_pp",
    "conf_high_pp_approx" %in% names(m) ~ "conf_high_pp_approx",
    TRUE ~ NA_character_
  )

  if (!is.na(estimate_col) && !is.na(low_col) && !is.na(high_col)) {
    est <- suppressWarnings(as.numeric(m[[estimate_col]]))
    low <- suppressWarnings(as.numeric(m[[low_col]]))
    high <- suppressWarnings(as.numeric(m[[high_col]]))

    bad_ci <- sum(
      !is.na(est) & !is.na(low) & !is.na(high) &
        (low > est | est > high),
      na.rm = TRUE
    )

    add_check(
      paste0("ci_", nm),
      "Modèles",
      "ERREUR",
      bad_ci == 0,
      paste0("Les intervalles de confiance doivent encadrer l'estimation dans ", nm, "."),
      bad_ci
    )
  }

  if ("p.value" %in% names(m)) {
    bad_p <- sum(!is.na(m$p.value) & (m$p.value < 0 | m$p.value > 1), na.rm = TRUE)

    add_check(
      paste0("pvalue_", nm),
      "Modèles",
      "ERREUR",
      bad_p == 0,
      paste0("Les valeurs p doivent être comprises entre 0 et 1 dans ", nm, "."),
      bad_p
    )
  }

  if ("p_fdr" %in% names(m)) {
    bad_fdr <- sum(!is.na(m$p_fdr) & (m$p_fdr < 0 | m$p_fdr > 1), na.rm = TRUE)

    add_check(
      paste0("fdr_", nm),
      "Modèles",
      "ERREUR",
      bad_fdr == 0,
      paste0("Les valeurs p ajustées FDR doivent être comprises entre 0 et 1 dans ", nm, "."),
      bad_fdr
    )
  }
}

# -----------------------------------------------------------------------------
# 5. Cohérence éditoriale du catalogue des figures
# -----------------------------------------------------------------------------
# Plan : production finale.
# Le catalogue destiné au lecteur ne doit plus contenir les marqueurs internes
# du workflow (source_dir, « figure issue des sorties », « points à rédiger »).

catalog_path <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")
catalog <- read_csv_if_exists(catalog_path)

if (has_rows(catalog)) {
  text_cols <- intersect(c("titre", "caption"), names(catalog))

  if (length(text_cols) > 0) {
    editorial_text <- catalog |>
      dplyr::select(dplyr::all_of(text_cols)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(as.character(.x), ""))) |>
      tidyr::unite("txt", dplyr::everything(), sep = " ", remove = TRUE) |>
      dplyr::pull(txt)

    internal_pattern <- "figure issue des sorties|points à rédiger|rapport_final|complements|diagnostic interne"

    bad_editorial <- sum(
      stringr::str_detect(
        stringr::str_to_lower(editorial_text),
        internal_pattern
      ),
      na.rm = TRUE
    )

    add_check(
      "catalogue_editorial",
      "Production finale",
      "AVERTISSEMENT",
      bad_editorial == 0,
      "Les titres et légendes du catalogue final doivent être rédigés pour le lecteur, sans vocabulaire de production interne.",
      bad_editorial
    )
  }
}

# -----------------------------------------------------------------------------
# 6. Export et décision de poursuite
# -----------------------------------------------------------------------------

checks <- checks |>
  dplyr::arrange(
    factor(niveau, levels = c("ERREUR", "AVERTISSEMENT")),
    bloc,
    id
  )

out_path <- file.path(diagnostic_dir, "integrite_methodologique.csv")
readr::write_csv(checks, out_path)

n_errors <- sum(checks$statut == "ERREUR")
n_warnings <- sum(checks$statut == "AVERTISSEMENT")

message("Contrôles méthodologiques : ", nrow(checks), " vérifications.")
message(" - erreurs : ", n_errors)
message(" - avertissements : ", n_warnings)
message(" - détail : ", normalizePath(out_path, mustWork = FALSE))

if (n_errors > 0) {
  stop(
    "Le workflow est interrompu : ",
    n_errors,
    " contrôle(s) méthodologique(s) critique(s) ont échoué. ",
    "Consulter integrite_methodologique.csv."
  )
}
