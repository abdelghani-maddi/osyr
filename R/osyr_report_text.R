# =============================================================================
# Synthèses factuelles pour le rapport final OSYR
# =============================================================================
# Les fonctions de ce fichier transforment les tableaux produits par le workflow
# en courts résumés factuels. Elles ne remplacent pas l'interprétation
# scientifique : elles servent à produire un premier niveau de rédaction fondé
# exclusivement sur les valeurs calculées par les scripts.
# =============================================================================

report_read_table <- function(name, dir = file.path(osyr_dirs()$report, "tables")) {
  path <- file.path(dir, paste0(name, ".csv"))
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

report_read_complement <- function(name, subdir = "tables") {
  path <- file.path(osyr_dirs()$complements, subdir, paste0(name, ".csv"))
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

report_read_final <- function(name, subdir = "tables") {
  path <- file.path(osyr_dirs()$final, subdir, paste0(name, ".csv"))
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

fmt_pct_report <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "n.d.", scales::percent(x, accuracy = accuracy, decimal.mark = ","))
}

fmt_pp_report <- function(x, accuracy = 0.1) {
  ifelse(
    is.na(x),
    "n.d.",
    paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = accuracy, decimal.mark = ","), " points")
  )
}

clean_report_label <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish()
}

top_rows_report <- function(data, value_col, n = 3, decreasing = TRUE) {
  if (!is.data.frame(data) || nrow(data) == 0 || !value_col %in% names(data)) {
    return(tibble::tibble())
  }

  data |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::arrange(if (decreasing) dplyr::desc(.data[[value_col]]) else .data[[value_col]]) |>
    dplyr::slice_head(n = n)
}

sentence_from_top <- function(data, label_col, value_col, intro, n = 3, value_fun = fmt_pct_report) {
  if (!all(c(label_col, value_col) %in% names(data)) || nrow(data) == 0) return(NULL)

  x <- top_rows_report(data, value_col, n = n)
  if (nrow(x) == 0) return(NULL)

  bits <- paste0(
    clean_report_label(x[[label_col]]),
    " (", value_fun(x[[value_col]]), ")"
  )

  paste0(intro, paste(bits, collapse = " ; "), ".")
}

group_difference_table <- function(data, group_col, label_col, value_col,
                                   group_a = "Aucun dispositif",
                                   group_b = "Dispositif organisé") {
  if (!all(c(group_col, label_col, value_col) %in% names(data))) return(tibble::tibble())

  wide <- data |>
    dplyr::filter(.data[[group_col]] %in% c(group_a, group_b)) |>
    dplyr::select(
      label = dplyr::all_of(label_col),
      group = dplyr::all_of(group_col),
      value = dplyr::all_of(value_col)
    ) |>
    tidyr::pivot_wider(names_from = group, values_from = value)

  if (!all(c(group_a, group_b) %in% names(wide))) return(tibble::tibble())

  wide |>
    dplyr::mutate(
      diff_pp = 100 * (.data[[group_b]] - .data[[group_a]]),
      abs_diff_pp = abs(diff_pp)
    ) |>
    dplyr::arrange(dplyr::desc(abs_diff_pp))
}

section_summary_text <- function(section) {
  out <- character()

  if (section == 1) {
    q8 <- report_read_final("q8_device_distribution_detail")
    q11 <- report_read_table("formation_evaluation_q11")
    by_year <- report_read_table("formation_exposition_par_annee")

    s <- sentence_from_top(
      q8, "device_label", "pct_respondents_w",
      "Les dispositifs les plus fréquemment déclarés sont : ", 4
    )
    if (!is.null(s)) out <- c(out, s)

    if (nrow(q8) > 0 && all(c("device_label", "pct_respondents_w") %in% names(q8))) {
      none <- q8 |>
        dplyr::filter(stringr::str_detect(stringr::str_to_lower(device_label), "aucune|aucun")) |>
        dplyr::slice(1)
      if (nrow(none) == 1) {
        out <- c(out, paste0(
          "La part pondérée déclarant n'avoir suivi aucune des modalités proposées est de ",
          fmt_pct_report(none$pct_respondents_w), "."
        ))
      }
    }

    s <- sentence_from_top(
      q11, "item_label", "pct_agree_w",
      "Parmi les dimensions d'évaluation disponibles, les appréciations les plus favorables concernent : ", 3
    )
    if (!is.null(s)) out <- c(out, s)

    if (nrow(by_year) > 0) {
      out <- c(out, "Les écarts selon l'année de thèse sont présentés séparément afin de ne pas confondre exposition aux dispositifs et avancement dans le doctorat.")
    }
  }

  if (section == 2) {
    q5 <- report_read_table("connaissances_q5_items_gap")
    scores <- report_read_table("scores_par_exposition")

    s <- sentence_from_top(q5, "item_label", "pct_known_w", "Les notions ou outils les mieux connus sont : ", 4)
    if (!is.null(s)) out <- c(out, s)

    s <- sentence_from_top(
      q5, "item_label", "gap_pp",
      "Les écarts les plus importants entre connaissance et usage concernent : ", 4,
      value_fun = function(x) paste0(scales::number(x, accuracy = 0.1, decimal.mark = ","), " points")
    )
    if (!is.null(s)) out <- c(out, s)

    if (nrow(scores) > 0 && all(c("group", "score", "mean_w") %in% names(scores))) {
      gap <- group_difference_table(scores, "group", "score", "mean_w")
      q5_gap <- gap |>
        dplyr::filter(label %in% c("score_q5_known_well", "score_q5_used")) |>
        dplyr::arrange(dplyr::desc(abs_diff_pp))
      if (nrow(q5_gap) > 0) {
        out <- c(out, paste0(
          "La comparaison exposés/non exposés montre un écart de ",
          fmt_pp_report(q5_gap$diff_pp[1]), " sur ",
          dplyr::recode(q5_gap$label[1],
            score_q5_known_well = "le score de connaissance",
            score_q5_used = "le score d'usage",
            .default = q5_gap$label[1]
          ), "."
        ))
      }
    }
  }

  if (section == 3) {
    q4 <- report_read_table("pratiques_q4_items")
    q5 <- report_read_table("connaissances_q5_items_gap")

    s <- sentence_from_top(q4, "item_label", "pct_positive_w", "Les pratiques de recherche les plus souvent déclarées sont : ", 4)
    if (!is.null(s)) out <- c(out, s)

    s <- sentence_from_top(q5, "item_label", "pct_used_w", "Pour les outils de science ouverte, les usages les plus fréquents sont : ", 4)
    if (!is.null(s)) out <- c(out, s)

    out <- c(out, "Les analyses par année de thèse et par discipline sont présentées séparément afin d'identifier les pratiques qui dépendent davantage de l'avancement doctoral ou du contexte disciplinaire.")
  }

  if (section == 4) {
    q13 <- report_read_table("intentions_q13_par_exposition")

    if (nrow(q13) > 0) {
      diff_yes <- group_difference_table(q13, "exposure2", "item_label", "pct_yes_w")
      if (nrow(diff_yes) > 0) {
        x <- diff_yes |> dplyr::slice_head(n = 3)
        bits <- paste0(clean_report_label(x$label), " (", fmt_pp_report(x$diff_pp), ")")
        out <- c(out, paste0(
          "Les plus grands écarts d'intention entre répondants exposés à un dispositif organisé et répondants sans dispositif concernent : ",
          paste(bits, collapse = " ; "), "."
        ))
      }

      if ("pct_dk_w" %in% names(q13)) {
        q13_dk <- q13 |>
          dplyr::group_by(item_label) |>
          dplyr::summarise(pct_dk_w = mean(pct_dk_w, na.rm = TRUE), .groups = "drop")
        s <- sentence_from_top(q13_dk, "item_label", "pct_dk_w", "Les niveaux d'incertitude les plus élevés portent sur : ", 3)
        if (!is.null(s)) out <- c(out, s)
      }
    }
  }

  if (section == 5) {
    q15 <- report_read_table("perceptions_q15_par_exposition")
    q12 <- report_read_table("perceptions_q12_environnement_par_exposition")

    if (nrow(q15) > 0) {
      q15_mean <- q15 |>
        dplyr::group_by(item_label) |>
        dplyr::summarise(pct_agree_w = mean(pct_agree_w, na.rm = TRUE), .groups = "drop")
      s <- sentence_from_top(q15_mean, "item_label", "pct_agree_w", "Les représentations recueillant le plus d'accord sont : ", 4)
      if (!is.null(s)) out <- c(out, s)

      diff <- group_difference_table(q15, "exposure2", "item_label", "pct_agree_w")
      if (nrow(diff) > 0) {
        x <- diff |> dplyr::slice_head(n = 3)
        bits <- paste0(clean_report_label(x$label), " (", fmt_pp_report(x$diff_pp), ")")
        out <- c(out, paste0(
          "Les perceptions qui différencient le plus les deux groupes sont : ",
          paste(bits, collapse = " ; "), "."
        ))
      }
    }

    if (nrow(q12) > 0) {
      out <- c(out, "Les dimensions d'incitation et de frein de l'environnement sont analysées conjointement afin d'éviter de réduire le contexte institutionnel à un indicateur unique.")
    }
  }

  if (section == 6) {
    auto <- report_read_table("profils_non_formes_autoformes_scores")
    robust <- report_read_complement("score_robustness_summary")

    if (nrow(auto) > 0 && all(c("exposure3", "score_label", "mean_w") %in% names(auto))) {
      out <- c(out, "La comparaison entre non formés, autoformés et répondants exposés à un dispositif organisé distingue explicitement l'autoformation de la formation organisée.")
    }

    if (nrow(robust) > 0 && "conclusion" %in% names(robust)) {
      stable <- robust |>
        dplyr::filter(stringr::str_detect(stringr::str_to_lower(conclusion), "stable")) |>
        dplyr::slice_head(n = 4)
      if (nrow(stable) > 0 && "outcome_label" %in% names(stable)) {
        out <- c(out, paste0(
          "Les résultats les plus stables aux différentes spécifications concernent : ",
          paste(clean_report_label(stable$outcome_label), collapse = " ; "), "."
        ))
      }
    }

    out <- c(out, "La classification exploratoire est utilisée pour décrire des configurations de réponses ; elle n'est pas interprétée comme une typologie définitive des doctorants.")
  }

  if (section == 7) {
    balance <- report_read_complement("covariate_balance_exposed_nonexposed", subdir = "methodology")
    robust <- report_read_complement("score_robustness_summary")

    if (nrow(robust) > 0 && "conclusion" %in% names(robust)) {
      n_sensitive <- sum(stringr::str_detect(stringr::str_to_lower(robust$conclusion), "sensible"), na.rm = TRUE)
      n_stable <- sum(stringr::str_detect(stringr::str_to_lower(robust$conclusion), "stable"), na.rm = TRUE)
      out <- c(out, paste0(
        "Les tests de sensibilité distinguent ", n_stable,
        " indicateur(s) au signe stable et ", n_sensitive,
        " résultat(s) plus sensibles aux spécifications."
      ))
    }

    if (nrow(balance) > 0 && "imbalance_flag" %in% names(balance)) {
      n_imb <- sum(balance$imbalance_flag != "Équilibre acceptable", na.rm = TRUE)
      out <- c(out, paste0(
        "La comparaison de composition entre répondants exposés et non exposés identifie ",
        n_imb, " modalité(s) ou covariable(s) présentant un déséquilibre modéré ou fort."
      ))
    }

    out <- c(out, osyr_method_note())
  }

  unique(out[nzchar(out)])
}

section_summary_table <- function(section) {
  if (section == 1) {
    x <- report_read_final("q8_device_distribution_detail")
    if (nrow(x) > 0 && all(c("device_label", "pct_respondents_w") %in% names(x))) {
      return(x |>
        dplyr::arrange(dplyr::desc(pct_respondents_w)) |>
        dplyr::slice_head(n = 8) |>
        dplyr::transmute(
          `Dispositif déclaré` = clean_report_label(device_label),
          `Part pondérée` = fmt_pct_report(pct_respondents_w)
        ))
    }
  }

  if (section == 2) {
    x <- report_read_table("connaissances_q5_items_gap")
    if (nrow(x) > 0) {
      return(x |>
        dplyr::arrange(dplyr::desc(gap_pp)) |>
        dplyr::slice_head(n = 8) |>
        dplyr::transmute(
          `Notion ou outil` = clean_report_label(item_label),
          `Connaît bien` = fmt_pct_report(pct_known_w),
          `A déjà utilisé` = fmt_pct_report(pct_used_w),
          `Écart` = paste0(scales::number(gap_pp, accuracy = 0.1, decimal.mark = ","), " pts")
        ))
    }
  }

  if (section == 3) {
    x <- report_read_table("pratiques_q4_items")
    if (nrow(x) > 0) {
      return(x |>
        dplyr::arrange(dplyr::desc(pct_positive_w)) |>
        dplyr::slice_head(n = 8) |>
        dplyr::transmute(
          Pratique = clean_report_label(item_label),
          `Part pondérée` = fmt_pct_report(pct_positive_w)
        ))
    }
  }

  if (section == 4) {
    x <- report_read_table("intentions_q13_par_exposition")
    if (nrow(x) > 0) {
      diff <- group_difference_table(x, "exposure2", "item_label", "pct_yes_w")
      if (nrow(diff) > 0) {
        return(diff |>
          dplyr::slice_head(n = 8) |>
          dplyr::transmute(
            Intention = clean_report_label(label),
            `Sans dispositif` = fmt_pct_report(.data[["Aucun dispositif"]]),
            `Dispositif organisé` = fmt_pct_report(.data[["Dispositif organisé"]]),
            `Écart` = fmt_pp_report(diff_pp)
          ))
      }
    }
  }

  if (section == 5) {
    x <- report_read_table("perceptions_q15_par_exposition")
    if (nrow(x) > 0) {
      diff <- group_difference_table(x, "exposure2", "item_label", "pct_agree_w")
      if (nrow(diff) > 0) {
        return(diff |>
          dplyr::slice_head(n = 8) |>
          dplyr::transmute(
            Affirmation = clean_report_label(label),
            `Sans dispositif` = fmt_pct_report(.data[["Aucun dispositif"]]),
            `Dispositif organisé` = fmt_pct_report(.data[["Dispositif organisé"]]),
            `Écart` = fmt_pp_report(diff_pp)
          ))
      }
    }
  }

  if (section == 6) {
    x <- report_read_table("profils_non_formes_autoformes_scores")
    if (nrow(x) > 0 && all(c("exposure3", "score_label", "mean_w") %in% names(x))) {
      return(x |>
        dplyr::mutate(value = fmt_pct_report(mean_w)) |>
        dplyr::select(score_label, exposure3, value) |>
        tidyr::pivot_wider(names_from = exposure3, values_from = value) |>
        dplyr::rename(Indicateur = score_label))
    }
  }

  if (section == 7) {
    x <- report_read_complement("score_robustness_summary")
    if (nrow(x) > 0) {
      keep <- intersect(c("outcome_label", "median_estimate_pp", "min_estimate_pp", "max_estimate_pp", "conclusion"), names(x))
      return(x |> dplyr::select(dplyr::all_of(keep)) |> dplyr::slice_head(n = 10))
    }
  }

  tibble::tibble()
}

report_section_intro <- function(section, plan_row) {
  paste0(
    plan_row$objectif,
    " Les analyses présentées ici répondent aux questions suivantes : ",
    plan_row$questions_principales
  )
}

executive_summary_text <- function(plan) {
  purrr::map_chr(plan$section, function(sec) {
    txt <- section_summary_text(sec)
    if (length(txt) == 0) return(NA_character_)
    paste0(plan$bloc[plan$section == sec][1], " — ", txt[1])
  }) |>
    stats::na.omit() |>
    as.character()
}
