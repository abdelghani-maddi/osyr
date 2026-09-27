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

fmt_ci_pp_report <- function(est, low, high, accuracy = 0.1) {
  paste0(
    fmt_pp_report(100 * est, accuracy = accuracy),
    " (IC 95 % : ",
    scales::number(100 * low, accuracy = accuracy, decimal.mark = ","),
    " à ",
    scales::number(100 * high, accuracy = accuracy, decimal.mark = ","),
    " points)"
  )
}

model_term_row <- function(data, pattern) {
  if (!is.data.frame(data) || nrow(data) == 0 || !"term" %in% names(data)) return(tibble::tibble())
  data |>
    dplyr::filter(stringr::str_detect(term, pattern)) |>
    dplyr::slice_head(n = 1)
}

is_monotone_non_decreasing <- function(x) {
  x <- x[!is.na(x)]
  length(x) > 1 && all(diff(x) >= -1e-10)
}

section_summary_text <- function(section) {
  out <- character()

  if (section == 1) {
    q8 <- report_read_final("q8_device_distribution_detail")
    exposure <- report_read_table("plan_formation_exposition_globale")
    q9 <- report_read_table("plan_q9_organisateurs_global")
    q10 <- report_read_table("plan_q10_distribution_globale")
    q11 <- report_read_table("formation_evaluation_q11")
    q11_volume <- report_read_table("plan_q11_selon_volume_q10")
    q11_practice <- report_read_table("plan_q11_evaluation_et_pratique_reelle")
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

    if (nrow(q9) > 0) {
      s <- sentence_from_top(
        q9, "organizer_label", "pct_respondents_w",
        "Parmi les répondants disposant d'une réponse Q9, les organisateurs les plus souvent cités sont : ", 4
      )
      if (!is.null(s)) out <- c(out, s)
    }

    if (nrow(q10) > 0 && all(c("training_intensity", "pct_w") %in% names(q10))) {
      s <- sentence_from_top(
        q10, "training_intensity", "pct_w",
        "Le nombre de formations ou actions suivies se répartit principalement entre : ", 4
      )
      if (!is.null(s)) out <- c(out, s)
    }

    s <- sentence_from_top(
      q11, "item_label", "pct_agree_w",
      "Pour Q11, les niveaux d'accord les plus élevés concernent : ", 3
    )
    if (!is.null(s)) out <- c(out, s)

    if (nrow(exposure) > 0 && all(c("exposure3", "pct_w") %in% names(exposure))) {
      org <- exposure |> dplyr::filter(exposure3 == "Dispositif organisé") |> dplyr::slice(1)
      self <- exposure |> dplyr::filter(exposure3 == "Autoformation / autre seulement") |> dplyr::slice(1)
      none <- exposure |> dplyr::filter(exposure3 == "Aucun dispositif") |> dplyr::slice(1)
      if (nrow(org) == 1 && nrow(self) == 1 && nrow(none) == 1) {
        out <- c(out, paste0(
          "Au total, ", fmt_pct_report(org$pct_w),
          " des répondants sont classés dans la catégorie « dispositif organisé », ",
          fmt_pct_report(self$pct_w), " dans l'autoformation ou une autre modalité seulement, et ",
          fmt_pct_report(none$pct_w), " sans dispositif. Cette classification est analytique : Q8 reste une question multiréponse."
        ))
      }
    }

    if (nrow(q11_volume) >= 3 && all(c("training_intensity", "mean_w") %in% names(q11_volume))) {
      one <- q11_volume |> dplyr::filter(training_intensity == "1 formation/action") |> dplyr::slice(1)
      two <- q11_volume |> dplyr::filter(training_intensity == "2 ou 3 formations/actions") |> dplyr::slice(1)
      four <- q11_volume |> dplyr::filter(training_intensity == "4 formations/actions ou plus") |> dplyr::slice(1)
      if (nrow(one) == 1 && nrow(two) == 1 && nrow(four) == 1) {
        out <- c(out, paste0(
          "L'évaluation synthétique de la formation augmente avec le volume déclaré : ",
          fmt_pct_report(one$mean_w), " pour une action, ",
          fmt_pct_report(two$mean_w), " pour deux ou trois actions et ",
          fmt_pct_report(four$mean_w), " pour quatre actions ou plus. Ce gradient porte sur l'appréciation déclarée de la formation, pas sur un effet causal."
        ))
      }
    }

    if (nrow(q11_practice) > 1 && all(c("q11_quartile", "usage_q5_w") %in% names(q11_practice))) {
      vals <- q11_practice |> dplyr::arrange(q11_quartile) |> dplyr::pull(usage_q5_w)
      if (!is_monotone_non_decreasing(vals)) {
        out <- c(out, "En revanche, l'usage Q5 ne progresse pas de façon monotone avec les quartiles d'évaluation Q11. Une appréciation plus favorable de la formation ne se traduit donc pas mécaniquement par davantage d'usages déclarés.")
      }
    }

    if (nrow(by_year) > 0) {
      out <- c(out, "Les écarts selon l'année de thèse sont examinés séparément afin de distinguer ce qui relève de l'exposition aux dispositifs de ce qui accompagne simplement l'avancement dans le doctorat.")
    }
  }

  if (section == 2) {
    q5 <- report_read_table("connaissances_q5_items_gap")
    scores <- report_read_table("scores_par_exposition")
    focus <- report_read_table("plan_focus_non_formes_autoformes_organises")
    families <- report_read_table("plan_q5_familles_par_exposition")

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

    if (nrow(focus) > 0 && all(c("exposure3", "indicator", "mean_w") %in% names(focus))) {
      f <- focus |>
        dplyr::filter(indicator %in% c("score_q5_known_well", "score_q5_used")) |>
        dplyr::select(exposure3, indicator, mean_w) |>
        tidyr::pivot_wider(names_from = indicator, values_from = mean_w)
      a <- f |> dplyr::filter(exposure3 == "Autoformation / autre seulement") |> dplyr::slice(1)
      o <- f |> dplyr::filter(exposure3 == "Dispositif organisé") |> dplyr::slice(1)
      n <- f |> dplyr::filter(exposure3 == "Aucun dispositif") |> dplyr::slice(1)
      if (nrow(a) == 1 && nrow(o) == 1 && nrow(n) == 1) {
        out <- c(out, paste0(
          "Le groupe autoformé présente descriptivement des niveaux élevés de connaissance et d'usage (",
          fmt_pct_report(a$score_q5_known_well), " et ", fmt_pct_report(a$score_q5_used),
          "), proches ou supérieurs à ceux du groupe exposé à un dispositif organisé (",
          fmt_pct_report(o$score_q5_known_well), " et ", fmt_pct_report(o$score_q5_used),
          "), alors que les répondants sans dispositif se situent plus bas (",
          fmt_pct_report(n$score_q5_known_well), " et ", fmt_pct_report(n$score_q5_used),
          "). Ce profil est compatible avec un effet de sélection de l'autoformation et ne doit pas être lu comme une supériorité de cette modalité."
        ))
      }
    }

    if (nrow(families) > 0 && all(c("exposure3", "item_family", "knowledge_w", "usage_w") %in% names(families))) {
      datafam <- families |> dplyr::filter(item_family == "Données / FAIR / PGD")
      if (nrow(datafam) > 0) {
        mx <- max(datafam$knowledge_w, na.rm = TRUE)
        mu <- max(datafam$usage_w, na.rm = TRUE)
        out <- c(out, paste0(
          "Les objets liés aux données, à FAIR et aux plans de gestion restent moins diffusés que les infrastructures de publication ou les identifiants : même dans le groupe le plus élevé, la connaissance de cette famille atteint ",
          fmt_pct_report(mx), " et l'usage ", fmt_pct_report(mu), "."
        ))
      }
    }
  }

  if (section == 3) {
    q4 <- report_read_table("pratiques_q4_items")
    q5 <- report_read_table("connaissances_q5_items_gap")
    model <- report_read_table("plan_modele_pratiques_q5_elargi")
    training_model <- report_read_table("plan_modele_pratiques_q5_caracteristiques_formation")

    s <- sentence_from_top(q4, "item_label", "pct_positive_w", "Les pratiques de recherche les plus souvent déclarées sont : ", 4)
    if (!is.null(s)) out <- c(out, s)

    s <- sentence_from_top(q5, "item_label", "pct_used_w", "Pour les outils de science ouverte, les usages les plus fréquents sont : ", 4)
    if (!is.null(s)) out <- c(out, s)

    out <- c(out, "Les analyses par année de thèse et par discipline sont présentées séparément afin d'identifier les pratiques qui dépendent davantage de l'avancement doctoral ou du contexte disciplinaire.")

    if (nrow(model) > 0) {
      exp <- model_term_row(model, "^exposure2Dispositif organisé$")
      q4m <- model_term_row(model, "^score_q4_practices_research$")
      if (nrow(exp) == 1 && all(c("estimate", "conf.low", "conf.high") %in% names(exp))) {
        out <- c(out, paste0(
          "Dans le modèle élargi de l'usage Q5, l'association propre au fait d'être exposé à un dispositif organisé est de ",
          fmt_ci_pp_report(exp$estimate, exp$conf.low, exp$conf.high),
          ". L'intervalle comprend zéro : une fois pris en compte l'année, la discipline, la langue, les pratiques de recherche, l'environnement et les perceptions, l'exposition binaire ne suffit plus à résumer les différences d'usage."
        ))
      }
      if (nrow(q4m) == 1 && all(c("estimate", "conf.low", "conf.high") %in% names(q4m))) {
        out <- c(out, paste0(
          "Le score de pratiques de recherche Q4 est, en revanche, fortement associé à l'usage Q5 : un écart d'une unité sur ce score compris entre 0 et 1 correspond à ",
          fmt_ci_pp_report(q4m$estimate, q4m$conf.low, q4m$conf.high),
          " dans le modèle. Cela suggère que l'inscription effective dans des activités de recherche est un déterminant descriptif majeur des usages déclarés."
        ))
      }
    }

    if (nrow(training_model) > 0) {
      two <- model_term_row(training_model, "training_intensity2 ou 3")
      four <- model_term_row(training_model, "training_intensity4 formations")
      if (nrow(two) == 1 && nrow(four) == 1) {
        out <- c(out, paste0(
          "Parmi les répondants concernés par une formation, le volume est associé à l'usage : par rapport à une seule action, deux ou trois actions sont associées à ",
          fmt_ci_pp_report(two$estimate, two$conf.low, two$conf.high),
          " et quatre actions ou plus à ",
          fmt_ci_pp_report(four$estimate, four$conf.low, four$conf.high),
          ". Cette relation reste associative et peut refléter à la fois l'offre de formation et l'engagement préalable des doctorants."
        ))
      }
    }
  }

  if (section == 4) {
    q13 <- report_read_table("intentions_q13_par_exposition")
    q14 <- report_read_table("plan_q14_raisons_non_adoption")
    q14_global <- report_read_table("plan_q14_raisons_global_respondants")
    cumul <- report_read_table("plan_cumul_formation_environnement_connaissance_usage_intentions")
    intent_k <- report_read_table("plan_intentions_selon_connaissance")
    intent_u <- report_read_table("plan_intentions_selon_usage")
    intent_p <- report_read_table("plan_intentions_selon_pratiques_q4")
    intent_q7 <- report_read_table("plan_intentions_par_q7_group")

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

    if (nrow(q14_global) > 0 && all(c("reason_label", "pct_respondents_w") %in% names(q14_global))) {
      s <- sentence_from_top(
        q14_global, "reason_label", "pct_respondents_w",
        "Parmi les répondants ayant indiqué au moins une raison de non-adoption, les motifs les plus répandus sont : ", 4
      )
      if (!is.null(s)) out <- c(out, s)
    } else if (nrow(q14) > 0) {
      out <- c(out, "Les raisons Q14 sont analysées intention par intention, avec un dénominateur propre à chaque pratique ; elles ne sont pas moyennées entre intentions.")
    }

    if (nrow(intent_k) > 1 && nrow(intent_u) > 1 && nrow(intent_p) > 1) {
      k <- intent_k |> dplyr::arrange(knowledge_quartile)
      u <- intent_u |> dplyr::arrange(usage_quartile)
      p <- intent_p |> dplyr::arrange(practice_quartile)
      out <- c(out, paste0(
        "Les intentions augmentent avec l'acculturation et surtout avec l'usage : du premier au quatrième quartile, le score moyen passe de ",
        fmt_pct_report(k$mean_w[1]), " à ", fmt_pct_report(k$mean_w[nrow(k)]),
        " pour la connaissance, de ", fmt_pct_report(u$mean_w[1]), " à ", fmt_pct_report(u$mean_w[nrow(u)]),
        " pour l'usage Q5, et de ", fmt_pct_report(p$mean_w[1]), " à ", fmt_pct_report(p$mean_w[nrow(p)]),
        " pour les pratiques Q4."
      ))
    }

    if (nrow(intent_q7) > 0 && all(c("q7_group", "mean_w") %in% names(intent_q7))) {
      yes <- intent_q7 |> dplyr::filter(q7_group == "Oui") |> dplyr::slice(1)
      dk <- intent_q7 |> dplyr::filter(q7_group == "Je ne sais pas") |> dplyr::slice(1)
      if (nrow(yes) == 1 && nrow(dk) == 1) {
        out <- c(out, paste0(
          "Les répondants déclarant connaître la politique de science ouverte de leur établissement présentent aussi un score d'intentions plus élevé (",
          fmt_pct_report(yes$mean_w), ") que ceux qui répondent « je ne sais pas » à Q7 (",
          fmt_pct_report(dk$mean_w), ")."
        ))
      }
    }

    if (nrow(cumul) > 0 && all(c("cumulative_support", "mean_w") %in% names(cumul))) {
      low <- cumul |> dplyr::arrange(cumulative_support) |> dplyr::slice_head(n = 1)
      high <- cumul |> dplyr::arrange(dplyr::desc(cumulative_support)) |> dplyr::slice_head(n = 1)
      if (nrow(low) == 1 && nrow(high) == 1) {
        out <- c(out, paste0(
          "Le score moyen d'intentions passe de ",
          fmt_pct_report(low$mean_w), " lorsque ", low$cumulative_support,
          " condition(s) favorable(s) sont réunies à ",
          fmt_pct_report(high$mean_w), " lorsque ", high$cumulative_support,
          " condition(s) sont réunies ; cet indicateur reste descriptif."
        ))
      }
    }
  }

  if (section == 5) {
    q15 <- report_read_table("perceptions_q15_par_exposition")
    q15_overall <- report_read_table("plan_q15_accord_desaccord_global")
    q12 <- report_read_table("perceptions_q12_environnement_par_exposition")
    pmodels <- report_read_table("plan_modeles_perceptions_q15")
    unofficial <- report_read_table("plan_perceptions_selon_usage_plateformes_non_officielles")

    if (nrow(q15_overall) > 0 && all(c("item_label", "pct_agree_w") %in% names(q15_overall))) {
      q15_mean <- q15_overall
    } else if (nrow(q15) > 0) {
      q15_mean <- q15 |>
        dplyr::group_by(item_label) |>
        dplyr::summarise(pct_agree_w = mean(pct_agree_w, na.rm = TRUE), .groups = "drop")
    } else {
      q15_mean <- tibble::tibble()
    }

    if (nrow(q15_mean) > 0) {
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

    if (nrow(pmodels) > 0) {
      benef_use <- pmodels |> dplyr::filter(model == "Perception - score_q15_benefits", term == "score_q5_used") |> dplyr::slice(1)
      benef_env <- pmodels |> dplyr::filter(model == "Perception - score_q15_benefits", term == "score_q12_incitation") |> dplyr::slice(1)
      constr_frein <- pmodels |> dplyr::filter(model == "Perception - score_q15_constraints", term == "score_q12_frein") |> dplyr::slice(1)
      if (nrow(benef_use) == 1 && nrow(benef_env) == 1) {
        out <- c(out, paste0(
          "Dans les modèles ajustés, les bénéfices scientifiques perçus sont positivement associés à l'usage Q5 (",
          fmt_ci_pp_report(benef_use$estimate, benef_use$conf.low, benef_use$conf.high),
          ") et à un environnement plus incitatif (",
          fmt_ci_pp_report(benef_env$estimate, benef_env$conf.low, benef_env$conf.high), ")."
        ))
      }
      if (nrow(constr_frein) == 1) {
        out <- c(out, paste0(
          "Les contraintes institutionnelles ou économiques perçues sont, elles, fortement liées au score de frein Q12 : ",
          fmt_ci_pp_report(constr_frein$estimate, constr_frein$conf.low, constr_frein$conf.high),
          ". Les perceptions apparaissent ainsi structurées par l'expérience du contexte de recherche davantage que par la seule exposition aux dispositifs."
        ))
      }
    }

    if (nrow(unofficial) > 0 && all(c("unofficial_platform_used", "dimension", "mean_w") %in% names(unofficial))) {
      b <- unofficial |> dplyr::filter(dimension == "score_q15_benefits") |> dplyr::arrange(unofficial_platform_used)
      if (nrow(b) == 2) {
        out <- c(out, paste0(
          "Les usagers de plateformes d'accès non officielles déclarent davantage de bénéfices scientifiques associés à la science ouverte (",
          fmt_pct_report(b$mean_w[b$unofficial_platform_used %in% TRUE]), ") que les non-usagers (",
          fmt_pct_report(b$mean_w[b$unofficial_platform_used %in% FALSE]), "). L'item ne permet toutefois pas d'identifier un service particulier."
        ))
      }
    }
  }

  if (section == 6) {
    auto <- report_read_table("profils_non_formes_autoformes_scores")
    cah <- report_read_table("plan_cah_choix_nombre_classes")
    cah_scores <- report_read_table("plan_cah_caracterisation_scores")
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

    if (nrow(cah) > 0 && all(c("k", "silhouette") %in% names(cah))) {
      best <- cah |> dplyr::arrange(dplyr::desc(silhouette)) |> dplyr::slice_head(n = 1)
      out <- c(out, paste0(
        "La classification hiérarchique exploratoire retient ", best$k,
        " classes parmi les solutions de 2 à 6 classes. La silhouette moyenne vaut ",
        scales::number(best$silhouette, accuracy = 0.01, decimal.mark = ","),
        " : la séparation est donc utile pour explorer les configurations de réponses, mais reste modeste."
      ))
    }

    if (nrow(cah_scores) > 0 && all(c("cah_profile", "indicator", "mean_w") %in% names(cah_scores))) {
      risks <- cah_scores |> dplyr::filter(indicator == "score_q15_risks") |> dplyr::arrange(dplyr::desc(mean_w))
      if (nrow(risks) >= 2) {
        out <- c(out, paste0(
          "La principale opposition entre les deux classes hiérarchiques porte sur les risques individuels perçus : ",
          fmt_pct_report(risks$mean_w[1]), " dans ", risks$cah_profile[1],
          " contre ", fmt_pct_report(risks$mean_w[2]), " dans ", risks$cah_profile[2],
          ". Les niveaux de connaissance, d'usage et de pratiques sont beaucoup plus proches."
        ))
      }
    }

    out <- c(out, "Les classifications exploratoires décrivent des configurations de réponses ; elles ne sont pas interprétées comme des catégories stables hors de l'échantillon.")
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
  out <- character()

  exposure <- report_read_table("plan_formation_exposition_globale")
  if (nrow(exposure) > 0 && all(c("exposure3", "pct_w") %in% names(exposure))) {
    org <- exposure |> dplyr::filter(exposure3 == "Dispositif organisé") |> dplyr::slice(1)
    self <- exposure |> dplyr::filter(exposure3 == "Autoformation / autre seulement") |> dplyr::slice(1)
    if (nrow(org) == 1 && nrow(self) == 1) {
      out <- c(out, paste0(
        "L'exposition aux dispositifs est fréquente mais prend plusieurs formes : ",
        fmt_pct_report(org$pct_w), " des répondants relèvent d'un dispositif organisé et ",
        fmt_pct_report(self$pct_w), " de l'autoformation ou d'une autre modalité seulement."
      ))
    }
  }

  q5 <- report_read_table("connaissances_q5_items_gap")
  if (nrow(q5) > 0) {
    high <- q5 |> dplyr::arrange(dplyr::desc(pct_known_w)) |> dplyr::slice_head(n = 1)
    gap <- q5 |> dplyr::arrange(dplyr::desc(gap_pp)) |> dplyr::slice_head(n = 1)
    if (nrow(high) == 1 && nrow(gap) == 1) {
      out <- c(out, paste0(
        "La familiarisation est forte pour certains outils installés dans les routines académiques, notamment ",
        clean_report_label(high$item_label), " (", fmt_pct_report(high$pct_known_w),
        " de bonne connaissance déclarée). Elle ne se convertit pas toujours en usage : le plus grand écart connaissance-usage atteint ",
        scales::number(gap$gap_pp, accuracy = 0.1, decimal.mark = ","), " points pour ",
        clean_report_label(gap$item_label), "."
      ))
    }
  }

  model <- report_read_table("plan_modele_pratiques_q5_elargi")
  if (nrow(model) > 0) {
    exp <- model_term_row(model, "^exposure2Dispositif organisé$")
    q4m <- model_term_row(model, "^score_q4_practices_research$")
    if (nrow(exp) == 1 && nrow(q4m) == 1) {
      out <- c(out, paste0(
        "Pour les usages Q5, le modèle élargi met davantage en évidence l'inscription dans des pratiques de recherche que la seule exposition binaire aux dispositifs : le coefficient du score Q4 est ",
        fmt_ci_pp_report(q4m$estimate, q4m$conf.low, q4m$conf.high),
        ", tandis que l'estimation associée au dispositif organisé est ",
        fmt_ci_pp_report(exp$estimate, exp$conf.low, exp$conf.high), "."
      ))
    }
  }

  intent_u <- report_read_table("plan_intentions_selon_usage")
  q13 <- report_read_table("intentions_q13_par_exposition")
  if (nrow(intent_u) > 1) {
    u <- intent_u |> dplyr::arrange(usage_quartile)
    out <- c(out, paste0(
      "Les intentions d'ouverture sont plus élevées parmi les répondants qui utilisent déjà davantage les outils de science ouverte : le score moyen passe de ",
      fmt_pct_report(u$mean_w[1]), " dans le premier quartile d'usage à ",
      fmt_pct_report(u$mean_w[nrow(u)]), " dans le quatrième."
    ))
  } else if (nrow(q13) > 0) {
    out <- c(out, "Les intentions sont globalement favorables à l'ouverture des publications et de la thèse, mais plus hésitantes pour les données et le code.")
  }

  q15 <- report_read_table("plan_q15_accord_desaccord_global")
  if (nrow(q15) > 0) {
    top <- q15 |> dplyr::arrange(dplyr::desc(pct_agree_w)) |> dplyr::slice_head(n = 3)
    out <- c(out, paste0(
      "Les représentations sont largement favorables sur les bénéfices scientifiques : reproductibilité, coopération et intégrité recueillent les niveaux d'accord les plus élevés (",
      paste(fmt_pct_report(top$pct_agree_w), collapse = ", "), "). Les différences selon l'exposition aux dispositifs restent faibles sur ces items."
    ))
  }

  q7 <- report_read_table("plan_q7_connaissance_politique_etablissement")
  if (nrow(q7) > 0 && all(c("q7_group", "pct_w") %in% names(q7))) {
    dk <- q7 |> dplyr::filter(q7_group == "Je ne sais pas") |> dplyr::slice(1)
    if (nrow(dk) == 1) {
      out <- c(out, paste0(
        "La visibilité institutionnelle demeure un enjeu : ",
        fmt_pct_report(dk$pct_w), " des répondants déclarent ne pas savoir si leur établissement dispose d'une politique ou de directives en matière de science ouverte."
      ))
    }
  }

  out
}

discussion_summary_text <- function() {
  c(
    paste(
      "Pris ensemble, les résultats décrivent moins une opposition entre doctorants « favorables » et « défavorables » à la science ouverte qu'un continuum d'acculturation et de mise en pratique.",
      "Les bénéfices scientifiques sont largement reconnus, tandis que la maîtrise des instruments plus techniques - gestion des données, archivage du code, protocoles ou registres - reste plus inégale."
    ),
    paste(
      "La formation est associée à plusieurs dimensions de cette acculturation, mais son rôle n'est pas réductible au simple fait d'avoir été exposé ou non à un dispositif.",
      "Les analyses par volume, par format et par profil d'autoformation montrent des trajectoires différenciées ; les modèles ajustés invitent à tenir compte simultanément de l'avancement dans le doctorat, de la discipline, des pratiques de recherche déjà engagées et du contexte institutionnel."
    ),
    paste(
      "Le passage des dispositions aux pratiques apparaît comme un enjeu central.",
      "La connaissance des outils est fortement corrélée à leur usage, mais les écarts item par item demeurent substantiels ; les intentions sont elles-mêmes plus élevées chez les répondants qui connaissent, utilisent et pratiquent déjà davantage.",
      "L'enjeu n'est donc pas uniquement de convaincre de l'intérêt de l'ouverture, mais aussi de rendre les pratiques réalisables dans les situations concrètes de recherche."
    ),
    paste(
      "Les analyses de perceptions vont dans le même sens.",
      "Les jugements positifs sur la reproductibilité, la coopération ou l'intégrité sont largement partagés et varient peu selon l'exposition aux dispositifs.",
      "En revanche, les contraintes et les bénéfices perçus sont davantage liés à l'environnement de recherche et aux usages effectifs, ce qui souligne le rôle des conditions organisationnelles."
    ),
    paste(
      "Ces résultats doivent rester interprétés comme des associations observées dans une enquête déclarative.",
      "La pondération corrige la composition selon le schéma fourni, mais elle ne supprime ni l'auto-sélection dans les formations ni les facteurs non observés.",
      "Les analyses de profils et de mots spontanés sont exploratoires et servent surtout à formuler des hypothèses pour des analyses ou enquêtes ultérieures."
    )
  )
}
