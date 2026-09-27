# =============================================================================
# SCRIPT 05 — GÉNÉRER LE RAPPORT WORD OSYR
# Version : 27/09/2026
# =============================================================================
# RÔLE DANS LE WORKFLOW
#   Étape de publication. Ce script ne calcule aucun résultat : il lit les tables,
#   les textes déterministes et le catalogue de figures préparés en amont.
#
# LIVRABLES
#   1. outputs_osyr_rapport_final/rapport_final_osyr.docx
#   2. outputs_osyr_rapport_final/annexe_graphique_osyr.docx
#
# CORRESPONDANCE AVEC LE PLAN
#   Le rapport reprend les sept blocs du registre osyr_final_plan_registry().
#   La sélection des figures privilégie les résultats nécessaires à la lecture du
#   plan ; les diagnostics de production restent dans les CSV ou dans l'annexe.
#
# RÈGLES ÉDITORIALES
#   - aucune mention de type « figure utile », « sortie finale », « à rédiger » ;
#   - les notes sous figures sont des notes de lecture, pas des sous-titres
#     techniques de production ;
#   - les titres de couverture ne sont pas des styles de titre numérotés Word ;
#   - la discussion ne produit aucune nouvelle estimation.
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

pkgs <- c("tidyverse", "officer", "flextable", "fs", "glue", "scales")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

if (!file.exists("R/osyr_report_text.R")) stop("Fichier manquant : R/osyr_report_text.R")
source("R/osyr_report_text.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$report)
ensure_dir(file.path(dirs$report, "tables"))
ensure_dir(file.path(dirs$report, "figures"))

# -----------------------------------------------------------------------------
# 1. Préparer le catalogue des résultats publiables
# -----------------------------------------------------------------------------
# Aucune analyse n'est calculée ici. Le catalogue relie chaque fichier graphique
# à une section du plan, un titre, une note de lecture et une priorité.
# -----------------------------------------------------------------------------

catalog_final_path <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")

if (!file.exists(catalog_final_path)) {
  if (!file.exists("R/osyr_final_analyses.R")) {
    stop("Fichier manquant : R/osyr_final_analyses.R")
  }
  source("R/osyr_final_analyses.R")
}

plan_rapport <- osyr_final_plan_registry()
safe_write_csv(plan_rapport, file.path(dirs$report, "tables", "plan_rapport_final.csv"))

figure_catalog <- if (file.exists(catalog_final_path)) {
  readr::read_csv(catalog_final_path, show_col_types = FALSE)
} else {
  build_figure_catalog()
}

if (!"caption" %in% names(figure_catalog)) {
  figure_catalog$caption <- "Source : enquête OSYR, données pondérées."
}
if (!"source_dir" %in% names(figure_catalog)) figure_catalog$source_dir <- "rapport_final"
if (!"priorite" %in% names(figure_catalog)) figure_catalog$priorite <- 9

figure_catalog <- figure_catalog |>
  dplyr::filter(file != "final_60_couverture_plan_depouillement.png") |>
  dplyr::mutate(
    section = as.integer(section),
    path = as.character(path),
    caption = purrr::map2_chr(file, caption, osyr_publication_caption),
    available = !is.na(path) & file.exists(path)
  ) |>
  dplyr::filter(available) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$report, "tables", "catalogue_figures_rapport.csv"))

# Le corps du rapport privilégie les figures produites spécifiquement pour le
# rapport final, puis les figures de priorité 1. Le reste est placé en annexe.
main_figures <- figure_catalog |>
  dplyr::group_by(section) |>
  dplyr::arrange(
    dplyr::desc(source_dir == "rapport_final"),
    priorite,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 3) |>
  dplyr::ungroup()

appendix_figures <- figure_catalog |>
  dplyr::anti_join(main_figures |> dplyr::select(section, file), by = c("section", "file"))

safe_write_csv(main_figures, file.path(dirs$report, "tables", "figures_rapport_principal.csv"))
safe_write_csv(appendix_figures, file.path(dirs$report, "tables", "figures_annexe.csv"))

# -----------------------------------------------------------------------------
# 2. Fonctions de mise en page Word
# -----------------------------------------------------------------------------
# Ces fonctions gèrent les tableaux, les paragraphes, les figures et la
# couverture. Elles ne doivent contenir aucune logique statistique.
# -----------------------------------------------------------------------------

add_section_table <- function(doc, data, max_rows = 10) {
  if (!is.data.frame(data) || nrow(data) == 0) return(doc)

  data <- data |> dplyr::slice_head(n = max_rows)

  ft <- flextable::flextable(data) |>
    style_flextable_osyr()

  flextable::body_add_flextable(doc, value = ft)
}

add_text_paragraphs <- function(doc, paragraphs) {
  if (length(paragraphs) == 0) return(doc)
  for (txt in paragraphs) {
    if (!is.na(txt) && nzchar(txt)) {
      doc <- officer::body_add_par(doc, txt, style = "Normal")
    }
  }
  doc
}

figure_dimensions <- function(title, file) {
  key <- stringr::str_to_lower(paste(title, file))

  if (stringr::str_detect(key, "discipline|q5_connaissance_usage|balance_covariables|profils_acp")) {
    return(c(width = 6.55, height = 5.35))
  }

  if (stringr::str_detect(key, "heatmap|q8|profil|environnement|robustesse")) {
    return(c(width = 6.45, height = 5.05))
  }

  c(width = 6.35, height = 4.65)
}

add_figure_if_exists <- function(doc, path, title, caption = NULL) {
  if (is.na(path) || !file.exists(path)) return(doc)

  dims <- figure_dimensions(title, basename(path))

  doc <- officer::body_add_par(doc, title, style = "heading 3")
  doc <- officer::body_add_img(
    doc,
    src = path,
    width = unname(dims["width"]),
    height = unname(dims["height"])
  )

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          caption,
          officer::fp_text(
            font.size = 8.5,
            italic = TRUE,
            color = cols[["grey"]]
          )
        )
      )
    )
  }

  doc
}

add_cover <- function(doc) {
  # La couverture utilise des paragraphes mis en forme, et non les styles
  # heading 1/2, afin d'éviter une numérotation automatique du type « 1. OSYR ».
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "OSYR",
        officer::fp_text(font.size = 30, bold = TRUE, color = cols[["dark_green"]])
      )
    )
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Rapport d'analyse de l'enquête auprès des doctorants",
        officer::fp_text(font.size = 20, bold = TRUE, color = cols[["black"]])
      )
    )
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Science ouverte, formations, connaissances, pratiques, intentions et perceptions",
        officer::fp_text(font.size = 12.5, color = cols[["grey"]])
      )
    )
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste0("Version du ", format(Sys.Date(), "%d/%m/%Y")),
        officer::fp_text(font.size = 10.5, color = cols[["grey"]])
      )
    )
  )
  doc <- officer::body_add_par(doc, " ", style = "Normal")
  doc <- officer::body_add_par(
    doc,
    paste(
      "Le rapport présente les résultats de l'enquête OSYR selon le plan de dépouillement de septembre 2026.",
      "Les tableaux détaillés, diagnostics de robustesse et figures complémentaires restent disponibles dans les sorties du workflow et dans l'annexe graphique."
    ),
    style = "Normal"
  )
  officer::body_add_break(doc)
}

# -----------------------------------------------------------------------------
# 3. Rapport principal
# -----------------------------------------------------------------------------
# Ordre : couverture, résumé exécutif, méthode, sept blocs de résultats,
# discussion et conclusion.
# -----------------------------------------------------------------------------

doc <- officer::read_docx()
doc <- add_cover(doc)

# Résumé
summary_lines <- executive_summary_text(plan_rapport)
doc <- officer::body_add_par(doc, "Résumé des principaux résultats", style = "heading 1")
doc <- add_text_paragraphs(doc, summary_lines)

# Méthode
doc <- officer::body_add_par(doc, "Méthode et principes d'analyse", style = "heading 1")

doc <- officer::body_add_par(doc, "Pondération et population d'analyse", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "La base comporte une seule variable de pondération, Poids, recodée en .weight.",
    "Les analyses pondérées excluent les observations dont le poids est manquant, nul ou négatif ; elles ne leur attribuent pas un poids de remplacement.",
    "Les analyses non pondérées utilisées dans les tests de sensibilité reposent sur une constante technique égale à 1 et ne constituent pas une seconde pondération."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Exposition aux dispositifs", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "Q8 est une question multiréponse. Les cinq modalités explicitement formulées comme formation, atelier, séminaire ou parcours de formation asynchrone dans la DATAMAP sont classées comme dispositifs organisés.",
    "L'autoformation documentaire et la modalité « autres » sont distinguées dans un profil séparé lorsque le répondant ne déclare aucun dispositif organisé.",
    "Le code « aucune de ces propositions » définit l'absence de dispositif ; une réponse contradictoire combinant « aucune » avec une autre modalité est classée comme indéterminée.",
    "Les pourcentages détaillés de Q8 utilisent comme dénominateur les répondants ayant fourni au moins une réponse valide à la question."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Dénominateurs et valeurs manquantes", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les proportions sont calculées parmi les réponses valides pour l'indicateur considéré.",
    "Les non-réponses et les codes hors champ ne sont pas assimilés à des réponses négatives.",
    "Pour Q13, « je ne sais pas » est conservé comme modalité analytique distincte ; pour les autres batteries, les modalités qui ne participent pas à l'indicateur sont exclues du dénominateur correspondant.",
    "Pour les questions multiréponses, notamment Q8, Q9 et Q14, la somme des pourcentages peut dépasser 100 %."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Variables synthétiques", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les scores Q4, Q5, Q11, Q12, Q13 et Q15 sont des proportions individuelles comprises entre 0 et 1, calculées sur les items valides de chaque répondant.",
    "Q15 est présenté selon trois dimensions distinctes : bénéfices scientifiques, contraintes institutionnelles ou économiques et risques individuels.",
    "Les regroupements de Q5 servent à décrire des familles d'objets ; ils ne remplacent pas les analyses item par item."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Comparaisons et modèles", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les descriptifs sont complétés par des comparaisons selon l'année de thèse, la discipline, la langue du questionnaire et l'exposition aux dispositifs.",
    "Les modèles pondérés utilisent un design sans grappes déclarées (ids = 1). Les scores sont analysés par régression linéaire ; les indicateurs binaires par modèle linéaire de probabilité lorsqu'un écart en points est recherché ; les modèles spécifiques de présence des dispositifs Q8 sont logistiques et restitués en odds ratios.",
    "Les modèles centraux ajustent au minimum sur l'année de thèse, la discipline et la langue ; des modèles élargis introduisent également les pratiques de recherche, l'environnement et les perceptions lorsque ces variables répondent à la question étudiée.",
    "Les observations incomplètes sur les variables d'un modèle sont exclues de ce modèle ; l'effectif utilisé est reporté avec les résultats."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Robustesse et multiplicité", style = "heading 2")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les principales associations sont comparées avec et sans pondération et avec deux niveaux de regroupement disciplinaire.",
    "Pour les batteries d'items et les modèles comportant de nombreux coefficients, les valeurs p sont complétées par une correction de Benjamini-Hochberg afin de limiter les faux positifs liés aux comparaisons multiples.",
    "Les intervalles de confiance à 95 % sont privilégiés pour apprécier l'incertitude autour des estimations."
  ),
  style = "Normal"
)

doc <- officer::body_add_par(doc, "Portée des résultats", style = "heading 2")
doc <- officer::body_add_par(doc, osyr_method_note(), style = "Normal")
doc <- officer::body_add_par(
  doc,
  paste(
    "Les analyses de profils, l'ACP, le k-means, la classification hiérarchique et l'analyse lexicale sont exploratoires.",
    "L'ACP utilise la pondération dans le centrage, la standardisation et la covariance ; les classifications restent des regroupements exploratoires d'individus et sont caractérisées ensuite avec les poids.",
    "Ces analyses servent à décrire des configurations de réponses et ne définissent pas des catégories stables de doctorants.",
    "Le dictionnaire utilisé pour les mots spontanés Q3 reste un outil de classement provisoire tant qu'une validation manuelle n'a pas été menée."
  ),
  style = "Normal"
)

# Sections de résultats
for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, sec_info$bloc, style = "heading 1")
  doc <- officer::body_add_par(doc, report_section_intro(sec, sec_info), style = "Normal")

  # Résultats factuels
  section_text <- section_summary_text(sec)
  if (length(section_text) > 0) {
    doc <- officer::body_add_par(doc, "Principaux résultats", style = "heading 2")
    doc <- add_text_paragraphs(doc, section_text)
  }

  # Tableau synthétique
  section_table <- section_summary_table(sec)
  if (is.data.frame(section_table) && nrow(section_table) > 0) {
    doc <- officer::body_add_par(doc, "Tableau de synthèse", style = "heading 2")
    doc <- add_section_table(doc, section_table, max_rows = 10)
  }

  # Figures principales uniquement
  figs <- main_figures |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(dplyr::desc(source_dir == "rapport_final"), priorite, titre)

  if (nrow(figs) > 0) {
    # Les figures principales commencent sur une nouvelle page. À partir de la
    # deuxième figure, chaque graphique dispose de sa propre page afin que les
    # libellés restent lisibles une fois le document affiché ou imprimé.
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "Figures", style = "heading 2")

    for (i in seq_len(nrow(figs))) {
      if (i > 1) doc <- officer::body_add_break(doc)

      doc <- add_figure_if_exists(
        doc,
        figs$path[i],
        figs$titre[i],
        figs$caption[i]
      )
    }
  }
}

# Discussion et conclusion
doc <- officer::body_add_break(doc)
doc <- officer::body_add_par(doc, "Discussion et conclusion", style = "heading 1")
doc <- add_text_paragraphs(doc, discussion_summary_text())

out_docx <- file.path(dirs$report, "rapport_final_osyr.docx")
print(doc, target = out_docx)

# -----------------------------------------------------------------------------
# 4. Annexe graphique
# -----------------------------------------------------------------------------
# L'annexe conserve les figures non retenues dans le corps principal sans
# transformer les diagnostics internes en conclusions scientifiques.
# -----------------------------------------------------------------------------

annex <- officer::read_docx()
annex <- officer::body_add_par(annex, "OSYR — Annexe graphique", style = "heading 1")
annex <- officer::body_add_par(
  annex,
  "Figures complémentaires produites par le workflow et non retenues dans le corps principal du rapport.",
  style = "Normal"
)

for (sec in sort(unique(appendix_figures$section))) {
  sec_name <- plan_rapport$bloc[match(sec, plan_rapport$section)][1]
  annex <- officer::body_add_break(annex)
  annex <- officer::body_add_par(annex, sec_name, style = "heading 1")

  figs <- appendix_figures |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(priorite, titre)

  for (i in seq_len(nrow(figs))) {
    annex <- add_figure_if_exists(annex, figs$path[i], figs$titre[i], figs$caption[i])
  }
}

annex_path <- file.path(dirs$report, "annexe_graphique_osyr.docx")
print(annex, target = annex_path)

message("Rapport final généré : ", normalizePath(out_docx, mustWork = FALSE))
message("Annexe graphique générée : ", normalizePath(annex_path, mustWork = FALSE))
message("Figures dans le rapport principal : ", nrow(main_figures))
message("Figures en annexe : ", nrow(appendix_figures))
