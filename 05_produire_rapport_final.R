# =============================================================================
# SCRIPT 05 — PRODUIRE LE RAPPORT WORD FINAL OSYR
# Version 2026-09-27
# =============================================================================
# RÔLE DANS LE WORKFLOW
#   Assembler le rapport destiné à la lecture scientifique à partir des tables,
#   textes analytiques et figures validés en amont.
#
# CORRESPONDANCE AVEC LE PLAN
#   Le corps suit les sept blocs du plan de dépouillement. Les diagnostics
#   techniques et la matrice de couverture restent dans les tables de sortie et
#   ne sont pas présentés comme des résultats de recherche.
#
# RÈGLES ÉDITORIALES
#   - sélectionner un nombre limité de figures centrales ;
#   - séparer résultats, méthode et discussion ;
#   - conserver les notes méthodologiques utiles à l'interprétation sans
#     reprendre le vocabulaire interne du workflow ;
#   - placer les figures complémentaires dans l'annexe graphique.
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
# 1. Analyses finales et catalogues
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
    caption = as.character(caption),
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
# 2. Helpers Word
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
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }

  doc
}

add_cover <- function(doc) {
  # La couverture n'utilise pas les styles Heading : elle reste ainsi hors de la
  # numérotation et du sommaire automatique.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "OSYR",
        officer::fp_text(
          font.size = 24, bold = TRUE,
          color = cols[["dark_green"]]
        )
      )
    )
  )

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Rapport d'analyse de l'enquête auprès des doctorants",
        officer::fp_text(
          font.size = 18, bold = TRUE,
          color = cols[["dark_green"]]
        )
      )
    )
  )

  doc <- officer::body_add_par(
    doc,
    "Science ouverte, formations, connaissances, pratiques, intentions et perceptions",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    paste0("Version du ", format(Sys.Date(), "%d/%m/%Y")),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, " ", style = "Normal")
  doc <- officer::body_add_par(
    doc,
    "Ce rapport présente les résultats de l'enquête OSYR selon le plan de dépouillement arrêté en septembre 2026.",
    style = "Normal"
  )
  officer::body_add_break(doc)
}

add_native_toc <- function(doc) {
  # Sommaire Word natif. Il est construit à partir des Heading 1 uniquement afin
  # de conserver un sommaire court ; Word met à jour les numéros de page lors de
  # l'ouverture / actualisation des champs.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Sommaire",
        officer::fp_text(
          font.size = 18, bold = TRUE,
          color = cols[["dark_green"]]
        )
      )
    )
  )
  doc <- officer::body_add_toc(doc, level = 1)
  officer::body_add_break(doc)
}

# -----------------------------------------------------------------------------
# 3. Rapport principal
# -----------------------------------------------------------------------------

doc <- officer::read_docx()
doc <- add_cover(doc)
doc <- add_native_toc(doc)

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
    "Les modèles pondérés utilisent un design sans grappes déclarées (ids = 1) et des régressions linéaires ou linéaires de probabilité selon la nature de la variable.",
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
    "Elles servent à décrire des configurations de réponses et ne définissent pas des catégories stables de doctorants.",
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
# 4. Annexe graphique séparée
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
