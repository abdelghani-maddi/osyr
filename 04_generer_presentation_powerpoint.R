# =============================================================================
# SCRIPT 04 — GÉNÉRATION DU POWERPOINT OSYR
# =============================================================================
# Version v3 — 06/07/2026
#
# Ce script assemble les sorties du script 03 dans un PPT. Il n'a plus besoin
# du package `rvg`, car les figures sont insérées comme images PNG. Cela évite
# l'erreur observée lorsque RStudio essaie d'installer `rvg` alors que des
# packages sont déjà chargés (j'ai un peu galéré pour trouver celle-ci :)).

# =============================================================================
# Rôle dans le workflow
#   Ce script construit une présentation PowerPoint à partir des analyses
#   complémentaires. La logique est : une diapo = une question = une figure/table
#   = un message à retenir.
#
# Prérequis :
#   Le script 03 doit avoir produit `outputs_osyr_v2_complements_30062026/`.
#   Si les sorties n'existent pas, le script essaie de lancer le script 03.
#
# À lancer seul :
#   source("04_generer_presentation_powerpoint.R")
#
# Sortie principale :
#   outputs_osyr_v2_ppt/OSYR_exploration_premium_v2_complements_30062026.pptx
# =============================================================================

# =============================================================================
# OSYR — PowerPoint premium v2 avec analyses complémentaires WP2 30/06/2026
# Version : 2026-06-30
#
# À lancer depuis la racine du projet :
#   source("04_generer_presentation_powerpoint.R")
#
# Ce script :
#   1) lance le workflow complémentaire si besoin ;
#   2) génère un PPT propre centré sur les nouvelles questions discutées ;
#   3) garde une logique : 1 question / 1 figure ou table / 1 message.
# =============================================================================

required_packages <- c("tidyverse", "officer", "flextable", "fs", "glue", "scales")
missing_required <- required_packages[
  !purrr::map_lgl(required_packages, requireNamespace, quietly = TRUE)
]
if (length(missing_required) > 0) {
  message("Packages manquants pour le PowerPoint : ", paste(missing_required, collapse = ", "))
  message("Installation automatique en cours. Si RStudio indique 'Updating loaded packages', redémarrer R puis relancer le script.")
  install.packages(missing_required, dependencies = TRUE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

run_complement_workflow_if_missing <- TRUE
complement_script <- "03_analyses_complementaires_wp2_30062026.R"

out_root <- "outputs_osyr_v2_complements_30062026"
fig_dir <- file.path(out_root, "figures")
tab_dir <- file.path(out_root, "tables")
method_dir <- file.path(out_root, "methodology")
exports_dir <- file.path(out_root, "exports")

ppt_dir <- "outputs_osyr_v2_ppt"
fs::dir_create(ppt_dir)
ppt_path <- file.path(ppt_dir, "OSYR_exploration_premium_v2_complements_30062026.pptx")

if (run_complement_workflow_if_missing &&
    (!dir.exists(out_root) || length(list.files(fig_dir, pattern = "\\.png$", full.names = TRUE)) == 0)) {
  if (file.exists(complement_script)) {
    source(complement_script)
  } else {
    stop("Le script complémentaire est introuvable : ", complement_script)
  }
}

read_csv_safe <- function(path) {
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else tibble::tibble()
}

summary_tbl <- read_csv_safe(file.path(exports_dir, "complement_summary_for_ppt.csv"))
registry_tbl <- read_csv_safe(file.path(method_dir, "analysis_registry_30062026.csv"))

# -----------------------------------------------------------------------------
# 2. Style
# -----------------------------------------------------------------------------

cols <- c(
  navy = "#17324D",
  teal = "#2A9D8F",
  coral = "#E76F51",
  orange = "#F4A261",
  blue = "#3B82F6",
  purple = "#7B2CBF",
  grey = "#667085",
  light = "#F2F4F7",
  white = "#FFFFFF"
)

ppt <- officer::read_pptx()
ppt <- officer::layout_summary(ppt) |>
  dplyr::slice(1) |>
  dplyr::pull(layout) |>
  (\(layout_name) officer::read_pptx())()

# Format 16:9 par défaut.
slide_w <- 13.333
slide_h <- 7.5

txt <- function(size = 18, bold = FALSE, color = cols[["navy"]]) {
  officer::fp_text(font.size = size, bold = bold, color = color, font.family = "Aptos")
}

add_footer <- function(ppt, page = NULL) {
  officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext("OSYR — analyses complémentaires WP2 30/06/2026", txt(size = 8.5, color = cols[["grey"]]))
    ),
    location = officer::ph_location(left = 0.55, top = 7.15, width = 7.5, height = 0.25)
  ) |>
    officer::ph_with(
      value = officer::fpar(
        officer::ftext(ifelse(is.null(page), "", as.character(page)), txt(size = 8.5, color = cols[["grey"]]))
      ),
      location = officer::ph_location(left = 12.15, top = 7.15, width = 0.6, height = 0.25)
    )
}

add_title <- function(ppt, title, subtitle = NULL) {
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(title, txt(size = 28, bold = TRUE))),
    location = officer::ph_location(left = 0.55, top = 0.32, width = 12.1, height = 0.55)
  )
  if (!is.null(subtitle) && subtitle != "") {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext(subtitle, txt(size = 14.5, color = cols[["grey"]]))),
      location = officer::ph_location(left = 0.57, top = 0.93, width = 12.1, height = 0.42)
    )
  }
  ppt
}

add_takeaway <- function(ppt, text) {
  ppt |>
    officer::ph_with(
      value = "",
      location = officer::ph_location(left = 0.55, top = 6.58, width = 12.2, height = 0.43),
      bg = cols[["light"]]
    ) |>
    officer::ph_with(
      value = officer::fpar(
        officer::ftext("À retenir — ", txt(size = 10.5, bold = TRUE, color = cols[["navy"]])),
        officer::ftext(text, txt(size = 10.5, color = cols[["navy"]]))
      ),
      location = officer::ph_location(left = 0.75, top = 6.67, width = 11.8, height = 0.28)
    )
}

add_bullets <- function(ppt, bullets, left = 0.85, top = 1.65, width = 11.7, height = 4.8, size = 17) {
  pars <- purrr::map(bullets, \(b) officer::fpar(
    officer::ftext("• ", txt(size = size, bold = TRUE, color = cols[["teal"]])),
    officer::ftext(b, txt(size = size, color = cols[["navy"]]))
  ))
  officer::ph_with(
    ppt,
    value = do.call(officer::block_list, pars),
    location = officer::ph_location(left = left, top = top, width = width, height = height)
  )
}

add_figure_slide <- function(ppt, title, subtitle, fig_path, takeaway, page) {
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- add_title(ppt, title, subtitle)

  if (file.exists(fig_path)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::external_img(fig_path),
      location = officer::ph_location(left = 0.7, top = 1.35, width = 11.95, height = 5.05)
    )
  } else {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext("Figure non disponible", txt(size = 24, bold = TRUE, color = cols[["grey"]]))),
      location = officer::ph_location(left = 0.7, top = 2.8, width = 12, height = 0.6)
    )
  }

  ppt <- add_takeaway(ppt, takeaway)
  add_footer(ppt, page)
}

add_table_slide <- function(ppt, title, subtitle, table_path, takeaway, page, max_rows = 8, max_cols = 5) {
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- add_title(ppt, title, subtitle)

  data <- read_csv_safe(table_path)
  if (nrow(data) > 0) {
    data <- data |>
      dplyr::slice_head(n = max_rows) |>
      dplyr::select(1:min(max_cols, ncol(.)))

    ft <- flextable::flextable(data)
    ft <- flextable::theme_vanilla(ft)
    ft <- flextable::fontsize(ft, size = 8.5, part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::bg(ft, bg = cols[["light"]], part = "header")
    ft <- flextable::color(ft, color = cols[["navy"]], part = "header")
    ft <- flextable::autofit(ft)
    if ("fit_to_width" %in% getNamespaceExports("flextable")) {
      ft <- flextable::fit_to_width(ft, max_width = 11.8)
    }

    ppt <- officer::ph_with(
      ppt,
      value = ft,
      location = officer::ph_location(left = 0.65, top = 1.45, width = 12, height = 4.9)
    )
  } else {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext("Table non disponible", txt(size = 24, bold = TRUE, color = cols[["grey"]]))),
      location = officer::ph_location(left = 0.7, top = 2.8, width = 12, height = 0.6)
    )
  }

  ppt <- add_takeaway(ppt, takeaway)
  add_footer(ppt, page)
}

add_section_slide <- function(ppt, number, title, subtitle, page) {
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h),
    bg = cols[["navy"]]
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(number, txt(size = 36, bold = TRUE, color = cols[["teal"]]))),
    location = officer::ph_location(left = 0.85, top = 2.1, width = 2.0, height = 0.8)
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(title, txt(size = 31, bold = TRUE, color = cols[["white"]]))),
    location = officer::ph_location(left = 0.85, top = 3.0, width = 11.6, height = 0.8)
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(subtitle, txt(size = 16, color = "#D0D5DD"))),
    location = officer::ph_location(left = 0.85, top = 3.85, width = 11.4, height = 0.6)
  )
  add_footer(ppt, page)
}

# -----------------------------------------------------------------------------
# 3. Slides
# -----------------------------------------------------------------------------

page <- 1

# Titre
ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- officer::ph_with(
  ppt, value = "", location = officer::ph_location(left = 0, top = 0, width = slide_w, height = slide_h),
  bg = cols[["navy"]]
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("OSYR", txt(size = 42, bold = TRUE, color = cols[["white"]]))),
  location = officer::ph_location(left = 0.75, top = 1.05, width = 11.5, height = 0.85)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("Analyses complémentaires après réunion WP2", txt(size = 26, bold = TRUE, color = cols[["teal"]]))),
  location = officer::ph_location(left = 0.75, top = 2.1, width = 12, height = 0.65)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("Pondérations, significativité, autoformation, dispositifs, langue, cadrages cognitifs et prochaines analyses", txt(size = 15.5, color = "#D0D5DD"))),
  location = officer::ph_location(left = 0.75, top = 2.95, width = 11.7, height = 0.6)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext(paste0("Document généré automatiquement le ", format(Sys.Date(), "%d/%m/%Y")), txt(size = 11, color = "#D0D5DD"))),
  location = officer::ph_location(left = 0.75, top = 6.75, width = 11.7, height = 0.3)
)
page <- page + 1

# Objet
ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- add_title(ppt, "Ce que cette version ajoute", "Traduction opérationnelle des remarques et hypothèses formulées en réunion.")
ppt <- add_bullets(
  ppt,
  c(
    "Documenter les recodages et tester la sensibilité aux pondérations.",
    "Ajouter des intervalles de confiance et des tests de significativité sur les écarts.",
    "Distinguer connaissance déclarée, usages réels, pratiques de recherche déjà possibles et intentions.",
    "Traiter les catégories souvent invisibilisées : je ne sais pas, non, absences de réponses.",
    "Analyser les autoformés, les dispositifs Q8-Q11, la langue du questionnaire et les cadrages cognitifs des trois mots.",
    "Relier environnement incitatif, freins, représentations et connaissance de la politique d'établissement."
  ),
  size = 16.3
)
ppt <- add_takeaway(ppt, "La présentation ne remplace pas le workflow principal : elle ajoute une couche de validation méthodologique et d'hypothèses à tester.")
ppt <- add_footer(ppt, page); page <- page + 1

# Roadmap
ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- add_title(ppt, "Nouvelle feuille de route analytique", "Chaque bloc répond à une critique ou piste formulée en réunion.")
steps <- c(
  "1. Méthode : recodages, pondérations, regroupements disciplinaires",
  "2. Validité : intervalles de confiance et modèles ajustés",
  "3. Action : connaissance → usage → pratique → intention",
  "4. Incertitude : je ne sais pas / non / missing",
  "5. Exposition : autoformation, Q8-Q11, nombre et type de dispositifs",
  "6. Contextes : langue, établissement, discipline",
  "7. Cadrages : lexicométrie et alignement cognitif",
  "8. Synthèse : résultats robustes vs pistes exploratoires"
)
ppt <- add_bullets(ppt, steps, size = 15.5)
ppt <- add_takeaway(ppt, "L'enjeu est de passer d'une exploration descriptive à une stratégie analytique défendable méthodologiquement.")
ppt <- add_footer(ppt, page); page <- page + 1

# Section méthode
ppt <- add_section_slide(ppt, "01", "Robustesse méthodologique", "Pondérations, discipline et tests de sensibilité.", page); page <- page + 1

figure_plan <- tibble::tribble(
  ~title, ~subtitle, ~file, ~takeaway,
  "Les résultats dépendent-ils du poids utilisé ?", "Effets ajustés selon les scénarios de pondération.", "01_sensibilite_ponderations_modeles.png", "Si les coefficients restent proches, les conclusions sont moins susceptibles d'être un artefact de pondération.",
  "Le regroupement disciplinaire change-t-il les conclusions ?", "Comparaison discipline regroupée vs discipline détaillée.", "02_sensibilite_regroupement_disciplinaire.png", "Cette analyse répond directement à la critique possible sur le choix des regroupements disciplinaires.",
  "Le gap connaissance → usage est central", "Écart entre familiarité déclarée et usage déclaré.", "03_gap_connaissance_usage_global.png", "Le cœur de l'analyse n'est pas seulement l'adhésion à la SO, mais la conversion en pratiques.",
  "Le gap existe-t-il aussi chez les non exposés ?", "Écart connaissance - usage par exposition.", "04_gap_connaissance_usage_par_exposition.png", "Permet de tester l'hypothèse d'une acculturation élevée mais d'une mise en action limitée.",
  "Tenir compte des pratiques de recherche déjà possibles", "Usage selon année, exposition et pratiques Q4.", "05_usages_par_annee_pratiques_q4_exposition.png", "Les premières années ont moins d'occasions concrètes : les usages doivent être lus à activité de recherche donnée.",
  "Qui répond « je ne sais pas », « non » ou ne répond pas ?", "Modalités d'incertitude et de non-projection.", "06_je_ne_sais_pas_non_missing_par_exposition.png", "Ces catégories sont analytiquement importantes pour comprendre la difficulté à se projeter.",
  "Les autoformés constituent-ils un profil à part ?", "Scores moyens selon les trois catégories d'exposition.", "07_profil_autoformes_scores.png", "L'autoformation ne doit pas être absorbée trop vite dans les non exposés ou les formés.",
  "Quels dispositifs sont effectivement déclarés ?", "Mentions pondérées des dispositifs Q8-Q11.", "08_dispositifs_q8_q11_mentions.png", "On passe d'une variable d'exposition générale à une analyse des dispositifs concrets.",
  "Le questionnaire anglais est-il lié à certains établissements ?", "Part du questionnaire anglais par établissement ou collège.", "09_langue_questionnaire_par_etablissement.png", "La langue doit rester un proxy : cette figure aide à tester l'hypothèse d'une offre de formation en anglais.",
  "Environnement incitatif et freins peuvent coexister", "Lien entre incitation et frein selon exposition.", "10_paradoxe_environnement_incitatif_frein.png", "La formation peut rendre l'environnement plus lisible, y compris dans ses limites.",
  "La politique d'établissement joue-t-elle un rôle ?", "Q7 et environnement incitatif.", "11_q7_politique_etablissement_environnement.png", "Q7 permet de distinguer exposition aux dispositifs et connaissance du cadre institutionnel.",
  "Quels cadrages spontanés de la science ouverte ?", "Concepts détectés dans les trois mots.", "12_q3_cadrages_spontanes_concepts.png", "Le dictionnaire est exploratoire et peut être remplacé par l'annotation manuelle.",
  "Les formations déplacent-elles le vocabulaire ?", "Différence de concepts exposés moins non exposés.", "13_q3_cadrage_exposition_valeurs_operationnel.png", "Permet de tester le passage d'un vocabulaire de valeurs à un vocabulaire plus opérationnel.",
  "Cadrage cognitif et usage sont-ils alignés ?", "Usage moyen selon type de cadrage.", "14_alignement_cadrage_cognitif_usage.png", "Une piste forte : relier ce que les doctorants associent à la SO et ce qu'ils déclarent pratiquer.",
  "Quels univers sémantiques coexistent ?", "Réseau de cooccurrences des concepts Q3.", "15_reseau_cooccurrences_cadrages_q3.png", "Le réseau aide à repérer les familles de sens autour de la science ouverte."
)

for (i in seq_len(nrow(figure_plan))) {
  row <- figure_plan[i, ]
  if (i == 3) {
    ppt <- add_section_slide(ppt, "02", "Connaissance, usage et mise en action", "Du déclaratif aux pratiques effectives.", page); page <- page + 1
  }
  if (i == 7) {
    ppt <- add_section_slide(ppt, "03", "Exposition et dispositifs concrets", "Autoformation, Q8-Q11, langue et établissement.", page); page <- page + 1
  }
  if (i == 10) {
    ppt <- add_section_slide(ppt, "04", "Perceptions et environnement", "Incitation, freins, politique d'établissement.", page); page <- page + 1
  }
  if (i == 12) {
    ppt <- add_section_slide(ppt, "05", "Cadrages cognitifs", "Lexicométrie, trois mots et alignement avec les pratiques.", page); page <- page + 1
  }
  ppt <- add_figure_slide(
    ppt,
    title = row$title,
    subtitle = row$subtitle,
    fig_path = file.path(fig_dir, row$file),
    takeaway = row$takeaway,
    page = page
  )
  page <- page + 1
}

# Section tables compactes
ppt <- add_section_slide(ppt, "06", "Tables de contrôle", "Tables compactes pour documenter les analyses.", page); page <- page + 1

table_plan <- tibble::tribble(
  ~title, ~subtitle, ~file, ~takeaway, ~dir,
  "Registre des analyses ajoutées", "Une ligne par remarque ou hypothèse intégrée.", "analysis_registry_30062026.csv", "Ce registre documente pourquoi chaque bloc a été ajouté.", method_dir,
  "Qualité des variables de pondération", "Min, max, moyenne, valeurs manquantes.", "method_weight_variables_quality.csv", "Permet de vérifier la structure des poids avant interprétation.", method_dir,
  "Synthèse des sorties complémentaires", "Question, analyse, sortie et lecture attendue.", "complement_summary_for_ppt.csv", "Cette table sert de pont entre réunion WP2, code et restitution.", exports_dir,
  "Profil des autoformés", "Scores moyens selon catégorie d'exposition.", "autoformed_profile_scores.csv", "À lire avec prudence si les effectifs autoformés sont faibles.", tab_dir,
  "Dispositifs Q8-Q11", "Résumé des dispositifs détectés.", "devices_q8_q11_summary.csv", "À valider avec la datamap, notamment MOOC, présentiel et distanciel.", tab_dir
)

for (i in seq_len(nrow(table_plan))) {
  row <- table_plan[i, ]
  ppt <- add_table_slide(
    ppt,
    title = row$title,
    subtitle = row$subtitle,
    table_path = file.path(row$dir, row$file),
    takeaway = row$takeaway,
    page = page
  )
  page <- page + 1
}

# Synthèse finale
ppt <- add_section_slide(ppt, "07", "Conclusion et priorités", "Ce que les nouvelles analyses permettent de consolider.", page); page <- page + 1

ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- add_title(ppt, "Ce que les compléments changent", "Lecture analytique consolidée après intégration des remarques.")
ppt <- add_bullets(
  ppt,
  c(
    "La conclusion principale devient plus défendable : les dispositifs sont surtout associés à la connaissance opérationnelle, aux usages et à un environnement perçu comme plus incitatif.",
    "La question de la mise en action doit être analysée à partir des pratiques déjà possibles : année de thèse, Q4, publication, données, code.",
    "Les non-réponses, les « non » et les « je ne sais pas » deviennent des résultats en soi, notamment pour les intentions futures.",
    "Les autoformés doivent être considérés comme une catégorie analytique propre si les effectifs le permettent.",
    "La langue du questionnaire doit être traitée comme un proxy contextualisé, à croiser avec établissement, offre de formation et MOOC.",
    "L'analyse textuelle peut devenir un axe fort : passage des valeurs de la science ouverte vers un cadrage opérationnel."
  ),
  size = 15.4
)
ppt <- add_takeaway(ppt, "Le workflow complémentaire transforme les remarques de réunion en hypothèses testables et en sorties directement intégrables à la restitution.")
ppt <- add_footer(ppt, page); page <- page + 1

ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- add_title(ppt, "Prochaines étapes", "Prioriser les analyses avant restitution au consortium.")
ppt <- add_bullets(
  ppt,
  c(
    "Valider collectivement les recodages et les regroupements disciplinaires.",
    "Demander / confirmer les informations OpinionWay sur les pondérations et la précision à 85 %.",
    "Stabiliser le dictionnaire Q3 avec une annotation manuelle, idéalement avec Adrien.",
    "Approfondir Q8-Q11 : type de dispositif, MOOC, présentiel/distanciel, nombre de formations suivies.",
    "Croiser langue du questionnaire avec établissement et offre réelle de formations en anglais.",
    "Préparer une version courte du PPT avec uniquement les résultats robustes et une annexe technique séparée."
  ),
  size = 15.8
)
ppt <- add_takeaway(ppt, "Priorité : distinguer ce qui est déjà robuste de ce qui doit rester présenté comme piste exploratoire.")
ppt <- add_footer(ppt, page); page <- page + 1

print(ppt, target = ppt_path)

message("\nPowerPoint complémentaire généré : ", normalizePath(ppt_path, mustWork = FALSE))
