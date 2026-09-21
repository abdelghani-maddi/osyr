# =============================================================================
# SCRIPT 06 — GÉNÉRER LA PRÉSENTATION FINALE OSYR
# Version 2026-09-21
# =============================================================================
# Ce script produit une présentation PowerPoint sobre, structurée selon le plan
# de dépouillement de septembre 2026 et alignée sur les couleurs OSYR.
#
# Entrées attendues :
#   - outputs_osyr_v2_final/figures/
#   - outputs_osyr_v2_complements_30062026/figures/
#
# Sortie :
#   - outputs_osyr_presentation_finale/presentation_finale_osyr.pptx
# =============================================================================

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
}

pkgs <- c("tidyverse", "officer", "fs", "glue", "scales")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Style OSYR
# -----------------------------------------------------------------------------

if (!file.exists("R/osyr_style.R")) {
  stop("Fichier manquant : R/osyr_style.R.")
}
source("R/osyr_style.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$ppt)

plan_rapport <- osyr_final_plan_registry()
figure_catalog <- build_figure_catalog()
figure_catalog <- figure_catalog |>
  dplyr::filter(available) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$ppt, "catalogue_figures_presentation.csv"))

# -----------------------------------------------------------------------------
# 2. Helpers PowerPoint
# -----------------------------------------------------------------------------

ppt <- officer::read_pptx()
layout_blank <- "Blank"
master <- "Office Theme"
slide_w <- 13.333
slide_h <- 7.5

ph <- officer::ph_location
fp_title <- officer::fp_text(font.size = 26, bold = TRUE, color = cols[["dark_green"]])
fp_subtitle <- officer::fp_text(font.size = 15, color = cols[["grey"]])
fp_text <- officer::fp_text(font.size = 13, color = cols[["black"]])
fp_small <- officer::fp_text(font.size = 9, color = cols[["grey"]])

add_osyr_bar <- function(ppt, page_number = NULL) {
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0, top = 0, width = slide_w, height = 0.22, bg = cols[["green"]])
  )
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0, top = slide_h - 0.18, width = slide_w, height = 0.18, bg = cols[["beige"]])
  )
  if (!is.null(page_number)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext(as.character(page_number), fp_small)),
      location = ph(left = 12.3, top = 6.95, width = 0.7, height = 0.25)
    )
  }
  ppt
}

add_title <- function(ppt, title, subtitle = NULL) {
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(title, fp_title)),
    location = ph(left = 0.55, top = 0.55, width = 11.9, height = 0.55)
  )
  if (!is.null(subtitle)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext(subtitle, fp_subtitle)),
      location = ph(left = 0.55, top = 1.12, width = 11.8, height = 0.45)
    )
  }
  ppt
}

add_bullets <- function(ppt, bullets, left = 0.75, top = 1.75, width = 11.8, height = 4.8, size = 14) {
  txt <- paste0("• ", bullets, collapse = "\n")
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(txt, officer::fp_text(font.size = size, color = cols[["black"]]))),
    location = ph(left = left, top = top, width = width, height = height)
  )
  ppt
}

add_section_slide <- function(ppt, section_number, section_title, section_objective, page) {
  ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
  ppt <- add_osyr_bar(ppt, page)
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0.8, top = 1.25, width = 1.15, height = 1.15, bg = cols[["green"]])
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(sprintf("%02d", section_number), officer::fp_text(font.size = 28, bold = TRUE, color = "white"))),
    location = ph(left = 0.95, top = 1.48, width = 0.8, height = 0.5)
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(section_title, officer::fp_text(font.size = 30, bold = TRUE, color = cols[["dark_green"]]))),
    location = ph(left = 2.25, top = 1.28, width = 10.2, height = 0.7)
  )
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(section_objective, fp_subtitle)),
    location = ph(left = 2.25, top = 2.05, width = 10.2, height = 1.3)
  )
  ppt
}

add_figure_slide <- function(ppt, title, subtitle, path, page) {
  ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
  ppt <- add_osyr_bar(ppt, page)
  ppt <- add_title(ppt, title, subtitle)
  ppt <- officer::ph_with(
    ppt,
    value = officer::external_img(path, width = 11.9, height = 5.15),
    location = ph(left = 0.72, top = 1.78, width = 11.9, height = 5.15)
  )
  ppt
}

# -----------------------------------------------------------------------------
# 3. Slides
# -----------------------------------------------------------------------------

page <- 1

# Couverture.
ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
ppt <- add_osyr_bar(ppt, page)
ppt <- officer::ph_with(
  ppt,
  value = "",
  location = ph(left = 0, top = 0, width = 3.9, height = slide_h, bg = cols[["pale_green"]])
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("OSYR", officer::fp_text(font.size = 46, bold = TRUE, color = cols[["dark_green"]]))),
  location = ph(left = 4.45, top = 1.75, width = 7.6, height = 0.8)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("Résultats de l'enquête doctorants", officer::fp_text(font.size = 26, bold = TRUE, color = cols[["dark_green"]]))),
  location = ph(left = 4.45, top = 2.65, width = 7.6, height = 0.65)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext("Présentation finale — plan de dépouillement septembre 2026", fp_subtitle)),
  location = ph(left = 4.45, top = 3.38, width = 7.6, height = 0.55)
)
ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(officer::ftext(format(Sys.Date(), "%d/%m/%Y"), fp_small)),
  location = ph(left = 4.45, top = 6.65, width = 4, height = 0.35)
)
page <- page + 1

# Plan.
ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
ppt <- add_osyr_bar(ppt, page)
ppt <- add_title(ppt, "Plan de la présentation", "Organisation selon la trame de dépouillement.")
ppt <- add_bullets(ppt, paste0(plan_rapport$section, ". ", plan_rapport$bloc), size = 13.5)
page <- page + 1

# Sections + figures prioritaires.
for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)
  ppt <- add_section_slide(ppt, sec, sec_info$bloc, sec_info$objectif, page)
  page <- page + 1

  figs <- figure_catalog |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(priorite, titre) |>
    dplyr::slice_head(n = 2)

  if (nrow(figs) > 0) {
    for (i in seq_len(nrow(figs))) {
      subtitle <- paste0("Source : sorties ", figs$source_dir[i], " — ", figs$bloc[i])
      ppt <- add_figure_slide(ppt, figs$titre[i], subtitle, figs$path[i], page)
      page <- page + 1
    }
  }
}

# Précautions.
ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
ppt <- add_osyr_bar(ppt, page)
ppt <- add_title(ppt, "Précautions d'interprétation", "Points à rappeler dans la restitution.")
ppt <- add_bullets(
  ppt,
  c(
    "Les résultats sont descriptifs et associatifs.",
    "Les modèles ajustés ne permettent pas de conclure à un effet causal des dispositifs.",
    "Les réponses sont déclaratives et exposées à des biais de désirabilité sociale.",
    "Les comparaisons par discipline doivent tenir compte des effectifs et des pratiques de recherche propres à chaque domaine.",
    "La langue du questionnaire est utilisée comme proxy prudent et ne doit pas être surinterprétée."
  ),
  size = 13.5
)
page <- page + 1

out_pptx <- file.path(dirs$ppt, "presentation_finale_osyr.pptx")
print(ppt, target = out_pptx)

message("Présentation finale générée : ", normalizePath(out_pptx, mustWork = FALSE))
message("Catalogue des figures : ", normalizePath(file.path(dirs$ppt, "catalogue_figures_presentation.csv"), mustWork = FALSE))
