# =============================================================================
# SCRIPT 06 — GÉNÉRER LA PRÉSENTATION FINALE OSYR
# Version 2026-09-21 v3
# =============================================================================
# Présentation finale alignée sur le rapport principal.
# Elle privilégie les messages-clés et un nombre limité de figures lisibles.
# Les figures complémentaires restent disponibles dans l'annexe graphique Word.
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

pkgs <- c("tidyverse", "officer", "fs", "glue", "scales")
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!file.exists("R/osyr_style.R")) stop("Fichier manquant : R/osyr_style.R")
source("R/osyr_style.R")

if (!file.exists("R/osyr_report_text.R")) stop("Fichier manquant : R/osyr_report_text.R")
source("R/osyr_report_text.R")

cols <- osyr_colors()
dirs <- osyr_dirs()
ensure_dir(dirs$ppt)

catalog_main <- file.path(dirs$report, "tables", "figures_rapport_principal.csv")
catalog_final <- file.path(dirs$report, "tables", "catalogue_figures_finales.csv")

if (!file.exists(catalog_main)) {
  if (!file.exists("05_produire_rapport_final.R")) {
    stop("Le catalogue des figures principales est absent. Lancez d'abord le script 05.")
  }
  source("05_produire_rapport_final.R")
}

plan_rapport <- osyr_final_plan_registry()

figure_catalog <- if (file.exists(catalog_main)) {
  readr::read_csv(catalog_main, show_col_types = FALSE)
} else {
  readr::read_csv(catalog_final, show_col_types = FALSE)
}

figure_catalog <- figure_catalog |>
  dplyr::filter(available, file.exists(path)) |>
  dplyr::arrange(section, priorite, titre)

safe_write_csv(figure_catalog, file.path(dirs$ppt, "catalogue_figures_presentation.csv"))

# -----------------------------------------------------------------------------
# Helpers PowerPoint
# -----------------------------------------------------------------------------

ppt <- officer::read_pptx()
layout_blank <- "Blank"
master <- "Office Theme"
slide_w <- 13.333
slide_h <- 7.5

ph <- officer::ph_location
fp_title <- officer::fp_text(font.size = 25, bold = TRUE, color = cols[["dark_green"]])
fp_subtitle <- officer::fp_text(font.size = 14, color = cols[["grey"]])
fp_text <- officer::fp_text(font.size = 13, color = cols[["black"]])
fp_small <- officer::fp_text(font.size = 9, color = cols[["grey"]])

add_osyr_bar <- function(ppt, page_number = NULL) {
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0, top = 0, width = slide_w, height = 0.2, bg = cols[["green"]])
  )
  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0, top = slide_h - 0.15, width = slide_w, height = 0.15, bg = cols[["beige"]])
  )

  if (!is.null(page_number)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext(as.character(page_number), fp_small)),
      location = ph(left = 12.35, top = 6.98, width = 0.55, height = 0.22)
    )
  }

  ppt
}

add_title <- function(ppt, title, subtitle = NULL) {
  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(title, fp_title)),
    location = ph(left = 0.62, top = 0.5, width = 12.0, height = 0.55)
  )

  if (!is.null(subtitle)) {
    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(officer::ftext(subtitle, fp_subtitle)),
      location = ph(left = 0.62, top = 1.05, width = 12.0, height = 0.48)
    )
  }

  ppt
}

add_bullets <- function(ppt, bullets, left = 0.85, top = 1.75, width = 11.6, height = 4.9, size = 14) {
  bullets <- bullets[!is.na(bullets) & nzchar(bullets)]
  if (length(bullets) == 0) return(ppt)

  txt <- paste0("• ", bullets, collapse = "\n")

  officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        txt,
        officer::fp_text(font.size = size, color = cols[["black"]])
      )
    ),
    location = ph(left = left, top = top, width = width, height = height)
  )
}

add_section_slide <- function(ppt, section_number, section_title, section_objective, page) {
  ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
  ppt <- add_osyr_bar(ppt, page)

  ppt <- officer::ph_with(
    ppt,
    value = "",
    location = ph(left = 0.8, top = 1.25, width = 1.1, height = 1.1, bg = cols[["green"]])
  )

  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        sprintf("%02d", section_number),
        officer::fp_text(font.size = 27, bold = TRUE, color = "white")
      )
    ),
    location = ph(left = 0.96, top = 1.46, width = 0.75, height = 0.5)
  )

  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        section_title,
        officer::fp_text(font.size = 29, bold = TRUE, color = cols[["dark_green"]])
      )
    ),
    location = ph(left = 2.2, top = 1.3, width = 10.3, height = 0.7)
  )

  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(officer::ftext(section_objective, fp_subtitle)),
    location = ph(left = 2.2, top = 2.05, width = 10.1, height = 1.2)
  )

  ppt
}

add_summary_slide <- function(ppt, title, bullets, page) {
  ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
  ppt <- add_osyr_bar(ppt, page)
  ppt <- add_title(ppt, title, "Résultats à retenir")
  ppt <- add_bullets(ppt, bullets, top = 1.8, size = 13.5)
  ppt
}

add_figure_slide <- function(ppt, title, subtitle, path, page) {
  ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
  ppt <- add_osyr_bar(ppt, page)
  ppt <- add_title(ppt, title, subtitle)

  ppt <- officer::ph_with(
    ppt,
    value = officer::external_img(path, width = 11.7, height = 5.1),
    location = ph(left = 0.82, top = 1.75, width = 11.7, height = 5.1)
  )

  ppt
}

# -----------------------------------------------------------------------------
# Slides
# -----------------------------------------------------------------------------

page <- 1

# Couverture
ppt <- officer::add_slide(ppt, layout = layout_blank, master = master)
ppt <- add_osyr_bar(ppt, page)

ppt <- officer::ph_with(
  ppt,
  value = "",
  location = ph(left = 0, top = 0, width = 3.7, height = slide_h, bg = cols[["pale_green"]])
)

ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(
    officer::ftext("OSYR", officer::fp_text(font.size = 45, bold = TRUE, color = cols[["dark_green"]]))
  ),
  location = ph(left = 4.3, top = 1.65, width = 7.7, height = 0.75)
)

ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(
    officer::ftext(
      "Résultats de l'enquête auprès des doctorants",
      officer::fp_text(font.size = 25, bold = TRUE, color = cols[["dark_green"]])
    )
  ),
  location = ph(left = 4.3, top = 2.55, width = 7.8, height = 0.75)
)

ppt <- officer::ph_with(
  ppt,
  value = officer::fpar(
    officer::ftext(
      "Science ouverte, formations, connaissances, pratiques, intentions et perceptions",
      fp_subtitle
    )
  ),
  location = ph(left = 4.3, top = 3.4, width = 7.8, height = 0.7)
)

page <- page + 1

# Résumé
ppt <- add_summary_slide(
  ppt,
  "Principaux résultats",
  executive_summary_text(plan_rapport),
  page
)
page <- page + 1

# Sections
for (sec in sort(unique(plan_rapport$section))) {
  sec_info <- plan_rapport |>
    dplyr::filter(section == sec) |>
    dplyr::slice(1)

  ppt <- add_section_slide(ppt, sec, sec_info$bloc, sec_info$objectif, page)
  page <- page + 1

  section_text <- section_summary_text(sec)
  if (length(section_text) > 0) {
    ppt <- add_summary_slide(ppt, sec_info$bloc, section_text, page)
    page <- page + 1
  }

  figs <- figure_catalog |>
    dplyr::filter(section == sec) |>
    dplyr::arrange(dplyr::desc(source_dir == "rapport_final"), priorite, titre) |>
    dplyr::slice_head(n = 2)

  if (nrow(figs) > 0) {
    for (i in seq_len(nrow(figs))) {
      subtitle <- if (!is.na(figs$caption[i]) && nzchar(figs$caption[i])) {
        figs$caption[i]
      } else {
        "Source : enquête OSYR, données pondérées."
      }

      ppt <- add_figure_slide(
        ppt,
        figs$titre[i],
        subtitle,
        figs$path[i],
        page
      )
      page <- page + 1
    }
  }
}

# Conclusion
ppt <- add_summary_slide(
  ppt,
  "Conclusion",
  c(
    "Les résultats montrent des écarts persistants entre connaissance, usage et mise en pratique de la science ouverte.",
    "L'exposition aux dispositifs est associée à plusieurs dimensions, mais ces différences doivent être lues en tenant compte de la discipline, de l'année de thèse et de la composition des groupes.",
    "Les analyses de robustesse distinguent les résultats relativement stables de ceux qui restent sensibles aux choix de pondération ou de spécification.",
    "Les réponses étant déclaratives, les résultats ne sont pas interprétés comme des effets causaux des dispositifs."
  ),
  page
)

out_pptx <- file.path(dirs$ppt, "presentation_finale_osyr.pptx")
print(ppt, target = out_pptx)

message("Présentation finale générée : ", normalizePath(out_pptx, mustWork = FALSE))
message("Figures mobilisées : ", nrow(figure_catalog))
