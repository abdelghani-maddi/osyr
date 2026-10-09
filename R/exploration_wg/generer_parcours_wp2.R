# OSYR - Parcours descriptif WP2 (V2, module autonome)
# Entrees en lecture seule : base analytique et tables DATAMAP du script 01.
# Execution : source("R/exploration_wg/generer_parcours_wp2.R")
pkgs <- c("openxlsx", "officer", "flextable")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Installer les packages : ", paste(missing, collapse = ", "))
root <- "outputs_osyr_v2_final/data_clean"
out <- "outputs_osyr_exploration_wp2_v2"
paths <- file.path(root, c("osyr_v2_corrigee_clean.rds",
                            "question_map_v2.csv", "answer_labels_v2.csv"))
if (!all(file.exists(paths))) stop("Entrées absentes : exécuter d'abord le script 01 OSYR.")
if (dir.exists(out)) stop("Le dossier de sortie existe déjà : ", out,
                           " ; archiver la version antérieure avant exécution.")
df <- readRDS(paths[1])
qm <- utils::read.csv(paths[2], check.names = FALSE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
am <- utils::read.csv(paths[3], check.names = FALSE, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
stopifnot(is.data.frame(df), all(c("item","question_label","item_label") %in% names(qm)),
          all(c("base","code","value") %in% names(am)))
dir.create(out, recursive = TRUE)
# Plan thématique de dépouillement (ordre de lecture, non ordre de modélisation).
sections <- list(
 "01. Profil des répondants" = c("Q1","Q2"),
 "02. Parcours de formation" = c("Q7","Q8","Q9","Q10","Q11"),
 "03. Connaissances et pratiques" = c("Q3","Q4","Q5"),
 "04. Environnement et conditions" = c("Q12"),
 "05. Intentions et non-adoption" = c("Q13","Q14"),
 "06. Perceptions" = c("Q15")
)
# Ne pas diffuser sans vérification : les petits effectifs sont signalés,
# non masqués. Les tableaux n'ont PAS valeur d'anonymisation.
MIN_N <- 5L
safe <- function(x) {x <- as.character(x); x[is.na(x) | trimws(x)==""] <- "Non-réponse"; x}
get_label <- function(v) {
  k <- match(v, qm$item)
  if (is.na(k)) return(paste0(v, " [libellé à vérifier]"))
  x <- qm$question_label[k]
  if (is.na(x) || !nzchar(x)) x <- qm$item_label[k]
  if (is.na(x) || !nzchar(x)) return(paste0(v, " [libellé à vérifier]"))
  paste0(v, " — ", x)
}
modalites <- function(v, x) {
  y <- safe(x)
  dict <- am[am$base == v & !is.na(am$code) & !is.na(am$value),]
  # Interdiction d'associer une modalité ambiguë à un code.
  if (nrow(dict)) {
    codes <- unique(dict$code)
    for (code in codes) {
      values <- unique(as.character(dict$value[dict$code == code]))
      if (length(values) == 1L) {
        idx <- !is.na(suppressWarnings(as.numeric(y))) &
               suppressWarnings(as.numeric(y)) == code
        y[idx] <- paste0(values, " [", code, "]")
      }
    }
  }
  y
}
question_id <- function(v) {
  k <- match(v, qm$item)
  source <- if (!is.na(k)) paste(qm$name[k], qm$question_label[k]) else v
  # L'identifiant dans le nom de variable prime.
  hit <- regmatches(toupper(v), regexpr("(^|_)Q(1[0-5]|[1-9])($|_)", toupper(v)))
  if (length(hit) && nzchar(hit)) return(gsub("[^A-Z0-9]", "", hit))
  hit <- regmatches(toupper(source), regexpr("Q(1[0-5]|[1-9])", toupper(source)))
  if (length(hit) && nzchar(hit)) return(hit)
  NA_character_
}
# Eliminer identifiants techniques, texte libre et variables continues
# de ce rapport de distribution des modalités.
vars <- names(df)
vars <- vars[!grepl("(^\\.|^id$|_id$|weight|poids)", vars, ignore.case=TRUE)]
is_categorical <- vapply(df[vars], function(x)
 is.factor(x) || is.logical(x) ||
 (is.numeric(x) && length(unique(stats::na.omit(x))) <= 20) ||
 (is.character(x) && length(unique(stats::na.omit(x))) <= 30 &&
   max(nchar(stats::na.omit(x)), 0) <= 130), logical(1))
vars <- vars[is_categorical]
meta <- data.frame(variable=vars, question=vapply(vars, question_id, character(1)),
                   libelle=vapply(vars, get_label, character(1)))
# Les noms non reconnus sont explicitement repris dans un onglet d'audit.
audit <- meta[is.na(meta$question) | grepl("à vérifier",meta$libelle), , drop=FALSE]
tables <- list()
for (s in names(sections)) {
  q <- sections[[s]]
  vset <- meta$variable[!is.na(meta$question) & meta$question %in% q]
  for (v in vset) {
    x <- modalites(v, df[[v]])
    counts <- as.data.frame(table(x), stringsAsFactors=FALSE)
    names(counts) <- c("Modalité", "Effectif")
    valid <- sum(!is.na(df[[v]]) & trimws(as.character(df[[v]]))!="")
    counts$Pourcentage_valides <- if (valid) round(100 * counts$Effectif/valid,1) else NA_real_
    counts$Pourcentage_total <- round(100 * counts$Effectif/nrow(df),1)
    counts$Petit_effectif <- counts$Effectif > 0 & counts$Effectif < MIN_N
    counts$N_valides <- valid
    counts$N_total <- nrow(df)
    counts$Question <- get_label(v)
    counts$Code_variable <- v
    counts$Section <- s
    tables[[paste0(s, "::", v)]] <- counts
  }
}
if (!length(tables)) stop("Aucune variable rattachée au plan : vérifier DATAMAP et variables.")
# Classeurs thématiques, avec table unique par question/variable.
write_section <- function(section, i) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "LIRE_EN_PREMIER")
  openxlsx::writeData(wb,"LIRE_EN_PREMIER",data.frame(
    Point=c("Section","Dénominateur","Attention","Questionnaire"),
    Explication=c(section,"N valides = réponses hors valeurs manquantes ; N total = lignes de la base.",
      "Non-réponse distincte ; réponses multiples et filtres à vérifier dans le questionnaire.",
      "Les libellés proviennent du DATAMAP ; les codes non résolus restent visibles.")))
  names_here <- names(tables)[startsWith(names(tables),paste0(section,"::"))]
  for (j in seq_along(names_here)) {
    nm <- sprintf("T%03d",j)
    openxlsx::addWorksheet(wb,nm)
    t <- tables[[names_here[j]]]
    openxlsx::writeData(wb,nm,t[,c("Question","Modalité","Effectif",
      "Pourcentage_valides","Pourcentage_total","N_valides","N_total","Petit_effectif")],
      withFilter=TRUE)
    openxlsx::freezePane(wb,nm,firstActiveRow=2)
    openxlsx::setColWidths(wb,nm,1:8,c(65,55,13,20,20,14,14,18))
  }
  openxlsx::saveWorkbook(wb,file.path(out,sprintf("%02d_%s.xlsx",i,
    gsub("[^A-Za-z0-9]+","_",iconv(section,to="ASCII//TRANSLIT")))))
}
for (i in seq_along(sections)) write_section(names(sections)[i],i)
utils::write.csv(audit,file.path(out,"audit_libelles_non_resolus.csv"),row.names=FALSE,
                 fileEncoding="UTF-8")
# Rapport Word = tableaux éditables, pas captures d'écran.
doc <- officer::read_docx()
doc <- officer::body_add_par(doc,"OSYR — Parcours descriptif pour le WP2",style="heading 1")
doc <- officer::body_add_par(doc,
 "Document de travail : distributions descriptives détaillées avant sélection des résultats.",
 style="Normal")
doc <- officer::body_add_par(doc,
 paste0("Population analysée : ", nrow(df),
 " lignes. Tableaux non pondérés ; aucune interprétation causale ni test de significativité."),
 style="Normal")
doc <- officer::body_add_par(doc,
 "Lecture : comparer effectifs et pourcentages, identifier les non-réponses et les modalités rares. Vérifier les questions filtrées et à réponses multiples avec le questionnaire.",
 style="Normal")
doc <- officer::body_add_par(doc,
 "Les catégories codées sans correspondance univoque dans le DATAMAP restent numériques. Vérifier l'audit des libellés avant diffusion.",
 style="Normal")
for (section in names(sections)) {
  doc <- officer::body_add_par(doc,section,style="heading 1")
  entries <- names(tables)[startsWith(names(tables),paste0(section,"::"))]
  if (!length(entries)) {
    doc <- officer::body_add_par(doc,"Aucune variable catégorielle automatiquement reconnue dans cette partie.",style="Normal")
    next
  }
  for (entry in entries) {
    t <- tables[[entry]]
    label <- as.character(t$Question[1])
    doc <- officer::body_add_par(doc,label,style="heading 2")
    doc <- officer::body_add_par(doc,
      paste0("Base : ", t$N_valides[1], " réponses valides sur ",
      t$N_total[1], " observations ; pourcentages non pondérés."),
      style="Normal")
    shown <- t[,c("Modalité","Effectif","Pourcentage_valides","Pourcentage_total")]
    names(shown) <- c("Réponse","n","% valides","% total")
    ft <- flextable::flextable(shown)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::fontsize(ft,size=9,part="all")
    ft <- flextable::autofit(ft)
    ft <- flextable::set_table_properties(ft,layout="autofit",width=0.98)
    doc <- flextable::body_add_flextable(doc,ft)
    if (any(t$Petit_effectif)) {
      doc <- officer::body_add_par(doc,
        "Attention : au moins une modalité a un effectif inférieur à 5 ; vérifier la confidentialité avant partage.",style="Normal")
    }
    doc <- officer::body_add_par(doc,
      "À discuter : modalités dominantes, réponses rares, non-réponses, éventuels croisements prioritaires.",
      style="Normal")
  }
}
doc <- officer::body_add_par(doc,"Questions pour la discussion collective",style="heading 1")
for (s in c("Les modalités d'origine permettent-elles de comprendre les résultats du rapport ?",
            "Quels croisements entre année de thèse, discipline et parcours de formation faut-il prioriser ?",
            "Quelles catégories regroupées faut-il rediscuter ?",
            "Quelles questions du plan nécessitent un tableau complémentaire ?")) {
 doc <- officer::body_add_par(doc,paste0("• ",s),style="Normal")
}
print(doc,target=file.path(out,"OSYR_rapport_descriptif_WP2.docx"))
message("Livrables descriptifs écrits dans : ", out)
