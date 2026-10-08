# OSYR - Exploration descriptive pour le groupe de travail
# Module independant : ne modifie ni la base source ni les sorties existantes.
# Lancement (depuis la racine du depot) : source("R/exploration_wg/generer_tableaux_wg.R")
# Dependances : openxlsx (deja utilise par le workflow principal).

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package manquant : install.packages('openxlsx')")
}

entree <- file.path("outputs_osyr_v2_final", "data_clean",
                       "osyr_v2_corrigee_clean.rds")
sortie <- "outputs_osyr_exploration_wg"
seuil_alerte <- 5L  # Petites cellules signalees; NE suffit PAS a anonymiser.
axes <- c("year", "discipline_detail", "language_group", "exposure3")
# Ces croisements servent a la lecture collective, pas a l'inference causale.
croisements <- list(
  c("discipline_detail", "exposure3"),
  c("year", "exposure3"),
  c("language_group", "exposure3"),
  c("discipline_broad", "exposure3")
)
# Ajouter ici des paires (variable d'origine, variable regroupee) apres audit.
recodages <- list()

if (!file.exists(entree)) stop(
  "Base analytique absente : ", entree,
  "\nExecuter d'abord 01_analyse_osyr_base_et_modeles.R."
)
df <- readRDS(entree)
if (!is.data.frame(df)) stop("La base attendue doit etre un data.frame.")
if (anyDuplicated(names(df))) stop("Noms de variables dupliques.")
dir.create(sortie, showWarnings = FALSE, recursive = TRUE)
N <- nrow(df)
if (!N) stop("Base vide.")

# Ne jamais exporter les reponses individuelles ou les verbatims dans ce module.
# Les tableaux agreges doivent eux aussi etre verifies avant diffusion externe.
evaluable <- names(df)[vapply(df, function(x)
  is.factor(x) || is.character(x) || is.logical(x) ||
    is.numeric(x) || inherits(x, "Date"), logical(1))]
evaluable <- setdiff(evaluable, c(".weight", "weight_none"))
# Exclut automatiquement les champs textuels tres longs ou presque uniques.
est_libre <- vapply(df[evaluable], function(x) {
  z <- as.character(stats::na.omit(x))
  length(z) > 0 && (max(nchar(z), 0) > 120 ||
    (length(z) > 25 && length(unique(z))/length(z) > 0.85 &&
     is.character(x)))
}, logical(1))
evaluable <- evaluable[!est_libre]

make_book <- function(titre, aide) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "A_LIRE", gridLines = FALSE)
  openxlsx::writeData(wb, "A_LIRE", data.frame(
    Rubrique = c("Document", "Population", "Lecture", "Attention", "Diffusion"),
    Information = c(titre, paste(N, "lignes dans la base analytique"),
      aide, "NA = non-réponse; les pourcentages sont non pondérés sauf mention explicite.",
      "Verifier les petits effectifs, les croisements rares et les risques de divulgation indirecte.")
  ))
  openxlsx::setColWidths(wb, "A_LIRE", cols = 1:2, widths = c(22, 105))
  wb
}
add <- function(wb, nom, table, note = NULL) {
  key <- sprintf("T%03d", length(names(wb)) + 1L)
  openxlsx::addWorksheet(wb, key, gridLines = FALSE)
  if (!is.null(note)) openxlsx::writeData(wb, key, note, startRow = 1)
  first <- if (is.null(note)) 1L else 3L
  if (!nrow(table)) table <- data.frame(Information = "Aucune donnee")
  openxlsx::writeData(wb, key, table, startRow = first, withFilter = TRUE,
    headerStyle = openxlsx::createStyle(
      fgFill = "#16465A", fontColour = "#FFFFFF", textDecoration = "bold",
      wrapText = TRUE))
  openxlsx::freezePane(wb, key, firstActiveRow = first + 1L)
  openxlsx::setColWidths(wb, key, cols = seq_len(ncol(table)), widths = "auto")
  invisible(key)
}
save <- function(wb, nom) {
  chemin <- file.path(sortie, nom)
  # Refuser toute substitution silencieuse, meme dans le dossier dedie.
  if (file.exists(chemin)) {
    stop("Fichier deja present : ", chemin,
         "\nArchiver/deplacer la version precedente avant de regenerer.")
  }
  openxlsx::saveWorkbook(wb, chemin, overwrite = FALSE)
  message("Cree : ", chemin)
}
freq <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- "(Non-reponse)"
  z <- as.data.frame(table(y, useNA = "no"), stringsAsFactors = FALSE)
  names(z) <- c("Modalite", "Effectif")
  z$Pourcentage_tous <- round(100*z$Effectif/length(x), 1)
  z$Alerte_petit_effectif <- z$Effectif > 0 & z$Effectif < seuil_alerte
  z
}
croiser <- function(a, b, mode = c("effectifs", "ligne", "colonne")) {
  mode <- match.arg(mode)
  x <- as.character(a); y <- as.character(b)
  x[is.na(a)] <- "(Non-reponse)"
  y[is.na(b)] <- "(Non-reponse)"
  tab <- table(x, y, useNA = "no")
  if (mode == "ligne") {
    tab <- prop.table(tab, 1)*100
  } else if (mode == "colonne") {
    tab <- prop.table(tab, 2)*100
  }
  out <- data.frame(Groupe = rownames(tab), unclass(tab),
                    check.names = FALSE, row.names = NULL)
  names(out)[-1] <- colnames(tab)
  if (mode != "effectifs") out[-1] <- lapply(out[-1], round, digits = 1)
  out
}

# 00. Mode d'emploi commun
guide <- make_book("Guide de lecture OSYR", "Parcours conseille : 01 -> 02 -> 03 -> 04 -> 05.")
add(guide, "Parcours", data.frame(
  Etape = c("01 Qualite", "02 Tris a plat", "03 Numeriques",
            "04 Croisements", "05 Recodages", "06 Discussion"),
  Question = c("Qui repond et quelles donnees manquent ?",
               "Que disent les reponses originales ?",
               "Comment se distribuent les variables quantitatives ?",
               "Quelles differences descriptives observe-t-on ?",
               "Que changent les regroupements analytiques ?",
               "Quelles questions poser au groupe ?")))
save(guide, "00_Guide_de_lecture.xlsx")

# 01. Completude - chaque variable visible, y compris celles non publiees
qual <- make_book("Population et completude", "Une ligne par variable; NA distinct des modalites reelles.")
inventaire <- data.frame(Variable = names(df),
  Type = vapply(df, function(x) paste(class(x), collapse = ","), character(1)),
  N = N,
  Renseignes = vapply(df, function(x) sum(!is.na(x)), integer(1)),
  Manquants = vapply(df, function(x) sum(is.na(x)), integer(1)))
inventaire$Pct_manquants <- round(100*inventaire$Manquants/N, 1)
add(qual, "Completude", inventaire)
for (v in intersect(axes, evaluable)) add(qual, v, freq(df[[v]]), paste("Variable :", v))
save(qual, "01_Population_et_completude.xlsx")

# 02. Tris a plat - modalites non recodees de la base analytique
plats <- make_book("Tris a plat", "Effectifs bruts, base totale et non-reponses.")
catvars <- evaluable[vapply(df[evaluable], function(x) {
  is.factor(x) || is.character(x) || is.logical(x) ||
    (is.numeric(x) && length(unique(stats::na.omit(x))) <= 12)
}, logical(1))]
index <- data.frame(Tableau = character(), Variable = character())
for (v in catvars) {
  if (length(unique(stats::na.omit(df[[v]]))) > 100) next
  s <- add(plats, v, freq(df[[v]]), paste("Tri a plat :", v))
  index <- rbind(index, data.frame(Tableau = s, Variable = v))
}
add(plats, "Index", index)
save(plats, "02_Tris_a_plat.xlsx")

# 03. Statistiques descriptives quantitatives
nums <- make_book("Variables numeriques", "Les scores et indices sont des variables construites.")
numvars <- evaluable[vapply(df[evaluable], is.numeric, logical(1))]
numerique <- do.call(rbind, lapply(numvars, function(v) {
  x <- df[[v]]
  non_na <- x[is.finite(x)]
  data.frame(Variable=v, N_valides=length(non_na), N_manquants=sum(!is.finite(x)),
    Moyenne=if(length(non_na)) mean(non_na) else NA_real_,
    Mediane=if(length(non_na)) stats::median(non_na) else NA_real_,
    Ecart_type=if(length(non_na)>1) stats::sd(non_na) else NA_real_,
    Minimum=if(length(non_na)) min(non_na) else NA_real_,
    Q1=if(length(non_na)) unname(stats::quantile(non_na,.25)) else NA_real_,
    Q3=if(length(non_na)) unname(stats::quantile(non_na,.75)) else NA_real_,
    Maximum=if(length(non_na)) max(non_na) else NA_real_)
}))
if (is.null(numerique)) numerique <- data.frame(Information="Aucune variable")
add(nums, "Resume", numerique)
save(nums, "03_Variables_numeriques.xlsx")

# 04. Tableaux croises, trois lectures distinctes, NA visibles
cross <- make_book("Croisements exploratoires",
  "Effectifs, % en ligne, % en colonne; le total de reference doit etre controle.")
for (p in croisements) {
  if (length(p) != 2 || !all(p %in% names(df))) next
  a <- df[[p[1]]]; b <- df[[p[2]]]
  if (length(unique(stats::na.omit(a))) > 30 ||
      length(unique(stats::na.omit(b))) > 30) next
  for (m in c("effectifs", "ligne", "colonne")) {
    add(cross, paste(p, collapse=" / "), croiser(a,b,m),
        paste(paste(p, collapse=" x "), "|", m, "| N total =", N))
  }
}
save(cross, "04_Croisements_exploratoires.xlsx")

# 05. Regroupements documentes; a alimenter apres validation du dictionnaire.
rec <- make_book("Variables et recodages",
  "Les modalites d'origine sont indispensables avant interpretation des agregats.")
for (p in recodages) {
  if (length(p) == 2 && all(p %in% names(df))) {
    add(rec, paste(p,collapse=" / "), croiser(df[[p[1]]],df[[p[2]]]),
        paste("Correspondance brute -> regroupee :", paste(p, collapse=" => ")))
  }
}
add(rec, "A_completer", data.frame(
  Consigne="Ajouter les paires de recodage controlees dans le script; aucune n'est supposee."))
save(rec, "05_Variables_et_recodages.xlsx")

# 06. Support vierge de contribution collective (aucune donnee individuelle)
discussion <- make_book("Commentaires pour le WG",
  "Renvoyer au numero de tableau, sans ajouter de donnees individuelles.")
add(discussion, "Commentaires", data.frame(
  Fichier=rep("",20), Tableau=rep("",20), Variable=rep("",20),
  Constat=rep("",20), Question_au_WG=rep("",20),
  Proposition=rep("",20), Auteur=rep("",20)))
save(discussion, "06_Commentaires_WG.xlsx")
message("Exploration descriptive terminee. Aucune sortie du workflow principal modifiee.")
