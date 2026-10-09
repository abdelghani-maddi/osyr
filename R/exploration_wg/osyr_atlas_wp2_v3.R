# OSYR — Module descriptif WG (V3)
# Depuis la racine du depot : source("R/exploration_wg/generer_tableaux_wg.R")
# Lecture seule des donnees ; n'ecrit que dans outputs_osyr_exploration_wp2_v3/.
for (p in c("readxl","openxlsx","officer","flextable")) {
 if (!requireNamespace(p,quietly=TRUE)) stop("Installer le package : ",p)
}
bdd <- "data/BJ30232 - BDD V2.csv"
datamap <- "data/BJ30232 - DATAMAP V2.xlsx"
if (!file.exists(datamap)) datamap <- "data/BJ30232 - DATAMAP V2(1).xlsx"
if (!file.exists(bdd) || !file.exists(datamap)) stop("BDD et DATAMAP absents du dossier data/.")
output <- "outputs_osyr_exploration_wp2_v3"
if (dir.exists(output)) stop("Sortie deja existante : ", output, ". Archiver avant de regenerer.")
df <- utils::read.csv2(bdd, fileEncoding="latin1",check.names=FALSE,stringsAsFactors=FALSE)
dm <- as.data.frame(readxl::read_excel(datamap),stringsAsFactors=FALSE)
stopifnot(all(c("name","type","label","value","code") %in% names(dm)))
for (nm in c("name","type","label")) {
 v <- as.character(dm[[nm]]); v[is.na(v)] <- ""
 for (i in seq_along(v)) if (i>1L && !nzchar(trimws(v[i]))) v[i] <- v[i-1L]
 dm[[nm]] <- v
}
dm$code <- as.character(dm$code)
dm$value <- as.character(dm$value)
fix <- function(s) {
 s <- as.character(s)
 s <- gsub("[\u0091\u0092\u2018\u2019]", "'", s)
 s <- gsub("[\u0093\u0094\u201c\u201d]", '"', s)
 s <- gsub("\u0085","…",s,fixed=TRUE)
 trimws(s)
}
dm$label <- fix(dm$label); dm$value <- fix(dm$value)
sections <- list(
 "01 - Profil"=c("Q1","Q2"),
 "02 - Formation"=c("Q7","Q8","Q9","Q10","Q11"),
 "03 - Connaissances et pratiques"=c("Q3","Q4","Q5","Q6"),
 "04 - Environnement"=c("Q12"),
 "05 - Intentions et non-adoption"=c("Q13","Q14"),
 "06 - Perceptions"=c("Q15")
)
qnum <- function(nm) {
 z <- regmatches(nm,regexpr("^Q(1[0-5]|[1-9])",nm))
 if(length(z)) z else NA_character_
}
dict <- function(nm) {
 d <- dm[dm$name==nm & !is.na(dm$code) & !is.na(dm$value),c("code","value")]
 split(d$value,d$code)
}
decode <- function(z,d) {
 z <- as.character(z)
 missing <- is.na(z) | !nzchar(trimws(z))
 z[missing] <- "Non-reponse"
 ii <- which(!missing)
 for(i in ii) {
  match_code <- d[[z[i]]]
  if(!is.null(match_code) && length(unique(match_code))==1L) z[i] <- unique(match_code)
  else z[i] <- paste0("Code ",z[i]," [a verifier]")
 }
 z
}
meta <- unique(dm[nzchar(dm$name),c("name","type","label")])
meta <- meta[!duplicated(meta$name),,drop=FALSE]
records <- list()
audit <- data.frame(Variable=character(),Probleme=character())
for(i in seq_len(nrow(meta))) {
 nm <- meta$name[i]; kind <- meta$type[i]; q <- qnum(nm)
 if(is.na(q) || !(kind %in% c("single","multiple"))) next
 si <- which(vapply(sections,function(z) q %in% z, logical(1)))
 if(!length(si)) next
 d <- dict(nm)
 if(kind=="single") {
  if(!(nm %in% names(df))) {
   audit <- rbind(audit,data.frame(Variable=nm,Probleme="Colonne absente"));next
  }
  zz <- decode(df[[nm]],d)
  f <- table(zz); labs <- names(f); n <- as.integer(f)
  base <- sum(zz!="Non-reponse")
  p <- if(base>0) round(100*n/base,1) else rep(NA_real_,length(n))
  p[labs=="Non-reponse"] <- NA_real_
  note <- "Question a reponse unique ; hors non-reponses dans le pourcentage valide."
 } else {
  cols <- grep(paste0("^",nm,"_M[0-9]+$"),names(df),value=TRUE)
  if(!length(cols)) {
   audit<-rbind(audit,data.frame(Variable=nm,Probleme="Colonnes multireponses absentes"));next
  }
  mat <- as.data.frame(lapply(df[cols],function(z) as.character(z)), stringsAsFactors=FALSE)
  options <- unique(unlist(mat,use.names=FALSE)); options <- options[!is.na(options)&nzchar(trimws(options))]
  n <- vapply(options,function(code) {
   sum(rowSums(as.data.frame(lapply(mat,function(z) !is.na(z)&z==code)))>0)
  },integer(1))
  labs <- decode(options,d)
  base <- sum(rowSums(as.data.frame(lapply(mat,function(z) !is.na(z)&nzchar(trimws(z)))))>0)
  p <- if(base>0) round(100*n/base,1) else rep(NA_real_,length(n))
  note <- "Multireponse : part des repondants ayant fourni au moins un choix ; somme possible >100%."
 }
 tab <- data.frame(Modalite=labs,Effectif=n,Pct_base_observee=p,
                   Pct_total=round(100*n/nrow(df),1),
                   Petit_effectif=n>0 & n<5,stringsAsFactors=FALSE)
 if(any(grepl("[a verifier]",labs,fixed=TRUE)))
  audit <- rbind(audit,data.frame(Variable=nm,Probleme="Au moins un code non resolu"))
 records[[nm]] <- list(section=names(sections)[si[1]],label=meta$label[i],
                       base=base,table=tab,note=note)
}
if(!length(records)) stop("Aucun tableau genere. Verifier l'encodage et les noms de colonnes.")
dir.create(output,recursive=TRUE)
utils::write.csv(audit,file.path(output,"audit_libelles.csv"),
                 row.names=FALSE,fileEncoding="UTF-8")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"Index")
ix <- data.frame(Onglet=character(),Partie=character(),Question=character(),Libelle=character(),Base=integer())
for(i in seq_along(records)) {
 nm <- names(records)[i]; r <- records[[nm]]; sh <- sprintf("T%03d",i)
 openxlsx::addWorksheet(wb,sh)
 openxlsx::writeData(wb,sh,paste0(nm," - ",r$label),startRow=1)
 openxlsx::writeData(wb,sh,paste0("Base observee : ",r$base,". ",r$note),startRow=2)
 openxlsx::writeData(wb,sh,r$table,startRow=4,withFilter=TRUE)
 openxlsx::setColWidths(wb,sh,1:5,c(70,14,22,18,17))
 openxlsx::freezePane(wb,sh,firstActiveRow=5)
 ix <- rbind(ix,data.frame(Onglet=sh,Partie=r$section,Question=nm,Libelle=r$label,Base=r$base))
}
openxlsx::writeData(wb,"Index",ix,withFilter=TRUE)
openxlsx::setColWidths(wb,"Index",1:5,c(12,40,18,95,14))
openxlsx::saveWorkbook(wb,file.path(output,"OSYR_atlas_WP2.xlsx"),overwrite=FALSE)
doc <- officer::read_docx()
doc <- officer::body_add_par(doc,"OSYR - Cahier descriptif WP2",style="heading 1")
doc <- officer::body_add_par(doc,paste0("N = ",nrow(df),
 " repondants. Descriptifs non ponderes. Bases eligibles des questions filtrees a verifier."))
for(section in names(sections)) {
 doc <- officer::body_add_par(doc,section,style="heading 1")
 names_here <- names(records)[vapply(records,function(r) identical(r$section,section),logical(1))]
 for(nm in names_here) {
  r <- records[[nm]]
  doc <- officer::body_add_par(doc,paste0(nm," - ",r$label),style="heading 2")
  doc <- officer::body_add_par(doc,paste0("Base observee : ",r$base," / ",nrow(df),". ",r$note))
  t <- r$table[,1:4]; names(t) <- c("Modalite","n","% base observee","% total")
  ft <- flextable::flextable(t)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::fontsize(ft,size=8.5,part="all")
  ft <- flextable::autofit(ft)
  doc <- flextable::body_add_flextable(doc,ft)
  if(any(r$table$Petit_effectif))
    doc <- officer::body_add_par(doc,"Petit effectif : controle de confidentialite requis.")
 }
 doc <- officer::body_add_par(doc,
  "A discuter au WP2 : quelles modalites retiennent l'attention et quels croisements prioriser ?")
}
print(doc,target=file.path(output,"OSYR_cahier_WP2.docx"))
message("Module WG V3 termine : ",output)
