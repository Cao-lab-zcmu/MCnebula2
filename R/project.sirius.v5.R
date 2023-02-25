# ==========================================================================
# directory and file names and path in SIRIUS 4 project, and some function
# for how to read or format these data.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

.validate_sirius.v5 <- 
  function(path){
    sig <- paste0(path, "/.format")
    content <- "%source_%name"
    if (file.exists(sig)) {
      if (!identical(readLines(sig, warn = F, n = 1), content)) {
        stop("the content of file \"", sig,
          "\" is not identical to \"", content, "\"")
      }
    }else{
      stop("file \"", sig, "\" not exists")
    }
    sig <- paste0(path, "/.compression")
    if (file.exists(sig)) {
      lines <- readLines(sig, warn = F)
      if (lines[1] != "compressionLevels\t1" |
        lines[2] != "compressionMethod\tDEFLATED")
        stop("file \"", sig, "\": Inappropriate compression method.")
    }
  }


.get_file_name_sirius.v5 <- 
  function(){
    set <- c(.id = "FUN_get_id_sirius.v5",
      .canopus = "^canopus.tsv",
      .canopus_summary = "canopus_compound_summary.tsv",
      .compound_identifications = "compound_identifications.tsv",
      .formula_identifications = "formula_identifications.tsv",
      .canopus_neg = "canopus_neg.tsv",
      .csi_fingerid = "csi_fingerid.tsv",
      .csi_fingerid_neg = "csi_fingerid_neg.tsv",
      .zip_canopus = "^canopus$",
      .zip_fingerid = "^fingerid$",
      .zip_scores = "^scores$",
      .zip_spectra = "^spectra$",
      .f2_ms = "spectrum.ms",
      .f2_msms = "spectrum.ms",
      .f2_info = "compound.info",
      .f2_formula = "formula_candidates.tsv",
      .f3_canopus = "\\.fpt$",
      .f3_fingerid = "\\.tsv$",
      .f3_scores = "\\.info$",
      .f3_spectra = "\\.tsv$"
    )
  }

FUN_get_id_sirius.v5 <-FUN_get_id_sirius.v4

.get_file_api_sirius.v5 <- function(){
  set <- c(.id = ".id",
    .canopus = ".canopus",
    .canopus_summary = ".canopus_summary",
    .compound_identifications = ".compound_identifications",
    .formula_identifications = ".formula_identifications",
    .canopus_neg = ".canopus_neg",
    .csi_fingerid = ".csi_fingerid",
    .csi_fingerid_neg = ".csi_fingerid_neg",
    .zip_canopus = ".id/.zip_canopus",
    .zip_fingerid = ".id/.zip_fingerid",
    .zip_scores = ".id/.zip_scores",
    .zip_spectra = ".id/.zip_spectra",
    .f2_ms = ".id/.f2_ms",
    .f2_msms = ".id/.f2_msms",
    .f2_info = ".id/.f2_info",
    .f2_formula = ".id/.f2_formula",
    .f3_canopus = ".id/.zip_canopus/.f3_canopus",
    .f3_fingerid = ".id/.zip_fingerid/.f3_fingerid",
    .f3_scores = ".id/.zip_scores/.f3_scores",
    .f3_spectra = ".id/.zip_spectra/.f3_spectra"
  )
}

.get_attribute_name_sirius.v5 <- 
  function(){
    set <- c(
      ## .f3_fingerid
      ...sig = ".f3_fingerid",
      inchikey2d = "inchikey2D",
      inchi = "inchi",
      mol.formula = "molecularFormula",
      rank.structure = "rank",
      csi.score = "score",
      synonym = "name",
      smiles = "smiles",
      xlogp = "xlogp",
      pubmed.ids = "PubMedIds",
      links = "links",
      tani.score = "tanimotoSimilarity",
      dbflags = "dbflags",
      ## .f3_spectra
      ...sig = ".f3_spectra",
      mz = "mz",
      int. = "intensity",
      rel.int. = "rel.intensity",
      exactmass = "exactmass",
      formula = "formula",
      ion. = "ionization",
      ## .f2_formula
      ...sig = ".f2_formula",
      adduct = "adduct",
      pre.formula = "precursorFormula",
      zodiac.score = "ZodiacScore",
      sirius.score = "SiriusScore",
      tree.score = "TreeScore",
      iso.score = "IsotopeScore",
      hit.num. = "numExplainedPeaks",
      hit.int. = "explainedIntensity",
      error.frag. = "medianMassErrorFragmentPeaks\\(ppm\\)",
      error.abs.frag. = "medianAbsoluteMassErrorFragmentPeaks\\(ppm\\)",
      error.mass = "massErrorPrecursor\\(ppm\\)",
      rank.formula = "rank",
      ## .f2_info
      ...sig = ".f2_info",
      rt.secound = "rt",
      mz = "ionMass",
      ## .canopus
      ...sig = ".canopus",
      rel.index = "relativeIndex",
      abs.index = "absoluteIndex",
      chem.ont.id = "id",
      class.name = "name",
      parent.chem.ont.id = "parentId",
      description = "description",
      ## .canopus_neg
      ...sig = ".canopus_neg",
      chem.ont.id = "id",
      class.name = "name",
      ## .canopus_summary
      ...sig = ".canopus_summary",
      .id = "id",
      npc_pathway = "NPC#pathway",
      npc_pathway_pp = "NPC#pathway Probability",
      npc_superclass = "NPC#superclass",
      npc_superclass_pp = "NPC#superclass Probability",
      npc_class = "NPC#class",
      npc_class_pp = "NPC#class Probability",
      classyfire_most_specific_class = "ClassyFire#most specific class",
      classyfire_most_specific_class_pp = "ClassyFire#most specific class Probability",
      classyfire_level_5 = "ClassyFire#level 5",
      classyfire_level_5_pp = "ClassyFire#level 5 Probability",
      classyfire_subclass = "ClassyFire#subclass",
      classyfire_subclass_pp = "ClassyFire#subclass Probability",
      classyfire_class = "ClassyFire#class",
      classyfire_class_pp = "ClassyFire#class Probability",
      classyfire_superclass = "ClassyFire#superclass",
      classyfire_superclass_pp = "ClassyFire#superclass probability",
      classyfire_all_classifications = "ClassyFire#all classifications",
      ## .compound_identifications
      ...sig = ".compound_identifications",
      cosmic.score = "ConfidenceScore",
      .id = "id",
      ## .f3_canopus
      ...sig = ".f3_canopus",
      pp.value = "V1",
      ...sig = "END"
    )
  }

.get_attribute_type_sirius.v5 <- .get_attribute_type_sirius.v4

list_files_top.sirius.v5 <- list_files_top.sirius.v4

#' @importFrom utils unzip
list_files.sirius.v5 <- function(path, upper, pattern, info){
  lst_file <- pbapply::pbmapply(path, upper, pattern, SIMPLIFY = F,
    FUN = function(path, upper, pattern){
      if (grepl("^\\.zip_", info)) {
        res <- try(utils::unzip(paste0(path, "/", upper), list = T), silent = T)
        if (!inherits(res, "try-error")) {
          files <- res$Name
          files <- files[ grepl(pattern, files) ]
        } else {
          files <- integer(0)
        }
      } else {
        files <- list.files(paste0(path, "/", upper), pattern)
      }
      if ( length(files) == 0)
        return( data.frame() )
      data.frame(upper = upper, files = files)
    })
  data.table::rbindlist(lst_file)
}

.get_methods_read_sirius.v5 <- 
  function(){
    set <- c(
      read.canopus = read_tsv,
      read.canopus_summary = read_tsv,
      read.compound_identifications = read_tsv,
      read.formula_identifications = read_tsv,
      read.f2_ms = pbsapply_read_tsv,
      read.f2_msms = pbsapply_read_msms,
      read.f2_formula = pbsapply_read_tsv,
      read.f2_info = pbsapply_read_info,
      read.f3_fingerid = pblapply_read_tsv_fromZip,
      read.f3_scores = pblapply_read_tsv_fromZip,
      read.f3_spectra = pblapply_read_tsv_fromZip,
      read.f3_canopus = .pblapply_read_fpt_fromZip
    )
  }

.pblapply_read_fpt_fromZip <- function(path) {
  pblapply_read_tsv_fromZip(path,
    function(path) {
      df <- data.table::fread(path, header = F)
      df$rel.index <- 0:(nrow(df) - 1)
      df
    })
}

pblapply_read_tsv_fromZip <- function(path, fun = read_tsv) {
  zips <- gsub("/[^/]*$", "", path)
  files <- stringr::str_extract(path, "[^/]*$")
  lst_files <- split(files, zips)
  zips <- names(lst_files)
  zip_upper <- gsub("/[^/]*$", "", zips)
  zip_name <- stringr::str_extract(zips, "[^/]*$")
  exdir <- paste0(zip_upper, "/.temp_", zip_name)
  lst <- pbapply::pblapply(1:length(lst_files),
    function(n) {
      utils::unzip(zips[n], exdir = exdir[n])
      files <- paste0(exdir[n], "/", lst_files[[ n ]])
      lst <- lapply(files, fun)
      unlink(exdir[n], T)
      return(lst)
    })
  lst <- unlist(lst, F)
  names(lst) <- paste0(zips, "/", unlist(lst_files))
  return(lst)
}

.get_methods_match_sirius.v5 <- .get_methods_match_sirius.v4
