# ==========================================================================
# directory and file names and path in SIRIUS 4 project, and some function
# for how to read or format these data.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.get_file_name_sirius.v4 <- 
  function(){
    set <- c(.id = "FUN_get_id_sirius.v4",
             .canopus = "^canopus.tsv",
             .canopus_summary = "canopus_summary.tsv",
             .compound_identifications = "compound_identifications.tsv",
             .formula_identifications = "formula_identifications.tsv",
             .canopus_neg = "canopus_neg.tsv",
             .csi_fingerid = "csi_fingerid.tsv",
             .csi_fingerid_neg = "csi_fingerid_neg.tsv",
             .dir_canopus = "^canopus$",
             .dir_fingerid = "^fingerid$",
             .dir_scores = "^scores$",
             .dir_spectra = "^spectra$",
             .f2_ms = "spectrum.ms",
             .f2_info = "compound.info",
             .f2_formula = "formula_candidates.tsv",
             .f3_canopus = "\\.fpt$",
             .f3_fingerid = "\\.tsv$",
             .f3_scores = "\\.info$",
             .f3_spectra = "\\.tsv$"
    )
  }
FUN_get_id_sirius.v4 <- 
  function(x){
    if (missing(x))
      return("^[0-9](.*)_(.*)_(.*)$")
    stringr::str_extract(x, "(?<=_)[^_|^/]{1,}(?=/|$)")
  }
.get_file_api_sirius.v4 <- 
  function(){
    set <- c(.id = ".id",
             .canopus = ".canopus",
             .canopus_summary = ".canopus_summary",
             .compound_identifications = ".compound_identifications",
             .formula_identifications = ".formula_identifications",
             .canopus_neg = ".canopus_neg",
             .csi_fingerid = ".csi_fingerid",
             .csi_fingerid_neg = ".csi_fingerid_neg",
             .dir_canopus = ".id/.dir_canopus",
             .dir_fingerid = ".id/.dir_fingerid",
             .dir_scores = ".id/.dir_scores",
             .dir_spectra = ".id/.dir_spectra",
             .f2_ms = ".id/.f2_ms",
             .f2_info = ".id/.f2_info",
             .f2_formula = ".id/.f2_formula",
             .f3_canopus = ".id/.dir_canopus/.f3_canopus",
             .f3_fingerid = ".id/.dir_fingerid/.f3_fingerid",
             .f3_scores = ".id/.dir_scores/.f3_scores",
             .f3_spectra = ".id/.dir_spectra/.f3_spectra"
    )
  }
.get_attribute_name_sirius.v4 <- 
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
             .id = "name",
             most.sp.class = "most specific class",
             level5 = "level 5",
             subclass = "subclass",
             class = "class",
             superclass = "superclass",
             all.class = "all classifications",
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
.get_attribute_type_sirius.v4 <- 
  function(){
    set <- c(
             rank.formula = "integer",
             rank.structure = "integer",
             csi.score = "numeric",
             xlogp = "numeric",
             tani.score = "numeric",
             mz = "numeric",
             rt.secound = "numeric",
             rt.min = "numeric",
             int. = "numeric",
             rel.int. = "numeric",
             exactmass = "numeric",
             zodiac.score = "numeric",
             sirius.score = "numeric",
             tree.score = "numeric",
             iso.score = "numeric",
             hit.num. = "integer",
             hit.int. = "numeric",
             error.frag. = "numeric",
             error.abs.frag. = "numeric",
             error.mass = "numeric",
             rel.index = "integer",
             abs.index = "integer",
             cosmic.score = "numeric",
             pp.value = "numeric"
    )
  }
.get_methods_read_sirius.v4 <- 
  function(){
    set <- c(
             read.canopus = MCnebula2::read_tsv,
             read.canopus_summary = MCnebula2::read_tsv,
             read.compound_identifications = MCnebula2::read_tsv,
             read.formula_identifications = MCnebula2::read_tsv,
             read.f2_ms = MCnebula2::pbsapply_read_tsv,
             read.f2_formula = MCnebula2::pbsapply_read_tsv,
             read.f2_info = MCnebula2::pbsapply_read_info,
             read.f3_fingerid = MCnebula2::pbsapply_read_tsv,
             read.f3_scores = MCnebula2::pbsapply_read_tsv,
             read.f3_spectra = MCnebula2::pbsapply_read_tsv,
             read.f3_canopus = .pbsapply_read_fpt
    )
  }
pbsapply_read_info <- function(path){
  pbapply::pbsapply(path, simplify = F,
                     function(path){
                       lines <- readLines(path)
                       lines <- lines[grepl("^ionMass|^rt", lines)]
                       data.frame(ionMass =
                                    stringr::str_extract(lines[1], "[0-9|.]{1,}"),
                                  rt = stringr::str_extract(lines[2], "[0-9|.]{1,}")
                       )
                     })
}
.pbsapply_read_fpt <- function(path){
  pbapply::pbsapply(path, simplify = F,
                    function(path){
                      df <- data.table::fread(path, header = F)
                      df$rel.index <- 0:(nrow(df) - 1)
                      df
                    })
}
.get_methods_match_sirius.v4 <- 
  function(){
    set <- c(
             match.features_id = MCnebula2::FUN_get_id_sirius.v4,
             match.candidates_id = function(x) stringr::str_extract(x, "[^/]*(?=\\.[a-z]*$)"),
             generate_candidates_id = function(df) {
               if (is.null(df$mol.formula) | is.null(df$adduct))
                 stop( "columns not found in `df`" )
               paste0(df$mol.formula, "_", gsub(" ", "", df$adduct))
             }
    )
  }

