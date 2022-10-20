# ==========================================================================
# some methods for class 'report', to fast generate layer of 'section' or
# 'code_block' etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("include_figure", 
          signature = c(file = "character",
                        name = "character",
                        caption = "character"),
          function(file, name, caption){
            .check_file(file)
            codes <- paste0("knitr::include_graphics('", file, "')")
            args <- list(echo = F, eval = T, message = F)
            new_code_block_figure(name = name,
                                  caption = caption,
                                  codes = codes,
                                  args = args)
          })
setMethod("include_table", 
          signature = c(data = "data.frame",
                        name = "character",
                        caption = "character"),
          function(data, name, caption){
            var <- rlang::as_label(substitute(data))
            codes <- paste0("knitr::kable(", var, ", ",
                            "format = 'markdown', ",
                            "caption = '", caption, "')")
            args <- list(echo = F, eval = T, message = F)
            new_code_block_table(name = name,
                                 codes = codes,
                                 args = args)
          })
setMethod("history_rblock", 
          signature = setMissing("history_rblock"),
          function(){
            list(exclude = 1)
          })
setMethod("history_rblock", 
          signature = c(nrow  = "numeric"),
          function(nrow, pattern_start, pattern_end, exclude){
            reCallMethod("history_rblock",
                         .fresh_param(history_rblock()))
          })
setMethod("history_rblock", 
          signature = setMissing("history_rblock",
                                 nrow = "numeric",
                                 exclude = "numeric"),
          function(nrow, exclude){
            his <- tail(get_history(exclude), n = nrow)
            args <- list(echo = T, eval = F)
            new_code_block(codes = his, args = args)
          })
setMethod("history_rblock", 
          signature = setMissing("history_rblock",
                                 pattern_start = "character",
                                 pattern_end = "character",
                                 exclude = "ANY"),
          function(pattern_start, pattern_end, exclude){
            if (missing(exclude))
              exclude <- 1
            his <- get_history(exclude)
            end <- tail(grep(pattern_end, his, perl = T), n = 1)
            start <- tail(grep(pattern_start, his[1:end]), n = 1)
            args <- list(echo = T, eval = F)
            new_code_block(codes = his[start:end], args = args)
          })

