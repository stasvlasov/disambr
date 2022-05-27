## -------->>  [[file:../disambr.src.org::*TEMPLATE][TEMPLATE:1]]
##' Reads the data for disambiguation
##' @param files_path Path to data. You can specify almost everything
##' @param save_sets_as How save sets
##' @param save_sets_dir where to save sets 
##' @param use_time_stamp wheather to use timestamp 
##' @return 
##' 
##' @md 
##' @export 
disambr_read <- function(files_path
                       , save_sets_as = NULL
                       , save_sets_dir = "disambr-data"
                       , use_time_stamp = FALSE) {
    disambr_mess_start()
    ## see if the data is available already
    if(is.character(save_sets_as) &&
       file.exists(file.path(save_sets_dir, save_sets_as))) {
        disambr_message(paste("- reusing saved sets:", save_sets_as))
        return(readRDS(file.path(save_sets_dir, save_sets_as)))
    }
    files_path <- parse_files_path(files_path)
    files_data_list <- lapply(files_path, disambr_read_file)
    sets <- disambr_make_data(files_data_list)
    ## save just in case
    if(is.character(save_sets_as)) {
        disambr_save_set(sets
                       , save_set_as =  save_sets_as
                       , save_set_dir = save_sets_dir
                       , use_time_stamp = use_time_stamp)
    }
    disambr_mess_finish()
    return(sets)
}
## --------<<  TEMPLATE:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_read][disambr_read:1]]
##' Reads the data for disambiguation
##' @param files_path Path to data. You can specify almost everything
##' @return 
##' 
##' @md 
##' @export 
disambr_read <- function(files_path
                       , save_sets_as = NULL
                       , save_sets_dir = "disambr-data"
                       , use_time_stamp = FALSE) {
    disambr_mess_start()
    ## see if the data is available already
    if(is.character(save_sets_as) &&
       file.exists(file.path(save_sets_dir, save_sets_as))) {
        disambr_message(paste("- reusing saved sets:", save_sets_as))
        return(readRDS(file.path(save_sets_dir, save_sets_as)))
    }
    files_path <- parse_files_path(files_path)
    files_data_list <- lapply(files_path, disambr_read_file)
    sets <- disambr_make_data(files_data_list)
    ## save just in case
    if(is.character(save_sets_as)) {
        disambr_save_set(sets
                       , save_set_as =  save_sets_as
                       , save_set_dir = save_sets_dir
                       , use_time_stamp = use_time_stamp)
    }
    disambr_mess_finish()
    return(sets)
}
## --------<<  disambr_read:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_read_file][disambr_read_file:1]]
##' Reads file based on file extention
##' @param f full file path name
##' @return data
##' 
##' @export 
disambr_read_file <- function(f) {
    f_extention <- tools::file_ext(f)
    switch(f_extention
         , "tsv" = disambr_read_tsv(f)
           ## here we can add reading from .txt wos files
         , "txt" = disambr_read_tsv(f)
         , message("Disambr: can not read file extention: ", f_extention
                 , "\n  - skipping file: ", f))
}
## --------<<  disambr_read_file:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_read_tsv][disambr_read_tsv:1]]
##' Read tsv file
##' @param f path
##' @return data
##' 
##' @export 
disambr_read_tsv <- function(f) {
      ## check tsv file type base on first line
      first_line <- readLines(f, n = 1
                            , warn = FALSE
                            , skipNul = TRUE)
      header <- parse_tsv_wos_header(first_line)
      if(!isFALSE(header)) {
          disambr_read_tsv_wos(f, header)
      } else {
          ## here we can add more tsv types
          message("Disambr: unrecognized header of tsv file: ", header
                , "\n  - skipping file: ", f)
          NULL
      }
  }
## --------<<  disambr_read_tsv:1 ends here



## -------->>  [[file:../disambr.src.org::*parse_tsv_wos_header][parse_tsv_wos_header:1]]
parse_tsv_wos_header <- function(first_line) {
    header <- stringi::stri_split_fixed(first_line, "\t")[[1]]
    if( ## check if at least 10 fields two big letters
        sum(stringi::stri_detect_regex(header, "^[A-Z0-9]{2}$")) > 10 &&
        ## check if main fields are present
        all(c('AU', 'TI') %in% header)) {
        stringi::stri_extract_first_regex(header, "[A-Z0-9]{2}")
    } else {FALSE}
}
## --------<<  parse_tsv_wos_header:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_read_tsv_wos][disambr_read_tsv_wos:1]]
##' Reads WoS tsv export file and makes disambr set out of it (just adding some attributes to the data.table)
##' @param f path
##' @param header header 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @export 
disambr_read_tsv_wos <- function(f, header) {
      s <- read_to_utf8(f)
      s <- recode_return_characters(s, f)
      f_data <- data.table::fread(text = s
                                , skip = 1
                                , strip.white = TRUE
                                , header = FALSE
                                , col.names = header
                                , select = 1:length(header)
                                  ## , colClasses = rep("character", length(header))
                                , quote=""
                                , keepLeadingZeros = FALSE
                                , encoding = "UTF-8"
                                , sep = "\t")
      ## set attrib (file, funcall, meanning of the fields and data scheme)
      disambr_add_set_attr(f_data, NULL
                     , unit = "publication"
                     , reference = "self"
                     , type = "different"
                     , id = "index"
                     , strength = 1
                     , name = "wos_tsv"
                     , collection = "unit_table"
                     , recipe = list(func = "disambr_read_tsv_wos"
                                   , file_name = f
                                   , file_md5sum = tools::md5sum(f)
                                   , file_header = header))
      return(f_data)
  }
## --------<<  disambr_read_tsv_wos:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_make_data][disambr_make_data:1]]
disambr_make_data <- function(files_data_list
                            , drop_ejected = FALSE) {
    ## TODO: add other data processing here
    ## TODO: add processing of wos data with differen headers
    ## check wos publication
    processabe_data <-
        disambr_in_sets(files_data_list, name = "wos_tsv")
    processabe_data <- files_data_list[processabe_data]
    if (length(processabe_data) != 0) {
        processabe_data_recipes <-
            lapply(processabe_data, attr, "disambr_set_recipe")
        processabe_data_headers <-
            lapply(processabe_data_recipes, `[[`, "file_header")
        ## check if all headers ate the same before rbindlist
        if (length(unique(processabe_data_headers)) == 1) {
            disambr_message("Processing wos tsv export data..")
            disambr_message("- rbinding wos publication tables..")
            wos_publication <-
                disambr_make_wos_tsv_publications(files_data_list, processabe_data_recipes)
            disambr_message("- making wos authors table..")
            wos_author <- disambr_make_wos_tsv_authors(wos_publication)
            if(drop_ejected) {
                ## remove fields that we do not need
                remove_headers <- c("AU", "AF", "C1", "RP", "EM", "RI", "OI")
                ## filter those that exists
                remove_headers <-
                    remove_headers[remove_headers %in% processabe_data_headers[[1]]]
                ## remove headers without hard copy
                ## to use a varialbe it should be in ()
                wos_publication[, (remove_headers) := NULL]
            }
            disambr_message("- making wos references table..")
            wos_reference <- disambr_make_wos_tsv_references(wos_publication)
            if(drop_ejected) {
                remove_headers <- c("CR")
                ## filter those that exists
                remove_headers <-
                    remove_headers[remove_headers %in% processabe_data_headers[[1]]]
                ## remove headers without hard copy
                wos_publication[, (remove_headers) := NULL]
            }
            disambr_message("- making author-year citations table..")
            citation_name_table <-
                disambr_make_wos_tsv_author_year_citations(wos_publication, wos_reference)
            ## if all data is wos data return only that
            if(length(processabe_data) == length(files_data_list)) {
                return(list(wos_publication
                          , wos_author
                          , wos_reference
                          , citation_name_table
                            ))
            } else {
                return(list(
                    ## TODO implement
                    ## disambr_subsets(files_data_list
                                  ## , list(disambr_set_name = "wos_records_tsv_export")
                                  ## , negate_subsets = TRUE)
                  wos_publication
                  , wos_author
                  , wos_reference
                  , citation_name_table
                ))
            }
        } else {
            disambr_message("Files data has different headers. Skipping processing...")
            return(files_data_list)
        }
    } else {
        return(files_data_list)
    }
}
## --------<<  disambr_make_data:1 ends here


