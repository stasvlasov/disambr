## -------->>  [[file:../disambr.src.org::*dhms][dhms:1]]
##' Formats time difference as X days HH:MM:SS
##'
##' from https://stackoverflow.com/questions/27312292
##' @param t time diff
##' @return formatted time diff string
##' 
##' @export 
dhms <- function(t) {
    t <-  abs(as.numeric(t, units = "secs"))
    paste(if((t %/% (60*60*24)) > 0) paste(t %/% (60*60*24), "days") else NULL
         ,paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
              , formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
              , formatC(t %% 60, width = 2, format = "d", flag = "0")
              , sep = ":"))
}
## --------<<  dhms:1 ends here



## -------->>  [[file:../disambr.src.org::*get_file_extension][get_file_extension:1]]
##' Extention extractor. Same as tools::file_ext but for NULL input returns NULL instead of logical(0).
##' @param f file name 
##' @return extention
##' 
##' @importFrom magrittr %>%
##' @export 
get_file_extension <- function(f) {
    if(length(f) == 1) {
        if(is.character(f)) {
            f %>% basename %>% 
                stringi::stri_split_fixed(".") %>% 
                extract2(1) %>%
                extract(ifelse(length(.) == 1, NA, length(.))) %>%
                ifelse(is.na(.), "", .)
        } else if(is.na(f)) {
            NA
        }
    } else {
        NULL
    }
}

## my.file <- '../data/Journals in Mathematical Psychology/Applied Psychological Measurement.txt' 
## my.file1 <- "/mnt/md5/data/wos/wos-sci-expanded.firm-names-query.analytical-instruments/LN Public NAICS records from 10001 to 10500.txt"

## get_file_extension(my.file)
## get_file_extension(my.file1)
## get_file_extension("sdfsdf....")
## get_file_extension("sdf")
## get_file_extension("")
## get_file_extension(NULL)
## get_file_extension(NA)
## get_file_extension("...sdf...sdf.df...sd.")
## get_file_extension(".")
## get_file_extension(".....")

## build in
## tools::file_ext(my.file)
## tools::file_ext(my.file1)
## tools::file_ext("sdfsdf....")
## tools::file_ext("sdf")
## tools::file_ext("")
## tools::file_ext(NULL)
## tools::file_ext(NA)
## tools::file_ext("...sdf...sdf.df...sd.")
## tools::file_ext(".")
## tools::file_ext(".....")
## --------<<  get_file_extension:1 ends here



## -------->>  [[file:../disambr.src.org::*stop_unless][stop_unless:1]]
##' Stops process unless cond is true
##' @param cond condition to test
##' @param message_if_false message_if_false
##' @param stop_if_false stop_if_false 
##' @param return_if_true return_if_true
##' @param return_if_false return_if_false 
##' @return 
##' 
##' @export 
stop_unless <- function(cond
                      , message_if_false = paste("cond in not TRUE")
                      , stop_if_false = TRUE
                      , return_if_true = TRUE
                      , return_if_false = isFALSE(return_if_true)) {
    if(isTRUE(cond)) {
        return(return_if_true)
    } else if(isTRUE(stop_if_false)){
        stop(message_if_false, call. = FALSE)
    } else {
        warning(message_if_false, call. = FALSE)
        return(return_if_false)
    }
}
## --------<<  stop_unless:1 ends here



## -------->>  [[file:../disambr.src.org::*parse_files_path][parse_files_path:1]]
##' Returns vector of file paths from path(s) recursively
  ##' @param files_path Path(s) where the files are
  ##' @param recursive Whether to look in subfolders recursively
  ##' @return Vector of file paths from path(s) recursively
  ##' 
  ##' @md
  ##' @importFrom magrittr %>%
  ##' @export 
  parse_files_path <- function(files_path, recursive = TRUE) {
      stop_unless(is.character(files_path), "Files path shoud be a character string!")
      files_path <- 
      lapply(files_path, function(file.path) {
          if(stop_unless(file.exists(file.path)
                       , paste(file.path, " - does not exist!")
                       , stop_if_false = FALSE
                       , return_if_true = FALSE)) {
              NULL
          } else if(dir.exists(file.path)) {
              dir(file.path
                , full.names = TRUE
                , recursive = recursive)
          } else {
              file.path
          }
      })
      return(unique(normalizePath(unlist(files_path))))
  }
## --------<<  parse_files_path:1 ends here



## -------->>  [[file:../disambr.src.org::*read_to_utf8][read_to_utf8:1]]
##' Reads file as UTF-8, convert it if other encoding is deteted
##' @param f file path
##' @param bytes_to_check how long to check for encoding (save time for large files)
##' @return file text as string
##' 
##' @export 
read_to_utf8 <- function(f, bytes_to_check = 2^14) {
    ## read file as raw bytes (not to Assume any encodings)
    bin <- readBin(f, raw(), n = file.size(f))
    ## check first 2^14 bytes for encoding
    encoding <- stringi::stri_enc_detect2(bin[1:bytes_to_check])[[1]][[1]][1]
    if(is.na(encoding)) {
        message("Could not detect encoding of file: ", f)
        s <- rawToChar(bin, multiple = FALSE)
    } else if(!(encoding %in% iconvlist())) {
        message("Does not know how to convert from ", encoding, "for file: ", f)
    } else if(encoding == "UTF8") {
        s <- rawToChar(bin, multiple = FALSE)
    } else {
        ## message("Converting to utf-8")
        s <- iconv(list(NULL, bin), from = encoding, to = "UTF-8")
    }
    return(s)
}

## stringi::stri_enc_detect2(NULL)[[1]][[1]][1]
## stringi::stri_enc_detect2(NA)[[1]][[1]][1]
## stringi::stri_enc_detect2(123)[[1]][[1]][1]
## stringi::stri_enc_detect2("")[[1]][[1]][1]
## stringi::stri_enc_detect2("sadf")[[1]][[1]][1]
## --------<<  read_to_utf8:1 ends here



## -------->>  [[file:../disambr.src.org::*recode_return_characters][recode_return_characters:1]]
##' Fixed end of line characters in wierd text
##' @param s text string
##' @param assoc.file file name where it came from
##' @param verbose Be chatty
##' @return fixed sting
##' 
##' @export 
recode_return_characters <- function(s, assoc.file = NA, verbose = FALSE) {
    has_return_chars <- function(s, test.first.n.char = 10^4) {
        s <- stringi::stri_sub(s, to = test.first.n.char)
        any(stringi::stri_detect_regex(s, "\\r"))
    }
    if(has_return_chars(s)) {
        if(verbose) message("disambr: '\\r' char in the file: ", assoc.file
                          , "\n- replacing with '\\n' to fix 'datatable::fread'")
        s <- stringi::stri_replace_all_regex(s, "\\R+", "\n")
    }
    return(s)
}
## --------<<  recode_return_characters:1 ends here



## -------->>  [[file:../disambr.src.org::*match_fuzzy][match_fuzzy:1]]
##' Fuzzy match all combinations of character vector
##' @param bank bank
##' @param method see method in stringdist
##' @param max_dist see maxDist in stringdist
##' @param id_name names that will be suffixed with _1 and _2
##' @return data.table
##' 
##' @export 
match_fuzzy <- function(bank, method, max_dist, id_name) {
        id_name_1 <- paste0(id_name, "_1")
        id_name_2 <- paste0(id_name, "_2")
        match_fuzzy_x <- function(x) {
            matched <- stringdist::ain(bank, x
                                     , maxDist = max_dist
                                     , method = method
                                     , matchNA = FALSE)
            if(any(matched)) {
                matched <- bank[matched]
                matched <- data.table::data.table(x, matched)
                data.table::setnames(matched, c(id_name_1, id_name_2))
            } else {
                NULL
            }
        }
        match_x <- function(x) {
            matched <- bank %in% x
            if(any(matched)) {
                matched <- bank[matched]
                matched <- data.table::data.table(x, matched)
                data.table::setnames(matched, c(id_name_1, id_name_2))
            } else {
                NULL
            }
        }
        if(max_dist > 0) {
            matched_list <- lapply(bank, match_fuzzy_x)
        } else if(max_dist == 0) {
            matched_list <- lapply(bank, match_x)
        } else {
            stop()
        }
        return(data.table::rbindlist(matched_list))
}
## --------<<  match_fuzzy:1 ends here



## -------->>  [[file:../disambr.src.org::*get_upper_triangle_index][get_upper_triangle_index:1]]
##' Returns the index of element in upper triangle of squared `n` by `n` matrix
##'
##' inspired from https://math.stackexchange.com/questions/2134011 but modified so
##' index starts from 1 as the original answer was for index starting from 0
##' 
##' @param i row index
##' @param j column index
##' @param n the size of squared matrix
##'
##' @return An index as integer number. Index starts from 1
get_upper_triangle_index <- function(i, j, n) {
    if(j >= i) {
        n*(n - 1)/2 - (n - i)*(n - i + 1)/2 + j
    } else {
        index_upper_triangle(j, i, n)
    }
}
## --------<<  get_upper_triangle_index:1 ends here


