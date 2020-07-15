#' @details
#' AND EVA Algorithm
#' This is work in progress. Please, file an issues or suggestion if you have any.
#' @keywords internal
"_PACKAGE"

##' Stops process unless cond is true
##' @param cond 
##' @param message.if.false 
##' @param stop.if.false 
##' @param return.if.true 
##' @param return.if.false 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @export 
stop.unless <- function(cond
                      , message.if.false = paste("cond in not TRUE")
                      , stop.if.false = TRUE
                      , return.if.true = TRUE
                      , return.if.false = isFALSE(return.if.true)) {
    if(isTRUE(cond)) {
        return(return.if.true)
    } else if(isTRUE(stop.if.false)){
        stop(message.if.false, call. = FALSE)
    } else {
        warning(message.if.false, call. = FALSE)
        return(return.if.false)
    }
}


##' Returns vector of file paths from path(s) recursively
##' @param files.path Path(s) where the files are
##' @param recursive Whether to look in subfolders recursively
##' @return Vector of file paths from path(s) recursively
##' 
##' @md
##' @importFrom magrittr %>%
##' @export 
parse.files.path <- function(files.path, recursive = TRUE) {
    stop.unless(is.character(files.path), "Files path shoud be a character string!")
    lapply(files.path, function(file.path) {
        if(stop.unless(file.exists(file.path)
                     , paste(file.path, " - does not exist!")
                     , stop.if.false = FALSE
                     , return.if.true = FALSE)) {
            NULL
        } else if(dir.exists(file.path)) {
            dir(file.path
              , full.names = TRUE
              , recursive = recursive)
        } else {
            file.path
        }
    }) %>% unlist %>% normalizePath %>% unique
}

##' Reads all the data
##' @param files.path anything goes
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @export 
disambr.read <- function(files.path) {
    files.path <- parse.files.path(files.path)
    lapply(files.path, disambr.read.file)
}


disambr.read.file <- function(f) {
    f.extention <- tools::file_ext(f)
    switch(f.extention
         , "tsv" = disambr.read.tsv(f)
           ## here we can add reading from .txt wos files
         , "txt" = disambr.read.tsv(f)
         , message("No procedure is defined for the extention: ", f.extention
                 , "\n\\->Skipping file: ", f))
}


disambr.read.tsv <- function(f) {
    ## check tsv file type base on first line
    first.line <- readLines(f, n = 1
                          , warn = FALSE
                          , skipNul = TRUE)
    header <- parse.tsv.wos.header(first.line)
    if(!isFALSE(header)) {
        disambr.read.tsv.wos(f, header)
    } else {
        ## here we can add more tsv types
        NULL
    } %>% return()
}

parse.tsv.wos.header <- function(first.line) {
    header <- stri_split_fixed(first.line, "\t")[[1]]
    if( ## check if at least 10 fields two big letters
        sum(stri_detect_regex(header, "^[A-Z0-9]{2}$")) > 10 &&
        ## check if main fields are present
        all(c('AU', 'TI') %in% header)) {
        stri_extract_first_regex(header, "[A-Z0-9]{2}")
    } else {FALSE}
}


disambr.read.tsv.wos <- function(f, header) {
    s <- read.to.utf8(f)
    s <- recode.return.characters(s, f)
    dat <- fread(text = s
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
    dat$AU <- disambr.read.tsv.wos.parse.AU(dat$AU)
    ## set attrib (file, funcall, meanning of the fields and data scheme)
    attributes(dat)$disambr.read.call <- "disambr.read.tsv.wos"
    attributes(dat)$disambr.read.file.md5 <- tools::md5sum(f)
    attributes(dat)$disambr.set.unit <- "publication"
    attributes(dat)$disambr.set.unit.ids.self <- TRUE
    return(dat)
}



disambr.read.tsv.wos.parse.AU <- function(au) {
    au <- stri_split_fixed(au, ";")
    parse.a <- function(a) {
        a <- stri_trim(a)
        last.name <- stri_extract_first_regex(a, "^.+(?=,)")
        initials <- stri_extract_first_regex(a, "(?<=, )[A-Z]+")
        mapply(function(x, y) list(initials = x, last.name = y)
             , initials
             , last.name
             , SIMPLIFY = FALSE
             , USE.NAMES = FALSE)
    }
    lapply(au, parse.a)
}



read.to.utf8 <- function(f, bytes.to.check = 2^14) {
    ## read file as raw bytes (not to Assume any encodings)
    bin <- readBin(f, raw(), n = file.size(f))
    ## check first 2^14 bytes for encoding
    encoding <- stringi::stri_enc_detect2(bin[1:bytes.to.check])[[1]][[1]][1]
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

recode.return.characters <- function(s, assoc.file = NA) {
    has.return.chars <- function(s, test.first.n.char = 10^4) {
        s <- stri_sub(s, to = test.first.n.char)
        any(stri_detect_regex(s, "\\r"))
    }
    if(has.return.chars(s)) {
        message("'\\r' characters in the file: ", assoc.file
              , "\n Removing to fix 'datatable::fread'")
        s <- stri_replace_all_regex(s, "\\R+", "\n")
    }
    return(s)
 }

##' Returns sets of people ids that are defenetely different based on co-authorship
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.different.authors <- function(sets, procedures = NULL) {
    if(!is.list(sets)) stop("sets should be list!")
    new.set <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "publication") %>%
        data.table::rbindlist(fill=TRUE) %>%
        ## extract2(1) %>%
        ## TODO: implement extraction from different data type
        extract2("AU") %>%
        lapply(length) %>%
        lapply(seq) %>%
        {mapply(function(x,y) lapply(x, function(x) c(y,x))
              , ., 1:length(.)
              , SIMPLIFY = FALSE)}
    ## set set's attributes
    attributes(new.set)$disambr.set.unit <- "person"
    attributes(new.set)$disambr.set.unit.ref.md5 <- digest(sets[[1]], "md5")
    return(c(sets, list(new.set)))
}


##' Returns set of people with similar initials
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.similar.initials <- function(sets, procedures = NULL) {
    ## sapply(sets,attr, "disambr.set.unit")
    set.different.authors <-
        sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1) ## %>% extract(1:3)
        ## sets$different.authors## [1:3]
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "publication") %>%
        data.table::rbindlist(fill=TRUE) 
        ## sets$data[[1]]
    ## procedurs
    get.initials.by.address <- function(address) {
        set.data$AU[[address[1]]][[address[2]]][["initials"]]
    }
    subset.similar.initials <- function(comb) {
        a <- set.different.authors[[comb[1]]]
        b <- set.different.authors[[comb[2]]]
        expand.grid(a, b)
    }
    new.set <- combn(1:length(set.different.authors), 2
               , simplify = FALSE
               , FUN = subset.similar.initials) %>%
        data.table::rbindlist() %>% 
        dplyr::mutate(initials.dist = stringdist(sapply(Var1, get.initials.by.address)
                                               , sapply(Var2, get.initials.by.address)
                                               , method = "lv")) %>%
        dplyr::filter(initials.dist < 2)
        attributes(new.set)$disambr.set.unit <- "similar.initials"
        return(c(sets, list(new.set)))
}


##' Returns set of people with similar last names
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.similar.last.names <- function(sets, procedures = NULL) {
    ## TODO: extract teh set that we need here (person, dyads)
    set.similar.initials <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.initials") %>%
        extract2(1) ## %>% extract(1:3)
    ## sets[['similar.initials']]
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "publication") %>%
        data.table::rbindlist(fill=TRUE) 
    get.attrib.by.address <- function(address, attrib) {
        set.data$AU[[address[1]]][[address[2]]][[attrib]]
    }
    set.similar.initials %>%
        ## {print(names(.))} %>% 
        dplyr::mutate(last.name.dist =
                      stringdist(sapply(Var1, get.attrib.by.address, "last.name")
                               , sapply(Var2, get.attrib.by.address, "last.name")
                               , method = "dl"))
}
