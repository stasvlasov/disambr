## [[file:~/org/research/disambr/disambr/disambr.src.org::*disambr_read][disambr_read:1]]
##' Stops process unless cond is true
##' @param cond 
##' @param message.if.false 
##' @param stop.if.false 
##' @param return.if.true 
##' @param return.if.false 
##' @return 
##' 
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


##' Reads the data for disambiguation
##' @param files.path Path to data. You can specify almost everything
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @export 
disambr.read <- function(files.path) {
    files.path <- parse.files.path(files.path)
    data.list <- lapply(files.path, disambr.read.file)
    ## TODO: check for consistensy between files
    ## TODO: break data into chunks
    wos.publication <- data.table::rbindlist(data.list, fill=TRUE)
    ## TODO: Combind attributes from files
    attributes(wos.publication)$disambr.set.unit <- "publication"
    attributes(wos.publication)$disambr.set.unit.ids.self <- TRUE
    wos.author <- disambr_eject_authors(wos.publication)
    wos.publication[, c("AU", "AF", "C1", "RP", "EM", "RI", "OI") := NULL]
    attributes(wos.author)$disambr.set.unit <- "person"
    attributes(wos.author)$disambr.set.unit.ids.self <- TRUE
    wos.reference <- disambr_eject_references(wos.publication)
    wos.publication[, "CR" := NULL]
    attributes(wos.reference)$disambr.set.unit <- "reference"
    attributes(wos.reference)$disambr.set.unit.ids.self <- TRUE
    return(list(wos.publication, wos.author, wos.reference))
}


## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2)


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
    }
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
    ## this should be ejected
    ## dat$AU <- disambr.read.tsv.wos.parse.authors(dat$AU
    ##                                            , dat$EM
    ##                                            , dat$RP)
    ## dat$AF <- disambr.read.tsv.wos.parse.AF(dat$AF)
    ## set attrib (file, funcall, meanning of the fields and data scheme)
    ## this also can be a separate function to set atribute to data
    attributes(dat)$disambr.read.call <- "disambr.read.tsv.wos"
    attributes(dat)$disambr.read.file.md5 <- tools::md5sum(f)
    attributes(dat)$disambr.set.unit <- "publication"
    attributes(dat)$disambr.set.unit.ids.self <- TRUE
    return(dat)
}





## disambr.read.tsv.wos.parse.authors <- function(au, em, rp) {
##     au <- stri_split_fixed(au, ";")
##     em <- stri_split_fixed(em, ";")
##     rp <- stri_split_fixed(rp, ";")
##     parse.a <- function(authrs, emails) {
##         authrs <- stri_trim(authrs)
##         emails <- stri_trim(emails)
##         reprints <- stri_trim(reprints)
##         reprints <- stri_replace_first_regex(authrs, "\\s+(corresponding author).*", "")
##         reprints <- stri_trim(reprints)
##         reprints <- unique(reprints)
##         last.name <- stri_extract_first_regex(authrs, "^.+(?=,)")
##         initials <- stri_extract_first_regex(authrs, "(?<=, )[A-Z]+")
##         if(length(emails) == length(authrs)) {
##             emails <- emails
##         } else if(length(emails) == length(reprints)) {
##             ## assume same names
##             emails <- emails[match(authrs, reprints)]
##         } else if(length(emails) == 1) {
##             emails <- emails[ifelse(authrs == reprints[1], 1, NA)]
##         } else {
##             emails <- ifelse(authrs == reprints[1], emails, NA)
##         }
##         mapply(function(x, y, z)
##             list(initials = x
##                , last.name = y
##                , email = z)
##           , initials
##           , last.name
##           , email
##           , SIMPLIFY = FALSE
##           , USE.NAMES = FALSE)
##     }
##     lapply(au, parse.a)
## }

## disambr.read.tsv.wos.parse.AU <- function(au) {
##     au <- stri_split_fixed(au, ";")
##     parse.a <- function(a) {
##         a <- stri_trim(a)
##         last.name <- stri_extract_first_regex(a, "^.+(?=,)")
##         initials <- stri_extract_first_regex(a, "(?<=, )[A-Z]+")
##         mapply(function(x, y) list(initials = x, last.name = y)
##              , initials
##              , last.name
##              , SIMPLIFY = FALSE
##              , USE.NAMES = FALSE)
##     }
##     lapply(au, parse.a)
## }

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




## utils

##' Makes list of each element of l
##' @param l sequence or list
##' @param l.name same name will be applies to each element
##' @return list of lists
##' 
##' @export 
disambr_listify_list <- function(l, l.name = NULL) {
    if(isTRUE(l.name == "")) l.name =  NULL
    ## case when all are 1 length (vector or list of single length elements)
    lapply(l, function(x) {
        x <- list(x)
        names(x) <- l.name
        return(x)
    })
}


##' cbinds lists and names each element as name of each list in ...
##' @param ... Lists to cbin
##' @return Lists
##' @export 
disambr_cbind_lists <- function(...) {
    lists <- eval(...)
    lists_n <- length(lists)
    lists_names <- names(lists)
    cbind_list <- disambr_listify_list(lists[[1]], lists_names[1])
    for (i in 2:lists_n) {
        cbind_list <- 
            mapply(c
                 , cbind_list
                 , disambr_listify_list(lists[[i]], lists_names[i])
                 , SIMPLIFY = FALSE)
    }
    return(cbind_list)
}


## parsers

##' Parses AU column of WoS saved records export
##' @param record.au a record string from AU column
##' @return data.table
##' 
##' @md 
disambr_eject_authors_parse_au <- function(record_au) {
    author_name <- stringi::stri_split_fixed(record_au, "; ")[[1]]
    author_last_name <-
        stringi::stri_extract_first_regex(author_name, "^[^,]+")
    author_initials <-
        stringi::stri_extract_first_regex(author_name, "(?<=, )[A-Z]+")
    data.table::data.table(author_name = author_name
                         , author_last_name = author_last_name
                         , author_initials = author_initials
                         , author_order = 1:length(author_name))
}


## tests

## "Tilly, TB; Nelson, MT; Chakravarthy, KB; Shira, EA; Debrose, MC; Grabinski, CM; Salisbury, RL; Mattie, DR; Hussain, SM" %>% 
## disambr_eject_authors_parse_au




##' Parses AF (author full name) column of WoS saved records export
##' @param record.au a record string from AF column
##' @return Data.table
disambr_eject_authors_parse_af <- function(record_af) {
    name <- stringi::stri_split_fixed(record_af, "; ")[[1]]
    last_name <- stringi::stri_extract_first_regex(name, "^[^,]+")
    first_names <- stringi::stri_extract_first_regex(name, "(?<=, ).*")
    first_names <-
        stringi::stri_split_fixed(first_names, " ", omit_empty = TRUE)
    ## first.full.name is first name without dot
    first_full_name <-
        lapply(first_names, function(n) {
            n[!stringi::stri_detect_regex(n, "\\.$")][1]
        })
    ## return
    data.table::data.table(
                    author_full_name = name
                  ## , author_last_name = last_name 
                  , author_first_names = first_names
                  , author_first_full_name =  first_full_name)
}


## test
## "Tilly, Trevor B.; Nelson, M. Tyler; Chakravarthy, Karthik B.; Shira, Emily A.; Debrose, Madeline C.; Grabinski, Christin M.; Salisbury, Richard L.; Mattie, David R.; Hussain, Saber M." %>%
## disambr_eject_authors_parse_af


##' Parses RP (reprint author) column of WoS saved records export
##' @param record_rp a record string from RP column
##' @return Data.table with two columns -  author_name and affiliations
disambr_eject_authors_parse_rp <- function(record_rp) {
    record_rp_init <- ""
    authors_table <-
        data.table::data.table(author_name = character(0)
                             , affiliations = character(0))
    while(record_rp != record_rp_init) {
        record_rp_init <- record_rp
        record_rp_split <- 
            stringi::stri_match_first_regex(
                         record_rp
                       , "\\s*([^()]+)\\s+\\((corresponding author|reprint author)\\)([^;]+)")
        authors <-
            stringi::stri_split_fixed(record_rp_split[1,2], "; ")[[1]]
        affiliation <-
            stringi::stri_replace_first_regex(
                         record_rp_split[1,4], "^[\\s,.;]+", "")
        for (author in authors) {
            ## check if author is already in the list
            authors_table_match <-
                authors_table$author_name %in% author
            if(any(authors_table_match)) {
                ## add affiliation to affiliations of author
                ## the data.table way..
                authors_table[authors_table_match
                            , affiliations :=
                                  list(c(unlist(affiliations), affiliation))]
            } else {
                ## add new author with affiliation otherwise
                authors_table <-
                    data.table::rbindlist(
                                    list(authors_table
                                       , list(author_name = author
                                            , affiliations =
                                                  list(affiliation))))
            }
        }
        record_rp <-
            stringi::stri_replace_first_regex(
                         record_rp
                       , "[^()]+\\((corresponding author|reprint author)\\)[^;]+[;]", "")
    }
    ## results are not printed but the data.table is returned
    return(authors_table)
}

## "Guesmi, S (corresponding author), Natl Agron Inst Tunisia INAT, 43 Ave Charles Nicolle, Tunis 1082, Tunisia.; Guesmi, S; Sghaier, H (corresponding author), Sidi Thabet Technopk, Natl Ctr Nucl Sci & Technol, Lab Energy & Matter Dev Nucl Sci LR16CNSTN02, Sidi Thabet 2020, Tunisia.; Sghaier, H (corresponding author), Sidi Thabet Technopk, Lab Biotechnol & Nucl Technol LR16CNSTN01, Sidi Thabet 2020, Tunisia.; Sghaier, H (corresponding author), Sidi Thabet Technopk, Lab Biotechnol & Biogeo Resources Valorizat LR11E, Sidi Thabet 2020, Tunisia." %>%
## disambr_eject_authors_parse_rp %>% print

## "" %>%
## disambr_eject_authors_parse_rp %>% nrow


##' Parses EM (email) column of WoS saved records export
##' @param record_em  a record string from EM column
##' @param record_au_table a data_tabe after parsing AU column with disambr_eject_authors_parse_au
##' @param record_rp_table a data_tabe after parsing RP column with disambr_eject_authors_parse_rp
##' @return Data.table with columns - author_name, affiliations and email
disambr_eject_authors_parse_em <- function(record_em
                                         , record_au_table
                                         , record_rp_table) {
    emails <- stringi::stri_split_fixed(record_em, "; ")[[1]]
    if (length(emails) == 1 && emails == "") {
        ## in case there are no emails
        record_au_table[, author_email := NA]
    } else if (length(emails) == nrow(record_rp_table)) {
        ## assume that emails corresponds RP authors
        record_au_table[match(record_rp_table$author_name, author_name)
                      , author_email := emails]
    } else if (length(emails) == nrow(record_au_table)) {
        ## assume that emails corresponds AU authors
        record_au_table[, author_email := emails]
    } else if (nrow(record_rp_table) != 0) {
        ## in other cases just use first email for first RP author
        record_au_table[match(record_rp_table$author_name, author_name)[1]
                      , author_email := emails[1]]
    } else {
        ## if no RP assignt to first in AU
        record_au_table[1, author_email := emails[1]]
    }
    ## we do not need to return things as it updates record_au_table
    return(record_au_table)
}


## tests
## disambr_eject_authors_parse_em(
## record_em = "a"
## , record_au_table = data.table(author_name = c(1,2,3,4))
## , record_rp_table = data.table(author_name = c(3))
## ) %>% print




##' Parses C1 (author adress/affiliation) column of WoS saved records export
##' @param record_c1 a record string from RP column
##' @return Data.table with two columns -  author_name and affiliations
disambr_eject_authors_parse_c1 <- function(record_c1
                                         , table_af = NULL) {
    record_c1_init <- ""
    authors_table <-
        data.table::data.table()
    while(record_c1 != record_c1_init) {
        record_c1_init <- record_c1
        record_c1_piece <- 
            stringi::stri_match_first_regex(
                         record_c1, "\\s*\\[([^\\[\\]]+)\\]\\s+([^;]+)\\s*")
        authors <-
            stringi::stri_split_fixed(record_c1_piece[1,2], "; ")[[1]]
        affiliation <- record_c1_piece[1,3]
        for (author in authors) {
            ## check if author is already in the list
            authors_table_match <-
                authors_table$author_full_name %in% author
            if(any(authors_table_match)) {
                ## add affiliation to affiliations of author
                ## the data.table way..
                authors_table[authors_table_match
                            , affiliations :=
                                  list(c(unlist(affiliations), affiliation))]
            } else {
                ## add new author with affiliation otherwise
                authors_table <-
                    data.table::rbindlist(list(authors_table
                                             , list(author_full_name = author
                                                  , affiliations = list(affiliation))))
            }
        }
        record_c1 <-
            stringi::stri_replace_first_regex(
                         record_c1, "\\s*\\[[^\\[\\]]+\\][^;]+[;]", "")
    }
    ## merge with table_af if provided
    if(length(table_af) != 0) {
        return(authors_table[table_af
                           , on = "author_full_name"
                           , .(affiliations)])
    } else {
        return(authors_table)
    }
}



## "[Wang, Menglei; Li, Shunyi; Zhu, Rencheng; Zhang, Ruiqin] Zhengzhou Univ, Sch Ecol & Environm, Zhengzhou 450001, Peoples R China; [Wang, Menglei] Zhengzhou Univ, Sch Chem Engn, Zhengzhou 450001, Peoples R China; [Zu, Lei; Wang, Yunjing; Bao, Xiaofeng] Chinese Res Inst Environm Sci, State Environm Protect Key Lab Vehicle Emiss Cont, Beijing 100012, Peoples R China" %>%
## disambr_eject_authors_parse_c1


## "[Wang, Menglei; Li, Shunyi; Zhu, Rencheng; Zhang, Ruiqin] Zhengzhou Univ, Sch Ecol & Environm, Zhengzhou 450001, Peoples R China; [Wang, Menglei] Zhengzhou Univ, Sch Chem Engn, Zhengzhou 450001, Peoples R China; [Zu, Lei; Wang, Yunjing; Bao, Xiaofeng] Chinese Res Inst Environm Sci, State Environm Protect Key Lab Vehicle Emiss Cont, Beijing 100012, Peoples R China" %>%
## disambr_eject_authors_parse_c1(disambr_eject_authors_parse_af("Wang, Menglei; Li, Shunyi; Zhu, Rencheng; Zhang, Ruiqin; Zu, Lei; Wang, Yunjing; Bao, Xiaofeng"))






##' Parses RI (researcher_id) column of WoS saved records export
##' @param record_ri a record string from RP column
##' @param table_af 
##' @return Data.table with columns - author_full_name and author_researcher_id 
disambr_eject_authors_parse_ri <- function(record_ri
                                         , table_af = NULL) {
    if(record_ri != "") {
        authors <- stringi::stri_split_fixed(record_ri, "; ")[[1]]
        authors_list <- lapply(authors, function(author) {
            author_split <- stringi::stri_split_fixed(author, "/", n = 2)[[1]]
            list(author_full_name = author_split[1]
               , author_researcher_id = author_split[2])
        })
        authors_table <- data.table::rbindlist(authors_list)
    } else {
        authors_table <- data.table::data.table(author_full_name = character()
                                              , author_researcher_id = character())
    }
    ## merge with table_af if provided
    if(length(table_af) != 0) {
        return(authors_table[table_af
                           , on = "author_full_name"
                           , .(author_researcher_id)])
    } else {
        return(authors_table)
    }
}

## "Girabent, Montserrat/B-8536-2008; Maydeu-Olivares, Alberto/B-5178-2010" %>%
## disambr_eject_authors_parse_ri


##' Parses OI (ORCID) column of WoS saved records export
##' @param record_RI a record string from OI column
##' @return Data.table with columns - author_full_name and author_orcid
disambr_eject_authors_parse_oi <- function(record_oi
                                         , table_af = NULL) {
    if(record_oi != "") {
        authors <- stringi::stri_split_fixed(record_oi, "; ")[[1]]
        authors_list <- lapply(authors, function(author) {
            author_split <- stringi::stri_split_fixed(author, "/", n = 2)[[1]]
            list(author_full_name = author_split[1]
               , author_orcid = author_split[2])
        })
        authors_table <- data.table::rbindlist(authors_list)
    } else {
        authors_table <- data.table::data.table(author_full_name = character()
                                              , author_orcid = character())
    }
    ## merge with table_af if provided
    if(length(table_af) != 0) {
        return(authors_table[table_af
                           , on = "author_full_name"
                           , .(author_orcid)])
    } else {
        return(authors_table)
    }
}


## "Estrela, Pedro/0000-0001-6956-1146; Maxted, Grace/0000-0002-6816-9107; Rainbow, Joshua/0000-0003-3911-928X; Richtera, Lukas/0000-0002-8288-3999; Moschou, Despina/0000-0001-9175-5852" %>% disambr_eject_authors_parse_oi


## related fields (as in Web of Science Field Tags 2018-06-27)
## au
## af full names
## - ba book
## - bf book
## - ca gp group author (usually organization or group name)
## - be editors
## c1 adresses
## rp reprint address (one you contact for reprint copy)
## em emails
## ri researcher ID
## oi ORCID Identifier (Open Researcher and Contributor ID)
## eject authors table (after combining initiall export tables)
disambr_eject_authors <- function(wos_data_table
                                , list_of_author_fields =
                                      c("author_order"
                                      , "author_short_name"
                                      , "author_initials"
                                      , "author_last_name"
                                      , "author_full_name"
                                      , "author_first_names"
                                      , "author_first_full_name"
                                      , "author_email"
                                      , "author_researcher_id"
                                      , "author_orcid"
                                      , "author_affiliations")) {
    authors_tables <- list()
    ## AU
    if(any(c("author_order"
           , "author_short_name"
           , "author_last_name"
           , "author_initials"
           , "author_email") %in% list_of_author_fields) &&
       "AU" %in% names(wos_data_table)) {
        authors_tables$au <-
            lapply(wos_data_table$AU
                 , disambr_eject_authors_parse_au)

    }
    ## AF
    if(any(c("author_full_name"
           , "author_first_names"
           , "author_first_full_name"
           , "author_researcher_id"
           , "author_orcid"
           , "author_affiliations") %in% list_of_author_fields) &&
       "AF" %in% names(wos_data_table)) {
        authors_tables$af <-
            lapply(wos_data_table$AF
                 , disambr_eject_authors_parse_af)
    }

    ## RP
    if(any(c("author_email") %in% list_of_author_fields) &&
       "RP" %in% names(wos_data_table)) {
        ## save RP separately as it is different order from AU
        rp <-
            lapply(wos_data_table$RP
                 , disambr_eject_authors_parse_rp)
    }

    ## EM
    if(any(c("author_email") %in% list_of_author_fields) &&
       all(c("AU", "EM", "RP") %in% names(wos_data_table))) {
        ## disambr_eject_authors_parse_em updates authors_tables$au
        ## so no need to save it
        mapply(disambr_eject_authors_parse_em
             , wos_data_table$EM
             , authors_tables$au
             , rp
             , SIMPLIFY = FALSE
             , USE.NAMES = FALSE)
    }

    ## C1
    ## if(any(c("author_affiliations") %in% list_of_author_fields) &&
    ##    all(c("C1", "AF") %in% names(wos_data_table))) {
    ##     authors_tables$c1 <-
    ##         mapply(disambr_eject_authors_parse_c1
    ##              , wos_data_table$C1
    ##              , authors_tables$af
    ##              , SIMPLIFY = FALSE)
    ## }

    ## RI
    if(any(c("author_researcher_id") %in% list_of_author_fields) &&
       "RI" %in% names(wos_data_table)) {
        authors_tables$ri <-
            mapply(disambr_eject_authors_parse_ri
                 , wos_data_table$RI
                 , authors_tables$af
                 , SIMPLIFY = FALSE
                 , USE.NAMES = FALSE)
    }

    ## OI
    if(any(c("author_orcid") %in% list_of_author_fields) &&
       "OI" %in% names(wos_data_table)) {
        authors_tables$oi <-
            mapply(disambr_eject_authors_parse_oi
                 , wos_data_table$OI
                 , authors_tables$af
                 , SIMPLIFY = FALSE
                 , USE.NAMES = FALSE)
    }
    ## remove duplicated columns
    authors_tables <- 
        lapply(authors_tables, rbindlist, idcol = "paper_id")
    authors_table <- do.call(cbind, c(authors_tables, list(deparse.level = 0)))
    authors_table_names <- 
        stringi::stri_replace_first_regex(names(authors_table), "^[^\\.]+\\.", "")
    authors_table_select <- which(!duplicated(authors_table_names))
    authors_table_new_names <- authors_table_names[authors_table_select]
    authors_table <- authors_table[, authors_table_select, with = FALSE]
    names(authors_table) <- authors_table_new_names
    return(authors_table)
}

## test
## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2)[[1]]
## dt %>% disambr_eject_authors

## testing dt merge
## a <- data.table(name = c("a", "b", "c"), order = c(1,2,3))
## b <- data.table(named = c("c", "b", "c"), affil = c("b-adfsa","c-sadfsd"))
## cbind(a, b, check.names = FALSE)

## ----------------------------------------------------------------------------





## CR (Cited References)

disambr_parse_references <- function(record_cr) {
    references <- stringi::stri_split_fixed(record_cr, "; ")[[1]]
    references_list <- stringi::stri_split_fixed(references, ", ")
    references_list <-
        lapply(references_list, function(ref) {
            first_author_name <- ref[1]
            year <- ref[2]
            outlet <- ref[3]
            ref_tail <- ref[-c(1:3)]
            vol <- stringi::stri_extract_first_regex(ref_tail, "^V(\\d+)")
            vol <- vol[!sapply(vol, is.na)]
            page <- stringi::stri_extract_first_regex(ref_tail, "^P(\\d+)")
            page <- page[!sapply(page, is.na)]
            doi <- stringi::stri_extract_first_regex(ref_tail, "^DOI \\[*(.*)\\]*")
            doi <- doi[!sapply(doi, is.na)]

            list(first_author_name = ref[1]
               , year = ref[2]
               , outlet = ref[3]
               , vol = vol
               , page = page
               , doi = doi)
        })
    suppressWarnings(rbindlist(references_list))
}

## "Allen C, 2017, ENVIRON SCI-NANO, V4, P741, DOI 10.1039/c7en90014g; Baek YW, 2011, SCI TOTAL ENVIRON, V409, P1603, DOI 10.1016/j.scitotenv.2011.01.014; Baker GL, 2008, TOXICOL SCI, V101, P122, DOI 10.1093/toxsci/kfm243; Bergstrom U, 2015, J TOXICOL ENV HEAL A, V78, P645, DOI 10.1080/15287394.2015.1017682; Bhushan B, 2011, PROG MATER SCI, V56, P1, DOI 10.1016/j.pmatsci.2010.04.003; Biswas P, 2005, J AIR WASTE MANAGE, V55, P708, DOI 10.1080/10473289.2005.10464656; Bitterle E, 2006, CHEMOSPHERE, V65, P1784, DOI 10.1016/j.chemosphere.2006.04.035; Bondarenko O, 2013, ARCH TOXICOL, V87, P1181, DOI 10.1007/s00204-013-1079-4; Bonner J. C., 2003, ENV HLTH PERSPECT, V111, P1289; Brossell D, 2013, J AEROSOL SCI, V63, P75, DOI 10.1016/j.jaerosci.2013.04.012; Clift MJD, 2011, ARCH TOXICOL, V85, P723, DOI 10.1007/s00204-010-0560-6; Cohen J, 2013, NANOTOXICOLOGY, V7, P417, DOI 10.3109/17435390.2012.666576; Cohen JM, 2014, PART FIBRE TOXICOL, V11, DOI 10.1186/1743-8977-11-20; Comouth A, 2013, J AEROSOL SCI, V63, P103, DOI 10.1016/j.jaerosci.2013.04.009" %>% disambr_parse_references

disambr_eject_references <- function(wos_data_table) {
    if("CR" %in% names(wos_data_table)) {
        references_list <-
            lapply(wos_data_table$CR, disambr_parse_references)
        references_table <-
            rbindlist(references_list, idcol = "paper_id")
    }
    references_table
}



## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2)[[1]]
## dt %>% disambr_eject_references


## my.dir <- '../data'
## my.dir.small <- '../data/Journals in Mathematical Psychology'
## my.dir.large <- '/mnt/md5/data/wos/wos-sci-expanded.firm-names-query.analytical-instruments'
## my.dir.huge <- '/mnt/md5/data/wos'


## my.file <- '../data/Journals in Mathematical Psychology/Applied Psychological Measurement.txt' 
## my.file1 <- "/mnt/md5/data/wos/wos-sci-expanded.firm-names-query.analytical-instruments/LN Public NAICS records from 10001 to 10500.txt"
## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## my.files <-
    ## c('../data/Journals in Mathematical Psychology/Applied Measurement in Education.txt'
    ## , '../data/Journals in Mathematical Psychology/Applied Psychological Measurement.txt')
## disambr_read:1 ends here
