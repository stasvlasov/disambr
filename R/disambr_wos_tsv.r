## -------->>  [[file:../disambr.src.org::*disambr_make_wos_tsv_publications][disambr_make_wos_tsv_publications:1]]
disambr_make_wos_tsv_publications <- function(tables_list, recipes) {
    publication_table <-
        data.table::rbindlist(tables_list, fill=TRUE)
    ## add ids, first_author_last_name, first_author_first_initials
    publication_table[
      , `:=`(id = 1:.N
           , first_author_last_name =
                 toupper(stringi::stri_extract_first_regex(AU, "^[^,]+"))
           , first_author_first_initial =
                 toupper(stringi::stri_extract_first_regex(AU, "(?<=, )[A-Za-z]"))
           , doi = stringi::stri_match_first_regex(DI
                                               , "10.\\d{4,9}/[-._;()/:A-Za-z0-9]+"))]
    publication_table[
      , name_year := paste(first_author_last_name
                         , first_author_first_initial
                         , PY)]
    ## set publication attributes
    disambr_add_set_attr(publication_table, NULL
                   , unit = "publication"
                   , reference = "self"
                   , type = "different"
                   , strength = 1
                   , name = "wos_tsv_publications"
                   , collection = "unit_table"
                     ## add files recipies
                   , recipe = c(list('disambr_make_wos_tsv_publications')
                              , recipes))
    return(publication_table)
}
## --------<<  disambr_make_wos_tsv_publications:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_au][disambr_wos_tsv_parse_au:1]]
##' Parses AU column of WoS saved records export
##' @param author_name a record string from AU column
##' @return data.table
##' 
##' @md 
disambr_wos_tsv_parse_au <- function(author_name) {
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

## "Tilly, TB; Nelson, MT; Chakravarthy, KB; Shira, EA; Debrose, MC; Grabinski, CM; Salisbury, RL; Mattie, DR; Hussain, SM" %>% stri_split_fixed("; ") %>% 
## disambr_wos_tsv_parse_au
## --------<<  disambr_wos_tsv_parse_au:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_af][disambr_wos_tsv_parse_af:1]]
##' Parses AF (author full name) column of WoS saved records export
##' @param name (a record string from AF column)?
##' @return Data.table
disambr_wos_tsv_parse_af <- function(name) {
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
## disambr_wos_tsv_parse_af
## --------<<  disambr_wos_tsv_parse_af:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_rp][disambr_wos_tsv_parse_rp:1]]
##' Parses RP (reprint author) column of WoS saved records export
##' @param record_rp a record string from RP column
##' @return Data.table with two columns -  author_name and affiliations
disambr_wos_tsv_parse_rp <- function(record_rp) {
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
## disambr_wos_tsv_parse_rp %>% print

## "" %>%
## disambr_wos_tsv_parse_rp %>% nrow
## --------<<  disambr_wos_tsv_parse_rp:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_em][disambr_wos_tsv_parse_em:1]]
##' Parses EM (email) column of WoS saved records export
##' @param emails a record string from EM column
##' @param record_au_table a data_tabe after parsing AU column with disambr_wos_tsv_parse_au
##' @param record_rp_table a data_tabe after parsing RP column with disambr_wos_tsv_parse_rp
##' @return Data.table with columns - author_name, affiliations and email
disambr_wos_tsv_parse_em <- function(emails
                                   , record_au_table
                                   , record_rp_table) {
    if (isTRUE(length(emails) == 1 && emails == "")) {
        ## in case there are no emails
        record_au_table[, author_email := NA]
    } else if (isTRUE(length(emails) == nrow(record_rp_table))) {
        ## assume that emails corresponds RP authors
        record_au_table[match(record_rp_table$author_name, author_name)
                      , author_email := emails]
    } else if (isTRUE(length(emails) == nrow(record_au_table))) {
        ## assume that emails corresponds AU authors
        record_au_table[, author_email := emails]
    } else if (isTRUE(nrow(record_rp_table) != 0)) {
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
## disambr_wos_tsv_parse_em(
## record_em = "a"
## , record_au_table = data.table(author_name = c(1,2,3,4))
## , record_rp_table = data.table(author_name = c(3))
## ) %>% print
## --------<<  disambr_wos_tsv_parse_em:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_c1][disambr_wos_tsv_parse_c1:1]]
##' Parses C1 (author adress/affiliation) column of WoS saved records export
##' @param record_c1 a record string from RP column 
##' @param table_af Table
##' @return Data.table with two columns -  author_name and affiliations
disambr_wos_tsv_parse_c1 <- function(record_c1
                                   , table_af = NULL) {
    record_c1_init <- ""
    authors_table <-
        data.table::data.table(author_full_name = character()
                             , affiliations = list())
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
## disambr_wos_tsv_parse_c1


## "[Wang, Menglei; Li, Shunyi; Zhu, Rencheng; Zhang, Ruiqin] Zhengzhou Univ, Sch Ecol & Environm, Zhengzhou 450001, Peoples R China; [Wang, Menglei] Zhengzhou Univ, Sch Chem Engn, Zhengzhou 450001, Peoples R China; [Zu, Lei; Wang, Yunjing; Bao, Xiaofeng] Chinese Res Inst Environm Sci, State Environm Protect Key Lab Vehicle Emiss Cont, Beijing 100012, Peoples R China" %>%
## disambr_wos_tsv_parse_c1(disambr_wos_tsv_parse_af("Wang, Menglei; Zu, Lei; Wang, Yunjing; Bao, Xiaofeng"))
## --------<<  disambr_wos_tsv_parse_c1:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_oi][disambr_wos_tsv_parse_oi:1]]
##' Parses OI column of WoS saved records export
##' @param authors a record string from OI column
##' @param table_af Table
##' @return data.table
##' 
##' @md 
disambr_wos_tsv_parse_oi <- function(authors
                                   , table_af = NULL) {
    author_full_name <-
        stringi::stri_extract_first_regex(authors, "^[^/]+")
    author_orcid <-
        stringi::stri_extract_first_regex(authors, "(?<=/).+")
    authors_table <- 
        data.table::data.table(author_full_name = author_full_name
                             , author_orcid = author_orcid)
    ## take care of propable case of multiple ID for one person
    author_full_name_unique <- unique(authors_table$author_full_name)
    author_orcid_list <-
        lapply(author_full_name_unique
             , function(x) {
                 authors_table$author_orcid[authors_table$author_full_name %in% x]
             })
    authors_table <-
        data.table::data.table(author_full_name = author_full_name_unique
                             , author_orcid = author_orcid_list)
    if(length(table_af) != 0) {
        return(authors_table[table_af
                           , on = "author_full_name"
                           , .(author_orcid)])
    } else {
        return(authors_table)
    }
}



## "Estrela, Pedro/0000-0001-6956-1146; Maxted, Grace/0000-0002-6816-9107; Rainbow, Joshua/0000-0003-3911-928X; Richtera, Lukas/0000-0002-8288-3999; Moschou, Despina/0000-0001-9175-5852" %>%
    ## disambr_make_wos_tsv_authors__parse_oi



## "Estrela, Pedro/0000-0001-6956-1146; Maxted, Grace/0000-0002-6816-9107; Rainbow, Joshua/0000-0003-3911-928X; Rainbow, Joshua/0000-0003-3911-928X; Richtera, Lukas/0000-0002-8288-3999; Moschou, Despina/0000-0001-9175-5852" %>%
    ## disambr_make_wos_tsv_authors__parse_oi(
        ## table_af = data.table(author_full_name =
                                  ## c("Rainbow, Joshua", "Moschou, Despina")))                                                                                       

## "" %>% disambr_make_wos_tsv_authors__parse_oi(
           ## table_af = data.table(author_full_name = c("Rainbow, Joshua")))

## NA %>% disambr_make_wos_tsv_authors__parse_oi
## --------<<  disambr_wos_tsv_parse_oi:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_ri][disambr_wos_tsv_parse_ri:1]]
##' Parses RI column of WoS saved records export
##' @param authors a record string from RI column
##' @param table_af Table
##' @return data.table
##' 
##' @md 
##' @export 
disambr_wos_tsv_parse_ri <- function(authors
                                   , table_af = NULL) {
    author_full_name <-
        stringi::stri_extract_first_regex(authors, "^[^/]+")
    author_researcher_id <-
        stringi::stri_extract_first_regex(authors, "(?<=/).+")
    authors_table <- 
        data.table::data.table(author_full_name = author_full_name
                             , author_researcher_id = author_researcher_id)
    ## take care of propable case of multiple ID for one person
    author_full_name_unique <- unique(authors_table$author_full_name)
    author_researcher_id_list <-
        lapply(author_full_name_unique
             , function(x) {
                 authors_table$author_researcher_id[
                                   authors_table$author_full_name %in% x]
             })
    authors_table <-
        data.table::data.table(author_full_name = author_full_name_unique
                             , author_researcher_id = author_researcher_id_list)
    if(length(table_af) != 0) {
        return(authors_table[table_af
                           , on = "author_full_name"
                           , .(author_researcher_id)])
    } else {
        return(authors_table)
    }
}
## --------<<  disambr_wos_tsv_parse_ri:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_make_wos_tsv_authors][disambr_make_wos_tsv_authors:1]]
##' Parses all WoS field related to authors and makes author table
##'
##' Relevant fields that are parsed (as in Web of Science Field Tags 2018-06-27):
##' - AU
##' - AF (full names)
##' - C1 adresses
##' - RP reprint address (one you contact for reprint copy)
##' - EM emails
##' - RI researcher ID
##' - OI ORCID Identifier (Open Researcher and Contributor ID)
##'
##' Fields that are not parsed
##' - BA book - not parsed
##' - BF book - not parsed
##' - CA gp group author (usually organization or group name) - not parsed
##' - BE editors - not parsed
##' @param wos_data_table WoS tsv export data.table
##' @param list_of_author_fields list of data to parse to construct (if not all selected as by default it will save time)
##' @param ... set verbose if needed here
##' @return author table
##' 
##' @md 
##' @export 
disambr_make_wos_tsv_authors <- function(wos_data_table
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
                                      , "author_affiliations")
                                , ...) {
    disambr_mess_start(start_mess_prefix = "-- making set")
    authors_tables <- list()
    ## AU
    if(any(c("author_order"
           , "author_short_name"
           , "author_last_name"
           , "author_initials"
           , "author_email") %in% list_of_author_fields) &&
       "AU" %in% names(wos_data_table)) {
        disambr_message("-- parsing AU field", ...)
        authors_tables$au <-
            lapply(
                stringi::stri_split_fixed(wos_data_table$AU, "; ")
              , disambr_wos_tsv_parse_au)
    }
    ## AF
    if(any(c("author_full_name"
           , "author_first_names"
           , "author_first_full_name"
           , "author_researcher_id"
           , "author_orcid"
           , "author_affiliations") %in% list_of_author_fields) &&
       "AF" %in% names(wos_data_table)) {
        disambr_message("-- parsing AF field", ...)
        authors_tables$af <-
            lapply(
                stringi::stri_split_fixed(wos_data_table$AF, "; ")
              , disambr_wos_tsv_parse_af)
    }
    ## RP
    if(any(c("author_email") %in% list_of_author_fields) &&
       "RP" %in% names(wos_data_table)) {
        disambr_message("-- parsing RP field", ...)
        ## save RP separately as it is different order from AU
        rp <-
            lapply(wos_data_table$RP
                 , disambr_wos_tsv_parse_rp)
    }

    ## EM
    if(any(c("author_email") %in% list_of_author_fields) &&
       all(c("AU", "EM", "RP") %in% names(wos_data_table))) {
        disambr_message("-- parsing EM field", ...)
        ## disambr_wos_tsv_parse_em updates authors_tables$au
        ## so no need to save it
        pbapply::pbmapply(disambr_wos_tsv_parse_em
             , stringi::stri_split_fixed(wos_data_table$EM, "; ")
             , authors_tables$au
             , rp
             , SIMPLIFY = FALSE
             , USE.NAMES = FALSE)
    }
    ## C1
    if(any(c("author_affiliations") %in% list_of_author_fields) &&
       all(c("C1", "AF") %in% names(wos_data_table))) {
        disambr_message("-- parsing C1 field", ...)
        authors_tables$c1 <-
            pbapply::pbmapply(disambr_wos_tsv_parse_c1
                 , wos_data_table$C1
                 , authors_tables$af
                 , SIMPLIFY = FALSE)
    }
    ## RI
    if(any(c("author_researcher_id") %in% list_of_author_fields) &&
       all(c("RI", "AF") %in% names(wos_data_table))) {
        disambr_message("-- parsing RI field", ...)
        authors_tables$ri <-
            pbapply::pbmapply(disambr_wos_tsv_parse_ri
                 , stringi::stri_split_fixed(wos_data_table$RI, "; ")
                 , authors_tables$af
                 , SIMPLIFY = FALSE
                 , USE.NAMES = FALSE)
    }
    ## OI
    if(any(c("author_orcid") %in% list_of_author_fields) &&
       all(c("OI", "AF") %in% names(wos_data_table))) {
        disambr_message("-- parsing OI field", ...)
        authors_tables$oi <-
            pbapply::pbmapply(disambr_wos_tsv_parse_oi
                 , stringi::stri_split_fixed(wos_data_table$OI, "; ")
                 , authors_tables$af
                 , SIMPLIFY = FALSE
                 , USE.NAMES = FALSE)
    }
    ## remove duplicated columns
    disambr_message("-- stacking author fields", ...)
    authors_tables <- 
        lapply(authors_tables, data.table::rbindlist, idcol = "paper_id")
    authors_table <- do.call(cbind, authors_tables)
    authors_table_names <- 
        stringi::stri_replace_first_regex(names(authors_table), "^[^\\.]+\\.", "")
    authors_table_select <- which(!duplicated(authors_table_names))
    authors_table_new_names <- authors_table_names[authors_table_select]
    authors_table <- authors_table[, authors_table_select, with = FALSE]
    ## set names
    data.table::setnames(authors_table, authors_table_new_names)
    ## set author attributes
    disambr_add_set_attr(authors_table
                       , wos_data_table
                       , unit = "person"
                       , reference = "wos_tsv_publications"
                       , type = "similar"
                       , strength = 0.1
                       , name = "wos_tsv_authors")
    #disambr_save_set(authors_table)
    disambr_mess_finish(mess = "-- finished -")
    return(authors_table)
}
## --------<<  disambr_make_wos_tsv_authors:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_wos_tsv_parse_cr][disambr_wos_tsv_parse_cr:1]]
##' Parses WoS CR (Cited References) record into separate talbe
##' @param references CR (Cited References) record, i.e., just one row
##' @return references table
##' 
##' @export 
disambr_wos_tsv_parse_cr <- function(references) {
    references_list <- stringi::stri_split_fixed(references, ", ")
    references_list <-
        lapply(references_list, function(ref) {
            first_author_last_name_first_initial <-
                stringi::stri_match_first_regex(ref[1], "([a-z ]*[A-Z][^ ]+)\\s+([A-Z])")
            ref_tail <- ref[-c(1:3)]
            vol <- stringi::stri_extract_first_regex(ref_tail, "(?<=^V)\\d+")
            vol <- vol[!sapply(vol, is.na)][1]
            page <- stringi::stri_extract_first_regex(ref_tail, "(?<=^P)\\d+")
            page <- page[!sapply(page, is.na)][1]
            doi <-
                stringi::stri_match_first_regex(ref_tail
                                              , "10.\\d{4,9}/[-._;()/:A-Za-z0-9]+")
            doi <- doi[!sapply(doi, is.na)][1]
            name_year <- paste(first_author_last_name_first_initial[2]
                             , first_author_last_name_first_initial[3]
                             , ref[2])
            ## combine
            list(first_author_name = ref[1]
               , first_author_last_name = first_author_last_name_first_initial[2]
               , first_author_first_initial = first_author_last_name_first_initial[3]
               , year = ref[2]
               , outlet = ref[3]
               , vol = vol
               , page = page
               , doi = doi
               , name_year = name_year)
        })
    suppressWarnings(data.table::rbindlist(references_list))
}
## --------<<  disambr_wos_tsv_parse_cr:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_make_wos_tsv_references][disambr_make_wos_tsv_references:1]]
##' Make references table from WoS tsv data
##' @param wos_data_table WoS data
##' @param ... set verbose here if neededj
##' @return references table
##' 
##' @export 
disambr_make_wos_tsv_references <- function(wos_data_table
                                          , ...) {
    ## disambr_mess_start()
    if("CR" %in% names(wos_data_table)) {
        disambr_message("-- parsing references", ...)
        references_list <-
            pbapply::pblapply(stringi::stri_split_fixed(wos_data_table$CR, "; ")
                   , disambr_wos_tsv_parse_cr)
        disambr_message("-- stacking references", ...)
        references_table <-
            data.table::rbindlist(references_list, idcol = "paper_id")
        disambr_message("-- matching DOI citations", ...)
        ## assume unique doi
        doi_match <- match(references_table$doi
                         , wos_data_table$doi
                         , incomparables = NA
                         , nomatch = NA)
        if(any(!is.na(doi_match))){
            references_table[, "doi_cited_id" := wos_data_table$id[doi_match]]
        } else {
            references_table[, "doi_cited_id" := NA]
        }
        ## set references attributes
        disambr_add_set_attr(references_table, wos_data_table
          , reference = "wos_tsv_publications"
          , type = "similar"
          , strength = 0.1
          , name = "wos_tsv_references"
          , collection = "unit_table")
        ## disambr_mess_finish()
        return(references_table)
    } else {
        disambr_stop("THERE IS NOT 'CR' FIELD WITH REFERENCES!")
    }
}
## --------<<  disambr_make_wos_tsv_references:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_make_wos_tsv_author_year_citations][disambr_make_wos_tsv_author_year_citations:1]]
##' Makes citations table by matching first author - year keys. This, of course, can produce false positives matched
##' @param pub_table WoS publications data table
##' @param ref_table WoS references data table
##' @return citation table
##' 
##' @export 
disambr_make_wos_tsv_author_year_citations <- function(pub_table, ref_table) {
    ## filter before maching
    pub_table[, "year" := as.character(PY)]
    pub_table <- pub_table[!is.na(first_author_last_name) &
                           !is.na(first_author_first_initial) &
                           !is.na(year)
                         , .(first_author_last_name
                           , first_author_first_initial
                           , year
                           , cited_id = id)]
    ref_table <- ref_table[!is.na(first_author_last_name) &
                           !is.na(first_author_first_initial) &
                           !is.na(year)
                         , .(first_author_last_name
                           , first_author_first_initial
                           , year
                           , citing_id = paper_id)]
    cit_table <- 
        merge(pub_table, ref_table
            , by = c("first_author_last_name"
                   , "first_author_first_initial"
                   , "year"), allow.cartesian = TRUE)
    cit_table <- cit_table[, .(citing_id, cited_id
                               ## , first_author_last_name
                               ## , first_author_first_initial
                               ## , year
                               )]
    disambr_add_set_attr(cit_table, ref_table
                       , name = "wos_tsv_author_year_citations"
                       , collection = "dyad_table"
                       , reference = "wos_tsv_publications")
    return(cit_table)
}
## --------<<  disambr_make_wos_tsv_author_year_citations:1 ends here


