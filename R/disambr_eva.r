## -------->>  [[file:../disambr.src.org::*disambr_filter_authors_by_researcher_ids][disambr_filter_authors_by_researcher_ids:1]]
##' Subsets WoS authors table with Researcher IDs
##'
##' This procedure does not alter sets attributes. Just filters WoS author table.
##'
##' @param sets WoS data
##' @param file_path path to Researcher IDs. The default list of Researcher IDs is taken from Tekles & Bornmann (2019) for reproducing their test samples (see reference)
##' @return updated WoS data
##'
##' @references Tekles, A., & Bornmann, L. (2019). Author name disambiguation of bibliometric data: A comparison of several unsupervised approaches. ArXiv:1904.12746. http://arxiv.org/abs/1904.12746

##'
##' @export
disambr_filter_authors_by_researcher_ids <-
    function(sets
           , file_path =
                 system.file("testdata" 
                           , "tekles-bornmann-researcher-ids.txt"
                           , package = "disambr")) {
        force(sets)
        disambr_message_start()
        if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
        ## check if output set is ready
        output_set <- disambr_get_output_set(sets)
        if(!is.null(output_set)) return(sets)
        output_set <- disambr_read_output_set()
        if(!is.null(output_set)) return(c(sets, list(output_set)))
        ## ----------------------------------------------------------------------
        disambr_message("- Tekles, A., & Bornmann, L. (2019) researcher IDs")
        if(file.exists(file_path)) {
            researcher_ids <- readLines(file_path)
        } else {
            disambr_stop("- Can not find file with researcher IDs. Please, provide")
        }
        disambr_message("- coping 'wos_tsv_authors' data set")
        authors_data_set_index <- disambr_in_sets(sets
                                                , recipe = "wos_tsv_authors"
                                                , match_attr_value_parcially = TRUE)
        authors_data_set_index <- which(authors_data_set_index)
        if(length(authors_data_set_index) != 1) {
            disambr_stop("- can not find 'wos_tsv_authors' data set!")
            return(sets)
        }
        authors_data_set <- sets[[authors_data_set_index]]
        ## ======================================================================
        disambr_message("- filtering authors by Researcher ID")
        authors_data_set_new <- authors_data_set[author_researcher_id %in% researcher_ids]
        ## ======================================================================
        mostattributes(authors_data_set_new) <- attributes(authors_data_set)
        disambr_set_attr(authors_data_set_new, name = "tekles_bornmann")
        ## disambr_save_set(authors_data_set_new)
        sets[[authors_data_set_index]] <- authors_data_set_new
        disambr_message_finish()
        return(sets)
    }
## --------<<  disambr_filter_authors_by_researcher_ids:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_split_authors_if_on_the_same_paper][disambr_split_authors_if_on_the_same_paper:1]]
##' Makes sets of co-authors assuming that all authors on paper are different person.
##' @param sets Sests
##' @return Updated sets
##' 
##' @export 
disambr_split_authors_if_on_the_same_paper <- function(sets) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    ## ======================================================================
    disambr_message("- spliting co-authors")
    output_set <- split(1:nrow(author_data_set)
                      , author_data_set$paper_id)
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , type = "different"
                       , strength = 1
                       , collection = "list_of_lists"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}
## --------<<  disambr_split_authors_if_on_the_same_paper:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_if_not_on_the_same_paper][disambr_merge_authors_if_not_on_the_same_paper:1]]
## too slow... and eats all ram
disambr_set_not_on_same_paper <- function(sets
                                        , verbose = FALSE
                                        , save_set_as = TRUE
                                        , data_set_name =
                                              "wos_records_tsv_export_author_table") {
    force(sets)
    if(verbose) message("disambr: Starting disambr_set_not_on_same_paper...")
    if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
    data_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    if(verbose) message("- spliting co-authors")
    return_sets <- data_set %>% {split(1:nrow(.), .$paper_id)}
    if(verbose) message("- making combinations of co-authors sets..")
    if(length(return_sets) > 50000) stop("--- THE NUMBER OF COMBINATIONS IS TO HIGH!")
    return_sets_comb <-
        combn(length(return_sets), 2, simplify = FALSE)
    if(verbose) message("--- made ,", length(return_sets_comb), " combinations")
    if(verbose) message("- expanding combinations")
    return_sets <-
        pbapply::pblapply(return_sets_comb
                        , function(comb) {
                            expand.grid(author_id1 = return_sets[[comb[1]]]
                                      , author_id2 = return_sets[[comb[2]]])
                        })
    if(verbose) message("- rbinding combinations..")
    return_sets <- data.table::rbindlist(return_sets)
    if(verbose) message("--- rbinded into ", nrow(return_sets), " rows")
    ## set set's attributes
    data_set_recipe <- attr(data_set, "disambr_recipe")
    disambr_setattr(return_sets
                  , disambr_entity = "person"
                  , disambr_set_type = "similar"
                  , disambr_set_coefficient = 0.5
                  , disambr_set_name = "not_on_same_paper"
                  , disambr_set_collection = "dyads_table"
                  , disambr_entity_id_reference =
                        "wos_records_tsv_export_author_table"
                  , disambr_recipe = c(list("disambr_set_not_on_same_paper")
                                     , data_set_recipe))
    if(length(save_set_as) != 0)
        return(c(sets, list(return_sets)))
}
## --------<<  disambr_merge_authors_if_not_on_the_same_paper:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_similar_initials][disambr_merge_authors_with_similar_initials:1]]
##' Makes set of similar authors based on their initials
##' @param sets Sets
##' @param maxDist see max_dist in `match_fuzzy`
##' @return Sets with new appended
##' @export 
disambr_merge_authors_with_similar_initials <- function(sets
                                       , maxDist = 1) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    initials_data_set <- toupper(author_data_set$author_initials)
    ## use only first 2 initials
    initials_data_set <- stringi::stri_sub(initials_data_set, to = 2)
    input_set <- disambr_get_last_set(sets)
    ## ======================================================================
    disambr_message("- fuzzy matching initials")
    ## assume all authors will be used in the table
    initials_bank <- unique(initials_data_set)
    ## lets leave NAs
    initials_bank <- sort(initials_bank, na.last = TRUE)
    initials_match <- 
        lapply(initials_bank
             , function(ini) {
                 matched_initials <- 
                     stringdist::ain(initials_bank
                                   , ini
                                   , maxDist = maxDist
                                   , method = "lv"
                                   , matchNA = FALSE)
                 if(any(matched_initials)) {
                     matched_initials <- initials_bank[matched_initials]
                     data.table::data.table(
                                     author_initials_1 = ini
                                   , author_initials_2 = matched_initials)
                 } else NULL
             })
    initials_match <- data.table::rbindlist(initials_match)
    ## this is the case if we apply this procedure first
    if(attr(input_set, "disambr_set_type") == "different") {
        input_set_l <- length(input_set)
        disambr_message(paste("- doing combinations on", input_set_l))
        ## try cluster
        ## cl <- parallel::makeCluster(20,type="SOCK")
        output_set <- 
            pbapply::pblapply(1:(input_set_l-1), function(i) {
                ## combn using is data.table method
                comb <- data.table::CJ(author_id1 = input_set[[i]]
                                     , author_id2 = unlist(input_set[(i+1):input_set_l])
                                     , sorted = FALSE)
                ## add initials_bank
                comb[, `:=`(
                    author_initials_1 = initials_data_set[author_id1]
                  , author_initials_2 = initials_data_set[author_id2]
                )]
                ## check matches
                comb <- merge(comb, initials_match
                            , by = c("author_initials_1", "author_initials_2"))
                return(comb[,.(author_id1, author_id2)])
            })
        ##parallel::stopCluster(cl = cl)
        disambr_message("- rbinding dyads")
        output_set <- data.table::rbindlist(output_set)
        ## other case is when follow matching last names procedure
    } else if(attr(input_set, "disambr_set_type") == "similar") {
        output_set <- input_set
        ## add names
        output_set[, `:=`(
            author_initials_1 = initials_data_set[author_id1]
          , author_initials_2 = initials_data_set[author_id2]
        )]
        ## check matches
        output_set <- merge(output_set, initials_match
                          , by = c("author_initials_1", "author_initials_2"))
        output_set <- output_set[,.(author_id1, author_id2)]
        ## } else if(attr(input_set, "disambr_set_type") == "similar") {
        ##     disambr_message("- subsetting first two initials_bank")
        ##     initials_bank1 <-
        ##         stri_sub(author_data_set$author_initials_bank[input_set$author_id1], to = 2)
        ##     initials_bank2 <-
        ##         stri_sub(author_data_set$author_initials_bank[input_set$author_id2], to = 2)
        ##     disambr_message("- calculating distance b/w initials_bank")
        ##     dist <- stringdist(initials_bank1
        ##                      , initials_bank2
        ##                      , method = "lv")
        ##     output_set <- input_set[dist < 2]
    } else disambr_stop("- UNKNOWN INPUT_SET_NAME!")
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 0.5
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}


## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:100]
## mostattributes(dt[[4]]) <- dt_atributes


## dt %>%
## disambr_set_similar_last_names(verbose = TRUE) %>% 
## disambr_set_similar_initials_bank(verbose = TRUE)
## --------<<  disambr_merge_authors_with_similar_initials:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_similar_last_names][disambr_merge_authors_with_similar_last_names:1]]
##' Makes set of similar authors based on their last names
##' @param sets Sets
##' @param max_dist see max_dist in `match_fuzzy`
##' @param max_dist_short max_dist for short last names, default 0
##' @param min_length who is short names defined, default 0 which means do not condider short names
##' @return Sets with new appended
##' @export 
disambr_merge_authors_with_similar_last_names <- function(sets
                                         , max_dist = 1
                                         , max_dist_short = 0
                                         , min_length = 0) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    last_name_data_set <- toupper(author_data_set$author_last_name)
    input_set <- disambr_get_last_set(sets)
    input_set_l <- length(input_set)
    ## ======================================================================
    disambr_message("- fuzzy matching last names")
    ## assume all authors will be used in the table
    last_names_bank <- unique(last_name_data_set)
    ## lets leave NAs
    last_names_bank <- sort(last_names_bank, na.last = TRUE)
    ## treshholds
    last_names_bank_short <- nchar(last_names_bank) <= min_length
    last_names_match <- match_fuzzy(last_names_bank[!last_names_bank_short]
                                  , method = "dl"
                                  , max_dist = max_dist
                                  , id_name = "author_last_name")
    if(any(last_names_bank_short)) {
        last_names_match <- rbind(
            last_names_match
          , match_fuzzy(last_names_bank[last_names_bank_short]
                      , method = "dl"
                      , max_dist = max_dist_short
                      , id_name = "author_last_name"))
    }
    ## ----------------------------------------------------------------------
    if(attr(input_set, "disambr_set_type") == "different") {
        disambr_message(paste("- doing combinations on", input_set_l))
        output_set <- 
            pbapply::pblapply(1:(input_set_l-1), function(i) {

                ## this is data.table method
                combs <- data.table::CJ(author_id1 = input_set[[i]]
                                      , author_id2 = unlist(input_set[(i+1):input_set_l])
                                      , sorted = FALSE)
                ## add names
                combs[, `:=`(
                    author_last_name_1 = last_name_data_set[author_id1]
                  , author_last_name_2 = last_name_data_set[author_id2]
                )]
                ## check matches
                combs <- 
                    merge(combs, last_names_match
                        , by = c("author_last_name_1", "author_last_name_2"))
                return(combs[,.(author_id1, author_id2)])
            })
        disambr_message("- rbinding dyads")
        output_set <- data.table::rbindlist(output_set)
    } else if(attr(input_set, "disambr_set_type") == "similar") {
        disambr_message(paste("- fuzzy matching authors by last name"))
        output_set <- input_set
        ## add names
        output_set[, `:=`(
            author_last_name_1 = last_name_data_set[author_id1]
          , author_last_name_2 = last_name_data_set[author_id2]
        )]
        ## check matches
        output_set <- merge(output_set, last_names_match
                          , by = c("author_last_name_1", "author_last_name_2"))
        output_set <- output_set[,.(author_id1, author_id2)]
    } else disambr_stop("- UNKNOWN INPUT_SET_NAME!")
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 0.5
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}
## --------<<  disambr_merge_authors_with_similar_last_names:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_same_emails][disambr_merge_authors_with_same_emails:1]]
##' Makes set of matched authors bases on same email addresses
##' @param sets Sets
##' @return Sets with new attached
##' @export 
disambr_merge_authors_with_same_emails <- function(sets) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    ## emails case insensitive
    email_data_set <- toupper(author_data_set$author_email)
    input_set <- disambr_get_last_unstrong_set(sets)
    ## ======================================================================
    disambr_message("- checking emails")
    output_set <-
        email_data_set[input_set$author_id1] == 
        email_data_set[input_set$author_id2]
    output_set <- input_set[sapply(output_set, isTRUE)]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}


## ## full
## dt <- readRDS(file = "my.dir.wos.rds") %>%
##     disambr_set_tekles_bornmann(verbose = TRUE) %>%
##     disambr_set_on_same_paper(verbose = TRUE) %>% 
##     disambr_set_similar_last_names(verbose = TRUE)

## partial
## dt <- readRDS(file = "my.dir.wos.rds") %>%
## disambr_set_tekles_bornmann(verbose = TRUE) %>%
## disambr_set_on_same_paper(verbose = TRUE)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:1000]
## mostattributes(dt[[4]]) <- dt_atributes

## dt <- 
## dt %>%
## disambr_set_similar_last_names(verbose = TRUE) %>%
## disambr_set_similar_initials(verbose = TRUE) %>% 
## disambr_set_same_email(verbose = TRUE)

## dt[[7]]

## dt[[3]][26]$author_email
## dt[[3]][90]$author_email

## dt[[3]][2]$author_email
## dt[[3]][264]$author_email
## dt[[3]][406]$author_email
## --------<<  disambr_merge_authors_with_same_emails:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_same_affiliation][disambr_merge_authors_with_same_affiliation:1]]
##' Makes set of matched authors based on same affiliation
##' @param sets Sets
##' @return Sets with new attached
##' @export 
disambr_merge_authors_with_same_affiliation <- function(sets) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    input_set <- disambr_get_last_unstrong_set(sets)
    ## ======================================================================
    disambr_message("- checking overlapping affiliations")
    affiliations1 <- author_data_set$affiliations[input_set$author_id1]
    affiliations1 <- lapply(affiliations1, toupper)
    ## affiliations1 <- ifelse(is.na(affiliations1), NULL, affiliations1)
    affiliations2 <- author_data_set$affiliations[input_set$author_id2]
    affiliations2 <- lapply(affiliations2, toupper)
    ## affiliations2 <- ifelse(is.na(affiliations2), NULL, affiliations2)
    affiliations_match <-
        mapply(function(a1, a2) {
            any(match(a1, a2, incomparables = NA, nomatch = 0) > 0)
        }
      , affiliations1
      , affiliations2)
    output_set <- input_set[affiliations_match]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}

## full
## dt <- readRDS(file = "my.dir.wos.rds") %>%
##     disambr_set_tekles_bornmann(verbose = TRUE) %>%
##     disambr_set_on_same_paper(verbose = TRUE) %>% 
##     disambr_set_similar_last_names(verbose = TRUE)

## partial
## dt <- readRDS(file = "my.wos.rds") %>%
## disambr_set_tekles_bornmann(verbose = TRUE) %>%
## disambr_set_on_same_paper(verbose = TRUE)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:100]
## mostattributes(dt[[4]]) <- dt_atributes

## dt.new <- 
## dt %>%
## disambr_set_similar_last_names(verbose = TRUE) %>%
## disambr_set_same_affiliation(verbose = TRUE, input_set_name = "similar_last_names")

## dt.new[[6]] %>% head

## dt[[3]][96]$affiliations
## dt[[3]][31]$affiliations

## dt[[3]][2]$author_email
## dt[[3]][264]$author_email
## dt[[3]][406]$author_email
## --------<<  disambr_merge_authors_with_same_affiliation:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_if_citing_others_papers][disambr_merge_authors_if_citing_others_papers:1]]
##' Make a set of matched authors bases on the cases when one author cites the others paper.
##' @param sets Sets
##' @param match_refrerences_by_name_year Whether to check citations based on first author name and year pair in addition to machich citations based on DOI
##' @return 
##' @export 
disambr_merge_authors_if_citing_others_papers <- function(sets
                                        , match_refrerences_by_name_year = TRUE) {
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    reference_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_references")
    citations_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_author_year_citations")
    input_set <- disambr_get_last_unstrong_set(sets)
      ## ======================================================================
    disambr_message("- checking if author sites other author's paper")
    ## TODO: Add papers that were already matched previously
    ## get paper ids
    input_set[, `:=`(
        paper_ids_1 = author_data_set$paper_id[author_id1]
      , paper_ids_2 = author_data_set$paper_id[author_id2])]
    match_list <- list()
    match_list$doi_1 <-
        merge(input_set, reference_data_set,
            , by.x = c("paper_ids_1", "paper_ids_2")
            , by.y = c("paper_id", "doi_cited_id"))[, .(author_id1, author_id2)]
    match_list$doi_2 <-
        merge(input_set, reference_data_set,
            , by.x = c("paper_ids_2", "paper_ids_1")
            , by.y = c("paper_id", "doi_cited_id"))[, .(author_id1, author_id2)]
    if(match_refrerences_by_name_year) {
        match_list$name_1 <-
            merge(input_set, citations_data_set,
                , by.x = c("paper_ids_1", "paper_ids_2")
                , by.y = c("citing_id", "cited_id"))[, .(author_id1, author_id2)]
        match_list$name_2 <-
            merge(input_set, citations_data_set,
                , by.x = c("paper_ids_2", "paper_ids_1")
                , by.y = c("citing_id", "cited_id"))[, .(author_id1, author_id2)]
    }
    output_set <- data.table::rbindlist(match_list)
    output_set <- unique(output_set)
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}


## partial
## dt <- readRDS(file = "my.wos.rds") %>%
## disambr_set_tekles_bornmann(verbose = TRUE) %>%
## disambr_set_on_same_paper(verbose = TRUE)

## dt.test <- 
    ## dt %>%
    ## disambr_set_on_same_paper %>% 
    ## disambr_set_similar_last_names


## dt.test %>% disambr_set_cite_others_paper %>% extract2(7)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:2000]
## mostattributes(dt[[4]]) <- dt_atributes

## dt.short <-
## dt %>%
## disambr_set_similar_last_names(verbose = TRUE)
## dt.short.test <-
## dt.short %>%
## disambr_set_cite_others_paper
## dt.short.test[[5]] %>% sum





## https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
## https://www.crossref.org/blog/dois-and-matching-regular-expressions/
## "/^10.\d{4,9}/[-._;()/:A-Z0-9]+$/i"


## testing data.table
## p <- data.table(id = c(1,2,3,4)
## , b = TRUE
## , y = c(7,4,6,3)
## , n = c("o", "z", "o", "e")
## , DI = c(11,22,33,44))

## r <- data.table(id = c(6)
## , b = FALSE
## , y = c(2,6,4,8,3,1)
## , n = c("a", "c", "z", "o", "e", "o")
## , doi = c(88,22,99,55,11,55))

## merge(p,r,by = "id")


## p[r
## , on = .(y, n)
## , .(DI, doi, , paper_id, b, y, n)]
## --------<<  disambr_merge_authors_if_citing_others_papers:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_match_authors_if_sharing_coauthors][disambr_match_authors_if_sharing_coauthors:1]]
##' Make a set of matched authors that share co-authors
##'
##' - First we index dyads of papers for matched authors by indexing elements in an upper triangle in square paper by paper matrix (see `get_upper_triangle_index` function)
##' - Then we group and count matched author dyads that are associated with the same paper dyad index. If there are no duplicates in authors ids then it would be the number of co-shared co-authors but there is an issue when we try to match same author name to several author names on the other paper (next steps meant to fix this issue)
##' - Within these groups of same paper dyads we count same authors ids on each paper to access the number of open triads (when the same author is matched to two different authors from the same paper) for every author dyad in a group (Nid1 + Nid2 - 2)
##' The algorithm for matching authors based on shared co-authors is the following:
##' - Finally, we filter matched author dyads based on the difference between number of paper dyads and number of open triangles for authors (records_per_paper - open_triangles > 1). Also see `min_number_of_shared_coauthors`
##' 
##' @param sets Sets of matched author names dyads
##' @param min_number_of_shared_coauthors Minimum number of co-authors that should be shared in order for author names to be cosidered as matched/merged
##' @return Original sets with table of matched author dyads appended to it
##' @export
##' @md
disambr_match_authors_if_sharing_coauthors <-
    function(sets
           , min_number_of_shared_coauthors = 1) {
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    paper_id_max <- max(author_data_set$paper_id)
    input_set <- disambr_get_last_unstrong_set(sets)
      ## ======================================================================
    disambr_message("- filtering cases with shared co-authors")
    ## get paper ids
    output_set <- 
        input_set[
          , paper_dyad_ids :=
                mapply(get_upper_triangle_index
                     , author_data_set$paper_id[author_id1]
                     , author_data_set$paper_id[author_id2]
                     , paper_id_max)
        ][
          , `:=`(records_per_paper = .N
               , open_triangles =
                     as.vector(table(author_id1)[as.character(author_id1)]) +
                     as.vector(table(author_id2)[as.character(author_id2)]) - 2)
          , keyby = paper_dyad_ids
        ][records_per_paper - open_triangles > min_number_of_shared_coauthors
        , .(author_id1, author_id2)
          ]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}



## TESTS
## data.table(
##     a1 = c(1,1,1, 8,1, 2, 4)
##   , a2 = c(5,2,3, 2, 5 , 3, 5)
##   , p1 = c(1,1,10,1,3,4,5)
##   , p2 = c(10,10,1,10,8,5,4))[
##   , paper_dyad_ids :=
##         mapply(get_upper_triangle_index
##              , p1
##              , p2
##              , max(c(p1, p2)))
## ][
##   , `:=`(records_per_paper = .N
##        , open_triangles =
##              as.vector(table(a1)[as.character(a1)]) +
##              as.vector(table(a2)[as.character(a2)]) - 2)
##   , keyby = paper_dyad_ids
## ][records_per_paper - open_triangles > 1]
## --------<<  disambr_match_authors_if_sharing_coauthors:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_common_references][disambr_merge_authors_with_common_references:1]]
##' Make set of authors that have number of references in common
##' @param sets Sets
##' @param references_in_common number of references in common
##' @return Sets with new set
##' @export 
disambr_merge_authors_with_common_references <- function(sets
                                        , references_in_common = 3) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    reference_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_references")
    citation_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_author_year_citations")
    input_set <- disambr_get_last_unstrong_set(sets)
    ## ======================================================================
    disambr_message("- checking references in common")
    ## TODO: Add papers that were already matched previously
    input_set[, `:=`(
        paper_ids_1 = author_data_set$paper_id[author_id1]
      , paper_ids_2 = author_data_set$paper_id[author_id2]
    )]
    paper_ids_set <- unique(input_set[,.(paper_ids_1, paper_ids_2)])
    ## for blade
    if(.Platform$OS.type == "windows") {
        cl <-
            parallel::makePSOCKcluster(
                          round(parallel::detectCores() * .70))
        output_set <-
            parallel::parLapply(
                          cl, 1:nrow(paper_ids_set)
                        , function(i) {
                            id1 <- paper_ids_set$paper_ids_1[i]
                            id2 <- paper_ids_set$paper_ids_2[i]
                            common_refs <- 
                                match(reference_data_set[paper_id == id1, c(doi_cited_id)]
                                    , reference_data_set[paper_id == id2, c(doi_cited_id)]
                                    , nomatch = 0
                                    , incomparables = NA)
                            common_refs <- sum(common_refs > 0)
                            if(common_refs < references_in_common) {
                                name_common_refs <- 
                                    match(citation_data_set[citing_id == id1, c(cited_id)]
                                        , citation_data_set[citing_id == id2, c(cited_id)]
                                        , nomatch = 0
                                        , incomparables = NA)
                                name_common_refs <- sum(name_common_refs > 0)
                                common_refs <- common_refs + name_common_refs
                                if(common_refs < references_in_common) {
                                    return(FALSE)
                                } else {
                                    return(TRUE)
                                }
                            } else return(TRUE)
                        }
                      )
        parallel::stopCluster(cl)
    } else {
        output_set <- 
            pbmapply(function(id1, id2) {
                common_refs <- 
                    match(reference_data_set[paper_id == id1, c(doi_cited_id)]
                        , reference_data_set[paper_id == id2, c(doi_cited_id)]
                        , nomatch = 0
                        , incomparables = NA)
                common_refs <- sum(common_refs > 0)
                if(common_refs < references_in_common) {
                    name_common_refs <- 
                        match(citation_data_set[citing_id == id1, c(cited_id)]
                            , citation_data_set[citing_id == id2, c(cited_id)]
                            , nomatch = 0
                            , incomparables = NA)
                    name_common_refs <- sum(name_common_refs > 0)
                    common_refs <- common_refs + name_common_refs
                    if(common_refs < references_in_common) return(FALSE) else return(TRUE)
                } else return(TRUE)
            }
          , paper_ids_set$paper_ids_1
          , paper_ids_set$paper_ids_2)
    }
    output_set <- paper_ids_set[output_set]
    output_set <- merge(output_set, input_set, by = c("paper_ids_1", "paper_ids_2"))
    output_set <- output_set[,.(author_id1, author_id2)]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}


## partial
## dt <- readRDS(file = "my.wos.rds") %>%
##     disambr_set_tekles_bornmann(verbose = TRUE) %>%
##     disambr_set_on_same_paper(verbose = TRUE)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:2000]
## mostattributes(dt[[4]]) <- dt_atributes

## dt.test %>% disambr_set_common_references %>% extract2(7)

## dt.short <-
##     dt %>%
##     disambr_set_similar_last_names(verbose = TRUE)
## dt.short.test <-
##     dt.short %>%
##     disambr_set_common_references
## dt.short.test[[5]] %>% sum





## https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
## https://www.crossref.org/blog/dois-and-matching-regular-expressions/
## "/^10.\d{4,9}/[-._;()/:A-Z0-9]+$/i"

## a <- data.table(a = c(1,2,3,4), b = c(11,22,33,44))

## a[a %in% c(2,3), c(b)]

## c(NA,NA,1) %in% c(32,3,1,3, NA)
## --------<<  disambr_merge_authors_with_common_references:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_if_citing_self_citation][disambr_merge_authors_if_citing_self_citation:1]]
##' Make a set of matched authors based on cases when one author cites others self citation. Self-citations here are detected based on DOI.
##' @param sets Sets
##' @return Sets with new appended
##' @export 
disambr_merge_authors_if_citing_self_citation <- function(sets) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    reference_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_references")
    strong_set <- disambr_get_strong_set(sets)
    input_set <- disambr_get_last_unstrong_set(sets)
    ## ======================================================================
    ## TODO: check named citations
    disambr_message("- checking if author cites a self-citation of other")
    check_self_citations <- function(id1, id2) {
        own_papers <- 
            author_data_set$paper_id[
                                unique(strong_set[author_id1 %in% id1 |
                                                  author_id2 %in% id1
                                                , c(author_id1, author_id2)])]
        ## mach own papers to own citations
        self_citations <- 
            match(own_papers
                , reference_data_set[paper_id %in% own_papers, c(doi_cited_id)]
                , nomatch = 0
                , incomparables = NA) > 0
        self_citations <- own_papers[self_citations]
        cite_self_citations <-
            match(reference_data_set[paper_id == id2, c(doi_cited_id)]
                , own_papers
                , nomatch = 0
                , incomparables = NA) > 0
        return(any(cite_self_citations))
    }
    ## blade option
    if(.Platform$OS.type == "windows") {
        cl <- parallel::makePSOCKcluster(round(parallel::detectCores() * .70))
        output_set <-
            parallel::parLapply(
                          cl, 1:nrow(input_set)
                        , function(i){
                            id1 <- input_set$author_id1[i]
                            id2 <- input_set$author_id2[i]
                            if(check_self_citations(id1, id2)) {
                                return(TRUE)
                            } else if(check_self_citations(id2, id1)) {
                                return(TRUE)
                            } else {
                                return(FALSE)
                            }
                        })
        parallel::stopCluster(cl)
    } else {
        output_set <- 
            pbmapply(function(id1, id2) {
                if(check_self_citations(id1, id2)) {
                    return(TRUE)
                } else if(check_self_citations(id2, id1)) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }
          , input_set$author_id1
          , input_set$author_id2)
    }
    output_set <- input_set[output_set]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}


## partial
## dt <- readRDS(file = "my.wos.rds") %>%
##     disambr_set_tekles_bornmann(verbose = TRUE) %>%
##     disambr_set_on_same_paper(verbose = TRUE)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:2000]
## mostattributes(dt[[4]]) <- dt_atributes

## dt.test.plus <- 
## dt.test %>%
## disambr_set_common_references


## dt.test.plus[[7]]

## dt.test.plus %>% 
## disambr_set_cite_self_citation %>% extract2(8)


## 7 out of 416 pairs matched
## dt.test.plus[[2]]$author_name[c(923
## , 2353         
## , 2403
## , 2464
## , 2525
## , 4234
## , 4420
## , 7675)]

## not very accurate
## [1] "COHEN, AS" "COHEN, AS" "COHEN, AS" "COHEN, AS" "COHEN, AS" "Yap, MJ"  
## [7] "Mayes, AR" "Burns, GN"





## dt.short <-
##     dt %>%
##     disambr_set_similar_last_names(verbose = TRUE)
## dt.short.test <-
##     dt.short %>%
##     disambr_set_cite_self_citation
## dt.short.test[[5]] %>% sum





## https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
## https://www.crossref.org/blog/dois-and-matching-regular-expressions/
## "/^10.\d{4,9}/[-._;()/:A-Z0-9]+$/i"

## a <- data.table(a = c(1,2,3,4), b = c(11,22,33,44))

## a[a %in% c(2,3), c(b)]

## c(NA,NA,1) %in% c(32,3,1,3, NA)
## --------<<  disambr_merge_authors_if_citing_self_citation:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_common_keywords][disambr_merge_authors_with_common_keywords:1]]
##' Makes set of authors with number of keywords in their papers in common
##' @param sets Sets
##' @param keywords_in_common number of keywords in common
##' @return Sets with new set appended
##' 
##' @export 
disambr_merge_authors_with_common_keywords <- function(sets
                                      , keywords_in_common = 3) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    publication_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_publications")
    input_set <- disambr_get_last_unstrong_set(sets)
    ## ======================================================================
    disambr_message("- checking common keywords (Author Keywords)")
    keywords_1 <- 
        publication_data_set[author_data_set[input_set$author_id1, c(paper_id)], c(DE)]
    keywords_1 <- stringi::stri_split_fixed(keywords_1, "; ")
    keywords_2 <- 
        publication_data_set[author_data_set[input_set$author_id2, c(paper_id)], c(DE)]
    keywords_2 <- stringi::stri_split_fixed(keywords_2, "; ")
    keywords_matched <- 
        pbmapply(function(k1, k2) {
            sum(match(k1, k2, incomparables = c(NA, ""), nomatch = 0) > 0)
        }
      , keywords_1
      , keywords_2)
    output_set <- input_set[keywords_matched >= keywords_in_common]
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 1
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}




## ## full
## dt <- readRDS(file = "my.dir.wos.rds") %>%
##     disambr_set_tekles_bornmann(verbose = TRUE) %>%
##     disambr_set_on_same_paper(verbose = TRUE) %>% 
##     disambr_set_similar_last_names(verbose = TRUE)

## partial
## dt <- readRDS(file = "my.dir.wos.rds") %>%
## disambr_set_tekles_bornmann(verbose = TRUE) %>%
## disambr_set_on_same_paper(verbose = TRUE)

## dt_atributes <- attributes(dt[[4]])
## dt[[4]] <- dt[[4]][1:1000]
## mostattributes(dt[[4]]) <- dt_atributes

## dt.test %>% disambr_set_common_keywords %>% extract2(7)

## dt.test[[1]]$DE[c(1113
## , 8390
## , 8438                  
## , 8455)]

## dt <- 
## dt %>%
## disambr_set_similar_last_names(verbose = TRUE) %>%
## disambr_set_similar_initials(verbose = TRUE) %>% 
## disambr_set_common_keywords(verbose = TRUE)

## dt[[7]]

## dt[[3]][26]$author_email
## dt[[3]][90]$author_email

## dt[[3]][2]$author_email
## dt[[3]][264]$author_email
## dt[[3]][406]$author_email
## --------<<  disambr_merge_authors_with_common_keywords:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_merge_authors_with_same_researcher_ids][disambr_merge_authors_with_same_researcher_ids:1]]
##' For testing. Makes so called ground truth set based on same research IDs
##' @param sets Sets
##' @return Sets with new set appended
##' 
##' @export 
disambr_merge_authors_with_same_researcher_ids <- function(sets) {
    force(sets)
    disambr_message_start()
    if(!is.list(sets)) disambr_stop("- 'sets' parameter should be list!")
    ## check if output set is ready
    output_set <- disambr_get_output_set(sets)
    if(!is.null(output_set)) return(sets)
    output_set <- disambr_read_output_set()
    if(!is.null(output_set)) return(c(sets, list(output_set)))
    ## ----------------------------------------------------------------------
    author_data_set <-
        disambr_get_first_data_set(sets, recipe = "wos_tsv_authors")
    ri_data_set <- author_data_set$author_researcher_id
    ri_bank <- unique(unlist(ri_data_set))
    ri_bank <- sort(ri_bank) # removes NAs
    ## ======================================================================
    disambr_message("- expanding grid and cheching researcher IDs")
    ## this is fast combn
    combi <- function(vect)
    {
        l <- length(vect)
        if(l == 1) return()
        first <- rep(vect, (l-1):0)
        vectR <- rev(vect)
        second <- vectR[rev(sequence(1:(l-1)))]
        combi <- data.table(first, second)
        return(combi)
    }
    output_set <-
        pbapply::pblapply(ri_bank, function(ri) {
            same_ri <- sapply(ri_data_set, function(i) ri %in% i)
            same_ri <- which(same_ri)
            combi(same_ri)
        })
    output_set <- data.table::rbindlist(output_set)
    ## ======================================================================
    disambr_add_set_attr(output_set, author_data_set
                       , strength = 10
                       , collection = "dyad_table"
                       , reference = "wos_tsv_authors")
    disambr_save_set(output_set)
    disambr_message_finish()
    return(c(sets, list(output_set)))
}
## --------<<  disambr_merge_authors_with_same_researcher_ids:1 ends here


