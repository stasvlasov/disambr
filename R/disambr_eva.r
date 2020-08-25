## [[id:org:8c83hcq0hti0][disambr_set_tekles_bornmann:1]]
disambr_set_tekles_bornmann <- function(sets
                                      , file_path = "../data/tekles_bornmann_rids.txt"
                                      , verbose = FALSE
                                      , save_set_as = TRUE
                                      , data_set_name = 
                                      "wos_records_tsv_export_author_table") {
    if(verbose) message("disambr: Starting disambr_set_tekles_bornmann...")
    if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
    data_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    sets <- disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , negate_subsets = TRUE)
    if(verbose) message("- filtering authors from Tekles & Bornmann (2019)")
    researcher_ids <- readLines(file_path)
    data_set <- data_set[author_researcher_id %in% researcher_ids]
    data_set_recipe <- attr(data_set, "disambr_recipe")
    data_set <-
        disambr_set_attributes(data_set
                             , disambr_recipe = c(list("disambr_set_tekles_bornmann")
                                                , data_set_recipe))
    disambr_save_set(data_set, save_set_as)
    return(c(sets, list(data_set)))
 }

## dt <- readRDS(file = "my.dir.wos.rds") %>%
    ## disambr_set_tekles_bornmann(verbose = TRUE) %>%
    ## disambr_set_on_same_paper(verbose = TRUE, save_set_as = TRUE)


## dt %>% length
## dt[[4]] %>% head


## after disambr_set_tekles_bornmann
## dt %>% length
## nrow(dt[[3]])
## 25868 vs 834090
## disambr_set_tekles_bornmann:1 ends here

## [[id:org:iab3hcq0hti0][disambr_set_on_same_paper:1]]
disambr_set_on_same_paper <- function(sets
                                      , verbose = FALSE
                                      , save_set_as = TRUE
                                      , data_set_name =
                                            "wos_records_tsv_export_author_table") {
    if(verbose) message("disambr: Starting disambr_set_not_on_same_paper...")
    if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
    data_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    if(verbose) message("- spliting co-authors")
    return_sets <- data_set %>% {split(1:nrow(.), .$paper_id)}
    data_set_recipe <- attr(data_set, "disambr_recipe")
    return_sets <- 
           disambr_set_attributes(return_sets
                         , disambr_entity = "person"
                         , disambr_set_type = "different"
                         , disambr_set_coefficient = 1
                         , disambr_set_name = "on_same_paper"
                         , disambr_set_collection = "sets_list"
                         , disambr_entity_id_reference =
                               "wos_records_tsv_export_author_table"
                         , disambr_recipe = c(list("disambr_set_on_same_paper")
                                            , data_set_recipe))
    disambr_save_set(return_sets, save_set_as)
    return(c(sets, list(return_sets)))
}
## disambr_set_on_same_paper:1 ends here

## [[id:org:nie3hcq0hti0][disambr_set_not_on_same_paper:1]]
## too slow... and eats all ram
disambr_set_not_on_same_paper <- function(sets
                                        , verbose = FALSE
                                        , save_set_as = TRUE
                                        , data_set_name =
                                              "wos_records_tsv_export_author_table") {
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
         pblapply(return_sets_comb
                , function(comb) {
                    expand.grid(author_id1 = return_sets[[comb[1]]]
                              , author_id2 = return_sets[[comb[2]]])
                })
     if(verbose) message("- rbinding combinations..")
     return_sets <- data.table::rbindlist(return_sets)
     if(verbose) message("--- rbinded into ", nrow(return_sets), " rows")
     ## set set's attributes
     data_set_recipe <- attr(data_set, "disambr_recipe")
     return_sets <- 
         disambr_set_attributes(return_sets
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
## disambr_set_not_on_same_paper:1 ends here

## [[id:org:i1i3hcq0hti0][disambr_set_similar_initials:1]]
disambr_set_similar_initials <- function(sets
                                       , verbose = FALSE
                                       , save_set_as = TRUE
                                       , data_set_name =
                                             "wos_records_tsv_export_author_table"
                                       , input_set_name = "on_same_paper") {
    if(verbose) message("disambr: Starting disambr_set_similar_initials...")
    if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
    data_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    input_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    ## sets <- disambr_subsets(sets
    ##                       , list(disambr_set_name = data_set_name)
    ##                       , negate_subsets = TRUE)
    if(verbose) message("- doing combinations")
    input_set_l <- length(input_set)
    return_set <- 
        pblapply(1:input_set_l, function(i) {
            comb <- lapply((i+1):input_set_l
                         , function(j) {
                             expand.grid(author_id1 = input_set[[i]]
                                       , author_id2 = input_set[[j]])
                         })
            comb <- data.table::rbindlist(comb)
            comb_dist <- stringdist(data_set$author_initials[comb$author_id1]
                                  , data_set$author_initials[comb$author_id2]
                                  , method = "lv")
            return(comb[comb_dist < 2])
        })
    if(verbose) message("- rbinding dyads")
    return_set <- data.table::rbindlist(return_set)
    input_set_recipe <- attr(data_set, "disambr_recipe")
    return_set <-
        disambr_set_attributes(
            return_sets
          , disambr_entity = "person"
          , disambr_set_type = "similar"
          , disambr_set_coefficient = 0.5
          , disambr_set_name = "similar_initials"
          , disambr_set_collection = "dyads_table"
          , disambr_entity_id_reference =
                "wos_records_tsv_export_author_table"
          , disambr_recipe = c(list("disambr_set_set_similar_initials")
                             , input_set_recipe))
    disambr_save_set(return_sets, save_set_as)
    return(c(sets, list(return_set)))
 }


  ## dt <- readRDS(file = "my.dir.wos.rds") %>%
      ## disambr_set_tekles_bornmann(verbose = TRUE) %>%
      ## disambr_set_on_same_paper(verbose = TRUE, save_set_as = TRUE) %>% 
      ## disambr_set_similar_initials(verbose = TRUE)
## disambr_set_similar_initials:1 ends here

## [[id:org:nnl3hcq0hti0][disambr_set_similar_last_names:1]]
disambr_set_similar_last_names <- function(sets
                                         , verbose = FALSE
                                         , save_set_as = TRUE
                                         , data_set_name =
                                               "wos_records_tsv_export_author_table"
                                         , input_set_name = "on_same_paper") {
    if(verbose) message("disambr: Starting disambr_set_similar_last_names...")
    if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
    data_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    input_set <-
        disambr_subsets(sets
                      , list(disambr_set_name = data_set_name)
                      , which_to_return = "first")
    ## sets <- disambr_subsets(sets
    ##                       , list(disambr_set_name = data_set_name)
    ##                       , negate_subsets = TRUE)
    if(verbose) message("- doing combinations")
    input_set_l <- length(input_set)
    return_set <- 
        pblapply(1:input_set_l, function(i) {
            comb <- pblapply((i+1):input_set_l
                         , function(j) {
                             expand.grid(author_id1 = input_set[[i]]
                                       , author_id2 = input_set[[j]])
                         })
            comb <- data.table::rbindlist(comb)
            comb_dist <- stringdist(data_set$author_initials[comb$author_id1]
                                  , data_set$author_initials[comb$author_id2]
                                  , method = "lv")
            return(comb[comb_dist < 2])
        })
    if(verbose) message("- rbinding dyads")
    return_set <- data.table::rbindlist(return_set)
    input_set_recipe <- attr(data_set, "disambr_recipe")
    return_set <-
        disambr_set_attributes(
            return_sets
          , disambr_entity = "person"
          , disambr_set_type = "similar"
          , disambr_set_coefficient = 0.5
          , disambr_set_name = "similar_initials"
          , disambr_set_collection = "dyads_table"
          , disambr_entity_id_reference =
                "wos_records_tsv_export_author_table"
          , disambr_recipe = c(list("disambr_set_set_similar_initials")
                             , input_set_recipe))
    disambr_save_set(return_sets, save_set_as)
    return(c(sets, list(return_set)))
 }


  ## dt <- readRDS(file = "my.dir.wos.rds") %>%
      ## disambr_set_tekles_bornmann(verbose = TRUE) %>%
      ## disambr_set_on_same_paper(verbose = TRUE, save_set_as = TRUE) %>% 
      ## disambr_set_similar_initials(verbose = TRUE)
## disambr_set_similar_last_names:1 ends here

## [[id:org:2mo3hcq0hti0][disambr_set_co_authors:1]]
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
  disambr_set_co_authors <- function(sets, procedures = NULL) {
      message("disambr: Starting disambr_set_co_authors...")
      if(!is.list(sets)) stop("disambr: 'sets' parameter should be list!")
      disambr_subsets("person")
      focal.set <- sets %>%
          extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
          ## TODO: implement extraction from different data type
          extract2(1)
      new.set <- focal.set %>%
          {split(1:nrow(.), .$paper_id)}
      ## set set's attributes
      attributes(new.set)$disambr.set.unit <- "person.distinct"
      attributes(new.set)$disambr.set.unit.ref.md5 <- digest(focal.set, "md5")
      return(c(sets, list(new.set)))
  }

  ## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
  ## dt <- disambr_read(my.file2) %>% disambr_set_co_authors
  ## dt[[4]]
## disambr_set_co_authors:1 ends here

## [[id:org:amr3hcq0hti0][disambr_get_3_refs_common:1]]
##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr_get_3_refs_common <- function(sets, procedures = NULL) {
    message("Starting disambr_get_3_refs_common...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.affiliations =
                          mapply(function(var1, var2)
                              any(set.data$author_affiliations[var1] %in% 
                                  set.data$author_affiliations[var2])
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.affiliation"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials %>%
    ## disambr_get_similar_last_names %>% 
    ## disambr_get_same_affiliation
## dt[[7]]$same.affiliations
## disambr_get_3_refs_common:1 ends here

## [[id:org:nqu3hcq0hti0][disambr_get_same_affiliation:1]]
##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr_get_same_affiliation <- function(sets, procedures = NULL) {
    message("Starting disambr_get_same_affiliation...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.affiliations =
                          mapply(function(var1, var2)
                              any(set.data$author_affiliations[var1] %in% 
                                  set.data$author_affiliations[var2])
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.affiliation"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials %>%
    ## disambr_get_similar_last_names %>% 
    ## disambr_get_same_affiliation
## dt[[7]]$same.affiliations
## disambr_get_same_affiliation:1 ends here

## [[id:org:hwx3hcq0hti0][disambr_get_same_coauthor:1]]
##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr_get_same_coauthor <- function(sets, procedures = NULL) {
    message("Starting disambr_get_same_email...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    fun <- function(var1, var2) {
        any(set.data[paper_id %in% paper_id[var1] &
                     !(author_name %in% author_name[var1])]$author_name %in% 
            set.data[paper_id %in% paper_id[var2] &
                     !(author_name %in% author_name[var2])]$author_name)
    }
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(
                   same.co.author = mapply(fun, Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.email"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials %>%
    ## disambr_get_similar_last_names %>% 
    ## disambr_get_same_coauthor

## dt[[7]]
## disambr_get_same_coauthor:1 ends here

## [[id:org:y114hcq0hti0][disambr_get_similar_last_names:1]]
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
disambr_get_similar_last_names <- function(sets, procedures = NULL) {
    message("Starting disambr_get_similar_last_names...")
    ## TODO: extract teh set that we need here (person, dyads)
    set.similar.initials <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.initials") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        dplyr::mutate(
                   set.similar.initials
                 , last.name.dist =
                       stringdist(set.data$author_last_name[Var1]
                                , set.data$author_last_name[Var2]
                                , method = "dl"))
    new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "similar.last.names"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials %>%
    ## disambr_get_similar_last_names
## dt[[6]]
## disambr_get_similar_last_names:1 ends here

## [[id:org:e544hcq0hti0][disambr_get_same_email:1]]
##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr_get_same_email <- function(sets, procedures = NULL) {
    message("Starting disambr_get_same_email...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.email = mapply(function(var1, var2)
                          set.data$author_email[var1] == set.data$author_email[var2]
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.email"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials %>%
    ## disambr_get_similar_last_names %>% 
    ## disambr_get_same_email
## disambr_get_same_email:1 ends here

## [[id:org:lk74hcq0hti0][disambr_get_similar_initials:1]]
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
disambr_get_similar_initials <- function(sets, procedures = NULL) {
    message("Starting disambr_get_similar_initials...")
    ## sapply(sets,attr, "disambr.set.unit")
    set.different.authors <-
        sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person.distinct") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    ## procedurs
    subset.similar.initials <- function(comb) {
        a <- set.different.authors[[comb[1]]]
        b <- set.different.authors[[comb[2]]]
        expand.grid(a, b)
    }
    new.set <- combn(1:length(set.different.authors), 2
                   , simplify = FALSE
                     ## , FUN = subset.similar.initials
                     )
    message("...combn produced ", length(new.set), " pairs of pubs")
    new.set <- pblapply(new.set, subset.similar.initials)
    message("...subset.similar.initials is done")
    new.set <- data.table::rbindlist(new.set)
    message("...rbindlist produced ", nrow(new.set), " pairs")
    new.set <- dplyr::mutate(new.set
                           , initials.dist =
                                 stringdist(set.data$author_initials[Var1]
                                          , set.data$author_initials[Var2]
                                          , method = "lv"))
    new.set <- dplyr::filter(new.set, initials.dist < 2)
    attributes(new.set)$disambr.set.unit <- "similar.initials"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr_read(my.file2) %>%
    ## disambr_set_co_authors %>%
    ## disambr_get_similar_initials
## dt[[5]]
## disambr_get_similar_initials:1 ends here
