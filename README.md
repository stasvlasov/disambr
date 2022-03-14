[![R-CMD-check](https://github.com/stasvlasov/disambr/workflows/R-CMD-check/badge.svg)](https://github.com/stasvlasov/disabmr/actions)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/stasvlasov/disambr)

`disambr` is an R :package: that provides a flexible framework for
disambiguation of named entities. Currently this package implements the
[AEV algorithm](id:org:dux2eyd1gti0) (van den Akker et al., 2020) for
Web of Science author disambiguation.

# Usage

The idea of basic usage is simply piping the disambiguation procedures.
Each procedure takes list of sets as input and returns list of sets
either by adding a new set or modifying input list of sets.

``` {.r org-language="R"}
data %>% 
    disambr.get.different.authors %>% 
    disambr.get.similar.initials %>%
    disambr.get.similar.last.names
```

Sequence (piped functions) of disambiguation procedures defines a
disambiguation algorithm.

``` {.r org-language="R"}
disambr.eva <- function(data) {
    data %>% 
        disambr.get.different.authors %>% 
        disambr.get.similar.initials %>%
        disambr.get.similar.last.names
}
```

A procedure can be defined as follows (not implemented). Just an idea.

``` {.r org-language="R"}
disambr.get.different.authors <- disambr.define.procedure(data %>% 
                                                            get(publication) %>%
                                                            for.each %>%
                                                            get(person = author))

  ## or
  disambr.get.different.authors <- disambr.define.procedure(data$
                                                            publication$
                                                            person(author))
```

# Implementation

**The EVA-algorithm: An open-source solution for the disambiguation of
author names in Web of Science data** Olmo R. van den Akker Sacha
Epskamp Stanislav Vlasov

The creation of co-authorship networks is a valuable way to depict the
social structure of scientific fields. However, these co-authorship
networks often get distorted because of the problems of author name
synonymy (the same author is split into two nodes because his name is
spelled differently in different publications) and author name homonymy
(different authors are compounded into one node because they share the
same name). The practice of author name disambiguation (AND) tries to
solve these problems by correctly identifying the authors of scientific
articles.

Several algorithms have been put forward in the context of AND, but none
of them are suitable for large datasets of the Web of Science database.
Therefore, in an earlier part of this project we proposed a new
unsupervised learning algorithm based on the most recent AND literature.
This so-called AEV-algorithm involves two phases: a blocking phase, in
which pairs of authors are selected that are sufficiently similar, and a
disambiguation phase, in which similar author names are either split or
combined into one node based on information retrieved from the Web of
Science database. In the disambiguation phase, the algorithm uses
information about co-authorship, e-mail addresses, institutional
affiliations, cited references, and article keywords (van den Akker et
al., 2020).

# Installation

``` {.r org-language="R"}
## Installs devtools it if not yet installed
if (!require("devtools")) {
    install.packages("devtools"
                   , repos = 'http://cloud.r-project.org')
}

## ## Remove old version if installed
## if (!require("disambr")) {
##     detach(package:disambr, unload = TRUE)
##     remove.packages("disambr")
## }

## Installs and loads disambr
devtools::install_github("stasvlasov/disambr")
library("disambr")
```

# Sets attributes

All disambiguation procedures used in `disambr` package work with sets.
A set is basically any R object that can represent [mathematical
sets](https://en.wikipedia.org/wiki/Set_(mathematics)) (e.g., set of
authors, set of companies) with special attributes that are used by
`disabmr` functions to identify the kind of set it is working with or
produce (e.g., a set of authors that are likely to be the same person, a
set of companies that are definitely different companies, etc.).

The attributes that are currently used to define/describe set as well as
their values are listed below:

-   `disambr_entity`
    -   `person`, `organization`, `publication`
-   `disambr_set_type`
    -   `similar_entities`, `different_entities`
-   `disambr_set_coefficient`
    -   number between 0 and 1 indicating how strongly entities are
        similar or different from each other. It is use only for
        establishing order of sets processing (e.g., start with sets of
        least similar entities)
-   `disambr_set_name`
    -   string name of the set
-   `disambr_set_collection`
    -   `single_set_table` (first column assumed to store entity id or
        entity id is just row number if `entity_id_reference` attribute
        is set to `self`, see below), `list_of_sets_as_lists` (each set
        is a list of entity ids), `dyads_table` (first and second
        columns assumed to be ids for the pair of entities)
-   `disambr_entity_id_reference`
    -   `self`, name of other set as in its `set_name` attribute
-   `disambr_entity_id_reference_md5_sum`
    -   md5 cache sum of the object where entities ids are referring to
        ensure that we will get to correct data for entities in the set.
-   `disambr_recipe`
    -   list of `disambr` procedures that were applied to produce given
        set(s)
    -   if it is a named list then first item is procedure name and the
        rest are properties:
        -   `procedure`
        -   `file_name`
        -   `file_md5sum` (to check file identity later)
        -   `file_header` (to check for consistency between read files)

# Overall design principles

-   To allow for modular design each disambiguation procedure should
    accept and return list of sets (e.g., same person sets, different
    person sets, other probability of being the same person sets)
-   List of sets from (chain of) various procedures will be then merged
    (using sets algebra) according to the specific disambiguation
    algorithm to produce final list of sets.
-   Initial input should be in the form of a list of initial sets (the
    simplest input is one set with every person likely to be non unique,
    e.g., data.table of authors from Web of Science bibliography data).
-   When reading data package should try to do as many sets as possible
    on a fly (cleaning and splitting initial data to different types of
    entities)
-   Try to implement lazy data loading and processing where possible

# Naming convention

-   All functions should have a verb
-   Variables should not have a verb
-   Everything that is available to user should start with \"disambr\_\"
    (i.e., package name)
-   Internal functions and variables does not have to have this prefix
-   Use mainstream coding style guides everywhere where possible
    -   <https://google.github.io/styleguide/Rguide.html>
    -   <http://adv-r.had.co.nz/Style.html>
    -   <https://style.tidyverse.org/syntax.html#object-names>

# Web of Science Field Tags 2018-06-27

<https://support.clarivate.com/ScientificandAcademicResearch/s/article/Web-of-Science-Core-Collection-List-of-field-tags-in-output?language=en_US>

  ---- ----------------------------------------------------------
  FN   File Name
  VR   Version Number
  PT   Publication Type (J=Journal; B=Book; S=Series; P=Patent)
  AU   Authors
  AF   Author Full Name
  BA   Book Authors
  BF   Book Authors Full Name
  CA   Group Authors
  GP   Book Group Authors
  BE   Editors
  TI   Document Title
  SO   Publication Name
  SE   Book Series Title
  BS   Book Series Subtitle
  LA   Language
  DT   Document Type
  CT   Conference Title
  CY   Conference Date
  CL   Conference Location
  SP   Conference Sponsors
  HO   Conference Host
  DE   Author Keywords
  ID   Keywords Plus®
  AB   Abstract
  C1   Author Address
  RP   Reprint Address
  EM   E-mail Address
  RI   ResearcherID Number
  OI   ORCID Identifier (Open Researcher and Contributor ID)
  FU   Funding Agency and Grant Number
  FX   Funding Text
  CR   Cited References
  NR   Cited Reference Count
  TC   Web of Science Core Collection Times Cited Count
  Z9   Total Times Cited Count\*
  U1   Usage Count (Last 180 Days)
  U2   Usage Count (Since 2013)
  PU   Publisher
  PI   Publisher City
  PA   Publisher Address
  SN   International Standard Serial Number (ISSN)
  EI   Electronic International Standard Serial Number (eISSN)
  BN   International Standard Book Number (ISBN)
  J9   29-Character Source Abbreviation
  JI   ISO Source Abbreviation
  PD   Publication Date
  PY   Year Published
  VL   Volume
  IS   Issue
  SI   Special Issue
  PN   Part Number
  SU   Supplement
  MA   Meeting Abstract
  BP   Beginning Page
  EP   Ending Page
  AR   Article Number
  DI   Digital Object Identifier (DOI)
  D2   Book Digital Object Identifier (DOI)
  PG   Page Count
  P2   Chapter Count (Book Citation Index)
  WC   Web of Science Categories
  SC   Research Areas
  GA   Document Delivery Number
  UT   Accession Number
  PM   PubMed ID
  ER   End of Record
  EF   End of File
  ---- ----------------------------------------------------------

# Existing tools for reading WoS data

  name               comments
  ------------------ ------------------------------------------------------------
  bibliometrix       reads only plaintext format into bibliometrixDB object
  wosr               Requires WoS API subscription
  refsplitr          package 'refsplitr' is not available (for R version 4.0.1)
  read.wos.R         Does not work...
  metagear           scrape~bibliography~ by DOI
  hindexcalculator   ?

## bibliometrix

<https://github.com/massimoaria/bibliometrix>

Site: <https://bibliometrix.org/index.html>

``` shell
git clone https://github.com/massimoaria/bibliometrix
```

``` {.r org-language="R"}
install.packages("bibliometrix")
library("bibliometrix")
library("magrittr")


bmdata <- convert2df(file = 'https://www.bibliometrix.org/datasets/wos_plaintext.txt', dbsource = 'wos', format = "plaintext")


bmdata %>% class
## [1] "data.frame"     "bibliometrixDB"

bmdata %>% names


bmdata <- convert2df(file = 'https://www.bibliometrix.org/datasets/wos_plaintext.txt', dbsource = 'wos', format = "csv")
```

## metagear

CRAN docs:
<https://cran.r-project.org/web/packages/metagear/metagear.pdf>

GitHub: <https://github.com/cran/metagear/>

``` shell
git clone https://github.com/cran/metagear/
```

``` {.r org-language="R"}
scrape_bibliography
```

## BibPlots

CRAN docs:
<https://cran.r-project.org/web/packages/BibPlots/BibPlots.pdf>

Paper: <https://arxiv.org/pdf/1905.09095.pdf>

## hindexcalculator

CRAN docs:
<https://cran.r-project.org/web/packages/hindexcalculator/hindexcalculator.pdf>

## refsplitr

``` shell
git clone https://github.com/ropensci/refsplitr
```

``` {.r org-language="R"}
install.packages("refsplitr")
library("refsplitr")
```

## wosr

Requires premium WoS API -
<https://clarivate.com/webofsciencegroup/solutions/xml-and-apis>

CRAN doc: <https://cran.r-project.org/web/packages/wosr/wosr.pdf>

Site: <https://github.com/vt-arc/wosr>

GitHub: <https://github.com/vt-arc/wosr>

``` shell
git clone https://github.com/vt-arc/wosr
```

``` {.r org-language="R"}
install.packages("wosr")
library(wosr)

## Get session ID
sid <- auth("s.vlasov@tilburguniversity.edu", password = "")
## Error: No matches returned for Username s.vlasov@tilburguniversity.edu

## Query WoS to see how many results match your query
query <- 'TS = ("animal welfare") AND PY = (2002-2003)'
query_wos(query, sid = sid)

## Download data
pull_wos(query, sid = sid)
```

## read.wos.R

GitHub: <https://github.com/alberto-martin/read.wos.R>

``` shell
git clone https://github.com/alberto-martin/read.wos.R
```

``` {.r org-language="R"}
## load functions
## --------------------------------------------------------------------------------
source("../lib/read.wos.R/read.wos.functions.R")
## --------------------------------------------------------------------------------


## test

wos.data.mp <- read.wos(dir("../data/Journals in Mathematical Psychology", no.. = TRUE, full.names = TRUE))
## Error in substring(fields, 4) : invalid multibyte string at '<ff><fe>P'
## In addition: Warning message:
## In readLines(files[1], n = 1) : line 1 appears to contain an embedded nul

wos.data.mp <- read.wos("../data/Journals in Mathematical Psychology/Psychonomic Bulletin & Review 2.txt")
## Error in substring(fields, 4) : invalid multibyte string at '<ff><fe>P'
## In addition: Warning message:
## In readLines(files[1], n = 1) : line 1 appears to contain an embedded nul

wos.data <- read.wos("/mnt/md5/data/wos/wos-sci-expanded.firm-names-query.analytical-instruments/LN Public NAICS records from 10001 to 10500.txt")
## Error in substring(fields, 4) : invalid multibyte string at '<ff><fe>P'
## In addition: Warning message:
## In readLines(files[1], n = 1) : line 1 appears to contain an embedded nul
```

# References

*This research was supported (in part) by the Fetzer Franklin Fund of
the John E. Fetzer Memorial Trust.*

van den Akker, O. R., Epskamp, Sacha, & Vlasov, S. A. (2020). The AEV
Algorithm---Author name disambiguation for large Web of Science
datasets.
