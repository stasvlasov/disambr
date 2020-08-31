
# Table of Contents

1.  [Usage](#orge7ff504)
2.  [Implementation of EVA algorithm (van den Akker et al., 2020)](#orga6d5f48)
3.  [Installation](#org2dbfb88)
4.  [Sets attributes](#orgded66f5)
5.  [Overall design principles](#org111101f)
6.  [Naming convention](#org8c46c8f)
7.  [Web of Science Field Tags 2018-06-27](#org4baa4d6)
8.  [Existing tools for reading WoS data](#orgb2333cc)
    1.  [bibliometrix](#org1ab96f9)
    2.  [metagear](#org5faca16)
    3.  [BibPlots](#org84778ce)
    4.  [hindexcalculator](#org0174010)
    5.  [refsplitr](#org2005afa)
    6.  [wosr](#org6c0bc7d)
    7.  [read.wos.R](#org8b6da53)
9.  [References](#orga413391)

`disambr` is an R :package: that provides a flexible framework for disambiguation of named entities. Currently this package implements the [AEV algorithm](#orga6d5f48) (van den Akker et al., 2020) for Web of Science author disambiguation.


<a id="orge7ff504"></a>

# Usage

The idea of basic usage is simply piping the disambiguation procedures. Each procedure takes list of sets as input and returns list of sets either by adding a new set or modifying input list of sets.

    data %>% 
        disambr.get.different.authors %>% 
        disambr.get.similar.initials %>%
        disambr.get.similar.last.names

Sequence (piped functions) of disambiguation procedures defines a disambiguation algorithm.

    disambr.eva <- function(data) {
        data %>% 
    	disambr.get.different.authors %>% 
    	disambr.get.similar.initials %>%
    	disambr.get.similar.last.names
    }

A procedure can be defined as follows (not implemented). Just an idea.

    disambr.get.different.authors <- disambr.define.procedure(data %>% 
    							    get(publication) %>%
    							    for.each %>%
    							    get(person = author))
    
      ## or
      disambr.get.different.authors <- disambr.define.procedure(data$
    							    publication$
    							    person(author))


<a id="orga6d5f48"></a>

# Implementation of EVA algorithm (van den Akker et al., 2020)

The creation of co-authorship networks is a valuable way to depict the social structure of scientific fields. However, these co-authorship networks often get distorted because of the problems of author name synonymy (the same author is split into two nodes because his name is spelled differently in different publications) and author name homonymy (different authors are compounded into one node because they share the same name). The practice of author name disambiguation (AND) tries to solve these problems by correctly identifying the authors of scientific articles.

Several algorithms have been put forward in the context of AND, but none of them are suitable for large datasets of the Web of Science database. Therefore, in an earlier part of this project we proposed a new unsupervised learning algorithm based on the most recent AND literature. This so-called AEV-algorithm involves two phases: a blocking phase, in which pairs of authors are selected that are sufficiently similar, and a disambiguation phase, in which similar author names are either split or combined into one node based on information retrieved from the Web of Science database. In the disambiguation phase, the algorithm uses information about co-authorship, e-mail addresses, institutional affiliations, cited references, and article keywords (van den Akker et al., 2020).


<a id="org2dbfb88"></a>

# Installation

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


<a id="orgded66f5"></a>

# Sets attributes

All disambiguation procedures used in `disambr` package work with sets. A set is basically any R object that can represent [mathematical sets](https://en.wikipedia.org/wiki/Set_(mathematics)) (e.g., set of authors, set of companies) with special attributes that are used by `disabmr` functions to identify the kind of set it is working with or produce (e.g., a set of authors that are likely to be the same person, a set of companies that are definitely different companies, etc.).

The attributes that are currently used to define/describe set as well as their values are listed below:

-   **disambr<sub>entity</sub>:** -   *person*, *organization*, *publication*
-   **disambr<sub>set</sub><sub>type</sub>:** -   *similar<sub>entities</sub>*, *different<sub>entities</sub>*
-   **disambr<sub>set</sub><sub>coefficient</sub>:** -   number between 0 and 1 indicating how strongly entities are similar or different from each other. It is use only for establishing order of sets processing (e.g., start with sets of least similar entities)
-   **disambr<sub>set</sub><sub>name</sub>:** -   string name of the set
-   **disambr<sub>set</sub><sub>collection</sub>:** -   *single<sub>set</sub><sub>table</sub>* (first column assumed to store entity id or entity id is just row number if `entity_id_reference` attribute is set to *self*, see below), *list<sub>of</sub><sub>sets</sub><sub>as</sub><sub>lists</sub>* (each set is a list of entity ids), *dyads<sub>table</sub>* (first and second columns assumed to be ids for the pair of entities)
-   **disambr<sub>entity</sub><sub>id</sub><sub>reference</sub>:** -   *self*, name of other set as in its `set_name` attribute
-   **disambr<sub>entity</sub><sub>id</sub><sub>reference</sub><sub>md5</sub><sub>sum</sub>:** -   md5 cache sum of the object where entities ids are referring to ensure that we will get to correct data for entities in the set.
-   **disambr<sub>recipe</sub>:** -   list of `disambr` procedures that were applied to produce given set(s)
    -   if it is a named list then first item is procedure name and the rest are properties:
        -   *procedure*
        -   *file<sub>name</sub>*
        -   *file<sub>md5sum</sub>* (to check file identity later)
        -   *file<sub>header</sub>* (to check for consistency between read files)


<a id="org111101f"></a>

# Overall design principles

-   To allow for modular design each disambiguation procedure should accept and return list of sets (e.g., same person sets, different person sets, other probability of being the same person sets)
-   List of sets from (chain of) various procedures will be then merged (using sets algebra) according to the specific disambiguation algorithm to produce final list of sets.
-   Initial input should be in the form of a list of initial sets (the simplest input is one set with every person likely to be non unique, e.g., data.table of authors from Web of Science bibliography data).
-   When reading data package should try to do as many sets as possible on a fly (cleaning and splitting initial data to different types of entities)
-   Try to implement lazy data loading and processing where possible


<a id="org8c46c8f"></a>

# Naming convention

-   All functions should have a verb
-   Variables should not have a verb
-   Everything that is available to user should start with "disambr\_" (i.e., package name)
-   Internal functions and variables does not have to have this prefix
-   Use mainstream coding style guides everywhere where possible
    -   <https://google.github.io/styleguide/Rguide.html>
    -   <http://adv-r.had.co.nz/Style.html>
    -   <https://style.tidyverse.org/syntax.html#object-names>


<a id="org4baa4d6"></a>

# Web of Science Field Tags 2018-06-27

<https://support.clarivate.com/ScientificandAcademicResearch/s/article/Web-of-Science-Core-Collection-List-of-field-tags-in-output?language=en_US>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">FN</td>
<td class="org-left">File Name</td>
</tr>


<tr>
<td class="org-left">VR</td>
<td class="org-left">Version Number</td>
</tr>


<tr>
<td class="org-left">PT</td>
<td class="org-left">Publication Type (J=Journal; B=Book; S=Series; P=Patent)</td>
</tr>


<tr>
<td class="org-left">AU</td>
<td class="org-left">Authors</td>
</tr>


<tr>
<td class="org-left">AF</td>
<td class="org-left">Author Full Name</td>
</tr>


<tr>
<td class="org-left">BA</td>
<td class="org-left">Book Authors</td>
</tr>


<tr>
<td class="org-left">BF</td>
<td class="org-left">Book Authors Full Name</td>
</tr>


<tr>
<td class="org-left">CA</td>
<td class="org-left">Group Authors</td>
</tr>


<tr>
<td class="org-left">GP</td>
<td class="org-left">Book Group Authors</td>
</tr>


<tr>
<td class="org-left">BE</td>
<td class="org-left">Editors</td>
</tr>


<tr>
<td class="org-left">TI</td>
<td class="org-left">Document Title</td>
</tr>


<tr>
<td class="org-left">SO</td>
<td class="org-left">Publication Name</td>
</tr>


<tr>
<td class="org-left">SE</td>
<td class="org-left">Book Series Title</td>
</tr>


<tr>
<td class="org-left">BS</td>
<td class="org-left">Book Series Subtitle</td>
</tr>


<tr>
<td class="org-left">LA</td>
<td class="org-left">Language</td>
</tr>


<tr>
<td class="org-left">DT</td>
<td class="org-left">Document Type</td>
</tr>


<tr>
<td class="org-left">CT</td>
<td class="org-left">Conference Title</td>
</tr>


<tr>
<td class="org-left">CY</td>
<td class="org-left">Conference Date</td>
</tr>


<tr>
<td class="org-left">CL</td>
<td class="org-left">Conference Location</td>
</tr>


<tr>
<td class="org-left">SP</td>
<td class="org-left">Conference Sponsors</td>
</tr>


<tr>
<td class="org-left">HO</td>
<td class="org-left">Conference Host</td>
</tr>


<tr>
<td class="org-left">DE</td>
<td class="org-left">Author Keywords</td>
</tr>


<tr>
<td class="org-left">ID</td>
<td class="org-left">Keywords Plus®</td>
</tr>


<tr>
<td class="org-left">AB</td>
<td class="org-left">Abstract</td>
</tr>


<tr>
<td class="org-left">C1</td>
<td class="org-left">Author Address</td>
</tr>


<tr>
<td class="org-left">RP</td>
<td class="org-left">Reprint Address</td>
</tr>


<tr>
<td class="org-left">EM</td>
<td class="org-left">E-mail Address</td>
</tr>


<tr>
<td class="org-left">RI</td>
<td class="org-left">ResearcherID Number</td>
</tr>


<tr>
<td class="org-left">OI</td>
<td class="org-left">ORCID Identifier (Open Researcher and Contributor ID)</td>
</tr>


<tr>
<td class="org-left">FU</td>
<td class="org-left">Funding Agency and Grant Number</td>
</tr>


<tr>
<td class="org-left">FX</td>
<td class="org-left">Funding Text</td>
</tr>


<tr>
<td class="org-left">CR</td>
<td class="org-left">Cited References</td>
</tr>


<tr>
<td class="org-left">NR</td>
<td class="org-left">Cited Reference Count</td>
</tr>


<tr>
<td class="org-left">TC</td>
<td class="org-left">Web of Science Core Collection Times Cited Count</td>
</tr>


<tr>
<td class="org-left">Z9</td>
<td class="org-left">Total Times Cited Count\*</td>
</tr>


<tr>
<td class="org-left">U1</td>
<td class="org-left">Usage Count (Last 180 Days)</td>
</tr>


<tr>
<td class="org-left">U2</td>
<td class="org-left">Usage Count (Since 2013)</td>
</tr>


<tr>
<td class="org-left">PU</td>
<td class="org-left">Publisher</td>
</tr>


<tr>
<td class="org-left">PI</td>
<td class="org-left">Publisher City</td>
</tr>


<tr>
<td class="org-left">PA</td>
<td class="org-left">Publisher Address</td>
</tr>


<tr>
<td class="org-left">SN</td>
<td class="org-left">International Standard Serial Number (ISSN)</td>
</tr>


<tr>
<td class="org-left">EI</td>
<td class="org-left">Electronic International Standard Serial Number (eISSN)</td>
</tr>


<tr>
<td class="org-left">BN</td>
<td class="org-left">International Standard Book Number (ISBN)</td>
</tr>


<tr>
<td class="org-left">J9</td>
<td class="org-left">29-Character Source Abbreviation</td>
</tr>


<tr>
<td class="org-left">JI</td>
<td class="org-left">ISO Source Abbreviation</td>
</tr>


<tr>
<td class="org-left">PD</td>
<td class="org-left">Publication Date</td>
</tr>


<tr>
<td class="org-left">PY</td>
<td class="org-left">Year Published</td>
</tr>


<tr>
<td class="org-left">VL</td>
<td class="org-left">Volume</td>
</tr>


<tr>
<td class="org-left">IS</td>
<td class="org-left">Issue</td>
</tr>


<tr>
<td class="org-left">SI</td>
<td class="org-left">Special Issue</td>
</tr>


<tr>
<td class="org-left">PN</td>
<td class="org-left">Part Number</td>
</tr>


<tr>
<td class="org-left">SU</td>
<td class="org-left">Supplement</td>
</tr>


<tr>
<td class="org-left">MA</td>
<td class="org-left">Meeting Abstract</td>
</tr>


<tr>
<td class="org-left">BP</td>
<td class="org-left">Beginning Page</td>
</tr>


<tr>
<td class="org-left">EP</td>
<td class="org-left">Ending Page</td>
</tr>


<tr>
<td class="org-left">AR</td>
<td class="org-left">Article Number</td>
</tr>


<tr>
<td class="org-left">DI</td>
<td class="org-left">Digital Object Identifier (DOI)</td>
</tr>


<tr>
<td class="org-left">D2</td>
<td class="org-left">Book Digital Object Identifier (DOI)</td>
</tr>


<tr>
<td class="org-left">PG</td>
<td class="org-left">Page Count</td>
</tr>


<tr>
<td class="org-left">P2</td>
<td class="org-left">Chapter Count (Book Citation Index)</td>
</tr>


<tr>
<td class="org-left">WC</td>
<td class="org-left">Web of Science Categories</td>
</tr>


<tr>
<td class="org-left">SC</td>
<td class="org-left">Research Areas</td>
</tr>


<tr>
<td class="org-left">GA</td>
<td class="org-left">Document Delivery Number</td>
</tr>


<tr>
<td class="org-left">UT</td>
<td class="org-left">Accession Number</td>
</tr>


<tr>
<td class="org-left">PM</td>
<td class="org-left">PubMed ID</td>
</tr>


<tr>
<td class="org-left">ER</td>
<td class="org-left">End of Record</td>
</tr>


<tr>
<td class="org-left">EF</td>
<td class="org-left">End of File</td>
</tr>
</tbody>
</table>


<a id="orgb2333cc"></a>

# Existing tools for reading WoS data

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">name</th>
<th scope="col" class="org-left">comments</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">bibliometrix</td>
<td class="org-left">reads only plaintext format into bibliometrixDB object</td>
</tr>


<tr>
<td class="org-left">wosr</td>
<td class="org-left">Requires WoS API subscription</td>
</tr>


<tr>
<td class="org-left">refsplitr</td>
<td class="org-left">package ‘refsplitr’ is not available (for R version 4.0.1)</td>
</tr>


<tr>
<td class="org-left">read.wos.R</td>
<td class="org-left">Does not work&#x2026;</td>
</tr>


<tr>
<td class="org-left">metagear</td>
<td class="org-left">scrape<sub>bibliography</sub> by DOI</td>
</tr>


<tr>
<td class="org-left">hindexcalculator</td>
<td class="org-left">?</td>
</tr>
</tbody>
</table>


<a id="org1ab96f9"></a>

## bibliometrix

<https://github.com/massimoaria/bibliometrix>

Site: <https://bibliometrix.org/index.html>

    git clone https://github.com/massimoaria/bibliometrix

    install.packages("bibliometrix")
    library("bibliometrix")
    library("magrittr")
    
    
    bmdata <- convert2df(file = 'https://www.bibliometrix.org/datasets/wos_plaintext.txt', dbsource = 'wos', format = "plaintext")
    
    
    bmdata %>% class
    ## [1] "data.frame"     "bibliometrixDB"
    
    bmdata %>% names
    
    
    bmdata <- convert2df(file = 'https://www.bibliometrix.org/datasets/wos_plaintext.txt', dbsource = 'wos', format = "csv")


<a id="org5faca16"></a>

## metagear

CRAN docs: <https://cran.r-project.org/web/packages/metagear/metagear.pdf>

GitHub: <https://github.com/cran/metagear/>

    git clone https://github.com/cran/metagear/

    scrape_bibliography


<a id="org84778ce"></a>

## BibPlots

CRAN docs: <https://cran.r-project.org/web/packages/BibPlots/BibPlots.pdf>

Paper: <https://arxiv.org/pdf/1905.09095.pdf>


<a id="org0174010"></a>

## hindexcalculator

CRAN docs:  <https://cran.r-project.org/web/packages/hindexcalculator/hindexcalculator.pdf>


<a id="org2005afa"></a>

## refsplitr

    git clone https://github.com/ropensci/refsplitr

    install.packages("refsplitr")
    library("refsplitr")


<a id="org6c0bc7d"></a>

## wosr

Requires premium WoS API - <https://clarivate.com/webofsciencegroup/solutions/xml-and-apis>

CRAN doc: <https://cran.r-project.org/web/packages/wosr/wosr.pdf>

Site: <https://github.com/vt-arc/wosr>

GitHub: <https://github.com/vt-arc/wosr>

    git clone https://github.com/vt-arc/wosr

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


<a id="org8b6da53"></a>

## read.wos.R

GitHub: <https://github.com/alberto-martin/read.wos.R>

    git clone https://github.com/alberto-martin/read.wos.R

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


<a id="orga413391"></a>

# References

*This research was supported (in part) by the Fetzer Franklin Fund of the John E. Fetzer Memorial Trust.*

van den Akker, O. R., Epskamp, Sacha, & Vlasov, S. A. (2020). The AEV Algorithm—Author name disambiguation for large Web of Science datasets.

