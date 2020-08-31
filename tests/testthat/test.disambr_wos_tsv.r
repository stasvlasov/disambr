## [[id:org:l7egnb60jti0][disambr_make_wos_tsv_publications:2]]
test_that("disambr_make_wos_tsv_publications", {
    my.file <- system.file("testdata", "wos-tsv-test-recent.txt", package = "disambr")
    skip_if_not(file.exists(my.file))
    dt <- list(disambr_read_file(my.file))
    expect_equal(disambr_make_wos_tsv_publications(dt, "recipe-lalala")$disambr_set_recipe, "recipe-lalala")
    expect_length(disambr_make_wos_tsv_publications(dt, "recipe-lalala"), 72)
})
## disambr_make_wos_tsv_publications:2 ends here

## [[id:org:7cofnb60jti0][disambr_make_wos_tsv_authors:2]]
test_that("disambr_make_wos_tsv_authors", {
    my.file <- system.file("testdata", "wos-tsv-test-recent.txt", package = "disambr")
    skip_if_not(file.exists(my.file))
    dt <- disambr_read_file(my.file)
    expect_length(attributes(disambr_make_wos_tsv_authors(dt))$disambr_set_recipe, 2)
})
## disambr_make_wos_tsv_authors:2 ends here

## [[id:org:35sfnb60jti0][disambr_wos_tsv_parse_cr:2]]
test_that("disambr_wos_tsv_parse_cr", {
  d <- "Allen C, 2017, ENVIRON SCI-NANO, V4, P741, DOI 10.1039/c7en90014g; Baek YW, 2011, SCI TOTAL ENVIRON, V409, P1603, DOI 10.1016/j.scitotenv.2011.01.014; Baker GL, 2008, TOXICOL SCI, V101, P122, DOI 10.1093/toxsci/kfm243; Bergstrom U, 2015, J TOXICOL ENV HEAL A, V78, P645, DOI 10.1080/15287394.2015.1017682; Bhushan B, 2011, PROG MATER SCI, V56, P1, DOI 10.1016/j.pmatsci.2010.04.003; Biswas P, 2005, J AIR WASTE MANAGE, V55, P708, DOI 10.1080/10473289.2005.10464656; Bitterle E, 2006, CHEMOSPHERE, V65, P1784, DOI 10.1016/j.chemosphere.2006.04.035; Bondarenko O, 2013, ARCH TOXICOL, V87, P1181, DOI 10.1007/s00204-013-1079-4; Bonner J. C., 2003, ENV HLTH PERSPECT, V111, P1289; Brossell D, 2013, J AEROSOL SCI, V63, P75, DOI 10.1016/j.jaerosci.2013.04.012; Clift MJD, 2011, ARCH TOXICOL, V85, P723, DOI 10.1007/s00204-010-0560-6; Cohen J, 2013, NANOTOXICOLOGY, V7, P417, DOI 10.3109/17435390.2012.666576; Cohen JM, 2014, PART FIBRE TOXICOL, V11, DOI 10.1186/1743-8977-11-20; Comouth A, 2013, J AEROSOL SCI, V63, P103, DOI 10.1016/j.jaerosci.2013.04.009"
  expect_equal(nrow(disambr_wos_tsv_parse_cr(stri_split_fixed(d, "; ")[[1]])), 14)
  })
## disambr_wos_tsv_parse_cr:2 ends here

## [[id:org:jxufnb60jti0][disambr_make_wos_tsv_references:2]]
test_that("disambr_make_wos_tsv_references", {
  my.file <- system.file("testdata", "wos-tsv-test-recent.txt", package = "disambr")
    skip_if_not(file.exists(my.file))
    dt <- disambr_read_file(my.file)
    expect_length(nrow(disambr_make_wos_tsv_references(dt)), 3760)
})
## disambr_make_wos_tsv_references:2 ends here
