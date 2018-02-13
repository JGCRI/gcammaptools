# Test add_region_id()

library(gcammaptools)
context("Mapping user data to map data")

add_region_ID(datatable,lookupfile,provincefile,drops,disaggregate)

test <- data.frame(region = c('USA'), stringsAsFactors = F)
test_that("32 regions get added even if only one provided", {
    # taiwan id = 30
    test.id <- add_region_ID(test)
    expect_equal(ncol(test.id), ncol(test) + 1) # Should only add 1 extra column
    expect_equal(nrow(test.id), 32) # 32 regions + 1 NA region - 1 Taiwan
    expect_equal(test.id[1, 'id'], 1)
    expect_equal(test.id[nrow(test.id), 'id'], 0) # Last row has NA region
})

test_that("lookupfiles work", {
    expect_equal(add_region_ID(test),
                 add_region_ID(test, system.file("extdata", "rgn32/lookup.txt", package = "gcammaptools")))
    expect_equal(nrow(add_region_ID(test, rgn14)), 15) # 14 regions + 1 NA
    expect_equal(nrow(add_region_ID(test, basin235)), 237) # 235 basins + 1 NA + 1 USA
    expect_equal(nrow(add_region_ID(test, chn)), 62) # 31 regions + 1 NA + 30 provinces
    expect_equal(nrow(add_region_ID(test, usa)), 84) # 31 regions + 1 NA + 51 states + 1 USA

    # Hawaii is both a basin name and state name
    test.hi <- data.frame(region = c('Hawaii'), stringsAsFactors = F)
    expect_equal(nrow(add_region_ID(test, basin235)), 236) # 235 basins + 1 NA
    expect_equal(nrow(add_region_ID(test, chn)), 63) # 31 regions + 1 NA + 30 provinces + 1 Hawaii
    expect_equal(nrow(add_region_ID(test, usa)), 83) # 31 regions + 1 NA + 51 states
})

test_that("provincefiles work", {
    # Only China has a province file
    test.chn <- read.csv(system.file('extdata', 'china_example.csv', package='gcammaptools'),
                         stringsAsFactors = F)
    test.chn.id <- add_region_ID(test.chn)
    expect_equal(sum(!is.na(test.chn.id$id)), 32) # Only GCAM-32 regions should match
    expect_equal(nrow(add_region_ID(test.chn, chn)), nrow(test.chn.id) + 30) # Add 30 provinces
    expect_equal(nrow(add_region_ID(test.chn, chn, chn)), nrow(test.chn.id)) # With provincefile, no extra rows
})

test_that("drops work", {
    test.drops <- data.frame(region = c('Taiwan'), stringsAsFactors = F)
    l1 <- nrow(add_region_ID(test.drops))
    l2 <- nrow(add_region_ID(test.drops, drops = rgn32))
    expect_equal(l1 - l2, 1)
})
