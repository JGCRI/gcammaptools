# tests for the functions contained in `spatial_utils.R`

library(gcammaptools)

context("Test")

test_that("simplified map is smaller", {
    expect_true(object.size(map.rgn32) >
                    object.size(simplify_mapdata(map.rgn32)))

    expect_true(object.size(map.basin235) >
                    object.size(simplify_mapdata(map.basin235)))
})
