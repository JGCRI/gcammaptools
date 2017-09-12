library(gcammaptools)
context("Loading data")

shp_path <- "shp/reg32_spart.shp"
json_path <- "json/reg32.geojson"

test_that("shape file loads successfully from path name", {
    expect_equal(nrow(import_mapdata(shp_path)), 948)
    expect_equal(names(import_mapdata(shp_path)),
                 c("region_id", "geometry"))
})

test_that("json file loads successfully from path name", {
    expect_equal(nrow(import_mapdata(json_path)), 249)
    expect_equal(names(import_mapdata(json_path)),
                 c("REGION_NAME","Area","GCAM_ID","geometry"))
})

shp <- import_mapdata(shp_path)
json <- import_mapdata(json_path)

test_that("imported shape file loads successfully", {
    expect_equal(nrow(shp), 948)
})

test_that("imported shape file loads successfully", {
    expect_equal(nrow(json), 249)
})
