library(gcammaptools)
context("Loading data")

shp_path <- "shp/reg32_spart.shp"
json_path <- "json/reg32.geojson"

test_that("shape file loads successfully from path name", {
  shp <- import_mapdata(shp_path)
  expect_equal(class(shp), c("sf", "data.frame"))
  expect_equal(nrow(shp), 948)
  expect_equal(names(import_mapdata(shp_path)),
                 c("region_id", "geometry"))
})

test_that("json file loads successfully from path name", {
  json <- import_mapdata(json_path)
  expect_equal(class(json), c("sf", "data.frame"))
  expect_equal(nrow(json), 249)
  expect_equal(names(import_mapdata(json_path)),
                 c("REGION_NAME","Area","GCAM_ID","geometry"))
})

test_that("loading an existing shape file returns same file", {
  shp <- import_mapdata(shp_path)
  expect_identical(shp, import_mapdata(shp))
})

test_that("imported json loads successfully", {
})
