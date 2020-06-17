# Test Spatial_Utils

library(gcammaptools)
context("Testing spatial_utils functionality")

# Load test shape and data objects
shape_path <- system.file("data/TM_WORLD_BORDERS_SIMPL-0.3.shp", package = "gcammaptools", mustWork = TRUE)
test_shape <- gcammaptools::import_mapdata(shape_path)
data_path <- system.file("data/data.csv", package = "gcammaptools", mustWork = TRUE)
test_data <- read.csv(data_path, stringsAsFactors = F)

# Test process_shape
test_that("testing process_shape",
          {
              # Test loading via path
              result <- gcammaptools::process_shape(shape_path)
              expect_equal(test_shape, result )

              # Test loading via obj
              result2 <- gcammaptools::process_shape(test_shape)
              expect_equal(test_shape, result2 )

              # Test no shape_data arg
              result3 <- gcammaptools::process_shape()
              expect_equal(substr(result3, start = 1, stop = 5),  "Error")

              # Test file not found
              result4 <- gcammaptools::process_shape("")
              expect_equal(substr(result4, start = 1, stop = 5),  "Error")

              # Test simplify
              result5 <- gcammaptools::process_shape(test_shape, simplify = TRUE)
              expect_equal(class(result5), c("sf", "data.frame"))
          })

# Test process_data
test_that("testing process_data",
          {
              # Test loading via path
              result <- gcammaptools::process_data(data_path)
              expect_equal(test_data, result )

              # Test loading via obj
              result2 <- gcammaptools::process_data(test_data)
              expect_equal(test_data, result2 )

              # Test no shape_data arg
              result3 <- gcammaptools::process_data()
              expect_equal(substr(result3, start = 1, stop = 5),  "Error")

              # # Test file not found
              result4 <- gcammaptools::process_data("")
              expect_equal(substr(result4, start = 1, stop = 5),  "Error")

              # # Test using shape data field
              result5 <- gcammaptools::process_data(shape_obj = test_shape, shape_data_field =  "POP2005")
              expect_equal(class(result5), c("sf", "data.frame"))

              # Test both data args together (invalid)
              result6 <- gcammaptools::process_data(test_data, shape_data_field =  "POP2005")
              expect_equal(substr(result4, start = 1, stop = 5),  "Error")
          })

# Test simplify_mapdata
test_that("testing simplify_mapdata",
          {
              # Test basic functionality
              result <- gcammaptools::simplify_mapdata(test_shape, min_area = 2, degree_tolerance = 0.2)
              expect_equal(class(result), c("sf", "data.frame"))

              # Test no args
              result2 <- gcammaptools::simplify_mapdata()
              expect_equal(substr(result2, start = 1, stop = 5),  "Error")

              # Test wrong arg type for map_data
              result3 <- gcammaptools::simplify_mapdata(test_data)
              expect_equal(substr(result3, start = 1, stop = 5),  "Error")

              # Test wrong args for min_area
              result4 <- gcammaptools::simplify_mapdata(test_shape, min_area = 0)
              expect_equal(substr(result4, start = 1, stop = 5),  "Error")

              # Test wrong args for degree_tolerance
              result5 <- gcammaptools::simplify_mapdata(test_shape, degree_tolerance = "0.1")
              expect_equal(substr(result5, start = 1, stop = 5),  "Error")

          })

# Test load_shp
test_that("testing load_shp",
          {
              # Test basic functionality
              result <- gcammaptools::load_shp(shape_path)
              expect_equal(class(result), c("sf", "data.frame"))

              # Test no args
              result2 <- gcammaptools::load_shp()
              expect_equal(substr(result2, start = 1, stop = 5),  "Error")

              # Test bad path
              result3 <- gcammaptools::load_shp("file.shp")
              expect_equal(substr(result3, start = 1, stop = 5),  "Error")

          })

# Test section for import_data function (using existing functionality for now)

# Load shape and data test paths
shp_path <- "shp/reg32_spart.shp"
json_path <- "json/reg32.geojson"

test_that("shape file loads successfully from path name",
          {
            shp <- import_mapdata(shp_path)
            expect_equal(class(shp), c("sf", "data.frame"))
            expect_equal(nrow(shp), 948)
            expect_equal(names(import_mapdata(shp_path)),
                         c("region_id", "geometry"))
})

test_that("json file loads successfully from path name",
          {
            json <- import_mapdata(json_path)
            expect_equal(class(json), c("sf", "data.frame"))
            expect_equal(nrow(json), 249)
            expect_equal(names(import_mapdata(json_path)),
                         c("REGION_NAME","Area","GCAM_ID","geometry"))
})

test_that("loading an existing shape file returns same file",
          {
            shp <- import_mapdata(shp_path)
            expect_identical(shp, import_mapdata(shp))
})
