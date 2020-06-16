# Test Spatial_Utils

library(gcammaptools)
context("Testing spatial_utils functionality")

# load test shape objects
shape_path <- system.file("data/TM_WORLD_BORDERS_SIMPL-0.3.shp", package = "gcammaptools", mustWork = TRUE)
test_shape <- gcammaptools::import_mapdata(shape_path)
data_path <- system.file("data/data.csv", package = "gcammaptools", mustWork = TRUE)
test_data <- read.csv(data_path, stringsAsFactors = F)

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

              # # Test both
              result6 <- gcammaptools::process_data(test_data, shape_data_field =  "POP2005")
              expect_equal(substr(result4, start = 1, stop = 5),  "Error")
          })
