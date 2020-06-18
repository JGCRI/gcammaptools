# Test Error Checking

library(gcammaptools)
context("Testing error_checking functionality")

# Load test shape and data objects
shape_path <- system.file("data/TM_WORLD_BORDERS_SIMPL-0.3.shp", package = "gcammaptools", mustWork = TRUE)
test_shape <- gcammaptools::import_mapdata(shape_path)
data_path <- system.file("data/data.csv", package = "gcammaptools", mustWork = TRUE)
test_data <- read.csv(data_path, stringsAsFactors = F)

# Test process_shape
test_that("testing verify_shape",
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
