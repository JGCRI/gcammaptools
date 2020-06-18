# Test Output

library(gcammaptools)
shape_path <- system.file("data/TM_WORLD_BORDERS_SIMPL-0.3.shp", package = "gcammaptools", mustWork = TRUE)
test_shape <- gcammaptools::import_mapdata(shape_path)
data_path <- system.file("data/data.csv", package = "gcammaptools", mustWork = TRUE)
test_data <- read.csv(data_path, stringsAsFactors = F)


test_that("testing save_plot",
          {
              # Test empty args
              result <- gcammaptools::save_plot()
              expect_equal(substr(result, start = 1, stop = 5),  "Error")

              # Test no file type
              result2 <- gcammaptools::save_plot("test")
              expect_equal(substr(result2, start = 1, stop = 5),  "Error")

              # Test invalid file type
              result3 <- gcammaptools::save_plot("test.gov")
              expect_equal(substr(result3, start = 1, stop = 5),  "Error")

              # Test valid file and type
              result4 <- gcammaptools::save_plot("test.png")
              expect_equal(substr(result4, start = 1, stop = 7),  "Success")

          })
