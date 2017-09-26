# Test Gridded Data

library(gcammaptools)
context("Plotting gridded data")

# load a gridded dataset
prj <- rgcam::loadProject(system.file('sample-gcam-data','gcam-longform-sample.dat', package='gcammaptools'))
co2grid <- rgcam::getQuery(prj, 'Cooling Degree Days', 'Reference')

test_that("dataset has lat/lon values", {
  expect_true("lon" %in% colnames(co2grid))
  expect_true("lat" %in% colnames(co2grid))
})

test_that("plot_GCAM_grid plots with all defaults", {
  plot_GCAM_grid(co2grid,
                 col='value'
  )
})

test_that("plot_GCAM_grid plots with provided basemaps", {
  tic()
  plot_GCAM_grid(co2grid,
                 col='value',
                 map = simplify_mapdata(map.rgn14),
                 proj = wgs84
  )
  toc()
  plot_GCAM_grid(co2grid,
                 col='value',
                 map = simplify_mapdata(map.basin235, 5)
  )
})
