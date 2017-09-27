# Test Gridded Data

library(gcammaptools)
context("Plotting gridded data")

# load a gridded dataset
prj <- rgcam::loadProject(system.file('sample-gcam-data','gcam-longform-sample.dat', package='gcammaptools'))
co2grid <- rgcam::getQuery(prj, 'Cooling Degree Days', 'Reference')
m <- map.rgn32.simple

test_that("dataset has lat/lon values", {
  expect_true("lon" %in% colnames(co2grid))
  expect_true("lat" %in% colnames(co2grid))
})

test_that("plot_GCAM_grid plots with all defaults", {
  plot_GCAM_grid(co2grid, 'value')
})

test_that("plot_GCAM_grid plots with provided basemaps", {
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=map.rgn14
  )
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=simplify_mapdata(map.basin235, 5)
  )
})

test_that("plots default correctly to EXTENT_WORLD", {
  plot_GCAM_grid(co2grid, 'value')
  plot_GCAM_grid(co2grid, 'value', extent = EXTENT_WORLD)
})

test_that("gridded data looks okay when projection is centered somewhere else", {
  plot_GCAM_grid(co2grid,
            col='value',
            map=m,
            proj=na_aea,
            extent=EXTENT_USA,
            zoom=15)
})

test_that("gridded data plots with EPSG projection type", {
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=m,
                 proj=4326,
                 proj_type='EPSG',
                 extent=EXTENT_USA)
})

test_that("gridded data plots with orthographic projections", {
  
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=m,
                 proj=af_ortho,
                 extent=EXTENT_AFRICA,
                 zoom=10)
  
  # No longer support ortho
  expect_error(
    plot_GCAM_grid(co2grid,
                   col='value',
                   map=m,
                   proj=ortho,
                   extent=EXTENT_USA)
  )
})

test_that("gridded data plots with SR-ORG projection type and zoom", {
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=m,
                 proj=7567,
                 proj_type='SR-ORG',
                 extent=EXTENT_LA,
                 zoom=8)
})
  
test_that("gridded data plots with robinson projection", {
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=m,
                 proj=robin)
})

test_that("gridded data plots with eck3 projection", {
  plot_GCAM_grid(co2grid,
                 col='value',
                 map=m,
                 proj=eck3)
})


test_that("grid plots even with incorrectly matched projections and extents", {
  plot_GCAM_grid(co2grid,
            col="value",
            proj=na_aea,
            extent=EXTENT_WORLD)
})


