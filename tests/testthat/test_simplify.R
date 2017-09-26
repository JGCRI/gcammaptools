# Test Gridded Data

library(gcammaptools)
context("Simplifying map data for faster plotting")

test_that("simplified map is smaller", {
  expect_true(object.size(map.rgn32) > 
              object.size(simplify_mapdata(map.rgn32)))
  
  expect_true(object.size(map.basin235) > 
              object.size(simplify_mapdata(map.basin235)))
})

test_that("simplified map is same type and class as original map", {
  expect_equal(class(map.rgn32), class(simplify_mapdata(map.rgn32)))
  expect_equal(typeof(map.rgn32), typeof(simplify_mapdata(map.rgn32)))
  
  expect_equal(class(map.rgn14), class(simplify_mapdata(map.rgn14)))
  expect_equal(typeof(map.rgn14), typeof(simplify_mapdata(map.rgn14)))
  
  expect_equal(class(map.basin235), class(simplify_mapdata(map.basin235)))
  expect_equal(typeof(map.basin235), typeof(simplify_mapdata(map.basin235)))
  
  expect_equal(class(map.chn), class(simplify_mapdata(map.chn)))
  expect_equal(typeof(map.chn), typeof(simplify_mapdata(map.chn)))
})

test_that("simplified map has fewer polygons", {
  expect_true(length(sf::st_geometry(map.rgn32)) > 
              length(sf::st_geometry(simplify_mapdata(map.rgn32))))
  
  expect_true(length(sf::st_geometry(map.basin235)) > 
              length(sf::st_geometry(simplify_mapdata(map.basin235))))
})
