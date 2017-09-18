library(gcammaptools)
context("Lookup proj4 string")

test_that("looking up a proj4 string with a default key gives correct result", {
    expect_equal(assign_prj4s("prj4s_key", "us"), "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83")
    expect_equal(assign_prj4s("prj4s_key", "africa"), "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 ")
    expect_equal(assign_prj4s("prj4s_key", "world"), "+proj=longlat +datum=WGS84 +no_defs")
    expect_equal(assign_prj4s("prj4s_key", "ch_aea"), "+proj=aea +lat_1=27 +lat_2=45 +x_0=0 +y_0=0 +lat_0=35 +lon_0=105 +ellps=WGS84 +datum=WGS84")
    expect_equal(assign_prj4s("prj4s_key", "eck3"), "+proj=eck3 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
})

test_that("looking up an ESRI proj4 string from http://spatialreference.org works", {
    expect_equal(assign_prj4s("ESRI", 37001), "+proj=longlat +ellps=WGS66 +no_defs ")
})

test_that("looking up an ESPG proj4 string from http://spatialreference.org works", {
    expect_equal(assign_prj4s("EPSG", 2044), "+proj=tmerc +lat_0=0 +lon_0=105 +k=1 +x_0=18500000 +y_0=0 +ellps=krass +towgs84=-17.51,-108.32,-62.39,0,0,0,0 +units=m +no_defs ")
})

test_that("looking up an SG-ORG proj4 string from http://spatialreference.org works", {
    expect_equal(assign_prj4s("SR-ORG", 6754), "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
})

test_that("looking up an invalid proj4 string from http://spatialreference.org returns a warning", {
    expect_warning(assign_prj4s("SR-ORG", 9004))
    expect_warning(x <- assign_prj4s("EPSG", "INVALID"))
    # also test that looking up an invalid proj4 string returns a default value
    expect_equal(x, wgs84)
})
