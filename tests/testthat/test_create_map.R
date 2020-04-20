library(gcammaptools)
context("Creating map from shape file and raster")

shp_path <- "shp/reg32_spart.shp"
raster_path <- "raster/gaplf2011lc_v30_ri.tif"

test_that("Creating map from shape and raster", {
    shp <- import_mapdata(shp_path)
    expect_equal(class(shp), c("sf", "data.frame"))
    expect_equal(nrow(shp), 948)
    expect_equal(names(import_mapdata(shp_path)),
                 c("region_id", "geometry"))
})


test_that("loading an existing shape file returns same file", {
    shp <- import_mapdata(shp_path)
    expect_identical(shp, import_mapdata(shp))
})

test_that("imported json loads successfully", {
})



#test <- create_map(shape_data = "e:/repos/github/data/ri/State_Boundary_1997.shp", raster_data = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT")
#test <- create_map(shape_path = "e:/repos/github/data/USA_adm/USA_adm0.shp", raster_path = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT", data_classification = "Land")
#test <- create_map(shape_data = "e:/repos/github/data/tm_world_borders_simpl-0.3.shp", raster_data = "E:/Repos/github/data/wc2.0_10m_tavg_01.tif", output_file = "./test", raster_col = "wc2.0_10m_tavg_01")

