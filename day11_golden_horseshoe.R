
# Create Elevation Map of Ontario ‘Horseshoe' -----------------------------
# Eugene Joh
# 2019-11-11

# Data Source:
# Manually extracted from Government of Canada Geo-elevation tool
# Job id : 130388
# Data source : Canadian Digital Elevation Model
# Product(s) : Digital Elevation Model
# Output format : DEM : GEOTIFF
# Reference coordinate system : WGS 84 / Pseudo-Mercator (EPSG:3857)
# Resolution : 20 m

library(dplyr)
library(rgdal)
library(raster)
library(rayshader)

# Import
file_path <- file.path("~", "Documents", "on_elevation_20191111", "DEM.tif")
d <- raster::raster(file_path)

# convert to matrix
dm <- matrix(
  raster::extract(d, raster::extent(d), buffer = 1000), 
  nrow = ncol(d), ncol = nrow(d)
)

# rayshade away!
ambmat <- ambient_shade(dm, zscale = 30)
raymat <- ray_shade(dm, zscale = 30, lambert = TRUE)
watermap <- detect_water(dm)

zscale <- 10
rgl::clear3d()
# make 3d snapshot
dm %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  # add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(dm, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(dm)/zscale, wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_snapshot()

# make 3d animation
dm %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>% 
  save_3d_gif(dm, file = "images/sf-flyby.gif", duration = 6,
            zscale = zscale, windowsize = c(1200, 1000), wateralpha = 0,
            water = TRUE, soliddepth = -max(dm)/zscale, 
            theta = theta, phi = phi, zoom = zoom, fov = 60)