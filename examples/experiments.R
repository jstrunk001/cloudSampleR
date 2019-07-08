library(raster)
library(lidR)
r1 = raster("D:\\data\\usgs_ascii\\dtm_tiles\\183_302.asc")
r2 = raster("D:\\data\\usgs_ascii\\dtm_tiles\\183_303.asc")
r3 = raster("D:\\data\\usgs_ascii\\dtm_tiles\\184_306.asc")

s1=bind(r1,r2,r3)

r4 = overlay(r1,r2,r3,fun=function(...,na.rm=T)mean(...,na.rm=na.rm))

files1=(list.files("D:\\data\\usgs_ascii\\dtm_tiles\\",pattern="[.]asc",full.names=T))

r5 = raster(unlist([1:5])

library

flaz = list.files("D:\\data\\wadnr_hood_canal\\laz\\hood_canal_NAIP_DSM_2015\\",full.names=T,pattern="[.]laz")

test0 = raster("D:\\data\\wadnr_hood_canal\\laz\\hood_canal_lidar_2015\\Hood_Canal_033.laz")

las1 = catalog("D:\\data\\wadnr_hood_canal\\laz\\hood_canal_lidar_2015\\Hood_Canal_033.laz")
opt_output_files(las1) <- "D:\\temp\\test"
dtm1 = grid_terrain(las1,algorithm = tin())
raster:::.rasterObjectFromFile

ctg = catalog(flaz[1:3])
opt_output_files(ctg) <- "D:\\temp\\test1"
gnd = lasground(ctg, csf(), last_returns = F)
dtm = grid_terrain(ctg, 1, tin())
