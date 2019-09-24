


if( F){
	library(rgdal)
	library(rgeos)
	#poly1=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las","las_polys")
	poly1=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/las_polys.rds")
	poly1a=gBuffer(poly1,width = 5)
	saveRDS(poly1a,"D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
	#poly2=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las","las_polys")
	poly2=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/las_polys.rds")
	poly2a=gBuffer(poly2,width = 5)
	saveRDS(poly2a,"D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

}
if( !"poly1" %in% ls() ){

	library(rgdal)
	library(rgeos)
	library(cloudSampleR)
	library(raster)

	poly1a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
	poly2a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

}
# build a "single" dtm from a vector of dtms
# gdalbuildvrt
# gdalUtils
# https://www.rdocumentation.org/packages/gdalUtils/versions/2.0.1.14/topics/gdalbuildvrt

if(F){

	tifs = list.files("D:\\data\\wadnr_hood_canal\\hood_canal_dtm",pattern="[.]tif",full.names=T)
	gdalUtils::gdalbuildvrt(tifs,"D:\\data\\wadnr_hood_canal\\hood_canal_dtm\\dtm_demo.vrt")

}


source("D:\\r_packages_dev\\my_packages\\cloudSampleR\\R\\cloud2xSample.R")
cloud2xSample(
	pathClipData = "c:/fusion/clipdata.exe"
	,pathOutA = "d:/temp/hood_canal_test/clip3in/"
	,pathOutB = "d:/temp/hood_canal_test/clipLidar/"
	#,pathOutB = "d:/temp/hood_canal_test/clip6in/"
	,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
	#,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
	,pathLasB = "D:\\data\\wadnr_hood_canal\\laz\\hood_canal_lidar_2015\\"
	,pathDTMA = "D:\\data\\wadnr_hood_canal\\hood_canal_dtm\\hood_canal_dtm.vrt"
	,pathDTMB = "D:\\data\\wadnr_hood_canal\\hood_canal_dtm\\hood_canal_dtm.vrt"
	# ,pathDTMA = "D:\\data\\usgs_ascii\\dtm_tiles\\usgsdtms.vrt"
	# ,pathDTMB = "D:\\data\\usgs_ascii\\dtm_tiles\\usgsdtms.vrt"
	,extentPolyA = poly1a
	,extentPolyB = poly2a
	,nCore = 3
	,nSample = 5
	#,procMethod = "FUSION"
	,procMethod = "lidR"
	#,sampleShpA = "D:\\temp\\hood_canal_test\\clip3in\\shapefiles\\2019Jun10165355_SamplePoints.shp"

)
