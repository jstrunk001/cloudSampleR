
library(rgdal)
library(rgeos)
library(cloudSampleR)

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
	poly1a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
	poly2a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

}

cloud2xSample(
	pathClipData = "c:/fusion/clipdata.exe"
	,pathOutA = "d:/temp/hood_canal_test/clip3in/"
	,pathOutB = "d:/temp/hood_canal_test/clip6in/"
	,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
	,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
	,extentPolyA = poly1a
	,extentPolyB = poly2a
	,nCore = 3
	,nSample = 150
	#,procMethod = "FUSION"
	,procMethod = "lidR"
)
