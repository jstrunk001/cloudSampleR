#install packages (only run 1 time)
if(F){


}

#load required packages
	library(rgdal)
	library(rgeos)
	library(cloudSampleR)

#A read in extents of data
	poly1=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las","las_polys")
	poly1a=gBuffer(poly1,width = 5)
	poly2=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las","las_polys")
	poly2a=gBuffer(poly2,width = 5)

#B (optional - one time) save polygon buffers so that we don't have to repeat step A above (A can be slow)
	saveRDS(poly1a,"D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
	saveRDS(poly2a,"D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

#C (optional - skip step A above) load polygon data from rds file - must complete steps A and B at least once
	poly1a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
	poly2a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

#D clip plots and send to output folders
	cloud2xSample(
		pathClipData = "c:/fusion/clipdata.exe"
		,pathOutA = "d:/temp/hood_canal_test/clip3in/"
		,pathOutB = "d:/temp/hood_canal_test/clip6in/"
		,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
		,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
		,extentPolyA = poly1a
		,extentPolyB = poly2a
		,nCore = 6
		,nSample = 150
		,procMethod = "FUSION"
		#,procMethod = "lidR"
	)
