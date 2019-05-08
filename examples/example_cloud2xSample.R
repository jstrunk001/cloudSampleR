#install packages (only run 1 time, or when package is updated)
#installing dependencies takes a little while for some reason
if(F){
	#this approach requires that you first install rtools.exe
	devtools::install_github("jstrunk001/CloudSampleR")

	#alternatively, you can download the github repository, zip it, and then use R to load package from zip file
}

#load required packages
	library(rgdal)
	library(rgeos)
	library(cloudSampleR)

#A read in tiles, prepare extents of data
	poly1=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las","las_polys")
	poly1a=gBuffer(poly1,width = 5)
	poly2=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las","las_polys")
	poly2a=gBuffer(poly2,width = 5)

#B clip plots and send to output folders
	cloud2xSample(
		pathClipData = "c:/fusion/clipdata.exe"
		,pathOutA = "d:/temp/hood_canal_test/clip3in/"
		,pathOutB = "d:/temp/hood_canal_test/clip6in/"
		,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
		,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
		,extentPolyA = poly1a
		,extentPolyB = poly2a
		,nCore = 3
		,nSample = 500
		#,procMethod = "FUSION"
		,procMethod = "lidR"
	)

#C compute cloudmetrics for samples and merge ?

