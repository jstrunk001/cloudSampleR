#'@name cloudSampleR
NULL
#'@title
#'  Clip random matched locations from multiple point cloud datasets (lidar or dap or ifsar etc.)
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2019 May 07 First Debug Version \cr
#'1.0 \tab 2019 May 09 Add Documentation for arguments \cr
#'}
#'
#'@author
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param pathClipData where to find FUSION clipdata function - only used with procMethod = "FUSION"
#'@param pathCloudmetrics (NON FUNCTIONING)
#'@param pathOutA directory to place clipped plots from project A
#'@param pathOutB directory to place clipped plots from project B
#'@param pathLasA directory containing project A las or laz files
#'@param pathLasB directory containing project B las or laz files
#'@param pathDTMA (Semi-functional with procMethod = "FUSION", NOT TESTED YET) DTM to use with project A to normalize point heights
#'@param pathDTMB (Semi-functional with procMethod = "FUSION", NOT TESTED YET) DTM to use with project B to normalize point heights
#'@param patternA grep pattern to match files on for project A
#'@param patternB grep pattern to match files on for project B
#'@param extentPolyA polygon of project A extent
#'@param extentPolyB polygon of project B extent
#'@param sampleShpA (Optional) use an existing shapefile with plot locations
#'@param proj4A (Optional) Provide proj4 strings if projects A and B are in different projections but do not contain projection information (e.g. missing .prj files)
#'@param proj4B (Optional) Provide proj4 strings if projects A and B are in different projections but do not contain projection information (e.g. missing .prj files)
#'@param extentSample (Optional) If the overlap between projects A and B is known, you can provide it here
#'@param nSample number of plots to sample
#'@param radii a vector of plot radii to use in clipping plots
#'@param sampleShape shape of clipped plots ("circle" or "square")
#'@param sampleType type of spatial distribution of of sample points ("regular" = square grid,"random","hexagonal" = hexagonal grid)
#'@param doSteps (NON FUNCTIONING) Select which steps should be completed in this run
#'@param switchesClipdata optional switches to FUSION's cloudmetrics.exe - see fusion documentation for options
#'@param switchesCloudmetrics (NON FUNCTIONING)
#'@param procMethod Should plots be clipped using R or FUSION?
#'@param nCore How many cores should be used for clipping (the number and speed of hdds is as important as the number of machine threads)
#'@param temp (use with procMethod = "FUSION")Where should batch files be placed
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples

#load required packages
#'			library(rgdal)
#'			library(rgeos)
#'			library(cloudSampleR)
#'
#'			#A read in tiles, prepare extents of data
#'			poly1=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las","las_polys")
#'			poly1a=gBuffer(poly1,width = 5)
#'			poly2=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las","las_polys")
#'			poly2a=gBuffer(poly2,width = 5)
#'
#'			#B clip plots and send to output folders
#'			cloud2xSample(
#'				pathClipData = "c:/fusion/clipdata.exe"
#'				,pathOutA = "d:/temp/hood_canal_test/clip3in/"
#'				,pathOutB = "d:/temp/hood_canal_test/clip6in/"
#'				,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
#'				,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
#'				,extentPolyA = poly1a
#'				,extentPolyB = poly2a
#'				,nCore = 3
#'				,nSample = 150
#'				#,procMethod = "FUSION"
#'				,procMethod = "lidR"
#'				,radii = list( feet = c(FtTenthAc = 37.2, FtAcre = 117.8, Ft5Acres = 263.3 ))
#'			)
#'
#'
#'@import plyr
#'@import sp
#'@import rgdal
#'@import raster
#'@import rgeos
#'
#'@seealso \code{\link{readLAS}}\cr \code{\link{spsample}}\cr

#Desired upgrades to this function:
#
#'@rdname cloudSampleR
#'@export
#'
cloud2xSample=function(

  pathClipData = "c:/fusion/clipdata.exe"
  #,pathCloudmetrics = "c:/fusion/cloudmetrics.exe"
  ,pathOutA = ""
  ,pathOutB = ""
  ,pathLasA = ""
  ,pathLasB = NA
  ,patternA = ".*[.]la[s|z]{1}$" #matches ".las" or ".laz"
  ,patternB = ".*[.]la[s|z]{1}$"
  ,pathDTMA = NA #directory or raster file
  ,pathDTMB = NA #directory or raster file
  ,patternDTMA = ".*[.]asc$"
  ,patternDTMB = ".*[.]asc$"
  ,extentPolyA = NA # polygon with extent of project A
  ,extentPolyB = NA # (optional) polygon with extent of project B which will be interesected with A
  ,sampleShpA = NA # (optional) provide shapefile of target sample locations - assumed to be in projection of A or extent
  ,proj4A = NA # (optional) see ?? for proj4 strings: https://www.spatialreference.org/
  ,proj4B = NA # (optional) see ?? for proj4 strings: https://www.spatialreference.org/
  ,extentSample = c(llx = NA, lly = NA, ulx = NA, uly = NA) # (optional) alternative to extentPolyA and extentPolyB
  ,nSample = 500
  ,radii = list( feet = c(FtTenthAc = 37.2, FtAcre = 117.8, Ft5Acres = 263.3 ) , meters = c(MtenthAc = 11.3, MAcre = 35.9, M5Acres = 80.3   ) )[[1]]
  ,sampleShape = c("circle","square")[1]
  ,sampleType = c("regular","random","hexagonal")
  ,doSteps = c("sample","clip","metrics")
  ,switchesClipdata = "" #optional switches to FUSION's polyclip.exe
  ,switchesCloudmetrics = ""  #optional switches to FUSION's cloudmetrics.exe
  ,procMethod = c("lidR","FUSION")
  ,nCore = 2
  ,temp = "c:\\temp\\clipdata"


){

  requireNamespace("sp")
  requireNamespace("rgdal")
  requireNamespace("raster")
  requireNamespace("rgeos")


  #deal with format of radii_in
  if("feet" %in% names(radii))  radii_in  = try(sort(unlist(radii[[1]]), decreasing = T))
	else radii_in  = try(sort(unlist(radii), decreasing = T))

  #figure out what is present
  hasOutA = !is.na(pathOutA)
  hasOutB = !is.na(pathOutB)
  hasPathA = !is.na(pathLasA)
  hasPathB = !is.na(pathLasB)
  hasPolyA = !isNA(extentPolyA)
  hasPolyB = !isNA(extentPolyB)
  hasProj4A = !is.na(proj4A )
  hasProj4B = !is.na(proj4B )
  hasPathDTMA = !is.na(pathDTMA)
  hasPathDTMB = !is.na(pathDTMB)
  hasSample = !is.na(sampleShpA)
  hasExt = !isNA(extentSample[1])
  hasClipSw = nchar(switchesClipdata) > 0
  hasCMSw = nchar(switchesCloudmetrics) > 0
  hasShape = sampleShape[1] %in% c("circle","square","round")
  hasType = sampleType[1] %in% c("regular","random","hexagonal")

  #auto assign variables
  date_in = format(Sys.time(), "%Y%b%d%H%M%S")

  #prepare spatial data for extents
  loadPoly=function(x,proj4){
    requireNamespace("rgdal")
    if(!inherits(x,"Spatial"))if(!is.na(x)) x_in = rgdal::readOGR(x)
    if(!"x_in" %in% ls()) x_in = x
    if(inherits(x_in,"Spatial")) if(!is.na(proj4)) sp::proj4string(x_in) = proj4
    return(x_in)
  }
  loadExtent=function(x){
    requireNamespace("raster")
    x_in = as(extent(x),"SpatialPolygons")
    return(x_in)
  }
  #load spatial data

  if(hasPolyA) extentPolyA_in = loadPoly(extentPolyA , proj4A)
  if(hasPolyB) extentPolyB_in = loadPoly(extentPolyB , proj4B)
  if(hasExt) extentPoly_in = loadExtent(extentSample)

  #chart path forward
  polyAOnly = (hasPathA & !hasPathB & !hasExt)
  polyAB = (hasPathA & hasPathB )
  polyAExt = (hasPathA & hasExt)
  extOnly = hasExt & !hasPolyA & !hasPolyB

  #check projections
  if(hasPolyA) hasCRSPolyA = !is.na(sp::proj4string(extentPolyA_in))
  if(hasPolyB) hasCRSPolyB = !is.na(sp::proj4string(extentPolyB_in))
  if(polyAB){

    #assign proj4
    if(!hasCRSPolyA & hasProj4A) sp::proj4string(extentPolyA_in) = proj4A
    if(!hasCRSPolyB & hasProj4B) sp::proj4string(extentPolyB_in) = proj4B
    bad_proj = !raster::compareCRS(extentPolyA_in, extentPolyB_in)

    #check for proj4 again
    hasCRSPolyA = !is.na(sp::proj4string(extentPolyA_in))
    hasCRSPolyB = !is.na(sp::proj4string(extentPolyB_in))

    #transform if needed
    if(!bad_proj) extentPolyB_proj4A = extentPolyB_in
    if(bad_proj & hasCRSPolyA & hasCRSPolyB) extentPolyB_proj4A = spTransform(extentPolyB_in, sp::proj4string(extentPolyA_in))
  }

  #catch errors
  errExtent = !hasExt & !hasPolyA & !hasPolyB & !hasSample
  errShape = !hasShape & !hasSample
  errType = !hasType & !hasSample
  errExtPolyB = hasExt & hasPolyB & !hasPolyA
  errProj4 = bad_proj & (!hasCRSPolyA | !hasCRSPolyB)
  errPath = !hasPolyA | (polyAB & !hasPathB) | (polyAExt & !hasPathB)
  warnProj4 = (polyAOnly & !hasCRSPolyA) | (polyAB & !hasCRSPolyA)

  #throw errors based on arguments
  if(errExtent) stop("must at minimum provide argument 'PolyA' or 'extentSample' ")
  if(errShape) stop("shape must be 'circle' or 'square' or you must provide 'sampleShpA'")
  if(errType) stop("'sampleType' must be 'regular','random','hexagonal' or you must provide 'sampleShpA'")
  if(errExtPolyB) stop("oops: argument 'extentSample' can be used with 'extentPolyA', but 'extentSample' cannot be used with argument 'extentPolyB' ")
  if(errProj4) stop("Couldn't confirm that 'extPolyA' and 'extPolyB' had the same projections - define both polygon projections (e.g. arcmap) or provide proj4 strings")
  if(errPath) stop("Either 'pathLasA' or 'pathLasB' is missing: 'pathLasB' is optional but must be provided if two extents are provided")

  #throw warnings
  if(!hasOutA){
    pathOutA = file.path(pathLasA,"clipLas",date_in)
    dir.create(pathOutA , recursive = T)
    warning("'pathOutA' argument not provided, using '",pathOutA,"'")
  }
  if(!hasOutB & hasPathB){
    pathOutB = file.path(pathLasB,"clipLas",date_in)
    dir.create(pathOutB , recursive = T)
    warning("'pathOuBt' argument not provided, using '",pathOutB,"'")
  }
  if(warnProj4) warning("'extentPolyA' does not have a projection and 'proj4A' string not provided")

  #create output folders
  if(hasOutA ){
    if(!is.null(names(radii_in))) pathsOutA_in = paste(pathOutA,paste("clipElev",names(radii_in),sep="_"),sep="")
    if(is.null(names(radii_in))) pathsOutA_in = paste(pathOutA,paste("clipElev_Rad",radii_in,"Elev",sep=""),sep="")
    sapply(pathsOutA_in , function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)

    if(hasPathDTMA & procMethod=="lidR"){
      if(!is.null(names(radii_in))) pathsOutAHt_in = paste(pathOutA,paste("clipHt",names(radii_in),sep="_"),sep="")
      if(is.null(names(radii_in))) pathsOutAHt_in = paste(pathOutA,paste("clipHt_Rad",radii_in,"Ht",sep=""),sep="")
      sapply(pathsOutAHt_in , function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)
    }
  }
  if(hasOutB ){
    if(!is.null(names(radii_in))) pathsOutB_in = paste(pathOutB,paste("clipElev",names(radii_in),sep="_"),sep="")
    if(is.null(names(radii_in))) pathsOutB_in = paste(pathOutB,paste("clipElev_Rad",radii_in,sep=""),sep="")
    sapply(pathsOutB_in ,function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)

    if(hasPathDTMB & procMethod=="lidR"){
      if(!is.null(names(radii_in))) pathsOutBHt_in = paste(pathOutB,paste("clipHt",names(radii_in),sep="_"),sep="")
      if(is.null(names(radii_in))) pathsOutBHt_in = paste(pathOutB,paste("clipHt_Rad",radii_in,"Ht",sep=""),sep="")
      sapply(pathsOutBHt_in , function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)
    }
  }

  #intersect extents
  if(polyAExt) extInA = gIntersection(extentPolyA_in, extentPoly_in)
  if(polyAB) extInA = gIntersection(extentPolyA_in, extentPolyB_in)
  if(polyAOnly) extInA = extentPolyA_in
  if(extOnly) extInA = extentPoly_in

  #try and load existing sample sampleShpA
  sampleShpA_in = loadPoly(sampleShpA,proj4A)

  #build sample if necessary
  if(!inherits(sampleShpA_in,"Spatial")){

    sInA = spsample( extInA , n = nSample , type = sampleType[1] )
    sInDFA = SpatialPointsDataFrame(sInA, data.frame(id=1:nrow(sInA@coords)))
    sInBuffA = lapply(radii_in, .fn_buff, sInDFA , sampleShape)

  }else{

    #use existing sample
    sInA = sampleShpA_in

    #get correct data type
    if(class(sInA) == "SpatialPoints"){
      sInDFA = SpatialPointsDataFrame(sInA, data.frame(id=1:nrow(sInA@coords)))
    }else{
      sInDFA = sInA
    }

    #buffer as needed
    if(class(sInDFA) == "SpatialPointsDataFrame"){
      sInBuffA = lapply(radii_in, .fn_buff, sInDFA , sampleShape)
    }else{
      sInBuffA = sInDFA
    }

  }


  #reproject spatial data for project B or extent - if necessary
  if(polyAB){
    if(bad_proj & hasCRSPolyB){
      sInB = spTransform(sInA , sp::proj4string(extentPolyB_in))
      extInB = spTransform(extInA , sp::proj4string(extentPolyB_in))
      sInDFB = spTransform(sInBuffA , sp::proj4string(extentPolyB_in))
      sInBuffB = sapply(sInBuffA , spTransform, sp::proj4string(extentPolyB_in))

    }else{
      sInB = sInA
      extInB = extInA
      sInDFB = sInDFA
      sInBuffB = sInBuffA

    }
  }
  if(polyAExt){
    if(bad_proj & hasProj4B){
      sInB = spTransform(sInA , proj4B)
      extInB = spTransform(extInA , proj4B)
      sInDFB = spTransform(sInBuffA , proj4B)
      sInBuffB = sapply(sInBuffA , spTransform, proj4B)

    }else{
      sInB = sInA
      extInB = extInA
      sInDFB = sInDFA
      sInBuffB = sInBuffA

    }
  }

  #write shape files
  if("sInDFA" %in% ls()){
    #prep
    pathSampleADir = file.path(pathOutA,"shapefiles")
    if(!dir.exists(pathSampleADir)) errPathOutA = try(dir.create(pathSampleADir, recursive = T))
    #write
    writeTestSInDFA = try(writeOGR(sInDFA ,pathSampleADir, paste(date_in,"_SamplePoints",sep=""), driver="ESRI Shapefile"),silent=T)
    writeTestExtA = try(writeOGR(extInA ,pathSampleADir, paste(date_in,"_SampleExtentA",sep=""), driver="ESRI Shapefile"),silent=T)
    for(i in 1:length(sInBuffA)){
      write_test=try(writeOGR(sInBuffA[[i]] ,pathSampleADir, paste(date_in,"_SamplePointPolys",names(sInBuffA)[[i]],sep=""), driver="ESRI Shapefile"),silent=T)
    }

  }

  if("sInDFB" %in% ls() ){
    #prep
    pathSampleBDir = file.path(pathOutB,"shapefiles")
    if(!dir.exists(pathSampleBDir)) errPathOutB = try(dir.create(pathSampleBDir, recursive = T))
    #write
    writeTestSInDFB = try(writeOGR(sInDFB ,pathSampleBDir, paste(date_in,"_SamplePoints",sep=""), driver="ESRI Shapefile"),silent=T)
    writeTestExtB = try(writeOGR(extInB ,pathSampleBDir, paste(date_in,"_SampleExtentB",sep=""), driver="ESRI Shapefile"),silent=T)
    for(i in 1:length(sInBuffB)){
      write_test=try(writeOGR(sInBuffB[[i]] ,pathSampleBDir, paste(date_in,"_SamplePointPolys",names(sInBuffB)[[i]],sep=""), driver="ESRI Shapefile"),silent=T)
    }

  }

  #clip plots
  if(procMethod[1] == "lidR"){

    #First clip plots

    warning("It is recommended to use 'lasindex -i *.las' from within 'pathLasA' las directory before using this function")
    #build lasR catalogs
    requireNamespace("lidR")
    if(hasPathA){

        ctgA_clip = NA
        for(i in 1:length(radii_in) ){

          print(c("A",i, as.character(Sys.time())))

          ctgA_clip =
            .clip_las(
              pathLAS = gsub("//","/",c(pathLasA,paste(pathsOutA_in[-length(pathsOutA_in)],"/",sep=""))[i])
              ,pathDTM = pathDTMA
              ,patternDTM = patternDTMA
              ,pathOut = pathsOutA_in[i]
              ,pathOutHt = pathsOutAHt_in[i]
              ,sampleSPDF = sInA
              ,shape = sampleShape
              ,radius = radii_in[i]
              ,nCore = nCore
              ,temp = temp
            )

      }
    }
    if(hasPathB){


        for(i in 1:length(radii_in) ){

          print(c("B",i,as.character(Sys.time())))

          ctgB_clip =
            .clip_las(
              pathLAS = gsub("//","/",c(pathLasB,paste(pathsOutB_in[-length(pathsOutB_in)],"/",sep="")))[i]
              ,pathDTM = pathDTMB
              ,patternDTM = patternDTMB
              ,pathOut = pathsOutB_in[i]
              ,pathOutHt = pathsOutBHt_in[i]
              ,sampleSPDF = sInB
              ,shape = sampleShape
              ,radius = radii_in[i]
              ,nCore = nCore
              ,temp = temp
            )

        }

      }


    #reproject coordinates if needed


    #compute plot metrics


    #combine plot metrics


  }

  if(procMethod == "FUSION"){

    if(sampleShape == "circle"){
      if(hasPathDTMA) swClipdata = "/height /shape:1"
      if(!hasPathDTMA) swClipdata = "/shape:1"
    }
    if(sampleShape == "square"){
      if(hasPathDTMA) swClipdata = "/height /shape:0"
      if(!hasPathDTMA) swClipdata = "/shape:0"
    }

    if(hasPathA){
      gc()
    #clip first radius
      .clipFusion(
        idxyd=data.frame(id=paste("clip",1:nrow(sInA@coords),sep="_"),coordinates((sInA)),2*radii_in[1])
        ,dir_las = pathLasA
        ,dir_dtm = pathDTMA
        ,dir_clipdata=pathClipData
        ,dir_out = pathsOutA_in[1]
        ,out_f = ".laz"
        ,clipdata_switches=swClipdata
        ,n_core = nCore
        ,temp = temp
        ,run=T
      )
      closeAllConnections()
      gc()

      #filter off height switch for sub-clips -> already height if desired
      swClipdata = gsub("^[ ]","",gsub("/height","",swClipdata))

      if(length(radii_in[1]) > 0){
        for(j in 2:length(radii_in) ){
          .clipFusion(
            idxyd=data.frame(id=paste("clip",1:nrow(sInA@coords),sep="_"),coordinates((sInA)),2*radii_in[j])
            ,dir_las = pathsOutA_in[1] #subsample from original clips
            ,dir_dtm = NA
            ,dir_clipdata=pathClipData
            ,dir_out = pathsOutA_in[j]
            ,out_f = ".laz"
            ,clipdata_switches=swClipdata
            ,n_core = nCore
            ,temp = temp
            ,run=T
          )
          closeAllConnections()
          gc()
        }
      }

    }
    if(hasPathB){
      gc()
      #clip first radius
      .clipFusion(
        idxyd=data.frame(id=1:nrow(sInB@coords),coordinates((sInB)),2*radii_in[1])
        ,dir_las = pathLasB
        ,dir_dtm = pathDTMB
        ,dir_clipdata=pathClipData
        ,dir_out = pathsOutB_in[1]
        ,out_f = ".laz"
        ,clipdata_switches=swClipdata
        ,n_core = nCore
        ,temp = temp
        ,run=T
      )
      closeAllConnections()
      gc()

      #filter off height switch for sub-clips -> already height if desired
      swClipdata = gsub("^[ ]","",gsub("/height","",swClipdata))

      if(length(radii_in[1]) > 0){
        for(j in 2:length(radii_in) ){
          .clipFusion(
            idxyd=data.frame(id=paste("clip",1:nrow(sInB@coords),sep="_"),coordinates((sInB)),2*radii_in[j])
            ,dir_las = pathsOutB_in[1] #subsample from original clips
            ,dir_dtm = NA
            ,dir_clipdata=pathClipData
            ,dir_out = pathsOutB_in[j]
            ,out_f = ".laz"
            ,clipdata_switches=swClipdata
            ,n_core = nCore
            ,temp = temp
            ,run=T
          )
          closeAllConnections()
          gc()
        }
      }

    }

  }


}

#clip las and
.clip_las = function(ctg=NA,pathLAS=NA,pathDTM,patternDTM,pathOut,pathOutHt,sampleSPDF,shape,radius,nCore,temp){

  closeAllConnections()
  ctg_in <- lidR::catalog(pathLAS)

  lidR::opt_cores(ctg_in) <- nCore
  lidR::opt_output_files(ctg_in) <- paste0(pathOut, "/clip_{ID}")
  lidR::opt_laz_compression(ctg_in) <- F
  if(shape == "circle") ctg_clip = lidR::lasclipCircle(ctg_in,sampleSPDF@coords[,1],sampleSPDF@coords[,2],radius)
  if(shape == "square") ctg_clip = lidR::lasclipRectangle(ctg_in
                                                          , sampleSPDF@coords[,1] - radius
                                                          , sampleSPDF@coords[,2] - radius
                                                          , sampleSPDF@coords[,1] + radius
                                                          , sampleSPDF@coords[,2] + radius)

  if(!is.na(pathDTM)){

    dtm_in = .fn_dtm(pathDTM,patternDTM,file.path(temp,"DTMA.vrt"))

    lidR::opt_cores(ctg_clip) <- nCore
    lidR::opt_output_files(ctg_clip) <- paste0(pathOutHt, "/clip_{ID}")
    lidR::opt_laz_compression(ctg_clip) <- F
    ctgHt_clip = try(lidR::lasnormalize(ctg_clip, dtm_in, na.rm=T),silent=T)

  }
  try(closeAllConnections())
  return(ctg_clip)

}

#build vrt from list of dtms
.fn_dtm=function(x,pattern,outNm){

  #check if a single raster or directory of rasters is provided
  is_file = file.exists(x) && !dir.exists(x)
  #link to raster
  if(is_file) r_in = try(raster::raster(x), silent = T)
  #build vrt from multiple rasters
  if(!is_file){
    if(!dir.exists(dirname(outNm))) dir.create(dirname(outNm),recursive = T)
    txtFile = paste(outNm,".inputfiles.txt",sep="")
    writeLines(list.files(x,pattern=pattern,full.names=T),txtFile)
    r_in = gdalUtils::gdalbuildvrt(input_file_list = txtFile,output.vrt = outNm)
  }
  return(r_in)

}

#buffer sample
.fn_buff = function(r, x, shape){
  if(shape == "circle" | shape == "round") res = rgeos::gBuffer( x , width= r ,capStyle="round" , byid=T)
  if(shape == "square") res = rgeos::gBuffer(x,width=r,capStyle="square" , byid=T)
  SpatialPolygonsDataFrame(res, data.frame(id=1:length(res@polygons)))
}


