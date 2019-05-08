#  if projections for polyA and polyB differ, then either make sure projections are defined in polyA,polyB or set proj4 strings below

## ?? update to scan las folder ???

rTenthAcreFt = sqrt(43560/10/pi)
rAcreFt = sqrt(43560/pi)
r5AcresFt = sqrt(5*43560/pi)

rTenthAcreM = sqrt(43560/10/pi) * 0.3048
rAcreM = sqrt(43560/pi) * 0.3048
r5AcresM = sqrt(5*43560/pi) * 0.3048

#from https://www.rdocumentation.org/packages/gbutils/versions/0.2-1/source
isNA <- function(x){
  is.atomic(x) && length(x) == 1 && is.na(x)
}

.clean_path=function(
  path
  ,backslash=T
  ,force_endslash=F
){

  path_in=unlist(path)

  #fix paths
  path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\\\\\$","",gsub("//$","",path_in))))))

  #if slashcap
  if(force_endslash) path_ok=paste(gsub("\\\\$","",path_ok),"\\",sep="")

  #use forward slash
  if(!backslash) path_ok= gsub("\\\\","/",path_ok)

  #return data
  return (path_ok)

}
.backslash=function(path) .clean_path(path,backslash=T)

# if(class(try(lidR::catalog_laxindex(),silent=T)) == "try-error") catalog_laxindex = function(ctg){
#   stopifnot(is(ctg, "LAScatalog"))
#
#   opt_chunk_size(ctg)   <- 0
#   opt_chunk_buffer(ctg) <- 0
#   opt_wall_to_wall(ctg) <- FALSE
#
#   create_lax_file = function(cluster)
#   {
#     rlas::writelax(cluster@files)
#     return(0)
#   }
#
#   options <- list(need_buffer = FALSE, drop_null = FALSE)
#
#   catalog_apply(ctg, create_lax_file,.options = options())
#   return(invisible())
# }

.clipFusion=function(
  idxyd=NA #id,x,y,diameter
  ,dir_las = NA
  ,dir_dtm = NA
  ,dir_clipdata="c:\\fusion\\clipdata.exe"
  ,dir_out = NA
  ,out_f = c(".las",".laz")
  ,clipdata_switches="/height /shape:1"
  ,n_core=6
  ,temp = "c:\\temp\\clipdata"
  ,run=T

){

  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")
  require(parallel)
  if(is.na(dir_las)) stop("dir_las not provided")
  if(is.na(dir_out)){
    warning("dir_out not provided, using temp:",temp)
    dir_out=temp
  }
  if(is.na(dir_dtm)){
    warning("dir_dtm not provided, points will not be elevation adjusted")
  }
  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  temp = .backslash(paste(temp,"/",proc_time,"/",sep=""))
  dir.create(temp,recursive=T)

  #prepare coordinates
  cds_df= data.frame(
    xmin = idxyd[,2] - idxyd[,4]/2
    ,ymin = idxyd[,3] - idxyd[,4]/2
    ,xmax = idxyd[,2] + idxyd[,4]/2
    ,ymax = idxyd[,3] + idxyd[,4]/2
  )
  #output files
  lasz_out = file.path(dir_out,paste(idxyd[,1],out_f[1],sep=""))

  #prepare dtm list
  if(!is.na(dir_dtm)){
    dtm_list=file.path(temp,"dtm_list.txt")
    dtm_files = list.files(dir_dtm, full.names=T,pattern="[.]dtm$")
    writeLines(dtm_files,dtm_list)
    dtm_switch = paste("/dtm:",dtm_list,sep="",collapse="")
  }
  #prepare las list
  lasz_list=file.path(temp,"lasz_list.txt")
  lasz_files = unlist(c(list.files(dir_las, full.names=T,pattern="[.]las$")
                        ,list.files(dir_las, full.names=T,pattern="[.]laz$")))
  writeLines(lasz_files,lasz_list)

  #prepare commands
  cmd_df = data.frame(dir_cd=dir_clipdata)
  if(!is.na(clipdata_switches[1]))if(nchar(clipdata_switches[1]) > 0) cmd_df = data.frame(cmd_df,sw=clipdata_switches[1])
  if(!is.na(dir_dtm)) cmd_df = data.frame(cmd_df,dtm_sw=dtm_switch)
  cmds_df = data.frame(cmd_df,lasz_list,lasz_out,cds_df)
  cmds = apply(cmds_df,1,paste,collapse=" ")

  #write commands to batch file
  cmds_out = file.path(temp,"fusion_commands.bat")
  writeLines(cmds, cmds_out)

  #run commands
  if(run){
    require(parallel)
    clus=makeCluster(n_core)
    res=parLapply(clus,cmds,shell);gc()
    gc();stopCluster(clus);gc()
    return(list(res=res,cmds=cmds))

  }else{

    return(list(res=NA,cmds=cmds))

  }

}


