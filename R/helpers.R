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


