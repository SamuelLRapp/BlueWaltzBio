library(memoise)
getCachePath <- function(cache_name){
  rappdirs::user_cache_dir(cache_name)
}
#createCache is actually a bad name, it should be more like "getCacheObject" or something
createCache <- function(cache_name){
  cache_path <- getCachePath(cache_name)
  cachem::cache_disk(dir = cache_path)
}
setCache <- function(key, value, cache_name){
  cache <- createCache(cache_name)
  cache$set(key, value)
}
setFileCache <- function(file_num, cell_num, content){
  cache_name <- paste("NCBI","files",file_num, sep="/")
  setCache(as.character(cell_num), content, cache_name)
}
getCache <- function(key, cache_name){
  cache <- createCache(cache_name)
  cache$get(key)
}
getFileCache <- function(db="NCBI", file_num, cell_num){
  cache_name <- paste(db,"files",file_num, sep="/")
  cache_path <- getCachePath(cache_name)
  getCache(as.character(cell_num), cache_path)
}
listCacheDir <- function(db="NCBI", subdir="", recur=F, showdir=T){
  cache_name <- paste(db, subdir, sep="/")
  d <- getCachePath(cache_name)
  list.files(d, recursive=recur, include.dirs = showdir)
}
emptyCache <- function(cache_name){
  #unlink will delete all the contents of the cache
  unlink(paste0(getCachePath(cache_name),"/*"),recursive=TRUE,force=TRUE)
}
