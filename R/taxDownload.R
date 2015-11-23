#' Download SILVA Taxonomy Mapping File
#'
#' Download the SILVA taxonomy mapping file from \href{http://www.arb-silva.de/}{SILVA}, which maps each entry in the SILVA SSU Ref dataset to a taxonomic path containing the taxonomy rank designations from kingdom to species.
#' @usage taxDownload(file = "taxmap_slv_ssu_123.txt", version = 123, destdir = ".")
#'
#' @param file a taxonomy mapping file to be downloaded, default to \code{taxmap_slv_ssu_123.txt}.
#' @param version a number or character to specify the version of the downloaded file, default to version \code{123}.
#' @param destdir a directory path where the downloaded file is saved, default to the current directory.
#' @details The tax mapping file should be consistent (same version) with the downloaded reference dataset by \code{\link{seqDownload}}. And also, you should make sure the file is listed in SILVA Archive.
#' @return The absolute path of the downloaded file.
#' @export
#' @seealso \code{\link{taxExtract}}
#' @examples
#' #download the taxonomy mapping file(v123) of SILVA SSU Ref dataset to the current directory
#' taxDownload()

taxDownload = function(file="taxmap_slv_ssu_123.txt",version=123,
                       destdir="."){
  release = paste("release",version,sep="_")
  t.file = paste("http://www.arb-silva.de/fileadmin/silva_databases",
                 release,"Exports/taxonomy",file,sep="/")
  if(grepl("/$",destdir)){
    destdir = gsub("/$","",destdir)
  }
  dest.file = file.path(destdir,file)
  download.file(t.file,destfile = dest.file,method="curl")
  destFile.path = getAbsolutePath(dest.file)
  return(destFile.path)
}
