#' Create directory signature
#'
#' @param path directory path to generate signatures
#' @param file.info,md5sum,recursive,... passed to \code{\link{fileSnapshot}}
#' @export
dir_signature = function(path = '.', file.info = FALSE, md5sum = TRUE, recursive = TRUE, ...){
  path = normalizePath(path)
  info = utils::fileSnapshot(path = path, file.info = file.info, md5sum = md5sum, recursive = recursive, ...)
  digest = digest::digest(info$info)
  digest
}
