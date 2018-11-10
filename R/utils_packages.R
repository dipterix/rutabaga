#' Check if packages are installed, returns missing packages
#' @param pkgs vector of packages to install
#' @param libs paths of libraries
#' @param auto_install automatically install packages if missing
#' @param ... other params for \code{install.packages}
#' @importFrom utils install.packages
#' @export
check_installed_packages <- function(pkgs, libs = base::.libPaths(), auto_install = FALSE, ...){
  instp = utils::installed.packages(lib.loc = libs)[,1]
  pkgs = pkgs[!pkgs %in% instp]
  if(auto_install && length(pkgs)){
    cat2('Installing packages:', paste0('[', pkgs, ']', collapse = ', '), level = 'INFO')
    do.call('install.packages', c(
      list(pkgs = pkgs),
      list(...)
    ))
  }
  return(pkgs)
}
