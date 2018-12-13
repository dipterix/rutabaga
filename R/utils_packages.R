#' @title Check If Packages Are Installed, Returns Missing Packages
#' @param pkgs vector of packages to install
#' @param libs paths of libraries
#' @param auto_install automatically install packages if missing
#' @param ... other params for \code{install.packages}
#' @importFrom utils install.packages
#' @export
check_installed_packages <- function(pkgs, libs = base::.libPaths(), auto_install = FALSE, ...){
  installed = sapply(pkgs, package_installed)
  pkgs = pkgs[!installed]
  if(auto_install && length(pkgs)){
    cat2('Installing packages:', paste0('[', pkgs, ']', collapse = ', '), level = 'INFO')
    do.call('install.packages', c(
      list(pkgs = pkgs),
      list(...)
    ))
  }
  return(pkgs)
}
