#' @importFrom grDevices col2rgb rgb
#' @importFrom graphics arrows axis barplot lines plot points polygon
#' @importFrom stats lm mad median runif sd t.test
#' @importFrom methods is
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics par
#' @importFrom stats aggregate
#' @importFrom magrittr %>%
#' @import dipsaus
NULL

#' @export
dipsaus::collapse
NULL

#' @export
dipsaus::cat2
NULL

#' @export
dipsaus::deparse_svec
NULL

#' @export
dipsaus::parse_svec
NULL

#' @export
dipsaus::time_delta
NULL

#' @export
dipsaus::to_ram_size
NULL

#' @export
dipsaus::drop_nulls
NULL

#' @export
dipsaus::col2hexStr
NULL

#' @export
dipsaus::qs_queue
NULL

#' @export
dipsaus::redis_queue
NULL

#' @export
dipsaus::eval_dirty
NULL

#' @export
dipsaus::match_calls
NULL

#' @export
dipsaus::package_installed
NULL

#' @export
dipsaus::check_installed_packages
NULL

#' @export
dipsaus::actionButtonStyled
NULL

#' @export
dipsaus::updateActionButtonStyled
NULL

#' @export
dipsaus::updateCompoundInput2
NULL

#' @export
dipsaus::progress2
NULL

#' @export
dipsaus::compoundInput2
NULL


package_installed <- function(pkgs, all = FALSE){
  re <- sapply(pkgs, function(p){
    system.file('', package = p) != ''
  })
  if(all){
    re <- all(re)
  }
  re
}
