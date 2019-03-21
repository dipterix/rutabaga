# # # calcluation helpers

#' @title Function To Return Mean And Standard Error
#'
#' (stable)
#'
#' @rdname mean-se
#'
#' @param x data
#' @export
m_se <- function(x) c('mean'=mean(x), 'se'=se(x))

#' @rdname mean-se
#' @param m matrix data
#' @param DIM margin, 1 for row, 2 for column. See \link{apply}
#' @export
mat_m_se <- function(m, DIM=2) apply(m, DIM, m_se)

se <- function(x, na.rm=FALSE) sd(x, na.rm=na.rm) / sqrt(sum(not_NA(x)))


# We're getting some extreme values (way beyond 6SD) so let's trim them out

#' @title Trim Data By Standard Error
#'
#' (stable)
#'
#' @param x data to be trimmed
#' @param cutoff default is 6, then x is clipped +-6 times sd
#' @export
trim <- function(x, cutoff=6) {
  xmed <- median(x)
  z <- abs(x - xmed) / mad(x, center=xmed)
  x[z <= cutoff]
}

#' @title Mean Of Data After Trimmed
#'
#' (questioning)
#'
#' @param x,cutoff passed to \link{trim}
#' @export
trimmed.mean <- function(x, cutoff=4) {
  mean(trim(x, cutoff))
}

#' @title Sd Of Data After Trimmed
#'
#' (questioning)
#'
#' @param x,cutoff passed to \link{trim}
#' @export
trimmed.mse <- function(x, cutoff=4) {
  m_se(trim(x,cutoff))
}


#' @title Function To Return Mean And Standard Deviation (Na Ignored)
#'
#' (stable)
#'
#' @rdname mean-sd
#'
#' @param x data
#' @param na.rm remove NAs?
#'
#' @export
m_sd <- function(x, na.rm=FALSE) c('mean'=mean(x,na.rm=na.rm), 'sd'=sd(x,na.rm=na.rm))

#' @title Return True If Not Null
#'
#' (stable)
#'
#' @param x data
#' @export
not_null <- function(x) !is.null(x)

#' @title Return True If Not Na
#'
#' (stable)
#'
#' @param x data
#' @export
not_NA = function(x) !is.na(x)

#' @title Clauses With Side Effects (Plotting Etc)
#'
#' (questioning)
#'
#' @param boolean_expression expression that returns true or false
#' @param if_clause if true, do if_clause
#' @param else_clause if false, do else_clause
#'
#' @export
do_if <- function(boolean_expression, if_clause, else_clause=NULL) {
  if(all(boolean_expression))
    return (if_clause)

  return (else_clause)
}

#' @title Easy Way To Get +/- From A Long Vector
#' @param x data
#' @param d plus minus value(s)
#' @export
pm <- function(x,d)c(x-d,x+d)

#' @title Easy Way To Get +/- From A Long Vector
#' @param x data
#' @param d plus minus value(s)
#' @export
plus_minus <- function(x,d)c(x-d,x+d)




# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

#' @title 0-1 Scale The Data So We Can Manage The Plot Ranges Easily
#'
#' (stable)
#'
#' @param x data to be rescaled
#' @export
scale_01 <- function(x) {
  m = min(x)
  s = diff(range(x))
  re = (x-m) / s
  attr(re, 'scale:min') = m
  attr(re, 'scale:range') = s
  re
}

#' @title Enforce Sum To 1, Ignoring Na In The Sum, But Keeping Them In The Output
#'
#' (questioning)
#'
#' @param x data
#' @export
pscl <- function(x) x /sum(x, na.rm=TRUE)




#' @title Check if a is within the range of b
#'
#' (stable)
#'
#' @param a element to check (numeric)
#' @param b vector of numbers
#' @rdname is_within
#' @export
is_within <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}

#' @rdname is_within
#' @export
`%within%` <- is_within
