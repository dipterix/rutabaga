# # # calcluation helpers

#' Function to return mean and standard deviation
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' @rdname mean-sd
#' @export
m_se <- function(x) c('mean'=mean(x), 'se'=se(x))

#' @rdname mean-sd
#' @export
mat_m_se <- function(m, DIM=2) apply(m, DIM, m_se)

se <- function(x, na.rm=FALSE) sd(x, na.rm=na.rm) / sqrt(sum(not_NA(x)))


# We're getting some extreme values (way beyond 6SD) so let's trim them out

#' Trim data by standard error
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @export
trim <- function(x, cutoff=6) {
  xmed <- median(x)
  z <- abs(x - xmed) / mad(x, center=xmed)
  x[z <= cutoff]
}

#' Mean of data after trimmed
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' @export
trimmed.mean <- function(x, cutoff=4) {
  mean(trim(x, cutoff))
}

#' Sd of data after trimmed
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' @export
trimmed.mse <- function(x, cutoff=4) {
  m_se(trim(x,cutoff))
}


#mean +/- sd
m_sd <- function(x, na.rm=FALSE) c('mean'=mean(x,na.rm=na.rm), 'sd'=sd(x,na.rm=na.rm))

not_null <- function(x) !is.null(x)


# this is primarily used for clauses with side effects (plotting etc)
do_if <- function(boolean_expression, if_clause, else_clause=NULL) {
  if(all(boolean_expression))
    return (if_clause)

  return (else_clause)
}

# easy way to get +/- from a long vector
pm <- function(x,d)c(x-d,x+d)
plus_minus <- function(x,d)c(x-d,x+d)


# make it easier to say not is.na in a pipe'd context
not_NA = function(x) !is.na(x)

# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

# 0-1 scale the data so we can manage the plot ranges easily
scl01 <- function(x) (x-min(x)) / diff(range(x))

#enforce sum to 1, ignoring NA in the sum, but keeping them in the output
pscl <- function(x) x /sum(x, na.rm=TRUE)




