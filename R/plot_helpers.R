# Plot helpers

# This file defines util functions for plotting figures

# ------------------------------------------------------------------------
#
# I'm thinking to put some constants here so that we get a general look/feel on our plots
rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4
# ------------------------------------------------------------------------

#' Create a blank plot with given x and y range
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @examples
#' \dontrun{
#' # create a blank plot with x from 0 to 10 and y from 1 to 5
#' plot_clean(0:10, 1:5, xlab = 'X')
#' }
#' @export
plot_clean = function(
  xlim, ylim, x = 1, y = 1, type = "n", xlab="", ylab="",
  cex.main=rave_cex.main, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab,...
) {

  plot(x, y, type = type, axes = F, ylab = ylab, xlab = xlab,
       xlim = range(xlim), ylim = range(ylim),
       cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab, ...)
}

#' Show a blank plot with messages
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' @export
#' @examples
#' \dontrun{
#' plot_msg("Let's Say Something")
#' }
#' @export
plot_msg <- function(main = 'No Conditions Specified') {
  plot_clean(1, 1, type='n', main=main)
}

#' A neat way to show axis
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @examples
#' \dontrun{
#' # create a blank plot with x from 0 to 10 and y from 1 to 5
#' plot_clean(0:10, 1:5, xlab = 'X')
#' rave_axis(side = 1, at = 1:8)
#' }
#' @export
rave_axis <- function(
  side, at, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis,
  cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), ...) {
  if(length(side) > 1) {
    return (invisible(sapply(
      side, rave_axis, at=at, tcl=tcl, labels=labels,
      cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...)
    ))
  }
  mgp <- mgpy
  if(side %% 2) mgp <- mgpx

  invisible(as.matrix(axis(side, at=at, labels=labels, tcl=tcl, mgp=mgp,
                           cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...)))
}


#' Draw symmetric error bars
#' @examples
#' \dontrun{
#' plot_clean(0:10, -1:5, xlab = 'X')
#' rave_axis(side = 2, at = -1:8)
#' ebars(x = c(2, 4), y = c(0, 3), sem = c(1, 0.5), col = c(2, 3))
#' }
#' @export
ebars = function(x, y=NULL, sem=NULL, length = 0.05, type='n', col='black', pt.col=col, code=2, ...) {
  if(is.null(y)) {
    if(is.matrix(x)) {
      y <- x[,1]
      sem <- x[,2]
    } else {
      y <- x
    }
    x <- seq_along(y)
  }

  if(is.matrix(y)) {
    sem <- y[,2]
    y <- y[,1]
  }

  if(is.null(sem)) {
    sem <- y
    y <- x
    x <- seq_along(y)
  }

  ebars.y(x, y, sem, length, code=code, col=col, ...)
  points(x, y, type=type, col=pt.col, ...)
}


ebars.x = function(x, y, sem, length = 0.05, ...) {
  arrows(x - sem, y, x + sem, y, angle = 90, code = 3, length = length, ...)
}

ebars.y = function(x, y, sem, length = 0.05, up = T, down = T, code = 2, ...) {
  if (up) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y + sem), angle = 90, code = code, length = length, ...)
  }
  if (down) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y - sem), angle = 90, code = code, length = length, ...)
  }
}


do_poly <- function(x, y, col, alpha=50, ...) {
  polygon(c(x,rev(x)), rep(y, each=2), col=getAlphaRGB(col, alpha), border=NA, ...)
}

#' Draw symmetric error bars
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @examples
#' \dontrun{
#' plot_clean(0:10, -1:5, xlab = 'X')
#' rave_axis(side = 2, at = -1:8)
#' ebar_polygon(1:10, (1:10)/2, rnorm(10))
#' }
#' @export
ebar_polygon = function(x, y, sem, alpha=100, col='black', fill=col,
                        stroke=col, border = NA, add_line=TRUE, lwd=1, ...) {
  sem = abs(sem)
  polygon(c(x, rev(x)), c(y + sem, rev(y - sem)), border = border, col = getAlphaRGB(fill, alpha))

  if(add_line) lines(x,y, col=stroke, lwd=lwd, ...)
}

#' Get hex color with transparency
#' @examples
#' \dontrun{
#' getAlphaRGB('red', 0.5)
#' }
#' @export
getAlphaRGB = function(colname, alpha) {
  c = col2rgb(colname)
  rgb(t(c), alpha = alpha, maxColorValue = 255)
}

#' Get elements/slot/attributes from list
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @export
get_list_elements <- function(ll, name, drop_nulls = TRUE, is_attr = FALSE, use_sapply = TRUE, ...) {
  if(use_sapply){
    lapply = sapply
  }
  if(is_attr){
    l = lapply(ll, attr, which = name, ...)
  }else{
    l = lapply(ll, getElement, name)
  }

  # remove NULLs
  if(drop_nulls){
    is_v = !vapply(l, is.null, FUN.VALUE = FALSE)
    idx = which(is_v)
    l = l[idx]
    attr(l, 'original_index') = idx
  }
  if(!length(l)){
    l = NULL
  }
  l
}


abs_cdiff <- function(m) {
  if(!is.matrix(m))
    m = t(as.matrix(m))

  abs(apply(m, 1, diff))
}

#' Get a integer interval that contains x
#' @examples
#' \dontrun{
#' # 0 - 11
#' round_range(0.5:10.5)
#' }
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' @export
round_range <- function(x) {
  c(floor(min(x)), ceiling(max(x)))
}

#' Get data range from a collection of named lists
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' @export
get_data_range <- function(ll, range_var='range') {
  range(
    unlist(lapply(ll, getElement, range_var)),
    na.rm=TRUE
  )
}

#' barplot function that uses all the rave sizes and colors
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @export
rave_barplot <- function(height, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, cex.names=rave_cex.lab, ...) {
  barplot(height, cex.axis=cex.axis, cex.lab=cex.lab, cex.names=cex.names, las=1, ...)
}

#' Return jittered x
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' @export
jitr = function(x, len=length(x), r) {
  if(missing(r)){
    r = (1/3)*min(abs_cdiff(sort(unique(x))))
  }
  x + runif(len, -r, r)
}


#' Same as points, but can be jittered
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' @export
add_points = function(x, y, jitr_x=0, pch=19, ...) {
  points(jitr(x, length(y), r=jitr_x), y, pch=pch, ...)
}

#' Ensure data are within some bounds
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @export
clip_x <- function(x, lim) {
  x[x<min(lim)] <- min(lim)
  x[x>max(lim)] <- max(lim)

  x
}

#' Useful for plotting when you want to go a bit beyond the data
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' @export
stretch <- function(x, pct) {
  d <- pct * diff(range(x))
  c(min(x)-d, max(x)+d)
}





