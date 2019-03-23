

#' @export
root_decorator <- function(message = ''){
  decor = RutaDecor$new({}, message = message)
  decor
}

#' @export
new_canvas <- function(
  decor,
  type = 'n', xlab = '', ylab = '', xlim = NULL, ylim = NULL,
  cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.3
) {


  # operator `!!` will evaluate the value NOW

  # render function
  d = RutaDecor$new(~{
    plot(x, y, type = !!type, axes = F, ylab = !!ylab, xlab = !!xlab,
         xlim = xlim, ylim = ylim,
         cex.main=!!cex.main, cex.axis=!!cex.axis, cex.lab= !!cex.lab)
  }, message = 'Generate a blank canvas')

  # debug:
  # print(d$render_expr)

  d$set_check(~{
    .set_data(xlim = !!xlim)
    if(is.null(xlim)){
      .set_data(xlim = range(x))
    }

    .set_data(ylim = !!ylim)
    if(is.null(ylim)){
      .set_data(ylim = range(y))
    }

  })

  # debug:
  # d$check_expr

  decor %?<-% root_decorator()
  decor$set_child_decor(d)
  return(d)
}

#' @export
add_axis <- function(
  decor,
  side, at = NULL, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis,
  cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0)) {

  decor %?<-% root_decorator()

  if(length(side) > 1) {
    for(s in side){
      add_axis(decor, side = s, at=at, tcl=tcl, labels=labels,
                   cex.axis=cex.axis, las=las, cex.lab=cex.lab)
    }
    return(decor)
  }


  # one side
  mgp <- mgpy
  if(side %% 2) mgp <- mgpx
  side_x = side %in% c(1,3)

  d = RutaDecor$new(~{
    at = !!at
    if(is.null(at)){
      if(side_x)  at = pretty(x)  else  at = pretty(y)
    }else if(is.function(at)){
      if(side_x)  at = at(x)  else  at = at(y)
    }
    labels = !!labels
    if(is.null(labels)){
      labels = at
    }else if(is.function(labels)){
      labels = labels(at)
    }

    res = axis(side = !!side, at=at, labels=labels, tcl=!!tcl, mgp=!!mgp,
               cex.axis=!!cex.axis, las=!!las, cex.lab=!!cex.lab)
    .set_data(key = !!paste0('axis_', side), val = res)
  }, message = sprintf('Draw axis at side %d', side))


  decor$set_child_decor(d)

  return(d)

}


# # Decorator, auto detect xlim, ylim
# require(rutabaga)
# require(magrittr)
# root = root_decorator()
# root %>%
#   new_canvas(type = 'l') %>%
#   add_axis(side = 1, at = c(1, 3, 5, 8)) %>%
#   add_axis(side = 2, at = function(y){ quantile(y, c(0,0.25,0.75,1) )}) %>%
#   add_axis(side = 3:4)
#
# root
#
# # Plot data
# g = RutaPlot$new(x=1:10, y = rnorm(10));
# g$show(root)
# g
