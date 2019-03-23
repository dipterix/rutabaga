#' R6 class for plot class
#' @author zhengjia wang
#' @export
RutaPlot <- R6::R6Class(
  classname = 'RutaPlot',
  portable = TRUE,
  cloneable = FALSE,

  private = list(

    # This is data environment and plot code should make barely assumptions on
    # what the data should look like
    env = NULL,
    decorator = NULL
  ),

  public = list(

    # Method to specify variables
    initialize = function(..., data = list()){

      # Data checks
      assert_that( is.list(data), msg = 'argument `data` must be a list, data.frame, or tibble.' )


      private$env = new.env(baseenv(), hash = TRUE)

      args = list(...)
      if(length(args)){
        list2env(args, envir = private$env)
      }

      if(length(data)){
        list2env(data, envir = private$env)
      }

      private$env$.set_data = self$set_data
    },

    set_data = function(..., key, val){
      args = list(...)
      if(!missing(key)){
        args[[key]] = val
      }

      if(length(args)){
        list2env(args, envir = private$env)
      }
      invisible(args)
    },

    # Do with data, function used for data validation, plot...
    # expr can be R expression, quoted expression, or rlang::quo
    do_with = function(expr, envir = parent.frame(), quoted = FALSE){
      if(!quoted){
        expr = eval(substitute(substitute(expr)), envir)
      }
      if(rlang::is_quosure(expr)){
        quo = expr
      }else{
        quo = do.call(rlang::quo, list(expr = expr), envir = envir)
      }

      rlang::eval_tidy(quo, data = private$env, env = envir)

    },

    set_root = function(d){
      assert_that(has_class(d, 'RutaDecor'), msg = 'd must be a RutaDecor instance')
      private$decorator = d
    },

    info = function(){
      cat2('================ Class: ', end = '', level = 'INFO')
      cat2(class(self), collapse = ' <= ', level = 'INFO', end = '')
      cat2(' ================', level = 'INFO')
      nms = ls(private$env, all.names = F)
      if(!length(nms)){
        cat2('  <No data>', level = 'INFO', pal = list('INFO' = '#f02c2c'))
        return(invisible())
      }

      lens = stringr::str_length(nms)
      l = max(lens)
      for(ii in seq_along(nms)){
        nm = nms[[ii]]
        pad = stringr::str_pad(nm, l, side = 'right')
        cls = class(private$env[[nm]])
        add = ''
        if(has_attr(private$env[[nm]], 'dim')){
          add = paste0(', Dim: [', paste(attr(private$env[[nm]], 'dim'), collapse = ' x '), ']')
        }
        if(is.numeric(private$env[[nm]])){
          if(length(private$env[[nm]]) > 1){
            a = paste(sprintf('%.3g', range(private$env[[nm]])), collapse = ' ~ ')
            add = paste(add, ', Range:', a)
          }else{
            add = paste(add, sprintf(', Range: %.3g', private$env[[nm]]))
          }

        }

        cat2('  ', pad, ' <=', paste(cls, collapse = ', '), add,
             level = 'INFO', pal = list('INFO' = '#009ACD'))
      }
      return(invisible())
    },

    show = function(d = NULL, check = TRUE){
      if(!is.null(d)){
        assert_that(has_class(d, 'RutaDecor'), msg = 'd must be a RutaDecor instance')
      }else{
        d = private$decorator
      }

      if(is.null(d)){
        # There is no decorator yet, print information
        self$info()
        return(invisible())
      }

      if(check){
        d$check(self)
      }
      d$render(self)
    }

  )
)


#' @export
print.RutaPlot <- function(x, ...){
  x$show(...)
}


has_class <- function(x, cls){
  return(cls %in% class(x))
}
