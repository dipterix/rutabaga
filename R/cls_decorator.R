#' Plot decorators
#' @author Zhengjia Wang
#' @export
RutaDecor <- R6::R6Class(
  classname = 'RutaDecor',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    children = NULL,
    unit_check = function(g, env){
      # Pre-render: check data, add plot data etc...
      g$do_with(self$check_expr, envir = env, quoted=TRUE)
    },
    unit_render = function(g, env){
      self$result = g$do_with(self$render_expr, quoted = TRUE, envir = env)
      self$result
    }
  ),
  public = list(
    render_expr = NULL,
    check_expr = TRUE,
    error_message = '',
    result = NULL,
    initialize = function(expr, envir = parent.frame(), quoted = FALSE){
      if(!quoted){
        expr = eval(substitute(substitute(expr)), envir)
      }
      if(rlang::is_quosure(expr)){
        self$render_expr = expr
      }else{
        self$render_expr = do.call(rlang::quo, list(expr = expr), envir = envir)
      }

      private$children = list()
    },

    set_check = function(expr, msg = NULL){
      envir = parent.frame()
      expr = eval(substitute(substitute(expr)), envir)
      self$check_expr = do.call(rlang::quo, list(expr = expr), envir = envir)
      if(!is.null(msg)){
        self$error_message = msg
      }
    },
    set_child_decor = function(d, at = 0, replace = TRUE){
      assert_that(has_class(d, 'RutaDecor'), msg = 'd must be a RutaDecor instance')
      nchild = length(private$children)
      if(at <= 0){
        at = nchild + 1
      }
      if(at <= nchild && isFALSE(replace)){
        if(at == 1){
          private$children = c(d, private$children)
        }else{
          private$children = c(private$children[seq_len(at - 1)], d, private$children[- seq_len(at - 1)])
        }
      }else{
        private$children[[at]] = d
      }

    },

    .param_env = NULL,


    check = function(g, env = parent.frame(), .is_top = TRUE){
      if(.is_top){
        assert_that(has_class(g, 'RutaPlot'), msg = 'g must be a RutaPlot instance')
        self$.param_env = new.env(parent = env)
      }else{
        self$.param_env = env
      }

      for(d in private$children){
        d$check(g, env = self$.param_env, .is_top = FALSE)
      }
      private$unit_check(g, env = self$.param_env)
    },

    render = function(g, .is_top = TRUE){
      if(.is_top){
        assert_that(has_class(g, 'RutaPlot'), msg = 'g must be a RutaPlot instance')
      }
      private$unit_render(g, self$.param_env)
      for(d in private$children){
        d$render(g, .is_top = FALSE)
      }
    },

    print = function(...){
      return(invisible(self))
    }
  )
)


`%?<-%` <- function(lhs, value){
  env = parent.frame()
  lhs = substitute(lhs)

  tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  }) ->
    isnull

  if(isnull){
    # quo <- quo(!!lhs <- !!value)
    quo <- rlang::quo(do.call('=', list(quote(!!lhs), !!value)))
    eval(rlang::quo_squash(quo), envir = env)
  }
}
