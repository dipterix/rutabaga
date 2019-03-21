# Shiny utils

#' Function to debug shiny script interactively (Needs RStudio)
#'
#' @param expr an expression ends with shinyApp
#' @param monitor_dirs directory to monitor. Usually scripts used by your shiny apps
#' @param env environment to evaluate \code{expr}, with side effect
#' @param action NULL, or expression to evaluate once \code{monitor_dirs} is changed
#' @param action_env  environment to evaluate \code{action}, with side effect
#' @param force_eval force evaluation
#'
#' @examples
#' \dontrun{
#' # Create a shinyapp that monitors your desktop
#' # if there is any change to your desktop, the app
#' # will re-launch
#'
#' library(shiny)
#' debug_shiny({
#'   # Launch shiny app
#'   shinyApp(
#'     ui = fluidPage(textInput('hey', 'Hey, add a file to your desktop!',
#'                              value = as.character(Sys.time()))),
#'     server = function(...){})
#' }, action = {
#'   cat2('One or more files are changed, restarting')
#' }, monitor_dirs = '~/Desktop/')
#' }
#' @export
debug_shiny <- function(expr, monitor_dirs, env = parent.frame(), action = NULL, action_env = env, force_eval = FALSE){
  expr = substitute(expr, environment())
  action = substitute(action, environment())
  call_expr = match.call()
  this_env = environment()

  if(!force_eval && (!base::interactive() || !identical(globalenv(), parent.frame()))){
    stop('You Must run this function in "RStudio" console. This is a debug function.')
  }


  # make sure shinyjs is installed
  check_installed_packages(c('shinyjs', 'rstudioapi'), auto_install = T)

  eval_f = function(){
    app = eval(expr, envir = env)
    if(!is(app, "shiny.appobj")){
      stop('expr Must end with a shiny app object. Create it using shiny::shinyApp(ui, server), or shinyAppFile(file)')
    }

    env = environment(app$serverFuncSource)
    server_func = env$server
    server_env = environment(server_func)
    server_env$...___this_env = this_env
    server_expr = body(server_func)

    # inject code into server function
    inject_expr = "local({
      session = shiny::getDefaultReactiveDomain()
      session$allowReconnect(TRUE)

      debug_timer = shiny::reactiveTimer()
      local_data = shiny::reactiveValues(sig = NULL)
      shiny::observeEvent(debug_timer(), {
        sig = rutabaga::dir_signature(monitor_dirs)
        old_sig = shiny::isolate(local_data$sig)
        if(is.null(old_sig)){
          local_data$sig = sig
        }else if(old_sig != sig){
          # file changed, do action
          eval(action, envir = action_env)

          shinyjs::runjs('setTimeout(function(){ location.reload(true); }, 1500);')
          shiny::stopApp()

          # relaunch!
          rstudioapi::sendToConsole(code = rlang::quo_text(call_expr), execute = TRUE)
        }
      })
    }, envir = ...___this_env)"

    quo = rlang::parse_expr(inject_expr)

    body(env$server) = rlang::quo_squash(rlang::quo({
      # !!server_expr

      !!quo
    }))

    return(app)
  }

  eval_f()
}





