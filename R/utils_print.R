#' @title Color Stdout
#' @importFrom crayon make_style
#' @param ... to be printed
#' @param level DEBUG, INFO, WARNING, ERROR, or FATAL (total 5 levels)
#' @param print_level if true, prepend levels before messages
#' @param file pass to base::cat
#' @param sep pass to base::cat
#' @param fill pass to base::cat
#' @param labels pass to base::cat
#' @param append pass to base::cat
#' @param pal a named list defining colors see details
#' @details
#' There are five levels by default: DEBUG, INFO, WARNING, ERROR, or FATAL.
#' Default colors to these levels are: DEBUG (grey60), INFO (#1d9f34), WARNING
#' (#ec942c), ERROR (#f02c2c), FATAL (#763053) and DEFAULT (#000000, black).
#' If level is not in pre-set five levels, the color will be "default"-black
#' color.
#' @export
cat2 <- function(
  ..., level = 'DEBUG', print_level = FALSE,
  file = "", sep = " ", fill = FALSE, labels = NULL,
  append = FALSE, pal = list(
    'DEBUG' = 'grey60',
    'INFO' = '#1d9f34',
    'WARNING' = '#ec942c',
    'ERROR' = '#f02c2c',
    'FATAL' = '#763053',
    'DEFAULT' = '#000000'
  )
){
  if(!level %in% names(pal)){
    level = 'DEFAULT'
  }
  .col = pal[[level]]
  if(is.null(.col)){
    .col = '#000000'
  }

  # check if interactive
  if(base::interactive()){
    # use colored console
    col = crayon::make_style(.col)
    if(print_level){
      base::cat('[', level, ']: ', sep = '')
    }

    base::cat(col(..., sep = sep), '\n', file = file, fill = fill, labels = labels, append = append)

  }else{
    # Just use cat
    base::cat(...)
  }

  if(level == 'FATAL'){
    # stop!
    stop()
  }

  invisible()
}



#' @title Pipe Function To Paste Two Characters
#' @param x character
#' @param y character
#' @return paste0(x,y)
`%&%` <- function(x, y){
  base::paste0(x, y)
}

