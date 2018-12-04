# String operations

#' @title Parse Text Into Numeric Vectors
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @param text string with chunks, e.g. \code{"1-10, 14, 16-20, 18-30"} has 4 chunks
#' @param sep default is ",", character used to separate chunks
#' @param connect characters defining connection links for example "1:10" is the same as "1-10"
#' @param sort sort the result
#' @param unique extract unique elements
#'
#' @examples
#' \dontrun{
#' parse_svec('1-10, 13:15,14-20')
#' }
#' @export
parse_svec <- function(text, sep = ',', connect = '-:|', sort = F, unique = T){
  connect = unlist(stringr::str_split(connect, ''))
  connect[connect %in% c('|', ':')] = paste0('\\', connect[connect %in% c('|', ':')])
  connect = paste(connect, collapse = '')


  if(length(text) == 0 || stringr::str_trim(text) == ''){
    return(NULL)
  }

  if(is.numeric(text)){
    return(text)
  }
  s = as.vector(stringr::str_split(text, sep, simplify = T))
  s = stringr::str_trim(s)
  s = s[s!='']

  s = s[stringr::str_detect(s, sprintf('^[0-9\\ %s]+$', connect))]

  re = NULL
  for(ss in s){
    if(stringr::str_detect(ss, sprintf('[%s]', connect))){
      ss = as.vector(stringr::str_split(ss, sprintf('[%s]', connect), simplify = T))
      ss = ss[stringr::str_detect(ss, '^[0-9]+$')]
      ss = as.numeric(ss)
      if(length(ss) >= 2){
        re = c(re, (ss[1]:ss[2]))
      }
    }else{
      re = c(re, as.numeric(ss))
    }
  }

  if(unique){
    re = unique(re)
  }

  if(sort){
    re = sort(re)
  }

  return(re)
}

#' @title Convert Integer Vectors To String
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' @param nums integer vector
#' @param connect character used to connect consecutive numbers
#' @param concatenate connect strings if there are multiples
#' @param collapse if concatenate, character used to connect strings
#' @param max_lag defines "consecutive", min = 1
#'
#' @examples
#' \dontrun{
#' deparse_svec(c(1:10, 15:18))
#' }
#' @export
deparse_svec <- function(nums, connect = '-', concatenate = T, collapse = ',', max_lag = 1){
  nums = nums[is.finite(nums)]
  if(length(nums) == 0){
    return('')
  }
  alag = 1:max(1, max_lag)
  nums = sort(unique(nums))
  lg = c(NA, nums)[1:length(nums)]
  ind = nums - lg; ind[1] = 0
  ind2 = c(ind[-1], -1)

  apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1,function(x){
    if(x[1] == x[2]){
      stringr::str_c(x[1])
    }else{
      stringr::str_c(x, collapse = connect)
    }
  }) ->
    re
  if(concatenate){
    re = stringr::str_c(re, collapse = collapse)
  }
  re
}




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

