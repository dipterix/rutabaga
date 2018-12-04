# String operations

#' @title Parse Text Into Numeric Vectors
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
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
