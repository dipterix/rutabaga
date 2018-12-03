# lifecycle
lifecycle <- function(stage) {
  substr(stage, 1, 1) <- toupper(substr(stage, 1, 1))
  sprintf(
    "\\ifelse{html}{%s}{\\strong{%s}}", substr
  )
}



#' @title Help Formatting Documents
#' @export
pretty_package_docs <- function(path = '.'){
  # detect all R files
  fs = list.files(path, recursive = T, pattern = '\\.R$')
  for(f in fs){
    cat2('Prettify ', f, level = 'INFO')
    s = readLines(f)
    # Find rows with docs - "#'"
    doc_lines = which(base::grepl("^[\\ ]{0}#\\'", s))
    if(!length(doc_lines)){
      next()
    }
    doc_lines = deparse_svec(doc_lines, concatenate = F)

    no_rds = NULL
    # Find rows starts with "#' @title"
    for(docl in doc_lines){
      idx = parse_svec(docl)

      # find lines with title
      sel_title = stringr::str_detect(s[idx], "^[\\ ]{0}#' @title")

      if(any(sel_title)){
        idx_title = idx[which(sel_title)[1]]
      }else{
        sel_title = !stringr::str_detect(s[idx], "^[\\ ]{0}#' @")
        if(any(sel_title)){
          idx_title = idx[which(sel_title)[1]]
        }else{
          idx_title = NULL
        }
      }
      if(length(idx_title) == 1){
        # capitalize title
        title = stringr::str_match(s[idx_title], "(^[\\ ]{0}#') (?:@title )?(.*)")
        if(anyNA(title)){
          stop('Error in parsing title ', s[idx_title])
        }

        tt = stringr::str_to_title(stringr::str_trim(title[3]))
        new_t = paste0(title[2], " @title ", tt)
        if(new_t != s[idx_title]){
          cat2(s[idx_title], ' => ', new_t)
          s[idx_title] = new_t
        }

      }else{
        no_rds = idx[1]
      }

    }

    writeLines(s, con = f)

  }
}
