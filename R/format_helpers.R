# # # Formatters for statistics


# helper function to build value labels
format_stat <- function(nm, stats=c('b', 't', 'p')) {
  sapply(stats, function(stat) sprintf('%s(%s)', stat, nm), USE.NAMES = FALSE)
}

get_f <- function(formula, data) {
  format_f(lm(formula, data))
}

format_f <-  function(lm.mod, test_name='All') {
  nms <- sapply(c('Rsq(%s)', 'F(%s)', 'p(%s)'), sprintf, test_name)

  with(summary(lm.mod), {
    c(r.squared, fstatistic[1],
      pf(fstatistic[1], fstatistic[2], fstatistic[3], lower.tail=FALSE))
  }) %>% set_names(nms) %>% `class<-`('fres')
}

# relying on a generic here
pretty.fres <- function(fres) {
  # don't save intermediate results back into fres or else it changes the type into character,
  # messing up following lines
  c(
    # R2
    ifelse(fres[1] < 0.01, '<0.01', round(fres[1],2)),
    #F stat
    ifelse(fres[2] < 0.01, '<0.01', round(fres[2],1)),
    #p value
    format(fres[3], digits=1)
  ) %>% `class<-`(c('fres', 'character'))
}

# helper function for t-tests that returns the values wanted by format_stat
get_t <- function(...) with(t.test(...), c(estimate, statistic, p.value)) %>% `class<-`('tres')

pretty.tres <- function(tres) {
  mapply(format, tres, digits=c(2,2,1)) %>%
    set_names(c('m', 't', 'p')) %>% `class<-`(c('tres', 'character'))
}

# these often won't look pretty unless they are used with pretty
# e.g., title(main=as.title(pretty(get_t(...))))
as.title <- function(res, ...) {
  UseMethod('as.title')
}

as.title.fres <- function(res, ...) {
  bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(res[1]) ~ ',' ~ F == .(res[2]) * ','~ p==.(res[3]))
}

as.title.tres <- function(res,...) {
  bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(res[1]) * ',' ~ t == .(res[2]) * ',' ~ p==.(res[3]))
}
