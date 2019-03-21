#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _rutabaga_arraypermer(SEXP, SEXP, SEXP);
extern SEXP _rutabaga_collapser(SEXP, SEXP, SEXP);
extern SEXP _rutabaga_vec_sum(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_rutabaga_arraypermer", (DL_FUNC) &_rutabaga_arraypermer, 3},
  {"_rutabaga_collapser",   (DL_FUNC) &_rutabaga_collapser,   3},
  {"_rutabaga_vec_sum",     (DL_FUNC) &_rutabaga_vec_sum,     3},
  {NULL, NULL, 0}
};

void R_init_rutabaga(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

