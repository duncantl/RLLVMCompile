#include <Rdefines.h>


SEXP
R_loadRObjectFromString(const char *txt)
{
    SEXP expr, el, ans;

    PROTECT(el = expr = Rf_allocVector(LANGSXP, 2));
    SETCAR(expr, Rf_install("loadRObjectFromString"));
    el = CDR(expr);
    SETCAR(el, Rf_mkString(txt));
    ans = Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(1);
    return(ans);
}
