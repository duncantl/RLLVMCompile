#include <Rdefines.h>

/* could use numClasses, ...  and vaargs. */
void
R_raiseStructuredError(const char *msg, char **classes, int numClasses)
{
    SEXP err, expr, rclass;
    int i;
    PROTECT(expr = Rf_allocVector(LANGSXP, 2));
    SETCAR(expr, Rf_install("stop"));

    PROTECT(err = NEW_LIST(1));
    SET_VECTOR_ELT(err, 0, Rf_mkString(msg));
    SET_NAMES(err, Rf_mkString("message"));

    PROTECT(rclass = NEW_CHARACTER(numClasses + 2));
    for(i = 0; i < numClasses; i++)
	SET_STRING_ELT(rclass, i, mkChar(classes[i]));
    SET_STRING_ELT(rclass, i++, mkChar("error"));
    SET_STRING_ELT(rclass, i++, mkChar("condition"));

    SET_CLASS(err, rclass);
    SETCAR(CDR(expr), err);

    Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(3);
    return;
}

#include <stdarg.h>
void
R_va_raiseStructuredError(const char *msg, int numClasses, ...)
{
    SEXP err, expr, rclass;
    int i;
    va_list argp;
    const char *str;

    PROTECT(expr = Rf_allocVector(LANGSXP, 2));
    SETCAR(expr, Rf_install("stop"));

    PROTECT(err = NEW_LIST(1));
    SET_VECTOR_ELT(err, 0, Rf_mkString(msg));
    SET_NAMES(err, Rf_mkString("message"));

    PROTECT(rclass = NEW_CHARACTER(numClasses + 2));
    va_start(argp, numClasses);
    for(i = 0; i < numClasses; i++) {
	str = va_arg(argp, const char *);
	SET_STRING_ELT(rclass, i, mkChar(str));
    }
    va_end(argp);
    SET_STRING_ELT(rclass, i++, mkChar("error"));
    SET_STRING_ELT(rclass, i++, mkChar("condition"));

    SET_CLASS(err, rclass);
    SETCAR(CDR(expr), err);

    Rf_eval(expr, R_GlobalEnv);
    UNPROTECT(3);
    return;
}
