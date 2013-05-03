#include <Rdefines.h>
typedef int (*worker)(int x);

SEXP
R_runWorker(SEXP r_vec, SEXP r_worker)
{
    worker f = (worker) R_ExternalPtrAddr(r_worker);
    int i, n;
    SEXP ans;

    n = Rf_length(r_vec);
    PROTECT(ans = NEW_INTEGER(n));
    for(i = 0; i < n; i++) {
	INTEGER(ans)[i] = f(INTEGER(r_vec)[i]);
    }
    UNPROTECT(1);
    return(ans);
}

int simpleWorker(int x)
{
    return(10 * x);
}
