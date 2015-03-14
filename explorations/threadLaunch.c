#include <pthread.h>
#include <Rdefines.h>

#include <stdlib.h>

typedef void *(*ThreadRoutine)(void *);

SEXP
R_pthread_apply(SEXP routine, SEXP args)
{
    SEXP ans;
    int i, n;

    n = Rf_length(args);
    pthread_t *threads;

    ThreadRoutine fun = (ThreadRoutine) R_ExternalPtrAddr(routine);
    threads = (pthread_t *) malloc(sizeof(pthread_t) * n);
    for(i = 0; i < n; i++) {
//	thread = (pthread_t *) malloc(sizeof(pthread_t));
	if(pthread_create(threads + i, NULL, fun, VECTOR_ELT(args, i)) != 0) {
	    PROBLEM "failed to create thread"
   	     WARN;
// can have the threads in this this routine and then join on them and never let the threads out of this routine.
//	    SET_VECTOR_ELT(ans, i, tmp = R_MakeExternalPtr(thread, Rf_install("pthread_t"), R_NilValue));
	}
    }


    PROTECT(ans = NEW_LIST(n));
    for(i = 0; i < n; i++) {
	void *val;
	pthread_join(threads[i], &val);
	SET_VECTOR_ELT(ans, i, (SEXP) val);
    }
    UNPROTECT(1);

    return(ans);
}


SEXP
R_greaterThan(SEXP x)
{
    int i, n;
    n = Rf_length(x);
    int total = 0;
    double *vals = REAL(x);
    for(i = 0; i < n; i++)
	if(vals[i] > .25)
	    total += 1;

    return(ScalarInteger(total));
}
