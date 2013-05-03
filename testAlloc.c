#include <Rdefines.h>

void sayHi()
{
    fprintf(stderr, "Hi\n");
}

SEXP
R_my_allocVector()
{
    return(Rf_allocVector(14, 9));
}


SEXP
R_NEW_NUMERIC(int len)
{
    SEXP ans = NEW_NUMERIC(len);
    for(int i = 0; i < len; i++)
	REAL(ans)[i] = 0;
    return(ans);
}


SEXP
R_my_allocVectorInit()
{
    SEXP ans = Rf_allocVector(14, 9);
 fprintf(stderr, "hi\n");
    for(int i = 0; i < 9; i++)
	REAL(ans)[i] = 2*i;
    return(ans);
}
