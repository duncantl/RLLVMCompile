typedef struct A {
    int i;
    double d;
    char *c;
    struct A *a;
} A;

    int i;
    double d;
    char *c;
    A *a;

void
foo(A *val)
{
    i = val->i;
    d = val->d;
    c = val->c;
    a = val->a;
}

