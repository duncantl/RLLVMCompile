#include <clang-c/Index.h>

const char *clang_CXCursor_getName(CXCursor cur);

int kind = 10;
int xdata = 0;
int
foo(CXCursor *cur, CXCursor *parent)
{
    kind = cur->kind;
    xdata= cur->xdata;
    return(2);
}


#include <stdio.h>

int
bar(CXCursor cur, CXCursor parent, void *unused)
{
/*
    kind = cur.kind;
    xdata= cur.xdata;
*/
    clang_CXCursor_getName(cur);
    printf("1\n");
    return(2);
}


