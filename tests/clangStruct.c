#include <clang-c/Index.h>

int kind = 10;
int xdata = 0;
int
foo(CXCursor cur, CXCursor parent)
{
    kind = cur.kind;
    xdata= cur.xdata;
    return(2);
}

