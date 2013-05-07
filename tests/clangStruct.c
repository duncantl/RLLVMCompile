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


int
bar(CXCursor cur, CXCursor parent)
{
    CXString cxstr = clang_getCursorSpelling(cur);
    const char *str = clang_getCString(cxstr);
    return(2);
}

