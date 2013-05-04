#include <clang-c/Index.h>

int kind = 10;

int
foo(CXCursor *cur, CXCursor *parent)
{
    kind = cur->kind;
    return(2);
}

