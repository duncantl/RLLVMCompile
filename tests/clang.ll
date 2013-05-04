; ModuleID = 'clang.c'

target triple = "x86_64-apple-macosx10.7.0"

%struct.CXCursor = type { i32, i32, [3 x i8*] }

@kind = global i32 10, align 4

define i32 @h(%struct.CXCursor* %cur, %struct.CXCursor* %parent)  {
entry:
  %cur.addr = alloca %struct.CXCursor*, align 8
  store %struct.CXCursor* %cur, %struct.CXCursor** %cur.addr, align 8
  %0 = load %struct.CXCursor** %cur.addr, align 8
  %kind = getelementptr inbounds %struct.CXCursor* %0, i32 0, i32 0
  %1 = load i32* %kind, align 4
  store i32 %1, i32* @kind, align 4
  ret i32 2
}

