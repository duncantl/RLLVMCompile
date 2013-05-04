; ModuleID = 'clang.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.7.0"

%struct.CXCursor = type { i32, i32, [3 x i8*] }

@kind = global i32 10, align 4
@xdata = global i32 0, align 4

define i32 @foo(%struct.CXCursor* %cur, %struct.CXCursor* %parent) #0 {
entry:
  %cur.addr = alloca %struct.CXCursor*, align 8
  %parent.addr = alloca %struct.CXCursor*, align 8
  store %struct.CXCursor* %cur, %struct.CXCursor** %cur.addr, align 8
  store %struct.CXCursor* %parent, %struct.CXCursor** %parent.addr, align 8
  %0 = load %struct.CXCursor** %cur.addr, align 8
  %kind = getelementptr inbounds %struct.CXCursor* %0, i32 0, i32 0
  %1 = load i32* %kind, align 4
  store i32 %1, i32* @kind, align 4
  %2 = load %struct.CXCursor** %cur.addr, align 8
  %xdata = getelementptr inbounds %struct.CXCursor* %2, i32 0, i32 1
  %3 = load i32* %xdata, align 4
  store i32 %3, i32* @xdata, align 4
  ret i32 2
}

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
