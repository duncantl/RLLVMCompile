; ModuleID = 'clang.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

%struct.CXCursor = type { i32, i32, [3 x i8*] }

@kind = global i32 10, align 4
@xdata = global i32 0, align 4
@.str = private unnamed_addr constant [3 x i8] c"1\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @foo(%struct.CXCursor* %cur, %struct.CXCursor* %parent) #0 {
  %1 = alloca %struct.CXCursor*, align 8
  %2 = alloca %struct.CXCursor*, align 8
  store %struct.CXCursor* %cur, %struct.CXCursor** %1, align 8
  store %struct.CXCursor* %parent, %struct.CXCursor** %2, align 8
  %3 = load %struct.CXCursor** %1, align 8
  %4 = getelementptr inbounds %struct.CXCursor* %3, i32 0, i32 0
  %5 = load i32* %4, align 4
  store i32 %5, i32* @kind, align 4
  %6 = load %struct.CXCursor** %1, align 8
  %7 = getelementptr inbounds %struct.CXCursor* %6, i32 0, i32 1
  %8 = load i32* %7, align 4
  store i32 %8, i32* @xdata, align 4
  ret i32 2
}

; Function Attrs: nounwind ssp uwtable
define i32 @bar(%struct.CXCursor* byval align 8 %cur, %struct.CXCursor* byval align 8 %parent, i8* %unused) #0 {
  %1 = alloca i8*, align 8
                      ; store i8* %unused, i8** %1, align 8
  %2 = call i8* @clang_CXCursor_getName(%struct.CXCursor* byval align 8 %cur)
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0))
  ret i32 2
}

declare i8* @clang_CXCursor_getName(%struct.CXCursor* byval align 8) #1

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final)"}
