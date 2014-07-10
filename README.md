# RLLVMCompile

RLLVMCompile is an experimental R compiler that uses
[LLVM](http://llvm.org) through RLLVM, currently being developed by
Duncan Temple Lang.

## Goals

The goal of this is to provide an R programmer-level framework for
compiling R-like code.  This is different from building a compiler
that requires changing the code for the interpreter or developing an
different implementation of R, i.e. separate from GNU R.  The idea is
that we can translate R-like code to LLVM instructions, create those
instructions using the Rllvm package and then generate native/compiled
code and invoke it from the R session, serialize it for use in other
sessions or even applications (e.g. JavaScript, Python).

We might compile the same code in different ways for different contexts.
As a result, a single one-size-fits-all approach to compiling R is 
probably too restrictive. Furthermore, we want to be able to explore
new approaches easily w/o having to recompile all of R or learn a new
compilation framework tha is specific to R.  

One of the strengths of LLVM is that it embeddable and extensible and
provides a user-level API. A compiler for R should also do the same.
We have learned that a centralized code source that requires a core
group to make all "official" and "distributed" changes limits
innovation (but does improve stability).


## History

Vince Buffalo and I started this package several years ago (late 2010)
after the development of the Rllvm package.  Unfortunately, I had
other committments (the book XML and Web Technologies with R, and
another "Data Science in R: A Case Studies Approach to Computational
Reasoning and Problem Solving").  I am now getting back to this
project and hope to push it forward quite a bit by early 2015.


