 source("tests/simpleIf.R")
 source("tests/basicIf.R")
 source("tests/forBreak.R")
 source("tests/nestedLoops.R")
 source("tests/nestedLoops1.R")
 source("tests/nestedLoops2.R")
 source("tests/nestedLoopsIf.R")
 source("explorations/simplerNestedIfInLoop.R")
 source("explorations/simplerNestedIfInLoop1.R")
 source("explorations/simplerNestedIfInLoop2.R")
 source("tests/whileBreak.R")
 source("tests/select.R")


if(require('XML')) {
   xmlSource("explorations/BML.Rdb")
   xmlSource("explorations/BML.Rdb", xnodes = "//section[@id = 'timings']//r:code", force = TRUE)
}

