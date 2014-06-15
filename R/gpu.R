compileGPUKernel = 
function(fun, types, module = ModuleForNVVM(name), name = NULL,
         .compilerHandlers = getGPUCompilerHandlers(),
         .insertReturn = TRUE,
         .builtInRoutines = getGPUBuiltInRoutines(),
         .useFloat = TRUE, ...)
{
   if(missing(name))
     name = deparse(substitute(fun))

 
   f = compileFunction(fun, VoidType, types, module = module, name = name,
                       .compilerHandlers = .compilerHandlers,
                       .builtInRoutines = .builtInRoutines, 
                       .useFloat = .useFloat, .insertReturn = FALSE, ...
                      )
  # add the terminator createRet() in the correct block

  setGPUKernel(f, module)

  f
}

getGPUCompilerHandlers = 
function(..., defaults = getCompilerHandlers(), call = getGPUCallHandler())
{
     # add our own defaults
  defaults[["$"]] = call

  others = list(...)
  defaults[names(others)] = others
  defaults
}

getGPUCallHandler = 
function(defaultHandler = callHandler)
{
   function(call, env, ir, ..., fun = env$.fun, name = getName(fun)) {

      op = as.character(call[[1]])
      if(( op == "$"  && is.name(call[[2]]) &&
             as.character(call[[2]]) %in% GPUIntrinsicNames)) {
         r = PTXRegisterRoutineNames[as.character(call[[2]])]
         funName = sprintf("%s.%s", r, as.character(call[[3]]))
         if(!(funName %in% names(env$.module)))
            Function(funName, Int32Type, list(), module = env$.module)

         call = substitute(f(), list(f = as.name(funName)))
       }

       ins = defaultHandler(call, env, ir, ..., fun = fun, name = name)
       attr(ins, "zeroBasedCounting") = TRUE

       return(ins)
   }
}

PTXRegisterRoutineNames = 
 c(blockIdx = "llvm.nvvm.read.ptx.sreg.ctaid",
   blockDim = "llvm.nvvm.read.ptx.sreg.ntid",
   threadIdx = "llvm.nvvm.read.ptx.sreg.tid",
   gridDim = "llvm.ptx.read.nctaid")

GPUIntrinsicNames = c("blockIdx", "blockDim", "threadIdx", "gridDim")
   # Need to make this an argument to compileFunction() so that we can adapt it.
ExcludeGlobalVariables = GPUIntrinsicNames


getGPUBuiltInRoutines = getBuiltInRoutines

fixPTXCodeForNVVM =
  # This removes the attributes with the id #0
  # which appear for the declared routines corresponding
  # to the PTXRegister accessor intrinsics.
function(code) 
{
  k = strsplit(code, "\\n")[[1]]
  k = grep("^attributes #0", k, invert = TRUE, value = TRUE)
  k = gsub("#0$", "", k)
  code = paste(k, collapse = "\n")
}


DefaultGPULayout =
"e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64"

ModuleForNVVM = 
function (id = Sys.time(), context = NULL,
           layout = DefaultGPULayout) 
{
  m = Module(id, context)
  setDataLayout(m, layout)
  m
}



setGPUKernel =
    #
    # Add nvvm annotation kernel = 1 for each Function object.
    #
function(funs, module)
{
  if(is.character(funs)) 
    funs = getModuleFunctions(module)[funs]

  if(is(funs, "Function"))
     funs = list(funs)

  if(missing(module))
     module = as(funs[[1]], "Module")

  lapply(funs, 
          function(fun)
              setMetadata(module, "nvvm.annotations", list(fun, "kernel", 1L)))
}
