whileHandler =
function(call, env, ir, ..., fun = env$.fun)
{
    label = deparse(call)
    
    cond = Block(fun, sprintf("cond.%s", label))
    bodyBlock = Block(fun, sprintf("body.%s", label))
    nextBlock = Block(fun, sprintf("next.%s", label))

   ir$createBr(cond)
    
    ir$setInsertPoint(cond)
       a = compile(call[[2]], env, ir)
       # ok = ir$createICmp(ICMP_SLT, a, ir$createIntegerConstant(1L))
       ir$createCondBr(a, bodyBlock, nextBlock)    

     ir$setInsertPoint(bodyBlock)
       compileExpressions(call[[3]], env, ir)
       ir$createBr(cond)

    ir$setInsertPoint(nextBlock)    
}
