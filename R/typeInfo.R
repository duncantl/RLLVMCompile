#XXX See code in compile.R - getBuiltInRoutines
FunctionTypeInfo =
  list (sqrt = list(return = "numeric", isElementwise = TRUE, inputs = c("numeric")),
        exp = list(return = "numeric", isElementwise = TRUE, inputs = c("numeric")),
        "^" = list(return = "numeric", isElementwise = TRUE, inputs = c("numeric")),                
        "/" = list(return = "numeric", isElementwise = TRUE, inputs = "numeric"),
        "*" = list(return = "numeric", isElementwise = TRUE, inputs = "numeric"),
        "+" = list(return = "numeric", isElementwise = TRUE, inputs = "numeric"),
        "-" = list(return = "numeric", isElementwise = TRUE, inputs = "numeric")                        
        )

ConstantInfo =
     list(pi = base::pi, e = exp(1))

