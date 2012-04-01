# Vince's Notes

## Casting in Comparison Operators

inst/examples/ifAssign.R's foo (and my foo.alt which compiles at this
state) illustrates a problem - the module will fail to build if
Int32Type and DoubleTypes are compared. This is because there are
type-specific comparison operators in LLVM, i.e. FCmp.


## Documentation Needed

- .functionInfo
- .routineInfo
- .compilerHandlers
- .insertReturn internals - how this works, what nested is, etc.


## Questions/Ideas:

- getMathOpType current looks to see if types are common and if not,
  returns a DoubleType, the type that we should coerce to. Could this
  be made more generic such that it does the conversion for us?

- fixIfAssign needs type checking - possible best done by scanning the
  body of the assignment first?
