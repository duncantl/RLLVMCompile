%.bc: %.c
	llvm-gcc $(OPT_LEVEL) -emit-llvm $< -c -o $@

%.llcpp: %.bc
	llc -march=cpp $< -o $@

# TODO make a target for something like 
# llvm-gcc -emit-llvm not.c -S -o not.s
