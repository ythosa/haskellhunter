.PHONY: b_fn
b_fn:
	GHCi ./Bases/Functions.hs

.PHONY: b_hi
b_hi:
	GHCi ./Bases/HelloWorld.hs

.PHONY: b_op
b_op:
	GHCi ./Bases/Operators.hs

.PHONY: b_re
b_re:
	GHCi ./Bases/Recursion.hs

.PHONY: b_ty
b_ty:
	GHCi ./Bases/Functions.hs

.PHONY: b_lb
b_lb:
	GHCi ./Bases/LocalBindings.hs

.PHONY: fp_pp
fp_pp:
	GHCi ./FunctionalProgramming/ParametricPolymorphism.hs

.PHONY: fp_tc
fp_tc:
	GHCi ./FunctionalProgramming/TypeClasses.hs
