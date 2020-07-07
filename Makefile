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

.PHONY: pb_pp
pb_pp:
	GHCi ./ProgrammingBasics/ParametricPolymorphism.hs

.PHONY: pb_tc
pb_tc:
	GHCi ./ProgrammingBasics/TypeClasses.hs

.PHONY: pb_nss
pb_nss:
	GHCi ./ProgrammingBasics/NonStrictSemantics.hs

.PHONY: pb_m
pb_m:
	GHCi ./ProgrammingBasics/Modules.hs

.PHONY: l_lf
l_lf:
	GHCi ./Lists/ListFunctions.hs

.PHONY: l_lhof
l_lf:
	GHCi ./Lists/ListHOC.hs
