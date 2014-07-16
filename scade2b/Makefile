OCAMLBUILD= ocamlbuild -no-links -classic-display \
		-libs unix,nums \
		-tags debug,annot

TARGET=native
MAIN=scade2b

all: $(MAIN)

native: TARGET := native
native: all
opt: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := byte
byte: all
$(MAIN).byte: byte


$(MAIN): $(MAIN).target
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).target:
	$(OCAMLBUILD) $(MAIN).$(TARGET)


clean:
	ocamlbuild -classic-display -clean

realclean: clean
	rm -f $(MAIN) *~

cleanall: realclean

cleanall_bis: realclean
	rm -rf *.out *.cmp *.log *.old

check_clean:
	rm -rf tests/*/Machines_B/ tests/*.ok tests/*.diff

build-cov:
	ocp-build build scade2b_tests_cov

cov: build-cov
	BISECT_FILE=scade2b ./_obuild/scade2b_tests_cov/scade2b_tests_cov.asm
	bisect-report -html coverage scade2b*.out
	rm scade2b*.out

_obuild/scade2b_tests/scade2b_tests.asm: run_tests.ml
	ocp-build build scade2b_tests

_obuild/scade2b/scade2b.asm: *.ml
	ocp-build

check: _obuild/scade2b_tests/scade2b_tests.asm _obuild/scade2b/scade2b.asm
	./$<
