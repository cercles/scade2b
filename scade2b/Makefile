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

#  +---------+              +--------+
#  | %.scade |   scade2b    |  %.out |
#  |         |              |        |
#  | Source  |  --------->  | Actual |
#  |  code   |              | output |
#  +---------+              +--------+
#                               |
# +----------+                  |           *.ok all exist
# |  %.spec  |                  |                 ==
# |          |                  |             tests pass
# | Expected | -------------+   |
# |  output  |              |   |                 /\
# +----------+              |   |                 ||
#                           v   v                 ||
#  \___  ___/            +---------+       +-------------+
#      \/                | %.diff  |       |    %.ok     |
#                        |         |       |             |
#    in git              | Unified | ----> |  Exists if  |
#                        |  diff   |       | out == spec |
#                        +---------+       +-------------+

check: $(patsubst %.scade, %.ok, $(wildcard tests/*.scade))

%.out: %.scade $(MAIN)
	./$(MAIN) --runtest $< > $@

%.diff: %.out %.spec
	diff -Nru $+ > $@

%.ok: %.diff
	# -e : file exists
	# -s : file exists and is not empty
	[ -e $< -a ! -s $< ] && touch $@
