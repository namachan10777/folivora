LIB_SRC = scad_ml/src/scad.ml
LIB = $(patsubst %.ml,%.cmo,$(LIB_SRC))

TARGETS := key.stl
SCAD_FILES := $(patsubst, %.scad,%.stl,$(TARGETS))

.PHONY: all
all: $(TARGETS)

.PHONY: preview
preview: %.scad

$(LIB): $(LIB_SRC) Makefile
	ocamlc -c $<

src/common.cmo: src/common.ml $(LIB)
	ocamlc -c $< -I scad_ml/src

src/key.cmo: src/key.ml src/common.cmo $(LIB)
	ocamlc -c $< -I src -I scad_ml/src

src/pad.cmo: src/pad.ml src/common.cmo $(LIB)
	ocamlc -c $< -I src -I scad_ml/src

src/thumb.cmo: src/thumb.ml src/common.cmo $(LIB)
	ocamlc -c $< -I src -I scad_ml/src

src/deploy.cmo: src/deploy.ml src/common.cmo src/key.cmo src/pad.cmo src/thumb.cmo
	ocamlc -c $< -I src -I scad_ml/src

deploy: src/deploy.cmo
	ocamlc -o $@ $(LIB) src/common.cmo src/key.cmo src/pad.cmo src/thumb.cmo src/deploy.cmo 

%.scad: deploy
	deploy

%.stl: %.scad
	openscad $< -o $@

.PHONY: clean
clean:
	rm -f src/*.cmo src/*.cmi src/*.cmt
	rm -f scad_ml/src/*.cmo scad_ml/src/*.cmi scad_ml/src/*.cmt
	rm -f *.scad
	rm -f *.stl
	rm -f deploy
