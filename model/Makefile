# Add output models
MODELS := cherry_mx

SCAD_SOURCES := $(addsuffix .scad,$(MODELS))
STL_FILES    := $(addsuffix .stl,$(MODELS))
DEPENDS := $(shell find . -name *.ml -type f)

%.scad: $(DEPENDS) Makefile
	dune exec model

%.stl: %.scad
	openscad $< -o $@

.PHONY: preview
preview: $(SCAD_SOURCES)

.PHONY: rendering
rendering: $(STL_FILES)
