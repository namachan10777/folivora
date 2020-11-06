# Add output models
MODELS := thumb top bottom

SCAD_SOURCES := $(addsuffix .scad,$(MODELS))
STL_FILES    := $(addsuffix .stl,$(MODELS))
DEPENDS := $(shell find . -name *.ml -type f)

.PHONY: preview
preview: $(SCAD_SOURCES)

.PHONY: rendering
rendering: $(STL_FILES)

.PHONY: archive
archive: archive.tar.gz

.PHONY: clean
clean:
	rm -f *.stl
	rm -f *.zst
	rm -fr _build/

%.scad: $(DEPENDS) Makefile
	dune exec model

%.stl: %.scad
	openscad $< -o $@

archive.tar.gz: $(STL_FILES)
	tar --zstd -cvf archive.tar.zst $(STL_FILES)


