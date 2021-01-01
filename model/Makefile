# Add output models
MODELS := thumb top bottom

SCAD_SOURCES := $(addsuffix .scad,$(MODELS))
STL_FILES    := $(addsuffix .stl,$(MODELS))
DEPENDS := $(shell find . -name *.ml -type f)
MODEL_BIN := _build/install/default/bin/model

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
	rm -f *.scad
	rm -fr _build/

$(MODEL_BIN): $(DEPENDS) Makefile
	dune build

$(SCAD_SOURCES): $(MODEL_BIN) Makefile
	./$(MODEL_BIN)

%.stl: %.scad Makefile
	openscad $< -o $@

archive.tar.gz: $(STL_FILES) Makefile
	tar --zstd -cvf archive.tar.zst $(STL_FILES)


