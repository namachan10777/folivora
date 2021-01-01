ARG BASE_IMAGE="ubuntu"
ARG BASE_TAG="20.04"

FROM $BASE_IMAGE:$BASE_TAG

ARG OPAM_VERSION="2.0.7"
ARG OCAML_VERSION="4.10.0"
ENV OCAML_VERSION $OCAML_VERSION
ENV DEBIAN_FRONTEND=noninteractive

# install dependencies
RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y openscad curl zip git make m4 zstd gcc
# install opam 
RUN curl -sSL https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-$(uname -m)-$(uname -s) > /usr/local/bin/opam && \
	chmod 755 /usr/local/bin/opam
# change user
RUN useradd -m folivora
USER folivora

RUN opam init -a -y --comp=$OCAML_VERSION --disable-sandboxing
RUN opam install -y dune core
WORKDIR /tmp/
RUN git clone https://github.com/namachan10777/scad-ml
WORKDIR scad-ml
RUN eval $(opam config env) && \
	dune build && \
	opam pin add --yes .

ENTRYPOINT ["/bin/bash"]
