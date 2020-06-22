ARG BASE_IMAGE="ubuntu"
ARG BASE_TAG="18.04"

FROM $BASE_IMAGE:$BASE_TAG

ARG OPAM_VERSION="2.0.7"
ARG OCAML_VERSION="4.10.0"
ENV OCAML_VERSION $OCAML_VERSION

# install dependencies
RUN apt-get update && \
	apt-get install -y software-properties-common && \
	add-apt-repository ppa:openscad/releases && \
	apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y openscad curl zip git make m4 zstd gcc
# install opam 
RUN curl -sSL https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-$(uname -m)-$(uname -s) > /usr/local/bin/opam && \
	chmod 755 /usr/local/bin/opam
# change user
RUN useradd -m folivora
USER folivora

RUN opam init -a -y --comp=$OCAML_VERSION --disable-sandboxing
RUN echo "eval $(opam config env)" >> ~/.bashrc

ENTRYPOINT ["/bin/bash"]
