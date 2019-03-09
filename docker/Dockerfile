FROM ubuntu:18.04

RUN apt-get update && \
	apt-get install -y software-properties-common && \
	add-apt-repository ppa:openscad/releases && \
	apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y openscad curl ocaml zip git

ENTRYPOINT ["/bin/bash"]
