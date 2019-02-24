FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository ppa:openscad/releases
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y openscad

ENTRYPOINT ["/bin/bash"]
