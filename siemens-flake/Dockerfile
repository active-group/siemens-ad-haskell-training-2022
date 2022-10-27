FROM ubuntu:latest

RUN apt-get update  && apt-get install -y gcc
COPY src src
RUN gcc src/main.c && mv a.out hello-siemens

CMD ./hello-siemens
