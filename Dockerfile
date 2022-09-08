FROM gerbil/alpine

RUN gxpkg install github.com/dlozeve/fancy

COPY sncf.ss .
RUN gxc -exe -static -cc-options -static -ld-options -lz -o sncf sncf.ss
