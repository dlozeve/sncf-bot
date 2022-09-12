FROM gerbil/alpine

RUN gxpkg install github.com/dlozeve/fancy

COPY . .
RUN ./build.ss
RUN gxc -exe -static -cc-options -static -ld-options -lz -o sncf-static sncf.ss
