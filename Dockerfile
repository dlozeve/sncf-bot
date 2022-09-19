FROM gerbil/alpine

RUN gxpkg install github.com/dlozeve/fancy

COPY . .
RUN ./build.ss lib
RUN ./build.ss server

EXPOSE 8080
CMD ["/src/server"]
