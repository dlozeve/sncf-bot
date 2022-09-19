FROM gerbil/alpine as builder

RUN gxpkg install github.com/dlozeve/fancy

COPY . .
RUN ./build.ss lib
RUN gxc -exe -static -cc-options -static -ld-options -lz -o server server.ss

FROM alpine:latest

RUN adduser -D appuser

# Copy the server binary
COPY --from=builder /src/server /

USER appuser
WORKDIR /home/appuser

EXPOSE 8080
CMD ["/server"]
