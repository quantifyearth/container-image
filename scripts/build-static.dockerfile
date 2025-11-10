FROM alpine:3.22.0 AS builder

RUN apk update && apk add \
    build-base \
    musl-dev \
    pkgconf \
    linux-headers \
    gmp-dev \
    gmp-static \
    curl \
    git \
    bash \
    ;

# Install Dune
RUN curl -4fsSL https://github.com/ocaml-dune/dune-bin-install/releases/download/v3/install.sh | sh -s 3.20.2 --install-root /usr --no-update-shell-config

RUN mkdir /app
WORKDIR /app
COPY --chmod=0755 src src
COPY --chmod=0755 bin bin
COPY --chmod=0755 dune.lock dune.lock
COPY --chmod=0755 dune-project container-image.opam .
ENV DUNE_PROFILE=static

RUN dune build @install --only-packages container-image --display=short
RUN mkdir /out
RUN dune install --prefix=/out container-image

FROM scratch
COPY --from=builder /out .
