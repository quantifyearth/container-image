# need to use alpine 3.22 as 3.23 breaks static binaries by enabling PIE
# https://discuss.ocaml.org/t/segfaults-on-static-compilation-with-alpine-3-23-fix-no-pie/17800
FROM alpine:3.22 AS builder

RUN apk update && \
    apk add \
    build-base \
    musl-dev \
    pkgconf \
    linux-headers \
    gmp-dev \
    gmp-static \
    curl \
    git \
    bash \
    && \
    adduser -D build

USER build

# install and set up dune
RUN curl -fsSL https://get.dune.build/install | sh -s - --release latest
ENV PATH=/home/build/.local/bin:$PATH
ENV DUNE_PROFILE=static

# set up build environment
WORKDIR /home/build/source
COPY --chmod=0755 src src
COPY --chmod=0755 bin bin
COPY --chmod=0755 dune.lock dune.lock
COPY --chmod=0755 dune-project container-image.opam .

# build
RUN dune build @install --only-packages container-image --display=short
RUN mkdir /home/build/out
RUN dune install --prefix=/home/build/out container-image

# copy artifacts to output
FROM scratch
COPY --from=builder /home/build/out .
