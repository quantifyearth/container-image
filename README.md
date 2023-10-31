# container-image - Manage OCI and Docker Images in OCaml

The `container-image` package provides a straightforward OCaml
interface for interacting with OCI and Docker image specifications. It
also provide a CLI tool (named `container-image) that allows users to
fetch image layers or inspect image contents on your filesystem.

## Features

- [x] An OCaml API to manage OCI and Docker images
- [x] Fetch layers of an OCI or Docker image.
- [ ] Inspect the contents of an image on the local filesystem,
  complete with a git history for easy diff inspection between layers.

## Installation

### From Source

```bash
git clone https://github.com/your-repo/container-image.git
cd container-image
opam install . --deps-only
dune build @install
```

### Using OPAM (When available)

```bash
opam install container-image
```

## Usage

### Fetching Image Layers

To fetch the layers of an image:

```bash
container-image fetch IMAGE_NAME[:TAG]
```

This command downloads the image layers to the current directory. By
default TAG is `latest`.

### Checking Out Image Contents

To inspect an image's contents on the local filesystem:

```bash
container-image checkout [TAG]
```

After running this command, you'll find the image's contents extracted
to the current directory. Importantly, this checkout will include a
git history, allowing you to seamlessly inspect the differences
between layers.

## Documentation

For an in-depth guide on the `container-image` commands and the
underlying OCaml API, check out the [official
documentation](link-to-docs).

## Contributing

Contributions to the `container-image` project are welcome!

## License

This project is licensed under the MIT License. See
[LICENSE](link-to-license-file) for more details.
