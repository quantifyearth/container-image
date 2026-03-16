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

#### Using OPAM

```bash
git clone https://github.com/your-repo/container-image.git
cd container-image
opam install . --deps-only
dune build @install
```

#### Using Dune package management

```bash
git clone https://github.com/your-repo/container-image.git
cd container-image
dune build @install --pkg enabled
```

For information on how to develop with Dune package management alongside Opam,
please refer to the [How to Use Opam Alongside Dune Package
Management](https://dune.readthedocs.io/en/stable/howto/use-opam-alongside-dune-package-management.html)
documentation in Dune.

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

### Creating prebuilt binaries for releases

This project comes with GitHub Actions which will automatically build binaries
upon release. Currently supported platforms are:

  * macOS on AMD64
  * macOS on ARM64
  * Linux on AMD64

To create the binaries create a GitHub release (either manually or via helper
tools like `dune-release`). This will trigger a GitHub Action which will check
out the revision linked with the release, build it on the specified platform
and upload the binaries to the GitHub release automatically. This process takes
a few minutes, depending on how fast the GitHub runners are and can be tracked
in the "Actions" tab of the project.

### Updating the dependency versions

The project contains a lock directory for Dune package management in the
`dune.lock` folder. This means that whenever you use Dune with the package
management feature enabled it will use the exact versions of the compiler as
well as the projects dependencies as specified in `dune.lock`.

However dependencies might need to change in some cases:

  * Updating the dependencies to new versions
  * Adding or removing dependencies
  * Changing the version constraints of existing packages

In such case `dune` will detect that the dependencies declared in the opam file
and `dune.lock` are out of sync. To update the dependencies use `dune pkg lock`
which will re-create a new `dune.lock` directory with the newest dependency
solution.

```bash
$ dune pkg lock
$ git add -A dune.lock
$ git commit
```

The dependency solver in Dune might add or remove files in `dune.lock` thus it
is advisable to use `git add -A` to add all the changes to the index to commit
the change. This avoids missing or duplicate files.

## License

This project is licensed under the MIT License. See
[LICENSE](link-to-license-file) for more details.
