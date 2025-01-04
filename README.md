# Hakyll Blog Project

A static blog site built with Hakyll, containerized using Docker.

## Prerequisites

- [Docker](https://www.docker.com/get-started)
- [Make](https://www.gnu.org/software/make/)

## Makefile Targets

### Build Docker Image

Build the Docker image for the project.

```sh
make build-image
```

### Build the Site

Compile the entire site into the `_site` directory.

```sh
make build-site
```

### Watch the Site

Start a development server with hot reload. You can view the site at http://localhost:8080.

```sh
make watch-site
```