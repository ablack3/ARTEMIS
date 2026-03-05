# ARTEMIS Docker Image

This folder contains the Docker setup for the ARTEMIS RStudio image.

## Files

- `Dockerfile`: single multi-architecture Dockerfile (supports `linux/amd64` and `linux/arm64`).

## GitHub Actions publish to Docker Hub

Workflow: `.github/workflows/build-rstudio-image.yml`  
Trigger: manual (`workflow_dispatch`)

Set these repository secrets:

- `DOCKERHUB_USER`: your Docker Hub username
- `DOCKERHUB_PAT`: Docker Hub personal access token

Optional repository variable:

- `DOCKERHUB_REPO`: target Docker Hub repo name (default: `artemis-rstudio`)

The workflow builds and pushes a multi-arch manifest for:

- `linux/amd64`
- `linux/arm64`

## Pull the built image

Replace values as needed:

```bash
docker pull <DOCKERHUB_USER>/<DOCKERHUB_REPO>:latest
```

Example:

```bash
docker pull myuser/artemis-rstudio:latest
```

## Run locally

Open RStudio at [http://127.0.0.1:8787](http://127.0.0.1:8787)

```bash
docker run --rm -p 8787:8787 myuser/artemis-rstudio:latest
```

Credentials:

- Username: `rstudio`
- Password: `artemis`

Notes:

- The image sets `WORKDIR` to `/home/rstudio/ARTEMIS`.
- This image is intended for development and convenience, not hardened production security.

## Build locally (optional)

Build for current architecture:

```bash
docker build -f docker/Dockerfile -t artemis-rstudio:local .
```

Build specific architecture:

```bash
docker buildx build --platform linux/amd64 -f docker/Dockerfile -t artemis-rstudio:amd64 .
docker buildx build --platform linux/arm64 -f docker/Dockerfile -t artemis-rstudio:arm64 .
```

## Offline transfer via USB (tarball workflow)

1. Save image to tarball on online machine:

```bash
docker save -o /path/to/usb/artemis-rstudio-latest.tar myuser/artemis-rstudio:latest
```

2. Move USB to offline machine with Docker installed.
3. Load image on offline machine:

```bash
docker load -i /path/to/usb/artemis-rstudio-latest.tar
```

4. Run on offline machine:

```bash
docker run --rm -p 8787:8787 myuser/artemis-rstudio:latest
```

Login credentials remain:

- Username: `rstudio`
- Password: `artemis`
