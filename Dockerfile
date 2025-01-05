# Stage 1: Builder
FROM haskell:9.4 AS builder

# Install system dependencies required by Hakyll
RUN apt-get update && apt-get install -y \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# Enable BuildKit cache mounts
# (Ensure BuildKit is enabled externally)

# Set the working directory inside the container
WORKDIR /app

# Copy only the cabal file first to leverage Docker cache
COPY blog.cabal ./

# Use cache mounts for Cabal, GHC, and dist-newstyle
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/root/.ghc \
    --mount=type=cache,target=/app/dist-newstyle \
    cabal update && \
    cabal build --only-dependencies

# Now copy the rest of the source code
COPY site.hs ./

# Build and install the executable to /usr/local/bin with additional cache mounts
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/root/.ghc \
    --mount=type=cache,target=/app/dist-newstyle \
    cabal install --overwrite-policy=always --install-method=copy --installdir=/usr/local/bin/

# Stage 2: Final Image
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory in the final image
WORKDIR /app

# Copy the built executable from the builder stage
COPY --from=builder /usr/local/bin/site /usr/local/bin/site

# Set the entrypoint to the site executable
ENTRYPOINT ["site"]