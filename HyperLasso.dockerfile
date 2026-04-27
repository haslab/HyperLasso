# --- Stage 1: Spot Builder ---
FROM ubuntu:24.04 AS spot-builder
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl build-essential python3-dev libgmp3-dev ca-certificates
WORKDIR /build
COPY docker/spot-2.14.tar.gz .
RUN tar -xvf spot-2.14.tar.gz && \
    cd spot-2.14 && \
    ./configure --prefix=/usr/local && \
    make -j$(nproc) && \
    make install

# --- Stage 2: AutoHyper Builder ---
FROM mcr.microsoft.com/dotnet/sdk:8.0-noble AS dotnet-builder
WORKDIR /AutoHyper
COPY docker/AutoHyper .
RUN cd src/AutoHyper && \
    dotnet build -c "release" -o /AutoHyper/app -p:DOTNET_VERSION=net8

# --- Stage 3: Haskell Builder ---
FROM ubuntu:24.04 AS haskell-builder
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.10.3
ENV PATH=/root/.ghcup/bin/:/root/.cabal/bin/:${PATH}
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl build-essential libgmp3-dev libffi-dev libncurses-dev ca-certificates
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
WORKDIR /HyperLasso
COPY docker/lts-24.38+sbv.config lts-24.38+sbv.config
COPY HyperLasso.cabal LICENSE cabal.project ./
COPY deps ./deps
COPY src ./src
RUN cabal update && \
    cabal install --installdir=/build-bin --install-method=copy --overwrite-policy=always --ghc-options="-O2"

# --- Final Runtime Stage ---
FROM ubuntu:24.04
USER root

# Install only runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    xz-utils \
    python3 \
    python3-pip \
    libgmp3-dev \
    openjdk-17-jre-headless \
    z3 \
    dotnet-runtime-8.0 \
    qemu-user \
    qemu-user-static \
    binfmt-support \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set up Paths
ENV PYTHONPATH=/usr/local/lib/python3.12/site-packages/
ENV PATH=/usr/local/bin:/AutoHyper/app:${PATH}
ENV PIP_BREAK_SYSTEM_PACKAGES=1

# Copy compiled artifacts from builders
COPY --from=spot-builder /usr/local /usr/local
COPY --from=dotnet-builder /AutoHyper/app /AutoHyper/app
COPY --from=haskell-builder /build-bin/HyperLasso /usr/local/bin/HyperLasso
RUN ldconfig
ENV LD_LIBRARY_PATH=/usr/local/lib:${LD_LIBRARY_PATH}

# Copy pre-compiled binaries and other assets
COPY docker/paths.json /AutoHyper/app/paths.json
COPY docker/AutoHyper/app/*.jar usr/local/bin/
COPY docker/nuXmv/nuXmv usr/local/bin/nuXmv
COPY docker/nuXmv-2.1.0-linux64.tar.xz /
RUN cd / && tar -xvf nuXmv-2.1.0-linux64.tar.xz nuXmv-2.1.0-linux64/ && rm -rf nuXmv-2.1.0-linux64.tar.xz

# Install remaining Python dependencies
RUN pip install --no-cache-dir fire

WORKDIR /HyperLasso