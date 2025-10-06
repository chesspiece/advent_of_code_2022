FROM debian:trixie-slim

ENV LANG C.UTF-8

ARG GHC_VERSION=recommended
ARG CABAL_VERSION=recommended
ARG HLS_VERSION=recommended
# probably should be the same as host user UID
ARG USER_UID=1000
# probably should be the same as host user GID
ARG USER_GID=1000
ARG USERNAME=vscode

ENV USERNAME=${USERNAME} \
    USER_UID=${USER_UID} \
    USER_GID=${USER_GID} \
    DEBIAN_FRONTEND=noninteractive \
    GHC_VERSION=${GHC_VERSION} \
    CABAL_VERSION=${CABAL_VERSION} \
    HLS_VERSION=${HLS_VERSION}

RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates apt-utils bash sudo git && \
    apt-get install -y --no-install-recommends  zlib1g-dev  && \
    apt-get install -y --no-install-recommends  build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 pkg-config

RUN groupadd --gid ${USER_GID} ${USERNAME} && \
    useradd -ms /bin/bash -K MAIL_DIR=/dev/null --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} && \
    echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USER_UID}:${USER_GID}
WORKDIR /home/${USERNAME}
ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:$PATH"

RUN echo "export PATH=${PATH}" >> /home/${USERNAME}/.profile

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=yes \
    BOOTSTRAP_HASKELL_NO_UPGRADE=yes

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN ghcup install ghc ${GHC_VERSION} --set
RUN ghcup install cabal ${CABAL_VERSION} --set
RUN ghcup install hls ${HLS_VERSION} --set

RUN cabal update

# Configure cabal
RUN cabal user-config update -f && \
    sed -i 's/-- ghc-options:/ghc-options: -haddock/g' ~/.cabal/config

RUN cabal install --haddock-hoogle --minimize-conflict-set \
    ghci-dap \
    haskell-debug-adapter \
    hlint \
    hoogle \
    fourmolu

# Generate hoogle db
RUN hoogle generate --download --haskell

ENV DEBIAN_FRONTEND=dialog

ENTRYPOINT ["/bin/bash"]