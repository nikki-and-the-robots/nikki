FROM ubuntu:16.04

# install system dependencies
RUN apt-get update && apt-get install --yes \
  cmake curl g++ libopenal-dev libqt4-dev libsndfile1-dev libzip-dev pkg-config

# install stack and ghc
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup --resolver=lts-3.22
RUN stack update --resolver=lts-3.22

# install haskell dependencies
WORKDIR /root/nikki/src/
ADD src/stack.yaml src/package.yaml /root/nikki/src/
ADD src/scripts /root/nikki/src/scripts
ADD src/level-server /root/nikki/src/level-server
RUN stack build --test --only-dependencies
ADD src/linuxDeploy.hs /root/nikki/src/
RUN ./linuxDeploy.hs --help ; true

# build nikki
ADD src /root/nikki/src
ADD data /root/nikki/data
ADD deploymentLicenses /root/nikki/deploymentLicenses
RUN ./build-qtwrapper.sh
RUN stack build --flag nikki:-devel
RUN ./linuxDeploy.hs
