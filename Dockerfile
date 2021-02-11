FROM fpco/stack-build:lts-16.8 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# end of dependencies
FROM fpco/stack-build:lts-16.8 as build
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# end of build

FROM ubuntu:20.04 as app
RUN mkdir -p /opt/data-scout
WORKDIR /opt/data-scout

# I want to move this command into dependencies and
# only copy over what I need to
ENV TZ=America/Los_Angeles
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update && apt-get install -y tzdata

RUN apt-get install postgresql -y && apt-get install ca-certificates -y



# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY static /opt/data-scout/static
COPY config /opt/data-scout/config
COPY entrypoint /opt/data-scout
COPY --from=build /opt/build/bin .


# This is the port we set in settings.yaml
EXPOSE 80
