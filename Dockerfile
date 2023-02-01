FROM fpco/stack-build:lts-12.26 AS gonito-build

MAINTAINER Filip Gralinski <filipg@ceti.pl>

WORKDIR /root/

RUN apt-get -y update && apt-get -y install libfcgi-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

COPY stack.yaml gonito.cabal /root/pre-build/

COPY geval /root/pre-build/geval

WORKDIR /root/pre-build

RUN ls /root

RUN stack upgrade

RUN stack --version

RUN stack build --system-ghc --only-dependencies

COPY . /root/gonito/

WORKDIR /root/gonito

RUN stack install --system-ghc && rm -rf /root/.stack

FROM ubuntu:18.04 AS gonito-run

RUN apt-get -y update && apt-get -y install git git-annex libcairo2 libpq5 locales && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
        locale-gen

ENV LANG=en_US.UTF-8 \ LANGUAGE=en_US \ LC_ALL=en_US.UTF-8

RUN mkdir /gonito

WORKDIR /gonito

COPY --from=gonito-build /root/.local/bin/gonito-bin ./

COPY --from=gonito-build /root/gonito/static static

COPY --from=gonito-build /root/gonito/config config

CMD ./gonito-bin
