FROM ubuntu:18.04

RUN apt-get update && apt-get install -y\
    build-essential \
    curl \
    libffi-dev \
    libgmp-dev \
    libtinfo-dev \
    xz-utils \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz \
  | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
