FROM ubuntu:18.10

RUN apt-get update && apt-get install -y\
    build-essential \
    curl \
    libffi-dev \
    libgmp-dev \
    xz-utils \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

ARG STACK_DIR="/usr/local/bin"

RUN mkdir -p ${STACK_DIR}
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz \
  | tar xz --wildcards --strip-components=1 -C ${STACK_DIR} '*/stack'
ENV PATH="${STACK_DIR}:${PATH}"

# workaround described in
# https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack
RUN (cd /usr/lib/gcc/x86_64-linux-gnu/8  \
    && mv crtbeginS.o crtbeginT.o --backup --suffix=.orig)
