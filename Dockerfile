FROM rust:1.88-slim

ARG BAT_VERSION
ENV BAT_VERSION=${BAT_VERSION}

RUN apt-get update && apt-get install -y wget tar cargo build-essential && \
    rm -rf /var/lib/apt/lists/*

RUN wget https://github.com/sharkdp/bat/archive/refs/tags/${BAT_VERSION}.tar.gz && \
    tar -xzf ${BAT_VERSION}.tar.gz && \
    cd bat-${BAT_VERSION#v} && \
    cargo build --release && \
    mv target/release/bat /usr/local/bin/ && \

    cd .. && rm -rf bat-${BAT_VERSION#v} ${BAT_VERSION}.tar.gz

ENTRYPOINT ["bat"]
