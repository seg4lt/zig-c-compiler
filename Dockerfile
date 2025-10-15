FROM ubuntu:latest

RUN apt update

RUN DEBIAN_FRONTEND=noninteractive apt install gcc make python3 binutils curl ripgrep xz-utils tini -y && \
  curl -LO https://ziglang.org/download/0.15.2/zig-x86_64-linux-0.15.2.tar.xz

RUN tar -xf zig-x86_64-linux-0.15.2.tar.xz && \
    ln -sfn /zig-x86_64-linux-0.15.2/zig /usr/bin/zig

ENTRYPOINT ["/usr/bin/tini", "--"]

CMD ["tail", "-f", "/dev/null"]
