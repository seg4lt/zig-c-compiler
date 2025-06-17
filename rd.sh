#!/bin/sh
# Run inside of docker

docker-compose exec app bash -c "zig build && ./zig-out/bin/zcc -- ./c/main.c $1"
