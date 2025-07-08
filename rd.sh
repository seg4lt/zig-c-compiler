#!/bin/sh


if [ -n "$TC" ]; then
  echo "Running tests..."
  docker-compose exec app bash -c "zig build && tests/test_compiler ./zig-out/bin/zcc --verbose --failfast $1"
  exit 0
fi

if [ -n "$RUN" ]; then
  echo "Running ./c/main.c"
  docker-compose exec app bash -c "zig build && ./zig-out/bin/zcc -- ./c/main.c && ./c/main"
  status=$?
  exit $status
fi

echo "Compiling ./c/main.c"
docker-compose exec app bash -c "zig build && ./zig-out/bin/zcc -- ./c/main.c $1"
