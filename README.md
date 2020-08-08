# hello-webserver

Basic web server implementations in a variety of languages.

The numbers given below are rough and not hard science. They are meant mostly to
demonstrate to myself the PROs and CONs are the approaches offered by each
language / framework.

## Binary Sizes

Final sizes of optimized, stripped release binaries:

|                   | Hello World | JSON  |
|-------------------|-------------|-------|
| Haskell (Wai)     | 2.0mb       | 2.5mb |
| Haskell (Servant) | 2.2mb       | 2.7mb |
| Rust (Hyper)      | 1.6mb       | 1.7mb |
| Rust (Warp)       | 1.6mb       | 1.8mb |
| Go                | 5.2mb       | 5.5mb |

## Code Length

All values are "significant lines of code", so blank lines and comments are not
included. No attempt at code-golfing to achieve smaller sizes was done.

|                   | Hello World | JSON |
|-------------------|-------------|------|
| Haskell (Wai)     |          13 |   53 |
| Haskell (Servant) |          13 |   41 |
| Rust (Hyper)      |          18 |   66 |
| Rust (Warp)       |           6 |   39 |
| Go                |          13 |   53 |

## Performance

The numbers below are rough measurements. No profiling or attempts at
hand-optimization have been done.

All servers were tested with the following command:

```
echo "POST http://127.0.0.1:8080/" | vegeta attack -duration=60s -rate=0 -max-workers=4 -body=test.json -header="Content-Type: application/json" | vegeta report
```

- Throughput: Higher is better.
- Values in parens for Haskell are when `+RTS -A64M -H1G` is given, altering the
  GC behaviour of the runtime.

|                   |  Throughput |
|-------------------|-------------|
| Haskell (Wai)     | 5879 (9819) |
| Haskell (Servant) | 5231 (8966) |
| Rust (Hyper)      |       15900 |
| Rust (Warp)       |       15001 |
| Go                |       11094 |
