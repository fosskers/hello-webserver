# hello-webserver

Basic web server implementations in a variety of languages.

The numbers given below are rough and not hard science. They are meant mostly to
demonstrate to myself the PROs and CONs of the approaches offered by each
language / framework.

## Binary Sizes

Final sizes of optimized, stripped release binaries:

|                   | Hello World | JSON  |
|-------------------|-------------|-------|
| Haskell (Wai)     | 2.0mb       | 2.5mb |
| Haskell (Servant) | 2.2mb       | 2.7mb |
| Haskell (Yesod)   | 3.2mb       | 3.6mb |
| Rust (Hyper)      | 1.6mb       | 1.7mb |
| Rust (Warp)       | 1.6mb       | 1.8mb |
| Go                | 5.2mb       | 5.5mb |
| Go (Gin)          | 12mb        | 12mb  |

**Notes:**

- Haskell: The key to small binary sizes is the following in your `stack.yaml`:
```
ghc-options:
  $everything: -split-sections
```
- Rust: Warp is built on top of Hyper. Upgrading to Warp adds around 100
  dependencies, and yet most of that weight seems to fade away with `lto = true`
  and `strip`.
- Go: Passing `-ldflags="-s -w"` or just running `strip` have the same effect
  and don't compound.

## Code Length

All values are "significant lines of code", so blank lines and comments are not
included. No attempt at code-golfing to achieve smaller sizes was done.

|                   | Hello World | JSON |
|-------------------|-------------|------|
| Haskell (Wai)     |          13 |   53 |
| Haskell (Servant) |          13 |   41 |
| Haskell (Yesod)   |          12 |   43 |
| Rust (Hyper)      |          18 |   66 |
| Rust (Warp)       |           6 |   39 |
| Go                |          13 |   53 |
| Go (Gin)          |          10 |   47 |

## Performance

Throughput via the JSON server.

The numbers below are rough measurements. The code is idiomatic, and no attempts
at hand-optimization nor profiling have been done. This should give a fair view
of "off-the-shelf" performance.

All servers were tested with the following command:

```
echo "POST http://127.0.0.1:8080/" | vegeta attack -duration=60s -rate=0 -max-workers=4 -body=test.json -header="Content-Type: application/json" | vegeta report
```

- Throughput: Higher is better.
- Values in parens for Haskell are when `+RTS -A64M -H1G` is given, altering the
  GC behaviour of the runtime.

|                   |    Throughput |
|-------------------|---------------|
| Haskell (Wai)     | 10351 (15142) |
| Haskell (Servant) |  9438 (13539) |
| Haskell (Yesod)   |   5422 (6859) |
| Rust (Hyper)      |         27703 |
| Rust (Warp)       |         23808 |
| Go                |         18349 |
| Go (Gin)          |         18274 |

**Notes:**

- Haskell: By default Yesod prints a lot of log messages to the console, so that
  probably affected performance. [It isn't
  clear](https://discourse.haskell.org/t/how-to-disable-logging-per-request-in-yesod/686)
  how to turn them off.
- Go: Gin code is a bit shorter, but balloons the binary size without offering
  better performance.
