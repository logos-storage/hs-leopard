Leopard fast erasure coding library
-----------------------------------

This is a Haskell binding to the ["Leopard" erasure coding library](https://github.com/catid/leopard)
by Christopher A. Taylor.

### What's this about?

Erasure coding allows you to reconstruct a redundantly encoded data even if some
pieces are missing. For example if you encode a piece of data with 10-out-of-15 
encoding (usually denoted by `K=10` and `N=15`), then the data is chunked into 15
pieces, and any 10 pieces (together with their index in 1..15) can reconstruct
the original data. 

This is very useful for example when dealing with unreliable networks.

Leopard uses [Reed-Solomon code](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction) 
over binary fields `GF(2^8)` or `GF(2^16)` and low-level optimizations to achieve 
high performance.

Reed-Solomon codes also guarantee that any `K` out of `N` pieces can recover the
data, where `K` pieces have exactly the size of the original data (however you also need
the additional information of which available piece is which one out of the `N`).

### Standard notations

The encoding algorithm is called the "code". The original data is chunked into 
`K >= 1` pieces. This is then encoded into `N > K` redundant pieces. The ratio 
`rho = K / N < 1` is called the "rate" of the code. The expansion factor `1 / rho = N / K`
is the redundancy overhead. Leopard only supports `1/2 <= rho < 1`, that is,
the encoded data is at most twice the size of the original data.

Leopard uses a so-called "systematic code", which means that the first `K` pieces
is simply the original data. The notation `M = N - K` for the number of the remaining,
"parity" pieces is also standard.

Internally, Leopard encodes `K` 8 or 16 bit words ("symbols") into `N` words. By
partitioning the original dataset into sets of `K` bytes (or 16 bit words), we can 
trivially recover the above semantics.

### Limitations

Leopard itself has some limitations on the parameters:

- `K >= 2`
- `M <= K`
- `N = K + M <= 65536`
- the chunk size must by divisible by 64 bytes.

### Compatibility

I have not much experience about linking C++ with Haskell. This was tested only
on a single ARM-based computer running macOS.

