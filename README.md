# A Haskell Library for Google Protocol Buffers

This library provides the ability to generate encoders and decoders
from [Google Protocol Buffer][pb] specifications.  The code will parse
a proto2 files and generate Haskell source code containing the encoder
and decoder.  This Haskell file will depend on the ``hpb`` library
contained in this repo.

[pb]: https://developers.google.com/protocol-buffers/
