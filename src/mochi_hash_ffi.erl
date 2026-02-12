-module(mochi_hash_ffi).
-export([sha256_hex/1]).

%% Compute SHA256 hash and return as lowercase hex string
sha256_hex(Input) when is_binary(Input) ->
    Hash = crypto:hash(sha256, Input),
    bin_to_hex(Hash).

%% Convert binary to lowercase hex string
bin_to_hex(Bin) ->
    << <<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Bin >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
