-record(titles, {
    literals :: gleam@dict:dict(binary(), birdie@internal@titles:test_info()),
    prefixes :: trie:trie(binary(), birdie@internal@titles:test_info())
}).
