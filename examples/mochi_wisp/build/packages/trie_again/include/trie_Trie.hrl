-record(trie, {
    entry :: gleam@option:option(any()),
    children_map :: gleam@dict:dict(any(), trie:trie(any(), any()))
}).
