-module(glexer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glexer.gleam").
-export([new/1, discard_whitespace/1, discard_comments/1, to_source/1, lex/1, unescape_string/1]).
-export_type([lexer/0, lexer_mode/0, position/0, comment_kind/0, lex_number_mode/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque lexer() :: {lexer,
        binary(),
        binary(),
        integer(),
        boolean(),
        boolean(),
        lexer_mode(),
        splitter:splitter()}.

-type lexer_mode() :: normal |
    check_for_minus |
    check_for_nested_dot |
    check_for_nested_dot_or_minus |
    has_nested_dot.

-type position() :: {position, integer()}.

-type comment_kind() :: regular_comment | doc_comment | module_comment.

-type lex_number_mode() :: lex_int | lex_float | lex_float_exponent.

-file("src/glexer.gleam", 42).
-spec new(binary()) -> lexer().
new(Source) ->
    {lexer,
        Source,
        Source,
        0,
        true,
        true,
        normal,
        splitter:new([<<"\r\n"/utf8>>, <<"\n"/utf8>>])}.

-file("src/glexer.gleam", 54).
-spec discard_whitespace(lexer()) -> lexer().
discard_whitespace(Lexer) ->
    {lexer,
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        erlang:element(4, Lexer),
        false,
        erlang:element(6, Lexer),
        erlang:element(7, Lexer),
        erlang:element(8, Lexer)}.

-file("src/glexer.gleam", 58).
-spec discard_comments(lexer()) -> lexer().
discard_comments(Lexer) ->
    {lexer,
        erlang:element(2, Lexer),
        erlang:element(3, Lexer),
        erlang:element(4, Lexer),
        erlang:element(5, Lexer),
        false,
        erlang:element(7, Lexer),
        erlang:element(8, Lexer)}.

-file("src/glexer.gleam", 613).
-spec some_token({lexer(), {glexer@token:token(), position()}}) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
some_token(Result) ->
    {Lexer, Token} = Result,
    {Lexer, {some, Token}}.

-file("src/glexer.gleam", 884).
-spec advance(lexer(), binary(), integer()) -> lexer().
advance(Lexer, Source, Offset) ->
    {lexer,
        erlang:element(2, Lexer),
        Source,
        erlang:element(4, Lexer) + Offset,
        erlang:element(5, Lexer),
        erlang:element(6, Lexer),
        erlang:element(7, Lexer),
        erlang:element(8, Lexer)}.

-file("src/glexer.gleam", 888).
-spec advanced({glexer@token:token(), position()}, lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
advanced(Token, Lexer, Source, Offset) ->
    {advance(Lexer, Source, Offset), Token}.

-file("src/glexer.gleam", 897).
-spec token(lexer(), glexer@token:token(), binary(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
token(Lexer, Token, Source, Offset) ->
    _pipe = {Token, {position, erlang:element(4, Lexer)}},
    _pipe@1 = advanced(_pipe, Lexer, Source, Offset),
    some_token(_pipe@1).

-file("src/glexer.gleam", 537).
-spec check_for_minus(lexer()) -> {ok,
        {lexer(), gleam@option:option({glexer@token:token(), position()})}} |
    {error, nil}.
check_for_minus(Lexer) ->
    case erlang:element(3, Lexer) of
        <<"-"/utf8, Source/binary>> ->
            {Lexer@1, Token} = token(Lexer, minus, Source, 1),
            {ok,
                {{lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        normal,
                        erlang:element(8, Lexer@1)},
                    Token}};

        _ ->
            {error, nil}
    end.

-file("src/glexer.gleam", 551).
-spec check_for_nested_dot(lexer()) -> {ok,
        {lexer(), gleam@option:option({glexer@token:token(), position()})}} |
    {error, nil}.
check_for_nested_dot(Lexer) ->
    case erlang:element(3, Lexer) of
        <<".."/utf8, Source/binary>> ->
            {ok, token(Lexer, dot_dot, Source, 2)};

        <<"."/utf8, Source@1/binary>> ->
            case Source@1 of
                <<"0"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"1"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"2"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"3"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"4"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"5"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"6"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"7"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"8"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                <<"9"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok,
                        {{lexer,
                                erlang:element(2, Lexer@1),
                                erlang:element(3, Lexer@1),
                                erlang:element(4, Lexer@1),
                                erlang:element(5, Lexer@1),
                                erlang:element(6, Lexer@1),
                                has_nested_dot,
                                erlang:element(8, Lexer@1)},
                            Token}};

                _ ->
                    {ok, token(Lexer, dot, Source@1, 1)}
            end;

        _ ->
            {error, nil}
    end.

-file("src/glexer.gleam", 875).
?DOC(" Turn a sequence of tokens back to their Gleam source code representation.\n").
-spec to_source(list({glexer@token:token(), position()})) -> binary().
to_source(Tokens) ->
    gleam@list:fold(
        Tokens,
        <<""/utf8>>,
        fun(Source, _use1) ->
            {Tok, _} = _use1,
            <<Source/binary, (glexer@token:to_source(Tok))/binary>>
        end
    ).

-file("src/glexer.gleam", 913).
-spec length(binary()) -> integer().
length(String) ->
    erlang:byte_size(String).

-file("src/glexer.gleam", 604).
?DOC(
    " Ignores the rest of the line until it finds a newline, and signals the\n"
    " caller to continue lexing.\n"
).
-spec skip_comment(lexer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
skip_comment(Lexer) ->
    {Prefix, Suffix} = splitter_ffi:split_before(
        erlang:element(8, Lexer),
        erlang:element(3, Lexer)
    ),
    Eaten = length(Prefix),
    Lexer@1 = advance(Lexer, Suffix, Eaten),
    {Lexer@1, none}.

-file("src/glexer.gleam", 620).
-spec comment(lexer(), comment_kind(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
comment(Lexer, Kind, Start) ->
    {Prefix, Suffix} = splitter_ffi:split_before(
        erlang:element(8, Lexer),
        erlang:element(3, Lexer)
    ),
    Eaten = length(Prefix),
    Lexer@1 = advance(Lexer, Suffix, Eaten),
    Token = case Kind of
        module_comment ->
            {comment_module, Prefix};

        doc_comment ->
            {comment_doc, Prefix};

        regular_comment ->
            {comment_normal, Prefix}
    end,
    {Lexer@1, {some, {Token, {position, Start}}}}.

-file("src/glexer.gleam", 387).
-spec lex_digits(lexer(), integer(), integer()) -> {lexer(), binary()}.
lex_digits(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"2"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"3"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"4"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"5"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"6"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"7"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"8"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        <<"9"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_digits(_pipe, Start, Slice_size + 1);

        _ ->
            Digits = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, Digits}
    end.

-file("src/glexer.gleam", 408).
-spec lex_lowercase_name(lexer(), integer(), integer()) -> {lexer(), binary()}.
lex_lowercase_name(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"a"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"b"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"c"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"d"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"e"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"f"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"g"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"h"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"i"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"j"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"k"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"l"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"m"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"n"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"o"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"p"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"q"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"r"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"s"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"t"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"u"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"v"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"w"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"x"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"y"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"z"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"2"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"3"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"4"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"5"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"6"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"7"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"8"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"9"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        <<"_"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_lowercase_name(_pipe, Start, Slice_size + 1);

        _ ->
            Name = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, Name}
    end.

-file("src/glexer.gleam", 460).
-spec lex_uppercase_name(lexer(), integer(), integer()) -> {lexer(), binary()}.
lex_uppercase_name(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"a"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"b"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"c"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"d"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"e"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"f"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"g"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"h"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"i"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"j"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"k"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"l"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"m"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"n"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"o"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"p"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"q"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"r"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"s"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"t"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"u"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"v"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"w"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"x"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"y"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"z"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"A"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"B"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"C"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"D"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"E"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"F"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"G"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"H"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"I"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"J"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"K"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"L"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"M"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"N"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"O"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"P"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"Q"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"R"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"S"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"T"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"U"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"V"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"W"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"X"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"Y"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"Z"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"2"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"3"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"4"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"5"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"6"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"7"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"8"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        <<"9"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_uppercase_name(_pipe, Start, Slice_size + 1);

        _ ->
            Name = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, Name}
    end.

-file("src/glexer.gleam", 580).
-spec whitespace(lexer(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
whitespace(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<" "/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            whitespace(_pipe, Start, Slice_size + 1);

        <<"\t"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            whitespace(_pipe, Start, Slice_size + 1);

        <<"\n"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            whitespace(_pipe, Start, Slice_size + 1);

        <<"\r"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            whitespace(_pipe, Start, Slice_size + 1);

        _ ->
            case erlang:element(5, Lexer) of
                false ->
                    {Lexer, none};

                true ->
                    Content = binary:part(
                        erlang:element(2, Lexer),
                        Start,
                        Slice_size
                    ),
                    {Lexer, {some, {{space, Content}, {position, Start}}}}
            end
    end.

-file("src/glexer.gleam", 639).
-spec lex_binary(lexer(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
lex_binary(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"_"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, Start, Slice_size + 1);

        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, Start, Slice_size + 1);

        _ ->
            Content = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, {some, {{int, Content}, {position, Start}}}}
    end.

-file("src/glexer.gleam", 656).
-spec lex_octal(lexer(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
lex_octal(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"_"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"2"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"3"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"4"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"5"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"6"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        <<"7"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, Start, Slice_size + 1);

        _ ->
            Content = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, {some, {{int, Content}, {position, Start}}}}
    end.

-file("src/glexer.gleam", 681).
-spec lex_hexadecimal(lexer(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
lex_hexadecimal(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"_"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"0"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"1"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"2"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"3"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"4"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"5"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"6"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"7"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"8"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"9"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"a"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"A"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"b"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"B"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"c"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"C"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"d"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"D"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"e"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"E"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"f"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        <<"F"/utf8, Source/binary>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, Start, Slice_size + 1);

        _ ->
            Content = binary:part(erlang:element(2, Lexer), Start, Slice_size),
            {Lexer, {some, {{int, Content}, {position, Start}}}}
    end.

-file("src/glexer.gleam", 726).
-spec lex_number(lexer(), lex_number_mode(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
lex_number(Lexer, Mode, Start, Slice_size) ->
    case {erlang:element(3, Lexer), Mode} of
        {<<"_"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"0"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"1"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"2"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"3"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"4"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"5"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"6"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"7"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"8"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"9"/utf8, Source/binary>>, _} ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, Mode, Start, Slice_size + 1);

        {<<"."/utf8, Source@1/binary>>, lex_int} ->
            _pipe@1 = advance(Lexer, Source@1, 1),
            lex_number(_pipe@1, lex_float, Start, Slice_size + 1);

        {<<"e-"/utf8, Source@2/binary>>, lex_float} ->
            _pipe@2 = advance(Lexer, Source@2, 2),
            lex_number(_pipe@2, lex_float_exponent, Start, Slice_size + 2);

        {<<"e"/utf8, Source@3/binary>>, lex_float} ->
            _pipe@3 = advance(Lexer, Source@3, 1),
            lex_number(_pipe@3, lex_float_exponent, Start, Slice_size + 1);

        {_, lex_int} ->
            Lexer@1 = {lexer,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer),
                erlang:element(4, Lexer),
                erlang:element(5, Lexer),
                erlang:element(6, Lexer),
                check_for_minus,
                erlang:element(8, Lexer)},
            Content = binary:part(erlang:element(2, Lexer@1), Start, Slice_size),
            {Lexer@1, {some, {{int, Content}, {position, Start}}}};

        {_, lex_float} ->
            Lexer@2 = {lexer,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer),
                erlang:element(4, Lexer),
                erlang:element(5, Lexer),
                erlang:element(6, Lexer),
                check_for_minus,
                erlang:element(8, Lexer)},
            Content@1 = binary:part(
                erlang:element(2, Lexer@2),
                Start,
                Slice_size
            ),
            {Lexer@2, {some, {{float, Content@1}, {position, Start}}}};

        {_, lex_float_exponent} ->
            Lexer@2 = {lexer,
                erlang:element(2, Lexer),
                erlang:element(3, Lexer),
                erlang:element(4, Lexer),
                erlang:element(5, Lexer),
                erlang:element(6, Lexer),
                check_for_minus,
                erlang:element(8, Lexer)},
            Content@1 = binary:part(
                erlang:element(2, Lexer@2),
                Start,
                Slice_size
            ),
            {Lexer@2, {some, {{float, Content@1}, {position, Start}}}}
    end.

-file("src/glexer.gleam", 777).
-spec lex_string(lexer(), integer(), integer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
lex_string(Lexer, Start, Slice_size) ->
    case erlang:element(3, Lexer) of
        <<"\""/utf8, Source/binary>> ->
            Content = binary:part(
                erlang:element(2, Lexer),
                Start + 1,
                Slice_size
            ),
            _pipe = {{string, Content}, {position, Start}},
            _pipe@1 = advanced(_pipe, Lexer, Source, 1),
            some_token(_pipe@1);

        <<"\\"/utf8, Source@1/binary>> ->
            case gleam_stdlib:string_pop_grapheme(Source@1) of
                {error, _} ->
                    _pipe@2 = advance(Lexer, Source@1, 1),
                    lex_string(_pipe@2, Start, Slice_size + 1);

                {ok, {Grapheme, Source@2}} ->
                    Offset = 1 + length(Grapheme),
                    _pipe@3 = advance(Lexer, Source@2, Offset),
                    lex_string(_pipe@3, Start, Slice_size + Offset)
            end;

        <<""/utf8>> ->
            Content@1 = binary:part(
                erlang:element(2, Lexer),
                Start + 1,
                Slice_size
            ),
            {Lexer,
                {some, {{unterminated_string, Content@1}, {position, Start}}}};

        _ ->
            _pipe@4 = advance(
                Lexer,
                glexer_ffi:drop_byte(erlang:element(3, Lexer)),
                1
            ),
            lex_string(_pipe@4, Start, Slice_size + 1)
    end.

-file("src/glexer.gleam", 81).
-spec next(lexer()) -> {lexer(),
    gleam@option:option({glexer@token:token(), position()})}.
next(Lexer) ->
    case erlang:element(7, Lexer) of
        check_for_minus ->
            case check_for_minus(Lexer) of
                {ok, Result} ->
                    Result;

                {error, nil} ->
                    {{lexer,
                            erlang:element(2, Lexer),
                            erlang:element(3, Lexer),
                            erlang:element(4, Lexer),
                            erlang:element(5, Lexer),
                            erlang:element(6, Lexer),
                            normal,
                            erlang:element(8, Lexer)},
                        none}
            end;

        check_for_nested_dot ->
            case check_for_nested_dot(Lexer) of
                {ok, Result@1} ->
                    Result@1;

                {error, nil} ->
                    {{lexer,
                            erlang:element(2, Lexer),
                            erlang:element(3, Lexer),
                            erlang:element(4, Lexer),
                            erlang:element(5, Lexer),
                            erlang:element(6, Lexer),
                            normal,
                            erlang:element(8, Lexer)},
                        none}
            end;

        check_for_nested_dot_or_minus ->
            case check_for_nested_dot(Lexer) of
                {ok, Result@2} ->
                    Result@2;

                {error, nil} ->
                    case check_for_minus(Lexer) of
                        {ok, Result@3} ->
                            Result@3;

                        {error, nil} ->
                            {{lexer,
                                    erlang:element(2, Lexer),
                                    erlang:element(3, Lexer),
                                    erlang:element(4, Lexer),
                                    erlang:element(5, Lexer),
                                    erlang:element(6, Lexer),
                                    normal,
                                    erlang:element(8, Lexer)},
                                none}
                    end
            end;

        has_nested_dot ->
            case erlang:element(3, Lexer) of
                <<"0"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"1"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"2"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"3"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"4"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"5"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"6"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"7"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"8"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                <<"9"/utf8, Source/binary>> ->
                    Byte_offset = erlang:element(4, Lexer),
                    {Lexer@1, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        lex_digits(_pipe, Byte_offset, 1)
                    end,
                    Lexer@2 = {lexer,
                        erlang:element(2, Lexer@1),
                        erlang:element(3, Lexer@1),
                        erlang:element(4, Lexer@1),
                        erlang:element(5, Lexer@1),
                        erlang:element(6, Lexer@1),
                        check_for_nested_dot,
                        erlang:element(8, Lexer@1)},
                    {Lexer@2, {some, {{int, Int}, {position, Byte_offset}}}};

                _ ->
                    {{lexer,
                            erlang:element(2, Lexer),
                            erlang:element(3, Lexer),
                            erlang:element(4, Lexer),
                            erlang:element(5, Lexer),
                            erlang:element(6, Lexer),
                            normal,
                            erlang:element(8, Lexer)},
                        none}
            end;

        normal ->
            case erlang:element(3, Lexer) of
                <<" "/utf8, Source@1/binary>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, erlang:element(4, Lexer), 1);

                <<"\n"/utf8, Source@1/binary>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, erlang:element(4, Lexer), 1);

                <<"\r"/utf8, Source@1/binary>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, erlang:element(4, Lexer), 1);

                <<"\t"/utf8, Source@1/binary>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, erlang:element(4, Lexer), 1);

                <<"////"/utf8, Source@2/binary>> ->
                    case erlang:element(6, Lexer) of
                        false ->
                            skip_comment(Lexer);

                        true ->
                            _pipe@2 = advance(Lexer, Source@2, 4),
                            comment(
                                _pipe@2,
                                module_comment,
                                erlang:element(4, Lexer)
                            )
                    end;

                <<"///"/utf8, Source@3/binary>> ->
                    case erlang:element(6, Lexer) of
                        false ->
                            skip_comment(Lexer);

                        true ->
                            _pipe@3 = advance(Lexer, Source@3, 3),
                            comment(
                                _pipe@3,
                                doc_comment,
                                erlang:element(4, Lexer)
                            )
                    end;

                <<"//"/utf8, Source@4/binary>> ->
                    case erlang:element(6, Lexer) of
                        false ->
                            skip_comment(Lexer);

                        true ->
                            _pipe@4 = advance(Lexer, Source@4, 2),
                            comment(
                                _pipe@4,
                                regular_comment,
                                erlang:element(4, Lexer)
                            )
                    end;

                <<"("/utf8, Source@5/binary>> ->
                    token(Lexer, left_paren, Source@5, 1);

                <<")"/utf8, Source@6/binary>> ->
                    token(Lexer, right_paren, Source@6, 1);

                <<"{"/utf8, Source@7/binary>> ->
                    token(Lexer, left_brace, Source@7, 1);

                <<"}"/utf8, Source@8/binary>> ->
                    token(Lexer, right_brace, Source@8, 1);

                <<"["/utf8, Source@9/binary>> ->
                    token(Lexer, left_square, Source@9, 1);

                <<"]"/utf8, Source@10/binary>> ->
                    token(Lexer, right_square, Source@10, 1);

                <<"@"/utf8, Source@11/binary>> ->
                    token(Lexer, at, Source@11, 1);

                <<":"/utf8, Source@12/binary>> ->
                    token(Lexer, colon, Source@12, 1);

                <<","/utf8, Source@13/binary>> ->
                    token(Lexer, comma, Source@13, 1);

                <<".."/utf8, Source@14/binary>> ->
                    token(Lexer, dot_dot, Source@14, 2);

                <<"."/utf8, Source@15/binary>> ->
                    token(Lexer, dot, Source@15, 1);

                <<"#"/utf8, Source@16/binary>> ->
                    token(Lexer, hash, Source@16, 1);

                <<"!="/utf8, Source@17/binary>> ->
                    token(Lexer, not_equal, Source@17, 2);

                <<"!"/utf8, Source@18/binary>> ->
                    token(Lexer, bang, Source@18, 1);

                <<"=="/utf8, Source@19/binary>> ->
                    token(Lexer, equal_equal, Source@19, 2);

                <<"="/utf8, Source@20/binary>> ->
                    token(Lexer, equal, Source@20, 1);

                <<"|>"/utf8, Source@21/binary>> ->
                    token(Lexer, pipe, Source@21, 2);

                <<"||"/utf8, Source@22/binary>> ->
                    token(Lexer, v_bar_v_bar, Source@22, 2);

                <<"|"/utf8, Source@23/binary>> ->
                    token(Lexer, v_bar, Source@23, 1);

                <<"&&"/utf8, Source@24/binary>> ->
                    token(Lexer, amper_amper, Source@24, 2);

                <<"<<"/utf8, Source@25/binary>> ->
                    token(Lexer, less_less, Source@25, 2);

                <<">>"/utf8, Source@26/binary>> ->
                    token(Lexer, greater_greater, Source@26, 2);

                <<"<-"/utf8, Source@27/binary>> ->
                    token(Lexer, left_arrow, Source@27, 2);

                <<"->"/utf8, Source@28/binary>> ->
                    token(Lexer, right_arrow, Source@28, 2);

                <<"<>"/utf8, Source@29/binary>> ->
                    token(Lexer, less_greater, Source@29, 2);

                <<"+."/utf8, Source@30/binary>> ->
                    token(Lexer, plus_dot, Source@30, 2);

                <<"-."/utf8, Source@31/binary>> ->
                    token(Lexer, minus_dot, Source@31, 2);

                <<"*."/utf8, Source@32/binary>> ->
                    token(Lexer, star_dot, Source@32, 2);

                <<"/."/utf8, Source@33/binary>> ->
                    token(Lexer, slash_dot, Source@33, 2);

                <<"<=."/utf8, Source@34/binary>> ->
                    token(Lexer, less_equal_dot, Source@34, 3);

                <<"<."/utf8, Source@35/binary>> ->
                    token(Lexer, less_dot, Source@35, 2);

                <<">=."/utf8, Source@36/binary>> ->
                    token(Lexer, greater_equal_dot, Source@36, 3);

                <<">."/utf8, Source@37/binary>> ->
                    token(Lexer, greater_dot, Source@37, 2);

                <<"0b"/utf8, Source@38/binary>> ->
                    _pipe@5 = advance(Lexer, Source@38, 2),
                    lex_binary(_pipe@5, erlang:element(4, Lexer), 2);

                <<"0o"/utf8, Source@39/binary>> ->
                    _pipe@6 = advance(Lexer, Source@39, 2),
                    lex_octal(_pipe@6, erlang:element(4, Lexer), 2);

                <<"0x"/utf8, Source@40/binary>> ->
                    _pipe@7 = advance(Lexer, Source@40, 2),
                    lex_hexadecimal(_pipe@7, erlang:element(4, Lexer), 2);

                <<"0"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"1"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"2"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"3"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"4"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"5"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"6"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"7"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"8"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"9"/utf8, Source@41/binary>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, lex_int, erlang:element(4, Lexer), 1);

                <<"-0"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-1"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-2"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-3"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-4"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-5"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-6"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-7"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-8"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"-9"/utf8, Source@42/binary>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, lex_int, erlang:element(4, Lexer), 2);

                <<"+"/utf8, Source@43/binary>> ->
                    token(Lexer, plus, Source@43, 1);

                <<"-"/utf8, Source@44/binary>> ->
                    token(Lexer, minus, Source@44, 1);

                <<"*"/utf8, Source@45/binary>> ->
                    token(Lexer, star, Source@45, 1);

                <<"/"/utf8, Source@46/binary>> ->
                    token(Lexer, slash, Source@46, 1);

                <<"<="/utf8, Source@47/binary>> ->
                    token(Lexer, less_equal, Source@47, 2);

                <<"<"/utf8, Source@48/binary>> ->
                    token(Lexer, less, Source@48, 1);

                <<">="/utf8, Source@49/binary>> ->
                    token(Lexer, greater_equal, Source@49, 2);

                <<">"/utf8, Source@50/binary>> ->
                    token(Lexer, greater, Source@50, 1);

                <<"%"/utf8, Source@51/binary>> ->
                    token(Lexer, percent, Source@51, 1);

                <<"\""/utf8, Source@52/binary>> ->
                    _pipe@10 = advance(Lexer, Source@52, 1),
                    lex_string(_pipe@10, erlang:element(4, Lexer), 0);

                <<"_"/utf8, Source@53/binary>> ->
                    Byte_offset@1 = erlang:element(4, Lexer),
                    {Lexer@3, Name} = begin
                        _pipe@11 = advance(Lexer, Source@53, 1),
                        lex_lowercase_name(_pipe@11, Byte_offset@1 + 1, 0)
                    end,
                    {Lexer@3,
                        {some,
                            {{discard_name, Name}, {position, Byte_offset@1}}}};

                <<"a"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"b"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"c"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"d"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"e"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"f"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"g"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"h"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"i"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"j"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"k"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"l"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"m"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"n"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"o"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"p"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"q"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"r"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"s"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"t"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"u"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"v"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"w"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"x"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"y"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"z"/utf8, Source@54/binary>> ->
                    Byte_offset@2 = erlang:element(4, Lexer),
                    {Lexer@4, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        lex_lowercase_name(_pipe@12, Byte_offset@2, 1)
                    end,
                    Token = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@5 = {lexer,
                        erlang:element(2, Lexer@4),
                        erlang:element(3, Lexer@4),
                        erlang:element(4, Lexer@4),
                        erlang:element(5, Lexer@4),
                        erlang:element(6, Lexer@4),
                        check_for_nested_dot_or_minus,
                        erlang:element(8, Lexer@4)},
                    {Lexer@5, {some, {Token, {position, Byte_offset@2}}}};

                <<"A"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"B"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"C"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"D"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"E"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"F"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"G"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"H"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"I"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"J"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"K"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"L"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"M"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"N"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"O"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"P"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"Q"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"R"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"S"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"T"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"U"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"V"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"W"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"X"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"Y"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                <<"Z"/utf8, Source@55/binary>> ->
                    Byte_offset@3 = erlang:element(4, Lexer),
                    {Lexer@6, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        lex_uppercase_name(_pipe@13, Byte_offset@3, 1)
                    end,
                    {Lexer@6,
                        {some,
                            {{upper_name, Name@2}, {position, Byte_offset@3}}}};

                _ ->
                    case gleam_stdlib:string_pop_grapheme(
                        erlang:element(3, Lexer)
                    ) of
                        {error, _} ->
                            {Lexer,
                                {some,
                                    {end_of_file,
                                        {position, erlang:element(4, Lexer)}}}};

                        {ok, {Grapheme, Source@56}} ->
                            token(
                                Lexer,
                                {unexpected_grapheme, Grapheme},
                                Source@56,
                                length(Grapheme)
                            )
                    end
            end
    end.

-file("src/glexer.gleam", 67).
-spec do_lex(lexer(), list({glexer@token:token(), position()})) -> list({glexer@token:token(),
    position()}).
do_lex(Lexer, Tokens) ->
    case next(Lexer) of
        {Lexer@1, none} ->
            do_lex(Lexer@1, Tokens);

        {_, {some, {end_of_file, _}}} ->
            Tokens;

        {Lexer@2, {some, Token}} ->
            do_lex(Lexer@2, [Token | Tokens])
    end.

-file("src/glexer.gleam", 62).
-spec lex(lexer()) -> list({glexer@token:token(), position()}).
lex(Lexer) ->
    _pipe = do_lex(Lexer, []),
    lists:reverse(_pipe).

-file("src/glexer.gleam", 855).
-spec unescape_codepoint(binary(), binary(), binary()) -> {ok, binary()} |
    {error, nil}.
unescape_codepoint(Escaped, Unescaped, Codepoint) ->
    case gleam_stdlib:string_pop_grapheme(Escaped) of
        {ok, {<<"}"/utf8>>, Escaped@1}} ->
            gleam@result:'try'(
                gleam@int:base_parse(Codepoint, 16),
                fun(Codepoint@1) ->
                    gleam@result:'try'(
                        gleam@string:utf_codepoint(Codepoint@1),
                        fun(Codepoint@2) ->
                            Codepoint@3 = gleam_stdlib:utf_codepoint_list_to_string(
                                [Codepoint@2]
                            ),
                            unescape_loop(
                                Escaped@1,
                                <<Unescaped/binary, Codepoint@3/binary>>
                            )
                        end
                    )
                end
            );

        {ok, {C, Escaped@2}} ->
            unescape_codepoint(
                Escaped@2,
                Unescaped,
                <<Codepoint/binary, C/binary>>
            );

        {error, nil} ->
            {error, nil}
    end.

-file("src/glexer.gleam", 836).
-spec unescape_loop(binary(), binary()) -> {ok, binary()} | {error, nil}.
unescape_loop(Escaped, Unescaped) ->
    case Escaped of
        <<"\\\""/utf8, Escaped@1/binary>> ->
            unescape_loop(Escaped@1, <<Unescaped/binary, "\""/utf8>>);

        <<"\\\\"/utf8, Escaped@2/binary>> ->
            unescape_loop(Escaped@2, <<Unescaped/binary, "\\"/utf8>>);

        <<"\\f"/utf8, Escaped@3/binary>> ->
            unescape_loop(Escaped@3, <<Unescaped/binary, "\f"/utf8>>);

        <<"\\n"/utf8, Escaped@4/binary>> ->
            unescape_loop(Escaped@4, <<Unescaped/binary, "\n"/utf8>>);

        <<"\\r"/utf8, Escaped@5/binary>> ->
            unescape_loop(Escaped@5, <<Unescaped/binary, "\r"/utf8>>);

        <<"\\t"/utf8, Escaped@6/binary>> ->
            unescape_loop(Escaped@6, <<Unescaped/binary, "\t"/utf8>>);

        <<"\\u{"/utf8, Escaped@7/binary>> ->
            unescape_codepoint(Escaped@7, Unescaped, <<""/utf8>>);

        <<"\\"/utf8, _/binary>> ->
            {error, nil};

        _ ->
            case gleam_stdlib:string_pop_grapheme(Escaped) of
                {error, _} ->
                    {ok, Unescaped};

                {ok, {Grapheme, Escaped@8}} ->
                    unescape_loop(
                        Escaped@8,
                        <<Unescaped/binary, Grapheme/binary>>
                    )
            end
    end.

-file("src/glexer.gleam", 832).
?DOC(
    " Convert the value of a string token to the string it represents.\n"
    "\n"
    " This function can fail if the original string contains invalid escape sequences.\n"
    "\n"
    " ```gleam\n"
    " unescape_string(\"\\\\\\\"X\\\\\\\" marks the spot\")\n"
    " // --> Ok(\"\\\"X\\\" marks the spot\")\n"
    "\n"
    " unescape_string(\"\\\\u{1F600}\")\n"
    " // --> Ok(\"😀\")\n"
    "\n"
    " unescape_string(\"\\\\x\")\n"
    " // --> Error(Nil)\n"
    " ```\n"
).
-spec unescape_string(binary()) -> {ok, binary()} | {error, nil}.
unescape_string(String) ->
    unescape_loop(String, <<""/utf8>>).
