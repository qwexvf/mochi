-module(glexer@token).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/glexer/token.gleam").
-export([to_source/1]).
-export_type([token/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type token() :: {name, binary()} |
    {upper_name, binary()} |
    {discard_name, binary()} |
    {int, binary()} |
    {float, binary()} |
    {string, binary()} |
    {comment_doc, binary()} |
    {comment_normal, binary()} |
    {comment_module, binary()} |
    as |
    assert |
    auto |
    'case' |
    const |
    delegate |
    derive |
    echo |
    'else' |
    fn |
    'if' |
    implement |
    import |
    'let' |
    macro |
    opaque |
    panic |
    pub |
    test |
    todo |
    type |
    use |
    left_paren |
    right_paren |
    left_brace |
    right_brace |
    left_square |
    right_square |
    plus |
    minus |
    star |
    slash |
    less |
    greater |
    less_equal |
    greater_equal |
    percent |
    plus_dot |
    minus_dot |
    star_dot |
    slash_dot |
    less_dot |
    greater_dot |
    less_equal_dot |
    greater_equal_dot |
    less_greater |
    at |
    colon |
    comma |
    hash |
    bang |
    equal |
    equal_equal |
    not_equal |
    v_bar |
    v_bar_v_bar |
    amper_amper |
    less_less |
    greater_greater |
    pipe |
    dot |
    dot_dot |
    left_arrow |
    right_arrow |
    end_of_file |
    {space, binary()} |
    {unterminated_string, binary()} |
    {unexpected_grapheme, binary()}.

-file("src/glexer/token.gleam", 100).
?DOC(" Turn a token back into its Gleam source representation.\n").
-spec to_source(token()) -> binary().
to_source(Tok) ->
    case Tok of
        {name, Str} ->
            Str;

        {upper_name, Str@1} ->
            Str@1;

        {int, Str@2} ->
            Str@2;

        {float, Str@3} ->
            Str@3;

        {discard_name, Str@4} ->
            <<"_"/utf8, Str@4/binary>>;

        {string, Str@5} ->
            <<<<"\""/utf8, Str@5/binary>>/binary, "\""/utf8>>;

        {comment_doc, Str@6} ->
            <<"///"/utf8, Str@6/binary>>;

        {comment_normal, Str@7} ->
            <<"//"/utf8, Str@7/binary>>;

        {comment_module, Str@8} ->
            <<"////"/utf8, Str@8/binary>>;

        as ->
            <<"as"/utf8>>;

        assert ->
            <<"assert"/utf8>>;

        auto ->
            <<"auto"/utf8>>;

        'case' ->
            <<"case"/utf8>>;

        const ->
            <<"const"/utf8>>;

        delegate ->
            <<"delegate"/utf8>>;

        derive ->
            <<"derive"/utf8>>;

        echo ->
            <<"echo"/utf8>>;

        'else' ->
            <<"else"/utf8>>;

        fn ->
            <<"fn"/utf8>>;

        'if' ->
            <<"if"/utf8>>;

        implement ->
            <<"implement"/utf8>>;

        import ->
            <<"import"/utf8>>;

        'let' ->
            <<"let"/utf8>>;

        macro ->
            <<"macro"/utf8>>;

        opaque ->
            <<"opaque"/utf8>>;

        panic ->
            <<"panic"/utf8>>;

        pub ->
            <<"pub"/utf8>>;

        test ->
            <<"test"/utf8>>;

        todo ->
            <<"todo"/utf8>>;

        type ->
            <<"type"/utf8>>;

        use ->
            <<"use"/utf8>>;

        left_paren ->
            <<"("/utf8>>;

        right_paren ->
            <<")"/utf8>>;

        left_brace ->
            <<"{"/utf8>>;

        right_brace ->
            <<"}"/utf8>>;

        left_square ->
            <<"["/utf8>>;

        right_square ->
            <<"]"/utf8>>;

        plus ->
            <<"+"/utf8>>;

        minus ->
            <<"-"/utf8>>;

        star ->
            <<"*"/utf8>>;

        slash ->
            <<"/"/utf8>>;

        less ->
            <<"<"/utf8>>;

        greater ->
            <<">"/utf8>>;

        less_equal ->
            <<"<="/utf8>>;

        greater_equal ->
            <<">="/utf8>>;

        percent ->
            <<"%"/utf8>>;

        plus_dot ->
            <<"+."/utf8>>;

        minus_dot ->
            <<"-."/utf8>>;

        star_dot ->
            <<"*."/utf8>>;

        slash_dot ->
            <<"/."/utf8>>;

        less_dot ->
            <<"<."/utf8>>;

        greater_dot ->
            <<">."/utf8>>;

        less_equal_dot ->
            <<"<=."/utf8>>;

        greater_equal_dot ->
            <<">=."/utf8>>;

        less_greater ->
            <<"<>"/utf8>>;

        at ->
            <<"@"/utf8>>;

        colon ->
            <<":"/utf8>>;

        comma ->
            <<","/utf8>>;

        hash ->
            <<"#"/utf8>>;

        bang ->
            <<"!"/utf8>>;

        equal ->
            <<"="/utf8>>;

        equal_equal ->
            <<"=="/utf8>>;

        not_equal ->
            <<"!="/utf8>>;

        v_bar ->
            <<"|"/utf8>>;

        v_bar_v_bar ->
            <<"||"/utf8>>;

        amper_amper ->
            <<"&&"/utf8>>;

        less_less ->
            <<"<<"/utf8>>;

        greater_greater ->
            <<">>"/utf8>>;

        pipe ->
            <<"|>"/utf8>>;

        dot ->
            <<"."/utf8>>;

        dot_dot ->
            <<".."/utf8>>;

        left_arrow ->
            <<"<-"/utf8>>;

        right_arrow ->
            <<"->"/utf8>>;

        end_of_file ->
            <<""/utf8>>;

        {space, Str@9} ->
            Str@9;

        {unterminated_string, Str@10} ->
            <<"\""/utf8, Str@10/binary>>;

        {unexpected_grapheme, Str@11} ->
            Str@11
    end.
