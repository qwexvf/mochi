-module(security_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/security_test.gleam").
-export([depth_calculation_test/0, complexity_calculation_test/0, alias_counting_test/0, root_field_counting_test/0, depth_limit_validation_test/0, complexity_limit_validation_test/0, no_limits_validation_test/0, error_message_test/0, fragment_spread_depth_test/0, fragment_spread_depth_limit_test/0, alias_limit_validation_test/0, root_field_limit_validation_test/0, all_limits_pass_test/0, complexity_error_message_test/0]).

-file("test/security_test.gleam", 7).
-spec depth_calculation_test() -> nil.
depth_calculation_test() ->
    Doc@1 = case mochi@parser:parse(<<"{ users { id } }"/utf8>>) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"depth_calculation_test"/utf8>>,
                        line => 9,
                        value => _assert_fail,
                        start => 183,
                        'end' => 236,
                        pattern_start => 194,
                        pattern_end => 201})
    end,
    Analysis = mochi@security:analyze(Doc@1),
    gleeunit@should:equal(erlang:element(2, Analysis), 2),
    Doc2@1 = case mochi@parser:parse(
        <<"{ users { posts { comments { id } } } }"/utf8>>
    ) of
        {ok, Doc2} -> Doc2;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"depth_calculation_test"/utf8>>,
                        line => 14,
                        value => _assert_fail@1,
                        start => 341,
                        'end' => 418,
                        pattern_start => 352,
                        pattern_end => 360})
    end,
    Analysis2 = mochi@security:analyze(Doc2@1),
    gleeunit@should:equal(erlang:element(2, Analysis2), 4).

-file("test/security_test.gleam", 19).
-spec complexity_calculation_test() -> nil.
complexity_calculation_test() ->
    Doc@1 = case mochi@parser:parse(<<"{ user { id name } }"/utf8>>) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"complexity_calculation_test"/utf8>>,
                        line => 21,
                        value => _assert_fail,
                        start => 557,
                        'end' => 614,
                        pattern_start => 568,
                        pattern_end => 575})
    end,
    Analysis = mochi@security:analyze(Doc@1),
    gleeunit@should:equal(erlang:element(3, Analysis), 3).

-file("test/security_test.gleam", 27).
-spec alias_counting_test() -> nil.
alias_counting_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ u1: user { id } u2: user { name } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"alias_counting_test"/utf8>>,
                        line => 28,
                        value => _assert_fail,
                        start => 762,
                        'end' => 836,
                        pattern_start => 773,
                        pattern_end => 780})
    end,
    Analysis = mochi@security:analyze(Doc@1),
    gleeunit@should:equal(erlang:element(4, Analysis), 2).

-file("test/security_test.gleam", 33).
-spec root_field_counting_test() -> nil.
root_field_counting_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ users { id } posts { id } comments { id } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"root_field_counting_test"/utf8>>,
                        line => 34,
                        value => _assert_fail,
                        start => 957,
                        'end' => 1043,
                        pattern_start => 968,
                        pattern_end => 975})
    end,
    Analysis = mochi@security:analyze(Doc@1),
    gleeunit@should:equal(erlang:element(5, Analysis), 3).

-file("test/security_test.gleam", 40).
-spec depth_limit_validation_test() -> mochi@security:security_error().
depth_limit_validation_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"depth_limit_validation_test"/utf8>>,
                        line => 42,
                        value => _assert_fail,
                        start => 1197,
                        'end' => 1316,
                        pattern_start => 1208,
                        pattern_end => 1215})
    end,
    Config = mochi@security:default_config(),
    Result = mochi@security:validate(Doc@1, Config),
    gleeunit@should:be_error(Result),
    Permissive_config = {security_config, {some, 15}, none, none, none},
    Result2 = mochi@security:validate(Doc@1, Permissive_config),
    gleeunit@should:be_ok(Result2),
    Strict_config = {security_config, {some, 5}, none, none, none},
    Result3 = mochi@security:validate(Doc@1, Strict_config),
    gleeunit@should:be_error(Result3).

-file("test/security_test.gleam", 75).
-spec complexity_limit_validation_test() -> mochi@security:security_error().
complexity_limit_validation_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ users { id name email role status createdAt updatedAt } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"complexity_limit_validation_test"/utf8>>,
                        line => 77,
                        value => _assert_fail,
                        start => 2185,
                        'end' => 2285,
                        pattern_start => 2196,
                        pattern_end => 2203})
    end,
    Strict_config = {security_config, none, {some, 5}, none, none},
    Result = mochi@security:validate(Doc@1, Strict_config),
    gleeunit@should:be_error(Result).

-file("test/security_test.gleam", 92).
-spec no_limits_validation_test() -> mochi@security:query_analysis().
no_limits_validation_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ a { b { c { d { e { f { g { h { i { j { k } } } } } } } } } } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"no_limits_validation_test"/utf8>>,
                        line => 93,
                        value => _assert_fail,
                        start => 2574,
                        'end' => 2693,
                        pattern_start => 2585,
                        pattern_end => 2592})
    end,
    Config = mochi@security:no_limits(),
    Result = mochi@security:validate(Doc@1, Config),
    gleeunit@should:be_ok(Result).

-file("test/security_test.gleam", 103).
-spec error_message_test() -> nil.
error_message_test() ->
    Error = {depth_limit_exceeded, 15, 10},
    Msg = mochi@security:error_message(Error),
    gleeunit@should:be_true(
        begin
            _pipe = Msg,
            gleam_stdlib:contains_string(_pipe, <<"15"/utf8>>)
        end
    ),
    gleeunit@should:be_true(
        begin
            _pipe@1 = Msg,
            gleam_stdlib:contains_string(_pipe@1, <<"10"/utf8>>)
        end
    ).

-file("test/security_test.gleam", 112).
-spec fragment_spread_depth_test() -> nil.
fragment_spread_depth_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"
      fragment DeepFragment on SomeType {
        b { c { d } }
      }
      { a { ...DeepFragment } }
      "/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"fragment_spread_depth_test"/utf8>>,
                        line => 114,
                        value => _assert_fail,
                        start => 3252,
                        'end' => 3417,
                        pattern_start => 3263,
                        pattern_end => 3270})
    end,
    Analysis = mochi@security:analyze(Doc@1),
    gleeunit@should:be_true(erlang:element(2, Analysis) >= 4).

-file("test/security_test.gleam", 129).
-spec fragment_spread_depth_limit_test() -> mochi@security:security_error().
fragment_spread_depth_limit_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"
      fragment Deep on T { d { e { f } } }
      { a { b { c { ...Deep } } } }
      "/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"fragment_spread_depth_limit_test"/utf8>>,
                        line => 131,
                        value => _assert_fail,
                        start => 3759,
                        'end' => 3899,
                        pattern_start => 3770,
                        pattern_end => 3777})
    end,
    Strict_config = {security_config, {some, 5}, none, none, none},
    Result = mochi@security:validate(Doc@1, Strict_config),
    gleeunit@should:be_error(Result).

-file("test/security_test.gleam", 151).
-spec alias_limit_validation_test() -> mochi@security:security_error().
alias_limit_validation_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ u1: user { id } u2: user { id } u3: user { id } u4: user { id } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"alias_limit_validation_test"/utf8>>,
                        line => 152,
                        value => _assert_fail,
                        start => 4297,
                        'end' => 4418,
                        pattern_start => 4308,
                        pattern_end => 4315})
    end,
    Strict_config = {security_config, none, none, {some, 3}, none},
    Result = mochi@security:validate(Doc@1, Strict_config),
    gleeunit@should:be_error(Result).

-file("test/security_test.gleam", 167).
-spec root_field_limit_validation_test() -> mochi@security:security_error().
root_field_limit_validation_test() ->
    Doc@1 = case mochi@parser:parse(
        <<"{ users { id } posts { id } comments { id } }"/utf8>>
    ) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"root_field_limit_validation_test"/utf8>>,
                        line => 168,
                        value => _assert_fail,
                        start => 4712,
                        'end' => 4798,
                        pattern_start => 4723,
                        pattern_end => 4730})
    end,
    Strict_config = {security_config, none, none, none, {some, 2}},
    Result = mochi@security:validate(Doc@1, Strict_config),
    gleeunit@should:be_error(Result).

-file("test/security_test.gleam", 181).
-spec all_limits_pass_test() -> mochi@security:query_analysis().
all_limits_pass_test() ->
    Doc@1 = case mochi@parser:parse(<<"{ users { id name } }"/utf8>>) of
        {ok, Doc} -> Doc;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"security_test"/utf8>>,
                        function => <<"all_limits_pass_test"/utf8>>,
                        line => 182,
                        value => _assert_fail,
                        start => 5080,
                        'end' => 5138,
                        pattern_start => 5091,
                        pattern_end => 5098})
    end,
    Config = {security_config, {some, 10}, {some, 100}, {some, 10}, {some, 10}},
    Result = mochi@security:validate(Doc@1, Config),
    gleeunit@should:be_ok(Result).

-file("test/security_test.gleam", 194).
-spec complexity_error_message_test() -> nil.
complexity_error_message_test() ->
    Error = {complexity_limit_exceeded, 20, 10},
    Msg = mochi@security:error_message(Error),
    gleeunit@should:be_true(
        begin
            _pipe = Msg,
            gleam_stdlib:contains_string(_pipe, <<"20"/utf8>>)
        end
    ),
    gleeunit@should:be_true(
        begin
            _pipe@1 = Msg,
            gleam_stdlib:contains_string(_pipe@1, <<"10"/utf8>>)
        end
    ).
