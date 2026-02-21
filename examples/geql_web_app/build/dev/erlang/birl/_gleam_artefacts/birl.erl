-module(birl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/birl.gleam").
-export([time_of_day_to_string/1, time_of_day_to_short_string/1, to_unix/1, from_unix/1, to_unix_milli/1, from_unix_milli/1, to_unix_micro/1, from_unix_micro/1, compare/2, difference/2, add/2, subtract/2, weekday_to_string/1, weekday_to_short_string/1, range/3, set_timezone/2, get_timezone/1, set_offset/2, get_offset/1, parse_time_of_day/1, parse_naive_time_of_day/1, utc_now/0, now_with_offset/1, now_with_timezone/1, monotonic_now/0, to_date_string/1, to_naive_date_string/1, to_time_string/1, to_naive_time_string/1, to_iso8601/1, to_naive/1, month/1, string_month/1, short_string_month/1, get_day/1, get_time_of_day/1, to_erlang_datetime/1, to_erlang_universal_datetime/1, parse/1, from_naive/1, set_day/2, set_time_of_day/2, weekday/1, string_weekday/1, short_string_weekday/1, to_http/1, to_http_with_offset/1, now/0, has_occured/1, from_erlang_local_datetime/1, from_erlang_universal_datetime/1, parse_relative/2, legible_difference/2, parse_weekday/1, from_http/1, parse_month/1]).
-export_type([time/0, day/0, time_of_day/0, weekday/0, month/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque time() :: {time,
        integer(),
        integer(),
        gleam@option:option(binary()),
        gleam@option:option(integer())}.

-type day() :: {day, integer(), integer(), integer()}.

-type time_of_day() :: {time_of_day, integer(), integer(), integer(), integer()}.

-type weekday() :: mon | tue | wed | thu | fri | sat | sun.

-type month() :: jan |
    feb |
    mar |
    apr |
    may |
    jun |
    jul |
    aug |
    sep |
    oct |
    nov |
    dec.

-file("src/birl.gleam", 514).
-spec time_of_day_to_string(time_of_day()) -> binary().
time_of_day_to_string(Value) ->
    <<<<<<<<<<<<(erlang:integer_to_binary(erlang:element(2, Value)))/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe = erlang:integer_to_binary(
                                erlang:element(3, Value)
                            ),
                            gleam@string:pad_start(_pipe, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@1 = erlang:integer_to_binary(erlang:element(4, Value)),
                    gleam@string:pad_start(_pipe@1, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@2 = erlang:integer_to_binary(erlang:element(5, Value)),
            gleam@string:pad_start(_pipe@2, 3, <<"0"/utf8>>)
        end)/binary>>.

-file("src/birl.gleam", 527).
-spec time_of_day_to_short_string(time_of_day()) -> binary().
time_of_day_to_short_string(Value) ->
    <<<<(erlang:integer_to_binary(erlang:element(2, Value)))/binary, ":"/utf8>>/binary,
        (begin
            _pipe = erlang:integer_to_binary(erlang:element(3, Value)),
            gleam@string:pad_start(_pipe, 2, <<"0"/utf8>>)
        end)/binary>>.

-file("src/birl.gleam", 848).
?DOC(" unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec to_unix(time()) -> integer().
to_unix(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T div 1000000
    end.

-file("src/birl.gleam", 855).
?DOC(" unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec from_unix(integer()) -> time().
from_unix(Value) ->
    {time, Value * 1000000, 0, none, none}.

-file("src/birl.gleam", 860).
?DOC(" unix milli timestamps are the number of milliseconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec to_unix_milli(time()) -> integer().
to_unix_milli(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T div 1000
    end.

-file("src/birl.gleam", 867).
?DOC(" unix milli timestamps are the number of milliseconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec from_unix_milli(integer()) -> time().
from_unix_milli(Value) ->
    {time, Value * 1000, 0, none, none}.

-file("src/birl.gleam", 872).
?DOC(" unix micro timestamps are the number of microseconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec to_unix_micro(time()) -> integer().
to_unix_micro(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T
    end.

-file("src/birl.gleam", 879).
?DOC(" unix micro timestamps are the number of microseconds that have elapsed since 00:00:00 UTC on January 1st, 1970\n").
-spec from_unix_micro(integer()) -> time().
from_unix_micro(Value) ->
    {time, Value, 0, none, none}.

-file("src/birl.gleam", 883).
-spec compare(time(), time()) -> gleam@order:order().
compare(A, B) ->
    {time, Wta, _, _, Mta} = A,
    {time, Wtb, _, _, Mtb} = B,
    {Ta@1, Tb@1} = case {Mta, Mtb} of
        {{some, Ta}, {some, Tb}} ->
            {Ta, Tb};

        {_, _} ->
            {Wta, Wtb}
    end,
    case {Ta@1 =:= Tb@1, Ta@1 < Tb@1} of
        {true, _} ->
            eq;

        {_, true} ->
            lt;

        {_, false} ->
            gt
    end.

-file("src/birl.gleam", 899).
-spec difference(time(), time()) -> birl@duration:duration().
difference(A, B) ->
    {time, Wta, _, _, Mta} = A,
    {time, Wtb, _, _, Mtb} = B,
    {Ta@1, Tb@1} = case {Mta, Mtb} of
        {{some, Ta}, {some, Tb}} ->
            {Ta, Tb};

        {_, _} ->
            {Wta, Wtb}
    end,
    {duration, Ta@1 - Tb@1}.

-file("src/birl.gleam", 996).
-spec add(time(), birl@duration:duration()) -> time().
add(Value, Duration) ->
    {time, Wt, O, Timezone, Mt} = Value,
    {duration, Duration@1} = Duration,
    case Mt of
        {some, Mt@1} ->
            {time, Wt + Duration@1, O, Timezone, {some, Mt@1 + Duration@1}};

        none ->
            {time, Wt + Duration@1, O, Timezone, none}
    end.

-file("src/birl.gleam", 1018).
-spec subtract(time(), birl@duration:duration()) -> time().
subtract(Value, Duration) ->
    {time, Wt, O, Timezone, Mt} = Value,
    {duration, Duration@1} = Duration,
    case Mt of
        {some, Mt@1} ->
            {time, Wt - Duration@1, O, Timezone, {some, Mt@1 - Duration@1}};

        none ->
            {time, Wt - Duration@1, O, Timezone, none}
    end.

-file("src/birl.gleam", 1059).
-spec weekday_to_string(weekday()) -> binary().
weekday_to_string(Value) ->
    case Value of
        mon ->
            <<"Monday"/utf8>>;

        tue ->
            <<"Tuesday"/utf8>>;

        wed ->
            <<"Wednesday"/utf8>>;

        thu ->
            <<"Thursday"/utf8>>;

        fri ->
            <<"Friday"/utf8>>;

        sat ->
            <<"Saturday"/utf8>>;

        sun ->
            <<"Sunday"/utf8>>
    end.

-file("src/birl.gleam", 1071).
-spec weekday_to_short_string(weekday()) -> binary().
weekday_to_short_string(Value) ->
    case Value of
        mon ->
            <<"Mon"/utf8>>;

        tue ->
            <<"Tue"/utf8>>;

        wed ->
            <<"Wed"/utf8>>;

        thu ->
            <<"Thu"/utf8>>;

        fri ->
            <<"Fri"/utf8>>;

        sat ->
            <<"Sat"/utf8>>;

        sun ->
            <<"Sun"/utf8>>
    end.

-file("src/birl.gleam", 1150).
?DOC(
    " can be used to create a time range starting from time `a` with step `s`\n"
    "\n"
    " if `b` is `option.None` the range will be infinite\n"
).
-spec range(time(), gleam@option:option(time()), birl@duration:duration()) -> gleam@yielder:yielder(time()).
range(A, B, S) ->
    Range@1 = case case B of
        {some, B@1} ->
            (ranger:create(
                fun(_) -> true end,
                fun(Duration) ->
                    {duration, Value} = Duration,
                    {duration, -1 * Value}
                end,
                fun add/2,
                fun compare/2
            ))(A, B@1, S);

        none ->
            (ranger:create_infinite(
                fun(_) -> true end,
                fun add/2,
                fun compare/2
            ))(A, S)
    end of
        {ok, Range} -> Range;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"range"/utf8>>,
                        line => 1151,
                        value => _assert_fail,
                        start => 28864,
                        'end' => 29315,
                        pattern_start => 28875,
                        pattern_end => 28884})
    end,
    Range@1.

-file("src/birl.gleam", 1173).
?DOC(" WARNING: Does not respect daylight saving time!\n").
-spec set_timezone(time(), binary()) -> {ok, time()} | {error, nil}.
set_timezone(Value, New_timezone) ->
    case gleam@list:key_find(
        [{<<"Africa/Abidjan"/utf8>>, 0},
            {<<"Africa/Algiers"/utf8>>, 3600},
            {<<"Africa/Bissau"/utf8>>, 0},
            {<<"Africa/Cairo"/utf8>>, 7200},
            {<<"Africa/Casablanca"/utf8>>, 3600},
            {<<"Africa/Ceuta"/utf8>>, 3600},
            {<<"Africa/El_Aaiun"/utf8>>, 3600},
            {<<"Africa/Johannesburg"/utf8>>, 7200},
            {<<"Africa/Juba"/utf8>>, 7200},
            {<<"Africa/Khartoum"/utf8>>, 7200},
            {<<"Africa/Lagos"/utf8>>, 3600},
            {<<"Africa/Maputo"/utf8>>, 7200},
            {<<"Africa/Monrovia"/utf8>>, 0},
            {<<"Africa/Nairobi"/utf8>>, 10800},
            {<<"Africa/Ndjamena"/utf8>>, 3600},
            {<<"Africa/Sao_Tome"/utf8>>, 0},
            {<<"Africa/Tripoli"/utf8>>, 7200},
            {<<"Africa/Tunis"/utf8>>, 3600},
            {<<"Africa/Windhoek"/utf8>>, 7200},
            {<<"America/Adak"/utf8>>, -36000},
            {<<"America/Anchorage"/utf8>>, -32400},
            {<<"America/Araguaina"/utf8>>, -10800},
            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
            {<<"America/Argentina/Salta"/utf8>>, -10800},
            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
            {<<"America/Asuncion"/utf8>>, -14400},
            {<<"America/Bahia"/utf8>>, -10800},
            {<<"America/Bahia_Banderas"/utf8>>, -21600},
            {<<"America/Barbados"/utf8>>, -14400},
            {<<"America/Belem"/utf8>>, -10800},
            {<<"America/Belize"/utf8>>, -21600},
            {<<"America/Boa_Vista"/utf8>>, -14400},
            {<<"America/Bogota"/utf8>>, -18000},
            {<<"America/Boise"/utf8>>, -25200},
            {<<"America/Cambridge_Bay"/utf8>>, -25200},
            {<<"America/Campo_Grande"/utf8>>, -14400},
            {<<"America/Cancun"/utf8>>, -18000},
            {<<"America/Caracas"/utf8>>, -14400},
            {<<"America/Cayenne"/utf8>>, -10800},
            {<<"America/Chicago"/utf8>>, -21600},
            {<<"America/Chihuahua"/utf8>>, -21600},
            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
            {<<"America/Costa_Rica"/utf8>>, -21600},
            {<<"America/Cuiaba"/utf8>>, -14400},
            {<<"America/Danmarkshavn"/utf8>>, 0},
            {<<"America/Dawson"/utf8>>, -25200},
            {<<"America/Dawson_Creek"/utf8>>, -25200},
            {<<"America/Denver"/utf8>>, -25200},
            {<<"America/Detroit"/utf8>>, -18000},
            {<<"America/Edmonton"/utf8>>, -25200},
            {<<"America/Eirunepe"/utf8>>, -18000},
            {<<"America/El_Salvador"/utf8>>, -21600},
            {<<"America/Fort_Nelson"/utf8>>, -25200},
            {<<"America/Fortaleza"/utf8>>, -10800},
            {<<"America/Glace_Bay"/utf8>>, -14400},
            {<<"America/Goose_Bay"/utf8>>, -14400},
            {<<"America/Grand_Turk"/utf8>>, -18000},
            {<<"America/Guatemala"/utf8>>, -21600},
            {<<"America/Guayaquil"/utf8>>, -18000},
            {<<"America/Guyana"/utf8>>, -14400},
            {<<"America/Halifax"/utf8>>, -14400},
            {<<"America/Havana"/utf8>>, -18000},
            {<<"America/Hermosillo"/utf8>>, -25200},
            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
            {<<"America/Indiana/Knox"/utf8>>, -21600},
            {<<"America/Indiana/Marengo"/utf8>>, -18000},
            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
            {<<"America/Indiana/Vevay"/utf8>>, -18000},
            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
            {<<"America/Indiana/Winamac"/utf8>>, -18000},
            {<<"America/Inuvik"/utf8>>, -25200},
            {<<"America/Iqaluit"/utf8>>, -18000},
            {<<"America/Jamaica"/utf8>>, -18000},
            {<<"America/Juneau"/utf8>>, -32400},
            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
            {<<"America/La_Paz"/utf8>>, -14400},
            {<<"America/Lima"/utf8>>, -18000},
            {<<"America/Los_Angeles"/utf8>>, -28800},
            {<<"America/Maceio"/utf8>>, -10800},
            {<<"America/Managua"/utf8>>, -21600},
            {<<"America/Manaus"/utf8>>, -14400},
            {<<"America/Martinique"/utf8>>, -14400},
            {<<"America/Matamoros"/utf8>>, -21600},
            {<<"America/Mazatlan"/utf8>>, -25200},
            {<<"America/Menominee"/utf8>>, -21600},
            {<<"America/Merida"/utf8>>, -21600},
            {<<"America/Metlakatla"/utf8>>, -32400},
            {<<"America/Mexico_City"/utf8>>, -21600},
            {<<"America/Miquelon"/utf8>>, -10800},
            {<<"America/Moncton"/utf8>>, -14400},
            {<<"America/Monterrey"/utf8>>, -21600},
            {<<"America/Montevideo"/utf8>>, -10800},
            {<<"America/New_York"/utf8>>, -18000},
            {<<"America/Nome"/utf8>>, -32400},
            {<<"America/Noronha"/utf8>>, -7200},
            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
            {<<"America/North_Dakota/Center"/utf8>>, -21600},
            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
            {<<"America/Nuuk"/utf8>>, -7200},
            {<<"America/Ojinaga"/utf8>>, -21600},
            {<<"America/Panama"/utf8>>, -18000},
            {<<"America/Paramaribo"/utf8>>, -10800},
            {<<"America/Phoenix"/utf8>>, -25200},
            {<<"America/Port-au-Prince"/utf8>>, -18000},
            {<<"America/Porto_Velho"/utf8>>, -14400},
            {<<"America/Puerto_Rico"/utf8>>, -14400},
            {<<"America/Punta_Arenas"/utf8>>, -10800},
            {<<"America/Rankin_Inlet"/utf8>>, -21600},
            {<<"America/Recife"/utf8>>, -10800},
            {<<"America/Regina"/utf8>>, -21600},
            {<<"America/Resolute"/utf8>>, -21600},
            {<<"America/Rio_Branco"/utf8>>, -18000},
            {<<"America/Santarem"/utf8>>, -10800},
            {<<"America/Santiago"/utf8>>, -14400},
            {<<"America/Santo_Domingo"/utf8>>, -14400},
            {<<"America/Sao_Paulo"/utf8>>, -10800},
            {<<"America/Scoresbysund"/utf8>>, -7200},
            {<<"America/Sitka"/utf8>>, -32400},
            {<<"America/St_Johns"/utf8>>, -12600},
            {<<"America/Swift_Current"/utf8>>, -21600},
            {<<"America/Tegucigalpa"/utf8>>, -21600},
            {<<"America/Thule"/utf8>>, -14400},
            {<<"America/Tijuana"/utf8>>, -28800},
            {<<"America/Toronto"/utf8>>, -18000},
            {<<"America/Vancouver"/utf8>>, -28800},
            {<<"America/Whitehorse"/utf8>>, -25200},
            {<<"America/Winnipeg"/utf8>>, -21600},
            {<<"America/Yakutat"/utf8>>, -32400},
            {<<"Antarctica/Casey"/utf8>>, 28800},
            {<<"Antarctica/Davis"/utf8>>, 25200},
            {<<"Antarctica/Macquarie"/utf8>>, 36000},
            {<<"Antarctica/Mawson"/utf8>>, 18000},
            {<<"Antarctica/Palmer"/utf8>>, -10800},
            {<<"Antarctica/Rothera"/utf8>>, -10800},
            {<<"Antarctica/Troll"/utf8>>, 0},
            {<<"Antarctica/Vostok"/utf8>>, 18000},
            {<<"Asia/Almaty"/utf8>>, 18000},
            {<<"Asia/Amman"/utf8>>, 10800},
            {<<"Asia/Anadyr"/utf8>>, 43200},
            {<<"Asia/Aqtau"/utf8>>, 18000},
            {<<"Asia/Aqtobe"/utf8>>, 18000},
            {<<"Asia/Ashgabat"/utf8>>, 18000},
            {<<"Asia/Atyrau"/utf8>>, 18000},
            {<<"Asia/Baghdad"/utf8>>, 10800},
            {<<"Asia/Baku"/utf8>>, 14400},
            {<<"Asia/Bangkok"/utf8>>, 25200},
            {<<"Asia/Barnaul"/utf8>>, 25200},
            {<<"Asia/Beirut"/utf8>>, 7200},
            {<<"Asia/Bishkek"/utf8>>, 21600},
            {<<"Asia/Chita"/utf8>>, 32400},
            {<<"Asia/Colombo"/utf8>>, 19800},
            {<<"Asia/Damascus"/utf8>>, 10800},
            {<<"Asia/Dhaka"/utf8>>, 21600},
            {<<"Asia/Dili"/utf8>>, 32400},
            {<<"Asia/Dubai"/utf8>>, 14400},
            {<<"Asia/Dushanbe"/utf8>>, 18000},
            {<<"Asia/Famagusta"/utf8>>, 7200},
            {<<"Asia/Gaza"/utf8>>, 7200},
            {<<"Asia/Hebron"/utf8>>, 7200},
            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
            {<<"Asia/Hong_Kong"/utf8>>, 28800},
            {<<"Asia/Hovd"/utf8>>, 25200},
            {<<"Asia/Irkutsk"/utf8>>, 28800},
            {<<"Asia/Jakarta"/utf8>>, 25200},
            {<<"Asia/Jayapura"/utf8>>, 32400},
            {<<"Asia/Jerusalem"/utf8>>, 7200},
            {<<"Asia/Kabul"/utf8>>, 16200},
            {<<"Asia/Kamchatka"/utf8>>, 43200},
            {<<"Asia/Karachi"/utf8>>, 18000},
            {<<"Asia/Kathmandu"/utf8>>, 20700},
            {<<"Asia/Khandyga"/utf8>>, 32400},
            {<<"Asia/Kolkata"/utf8>>, 19800},
            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
            {<<"Asia/Kuching"/utf8>>, 28800},
            {<<"Asia/Macau"/utf8>>, 28800},
            {<<"Asia/Magadan"/utf8>>, 39600},
            {<<"Asia/Makassar"/utf8>>, 28800},
            {<<"Asia/Manila"/utf8>>, 28800},
            {<<"Asia/Nicosia"/utf8>>, 7200},
            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
            {<<"Asia/Novosibirsk"/utf8>>, 25200},
            {<<"Asia/Omsk"/utf8>>, 21600},
            {<<"Asia/Oral"/utf8>>, 18000},
            {<<"Asia/Pontianak"/utf8>>, 25200},
            {<<"Asia/Pyongyang"/utf8>>, 32400},
            {<<"Asia/Qatar"/utf8>>, 10800},
            {<<"Asia/Qostanay"/utf8>>, 18000},
            {<<"Asia/Qyzylorda"/utf8>>, 18000},
            {<<"Asia/Riyadh"/utf8>>, 10800},
            {<<"Asia/Sakhalin"/utf8>>, 39600},
            {<<"Asia/Samarkand"/utf8>>, 18000},
            {<<"Asia/Seoul"/utf8>>, 32400},
            {<<"Asia/Shanghai"/utf8>>, 28800},
            {<<"Asia/Singapore"/utf8>>, 28800},
            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
            {<<"Asia/Taipei"/utf8>>, 28800},
            {<<"Asia/Tashkent"/utf8>>, 18000},
            {<<"Asia/Tbilisi"/utf8>>, 14400},
            {<<"Asia/Tehran"/utf8>>, 12600},
            {<<"Asia/Thimphu"/utf8>>, 21600},
            {<<"Asia/Tokyo"/utf8>>, 32400},
            {<<"Asia/Tomsk"/utf8>>, 25200},
            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
            {<<"Asia/Urumqi"/utf8>>, 21600},
            {<<"Asia/Ust-Nera"/utf8>>, 36000},
            {<<"Asia/Vladivostok"/utf8>>, 36000},
            {<<"Asia/Yakutsk"/utf8>>, 32400},
            {<<"Asia/Yangon"/utf8>>, 23400},
            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
            {<<"Asia/Yerevan"/utf8>>, 14400},
            {<<"Atlantic/Azores"/utf8>>, -3600},
            {<<"Atlantic/Bermuda"/utf8>>, -14400},
            {<<"Atlantic/Canary"/utf8>>, 0},
            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
            {<<"Atlantic/Faroe"/utf8>>, 0},
            {<<"Atlantic/Madeira"/utf8>>, 0},
            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
            {<<"Atlantic/Stanley"/utf8>>, -10800},
            {<<"Australia/Adelaide"/utf8>>, 34200},
            {<<"Australia/Brisbane"/utf8>>, 36000},
            {<<"Australia/Broken_Hill"/utf8>>, 34200},
            {<<"Australia/Darwin"/utf8>>, 34200},
            {<<"Australia/Eucla"/utf8>>, 31500},
            {<<"Australia/Hobart"/utf8>>, 36000},
            {<<"Australia/Lindeman"/utf8>>, 36000},
            {<<"Australia/Lord_Howe"/utf8>>, 37800},
            {<<"Australia/Melbourne"/utf8>>, 36000},
            {<<"Australia/Perth"/utf8>>, 28800},
            {<<"Australia/Sydney"/utf8>>, 36000},
            {<<"Etc/GMT"/utf8>>, 0},
            {<<"Etc/GMT+1"/utf8>>, -3600},
            {<<"Etc/GMT+10"/utf8>>, -36000},
            {<<"Etc/GMT+11"/utf8>>, -39600},
            {<<"Etc/GMT+12"/utf8>>, -43200},
            {<<"Etc/GMT+2"/utf8>>, -7200},
            {<<"Etc/GMT+3"/utf8>>, -10800},
            {<<"Etc/GMT+4"/utf8>>, -14400},
            {<<"Etc/GMT+5"/utf8>>, -18000},
            {<<"Etc/GMT+6"/utf8>>, -21600},
            {<<"Etc/GMT+7"/utf8>>, -25200},
            {<<"Etc/GMT+8"/utf8>>, -28800},
            {<<"Etc/GMT+9"/utf8>>, -32400},
            {<<"Etc/GMT-1"/utf8>>, 3600},
            {<<"Etc/GMT-10"/utf8>>, 36000},
            {<<"Etc/GMT-11"/utf8>>, 39600},
            {<<"Etc/GMT-12"/utf8>>, 43200},
            {<<"Etc/GMT-13"/utf8>>, 46800},
            {<<"Etc/GMT-14"/utf8>>, 50400},
            {<<"Etc/GMT-2"/utf8>>, 7200},
            {<<"Etc/GMT-3"/utf8>>, 10800},
            {<<"Etc/GMT-4"/utf8>>, 14400},
            {<<"Etc/GMT-5"/utf8>>, 18000},
            {<<"Etc/GMT-6"/utf8>>, 21600},
            {<<"Etc/GMT-7"/utf8>>, 25200},
            {<<"Etc/GMT-8"/utf8>>, 28800},
            {<<"Etc/GMT-9"/utf8>>, 32400},
            {<<"Etc/UTC"/utf8>>, 0},
            {<<"Europe/Andorra"/utf8>>, 3600},
            {<<"Europe/Astrakhan"/utf8>>, 14400},
            {<<"Europe/Athens"/utf8>>, 7200},
            {<<"Europe/Belgrade"/utf8>>, 3600},
            {<<"Europe/Berlin"/utf8>>, 3600},
            {<<"Europe/Brussels"/utf8>>, 3600},
            {<<"Europe/Bucharest"/utf8>>, 7200},
            {<<"Europe/Budapest"/utf8>>, 3600},
            {<<"Europe/Chisinau"/utf8>>, 7200},
            {<<"Europe/Dublin"/utf8>>, 3600},
            {<<"Europe/Gibraltar"/utf8>>, 3600},
            {<<"Europe/Helsinki"/utf8>>, 7200},
            {<<"Europe/Istanbul"/utf8>>, 10800},
            {<<"Europe/Kaliningrad"/utf8>>, 7200},
            {<<"Europe/Kirov"/utf8>>, 10800},
            {<<"Europe/Kyiv"/utf8>>, 7200},
            {<<"Europe/Lisbon"/utf8>>, 0},
            {<<"Europe/London"/utf8>>, 0},
            {<<"Europe/Madrid"/utf8>>, 3600},
            {<<"Europe/Malta"/utf8>>, 3600},
            {<<"Europe/Minsk"/utf8>>, 10800},
            {<<"Europe/Moscow"/utf8>>, 10800},
            {<<"Europe/Paris"/utf8>>, 3600},
            {<<"Europe/Prague"/utf8>>, 3600},
            {<<"Europe/Riga"/utf8>>, 7200},
            {<<"Europe/Rome"/utf8>>, 3600},
            {<<"Europe/Samara"/utf8>>, 14400},
            {<<"Europe/Saratov"/utf8>>, 14400},
            {<<"Europe/Simferopol"/utf8>>, 10800},
            {<<"Europe/Sofia"/utf8>>, 7200},
            {<<"Europe/Tallinn"/utf8>>, 7200},
            {<<"Europe/Tirane"/utf8>>, 3600},
            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
            {<<"Europe/Vienna"/utf8>>, 3600},
            {<<"Europe/Vilnius"/utf8>>, 7200},
            {<<"Europe/Volgograd"/utf8>>, 10800},
            {<<"Europe/Warsaw"/utf8>>, 3600},
            {<<"Europe/Zurich"/utf8>>, 3600},
            {<<"Indian/Chagos"/utf8>>, 21600},
            {<<"Indian/Maldives"/utf8>>, 18000},
            {<<"Indian/Mauritius"/utf8>>, 14400},
            {<<"Pacific/Apia"/utf8>>, 46800},
            {<<"Pacific/Auckland"/utf8>>, 43200},
            {<<"Pacific/Bougainville"/utf8>>, 39600},
            {<<"Pacific/Chatham"/utf8>>, 45900},
            {<<"Pacific/Easter"/utf8>>, -21600},
            {<<"Pacific/Efate"/utf8>>, 39600},
            {<<"Pacific/Fakaofo"/utf8>>, 46800},
            {<<"Pacific/Fiji"/utf8>>, 43200},
            {<<"Pacific/Galapagos"/utf8>>, -21600},
            {<<"Pacific/Gambier"/utf8>>, -32400},
            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
            {<<"Pacific/Guam"/utf8>>, 36000},
            {<<"Pacific/Honolulu"/utf8>>, -36000},
            {<<"Pacific/Kanton"/utf8>>, 46800},
            {<<"Pacific/Kiritimati"/utf8>>, 50400},
            {<<"Pacific/Kosrae"/utf8>>, 39600},
            {<<"Pacific/Kwajalein"/utf8>>, 43200},
            {<<"Pacific/Marquesas"/utf8>>, -34200},
            {<<"Pacific/Nauru"/utf8>>, 43200},
            {<<"Pacific/Niue"/utf8>>, -39600},
            {<<"Pacific/Norfolk"/utf8>>, 39600},
            {<<"Pacific/Noumea"/utf8>>, 39600},
            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
            {<<"Pacific/Palau"/utf8>>, 32400},
            {<<"Pacific/Pitcairn"/utf8>>, -28800},
            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
            {<<"Pacific/Rarotonga"/utf8>>, -36000},
            {<<"Pacific/Tahiti"/utf8>>, -36000},
            {<<"Pacific/Tarawa"/utf8>>, 43200},
            {<<"Pacific/Tongatapu"/utf8>>, 46800}],
        New_timezone
    ) of
        {ok, New_offset_number} ->
            case Value of
                {time, T, _, _, Mt} ->
                    _pipe = {time,
                        T,
                        New_offset_number * 1000000,
                        {some, New_timezone},
                        Mt},
                    {ok, _pipe}
            end;

        {error, nil} ->
            {error, nil}
    end.

-file("src/birl.gleam", 1187).
-spec get_timezone(time()) -> gleam@option:option(binary()).
get_timezone(Value) ->
    {time, _, _, Timezone, _} = Value,
    Timezone.

-file("src/birl.gleam", 1330).
-spec parse_offset(binary()) -> {ok, integer()} | {error, nil}.
parse_offset(Offset) ->
    gleam@bool:guard(
        gleam@list:contains([<<"Z"/utf8>>, <<"z"/utf8>>], Offset),
        {ok, 0},
        fun() ->
            Re@1 = case gleam@regexp:from_string(<<"([+-])"/utf8>>) of
                {ok, Re} -> Re;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl"/utf8>>,
                                function => <<"parse_offset"/utf8>>,
                                line => 1332,
                                value => _assert_fail,
                                start => 34005,
                                'end' => 34053,
                                pattern_start => 34016,
                                pattern_end => 34022})
            end,
            gleam@result:then(case gleam@regexp:split(Re@1, Offset) of
                    [<<""/utf8>>, <<"+"/utf8>>, Offset@1] ->
                        {ok, {1, Offset@1}};

                    [<<""/utf8>>, <<"-"/utf8>>, Offset@2] ->
                        {ok, {-1, Offset@2}};

                    [_] ->
                        {ok, {1, Offset}};

                    _ ->
                        {error, nil}
                end, fun(_use0) ->
                    {Sign, Offset@3} = _use0,
                    case gleam@string:split(Offset@3, <<":"/utf8>>) of
                        [Hour_str, Minute_str] ->
                            gleam@result:then(
                                gleam_stdlib:parse_int(Hour_str),
                                fun(Hour) ->
                                    gleam@result:then(
                                        gleam_stdlib:parse_int(Minute_str),
                                        fun(Minute) ->
                                            {ok,
                                                ((Sign * ((Hour * 60) + Minute))
                                                * 60)
                                                * 1000000}
                                        end
                                    )
                                end
                            );

                        [Offset@4] ->
                            case string:length(Offset@4) of
                                1 ->
                                    gleam@result:then(
                                        gleam_stdlib:parse_int(Offset@4),
                                        fun(Hour@1) ->
                                            {ok,
                                                ((Sign * Hour@1) * 3600) * 1000000}
                                        end
                                    );

                                2 ->
                                    gleam@result:then(
                                        gleam_stdlib:parse_int(Offset@4),
                                        fun(Number) -> case Number < 14 of
                                                true ->
                                                    {ok,
                                                        ((Sign * Number) * 3600)
                                                        * 1000000};

                                                false ->
                                                    {ok,
                                                        ((Sign * (((Number div 10)
                                                        * 60)
                                                        + (Number rem 10)))
                                                        * 60)
                                                        * 1000000}
                                            end end
                                    );

                                3 ->
                                    Hour_str@2 = case gleam@string:first(
                                        Offset@4
                                    ) of
                                        {ok, Hour_str@1} -> Hour_str@1;
                                        _assert_fail@1 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        file => <<?FILEPATH/utf8>>,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"parse_offset"/utf8>>,
                                                        line => 1362,
                                                        value => _assert_fail@1,
                                                        start => 34974,
                                                        'end' => 35020,
                                                        pattern_start => 34985,
                                                        pattern_end => 34997}
                                                )
                                    end,
                                    Minute_str@1 = gleam@string:slice(
                                        Offset@4,
                                        1,
                                        2
                                    ),
                                    gleam@result:then(
                                        gleam_stdlib:parse_int(Hour_str@2),
                                        fun(Hour@2) ->
                                            gleam@result:then(
                                                gleam_stdlib:parse_int(
                                                    Minute_str@1
                                                ),
                                                fun(Minute@1) ->
                                                    {ok,
                                                        ((Sign * ((Hour@2 * 60)
                                                        + Minute@1))
                                                        * 60)
                                                        * 1000000}
                                                end
                                            )
                                        end
                                    );

                                4 ->
                                    Hour_str@3 = gleam@string:slice(
                                        Offset@4,
                                        0,
                                        2
                                    ),
                                    Minute_str@2 = gleam@string:slice(
                                        Offset@4,
                                        2,
                                        2
                                    ),
                                    gleam@result:then(
                                        gleam_stdlib:parse_int(Hour_str@3),
                                        fun(Hour@3) ->
                                            gleam@result:then(
                                                gleam_stdlib:parse_int(
                                                    Minute_str@2
                                                ),
                                                fun(Minute@2) ->
                                                    {ok,
                                                        ((Sign * ((Hour@3 * 60)
                                                        + Minute@2))
                                                        * 60)
                                                        * 1000000}
                                                end
                                            )
                                        end
                                    );

                                _ ->
                                    {error, nil}
                            end;

                        _ ->
                            {error, nil}
                    end
                end)
        end
    ).

-file("src/birl.gleam", 1197).
?DOC(
    " use this to change the offset of a given time value.\n"
    "\n"
    " some examples of acceptable offsets:\n"
    "\n"
    " `\"+330\", \"03:30\", \"-8:00\",\"-7\", \"-0400\", \"03\", \"Z\"`\n"
).
-spec set_offset(time(), binary()) -> {ok, time()} | {error, nil}.
set_offset(Value, New_offset) ->
    gleam@result:then(
        parse_offset(New_offset),
        fun(New_offset_number) -> case Value of
                {time, T, _, Timezone, Mt} ->
                    _pipe = {time, T, New_offset_number, Timezone, Mt},
                    {ok, _pipe}
            end end
    ).

-file("src/birl.gleam", 1381).
-spec generate_offset(integer()) -> {ok, binary()} | {error, nil}.
generate_offset(Offset) ->
    gleam@bool:guard(
        Offset =:= 0,
        {ok, <<"Z"/utf8>>},
        fun() ->
            case begin
                _pipe = [{Offset, micro_second}],
                _pipe@1 = birl@duration:new(_pipe),
                birl@duration:decompose(_pipe@1)
            end of
                [{Hour, hour}, {Minute, minute}] ->
                    _pipe@10 = [case Hour > 0 of
                            true ->
                                erlang:list_to_binary(
                                    [<<"+"/utf8>>,
                                        begin
                                            _pipe@2 = Hour,
                                            _pipe@3 = erlang:integer_to_binary(
                                                _pipe@2
                                            ),
                                            gleam@string:pad_start(
                                                _pipe@3,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                );

                            false ->
                                erlang:list_to_binary(
                                    [<<"-"/utf8>>,
                                        begin
                                            _pipe@4 = Hour,
                                            _pipe@5 = gleam@int:absolute_value(
                                                _pipe@4
                                            ),
                                            _pipe@6 = erlang:integer_to_binary(
                                                _pipe@5
                                            ),
                                            gleam@string:pad_start(
                                                _pipe@6,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                )
                        end, begin
                            _pipe@7 = Minute,
                            _pipe@8 = gleam@int:absolute_value(_pipe@7),
                            _pipe@9 = erlang:integer_to_binary(_pipe@8),
                            gleam@string:pad_start(_pipe@9, 2, <<"0"/utf8>>)
                        end],
                    _pipe@11 = gleam@string:join(_pipe@10, <<":"/utf8>>),
                    {ok, _pipe@11};

                [{Hour@1, hour}] ->
                    _pipe@17 = [case Hour@1 > 0 of
                            true ->
                                erlang:list_to_binary(
                                    [<<"+"/utf8>>,
                                        begin
                                            _pipe@12 = Hour@1,
                                            _pipe@13 = erlang:integer_to_binary(
                                                _pipe@12
                                            ),
                                            gleam@string:pad_start(
                                                _pipe@13,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                );

                            false ->
                                erlang:list_to_binary(
                                    [<<"-"/utf8>>,
                                        begin
                                            _pipe@14 = Hour@1,
                                            _pipe@15 = gleam@int:absolute_value(
                                                _pipe@14
                                            ),
                                            _pipe@16 = erlang:integer_to_binary(
                                                _pipe@15
                                            ),
                                            gleam@string:pad_start(
                                                _pipe@16,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                )
                        end, <<"00"/utf8>>],
                    _pipe@18 = gleam@string:join(_pipe@17, <<":"/utf8>>),
                    {ok, _pipe@18};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("src/birl.gleam", 1206).
-spec get_offset(time()) -> binary().
get_offset(Value) ->
    {time, _, Offset, _, _} = Value,
    Offset@2 = case generate_offset(Offset) of
        {ok, Offset@1} -> Offset@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"get_offset"/utf8>>,
                        line => 1208,
                        value => _assert_fail,
                        start => 30429,
                        'end' => 30476,
                        pattern_start => 30440,
                        pattern_end => 30450})
    end,
    Offset@2.

-file("src/birl.gleam", 1497).
-spec is_invalid_date(binary()) -> boolean().
is_invalid_date(Date) ->
    _pipe = Date,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam_stdlib:identity/1),
    gleam@list:any(_pipe@2, fun(Code) -> case Code of
                _ when Code =:= 45 ->
                    false;

                _ when (Code >= 48) andalso (Code =< 57) ->
                    false;

                _ ->
                    true
            end end).

-file("src/birl.gleam", 1510).
-spec is_invalid_time(binary()) -> boolean().
is_invalid_time(Time) ->
    _pipe = Time,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam_stdlib:identity/1),
    gleam@list:any(_pipe@2, fun(Code) -> case Code of
                _ when (Code >= 48) andalso (Code =< 58) ->
                    false;

                _ ->
                    true
            end end).

-file("src/birl.gleam", 1522).
-spec parse_section(binary(), binary(), integer()) -> list({ok, integer()} |
    {error, nil}).
parse_section(Section, Pattern_string, Default) ->
    Pattern@1 = case gleam@regexp:from_string(Pattern_string) of
        {ok, Pattern} -> Pattern;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"parse_section"/utf8>>,
                        line => 1527,
                        value => _assert_fail,
                        start => 38997,
                        'end' => 39056,
                        pattern_start => 39008,
                        pattern_end => 39019})
    end,
    case gleam@regexp:scan(Pattern@1, Section) of
        [{match, _, [{some, Major}]}] ->
            [gleam_stdlib:parse_int(Major), {ok, Default}, {ok, Default}];

        [{match, _, [{some, Major@1}, {some, Middle}]}] ->
            [gleam_stdlib:parse_int(Major@1),
                gleam_stdlib:parse_int(Middle),
                {ok, Default}];

        [{match, _, [{some, Major@2}, {some, Middle@1}, {some, Minor}]}] ->
            [gleam_stdlib:parse_int(Major@2),
                gleam_stdlib:parse_int(Middle@1),
                gleam_stdlib:parse_int(Minor)];

        _ ->
            [{error, nil}]
    end.

-file("src/birl.gleam", 1442).
-spec parse_date_section(binary()) -> {ok, list(integer())} | {error, nil}.
parse_date_section(Date) ->
    gleam@bool:guard(
        is_invalid_date(Date),
        {error, nil},
        fun() ->
            _pipe = case gleam_stdlib:contains_string(Date, <<"-"/utf8>>) of
                true ->
                    Dash_pattern@1 = case gleam@regexp:from_string(
                        <<"(\\d{4})(?:-(1[0-2]|0?[0-9]))?(?:-(3[0-1]|[1-2][0-9]|0?[0-9]))?"/utf8>>
                    ) of
                        {ok, Dash_pattern} -> Dash_pattern;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"birl"/utf8>>,
                                        function => <<"parse_date_section"/utf8>>,
                                        line => 1447,
                                        value => _assert_fail,
                                        start => 37206,
                                        'end' => 37350,
                                        pattern_start => 37217,
                                        pattern_end => 37233})
                    end,
                    case gleam@regexp:scan(Dash_pattern@1, Date) of
                        [{match, _, [{some, Major}]}] ->
                            [gleam_stdlib:parse_int(Major), {ok, 1}, {ok, 1}];

                        [{match, _, [{some, Major@1}, {some, Middle}]}] ->
                            [gleam_stdlib:parse_int(Major@1),
                                gleam_stdlib:parse_int(Middle),
                                {ok, 1}];

                        [{match,
                                _,
                                [{some, Major@2},
                                    {some, Middle@1},
                                    {some, Minor}]}] ->
                            [gleam_stdlib:parse_int(Major@2),
                                gleam_stdlib:parse_int(Middle@1),
                                gleam_stdlib:parse_int(Minor)];

                        _ ->
                            [{error, nil}]
                    end;

                false ->
                    parse_section(
                        Date,
                        <<"(\\d{4})(1[0-2]|0?[0-9])?(3[0-1]|[1-2][0-9]|0?[0-9])?"/utf8>>,
                        1
                    )
            end,
            gleam@list:try_map(_pipe, fun gleam@function:identity/1)
        end
    ).

-file("src/birl.gleam", 1486).
-spec parse_time_section(binary()) -> {ok, list(integer())} | {error, nil}.
parse_time_section(Time) ->
    gleam@bool:guard(
        is_invalid_time(Time),
        {error, nil},
        fun() ->
            _pipe = parse_section(
                Time,
                <<"(2[0-3]|1[0-9]|0?[0-9])([1-5][0-9]|0?[0-9])?([1-5][0-9]|0?[0-9])?"/utf8>>,
                0
            ),
            gleam@list:try_map(_pipe, fun gleam@function:identity/1)
        end
    ).

-file("src/birl.gleam", 404).
?DOC(
    " this function parses `ISO8601` strings in which no date is specified, which\n"
    " means such inputs don't actually represent a particular moment in time. That's why\n"
    " the result of this function is an instance of `TimeOfDay` along with the offset specificed\n"
    " in the string. Some acceptable examples are given below:\n"
    "\n"
    "   - `t25z` -> `#(TimeOfDay(2, 5, 0, 0), \"Z\")`\n"
    "\n"
    "   - `14-4` -> `#(TimeOfDay(14, 0, 0, 0), \"-04:00\")`\n"
    "\n"
    "   - `T145+4` -> `#(TimeOfDay(14, 5, 0, 0), \"+04:00\")`\n"
    "\n"
    "   - `16:38-3` -> `#(TimeOfDay(16, 38, 0, 0), \"-03:00\")`\n"
    "\n"
    "   - `t14:65.9z` -> `#(TimeOfDay(14, 6, 5, 900), \"-04:00\")`\n"
    "\n"
    "   - `163823+0330` -> `#(TimeOfDay(16, 38, 23, 0), \"+03:30\")`\n"
    "\n"
    "   - `T16:38:23.050+03:30` -> `#(TimeOfDay(16, 38, 23, 50), \"+03:30\")`\n"
).
-spec parse_time_of_day(binary()) -> {ok, {time_of_day(), binary()}} |
    {error, nil}.
parse_time_of_day(Value) ->
    Offset_pattern@1 = case gleam@regexp:from_string(<<"(.*)([+|\\-].*)"/utf8>>) of
        {ok, Offset_pattern} -> Offset_pattern;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"parse_time_of_day"/utf8>>,
                        line => 405,
                        value => _assert_fail,
                        start => 9469,
                        'end' => 9538,
                        pattern_start => 9480,
                        pattern_end => 9498})
    end,
    Time_string = case {gleam_stdlib:string_starts_with(Value, <<"T"/utf8>>),
        gleam_stdlib:string_starts_with(Value, <<"t"/utf8>>)} of
        {true, _} ->
            gleam@string:drop_start(Value, 1);

        {_, true} ->
            gleam@string:drop_start(Value, 1);

        {_, _} ->
            Value
    end,
    gleam@result:then(
        case gleam_stdlib:string_ends_with(Time_string, <<"Z"/utf8>>) orelse gleam_stdlib:string_ends_with(
            Time_string,
            <<"z"/utf8>>
        ) of
            true ->
                {ok, {gleam@string:drop_end(Value, 1), <<"+00:00"/utf8>>}};

            false ->
                case gleam@regexp:scan(Offset_pattern@1, Value) of
                    [{match, _, [{some, Time_string@1}, {some, Offset_string}]}] ->
                        {ok, {Time_string@1, Offset_string}};

                    _ ->
                        {error, nil}
                end
        end,
        fun(_use0) ->
            {Time_string@2, Offset_string@1} = _use0,
            Time_string@3 = gleam@string:replace(
                Time_string@2,
                <<":"/utf8>>,
                <<""/utf8>>
            ),
            gleam@result:then(
                case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                    gleam@string:split(Time_string@3, <<","/utf8>>)} of
                    {[_], [_]} ->
                        {ok, {Time_string@3, {ok, 0}}};

                    {[Time_string@4, Milli_seconds_string], [_]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_end(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam_stdlib:parse_int(_pipe@2)
                                end}};

                    {[_], [Time_string@4, Milli_seconds_string]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_end(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam_stdlib:parse_int(_pipe@2)
                                end}};

                    {_, _} ->
                        {error, nil}
                end,
                fun(_use0@1) ->
                    {Time_string@5, Milli_seconds_result} = _use0@1,
                    case Milli_seconds_result of
                        {ok, Milli_seconds} ->
                            gleam@result:then(
                                parse_time_section(Time_string@5),
                                fun(Time_of_day) ->
                                    {Hour@1, Minute@1, Second@1} = case Time_of_day of
                                        [Hour, Minute, Second] -> {
                                        Hour,
                                            Minute,
                                            Second};
                                        _assert_fail@1 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        file => <<?FILEPATH/utf8>>,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"parse_time_of_day"/utf8>>,
                                                        line => 457,
                                                        value => _assert_fail@1,
                                                        start => 10868,
                                                        'end' => 10915,
                                                        pattern_start => 10879,
                                                        pattern_end => 10901}
                                                )
                                    end,
                                    gleam@result:then(
                                        parse_offset(Offset_string@1),
                                        fun(Offset) ->
                                            gleam@result:then(
                                                generate_offset(Offset),
                                                fun(Offset_string@2) ->
                                                    {ok,
                                                        {{time_of_day,
                                                                Hour@1,
                                                                Minute@1,
                                                                Second@1,
                                                                Milli_seconds},
                                                            Offset_string@2}}
                                                end
                                            )
                                        end
                                    )
                                end
                            );

                        {error, nil} ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-file("src/birl.gleam", 469).
?DOC(" accepts fromats similar to the ones listed for `parse_time_of_day` except that there shoundn't be any offset information\n").
-spec parse_naive_time_of_day(binary()) -> {ok, {time_of_day(), binary()}} |
    {error, nil}.
parse_naive_time_of_day(Value) ->
    Time_string = case {gleam_stdlib:string_starts_with(Value, <<"T"/utf8>>),
        gleam_stdlib:string_starts_with(Value, <<"t"/utf8>>)} of
        {true, _} ->
            gleam@string:drop_start(Value, 1);

        {_, true} ->
            gleam@string:drop_start(Value, 1);

        {_, _} ->
            Value
    end,
    Time_string@1 = gleam@string:replace(Time_string, <<":"/utf8>>, <<""/utf8>>),
    gleam@result:then(
        case {gleam@string:split(Time_string@1, <<"."/utf8>>),
            gleam@string:split(Time_string@1, <<","/utf8>>)} of
            {[_], [_]} ->
                {ok, {Time_string@1, {ok, 0}}};

            {[Time_string@2, Milli_seconds_string], [_]} ->
                {ok,
                    {Time_string@2,
                        begin
                            _pipe = Milli_seconds_string,
                            _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                            _pipe@2 = gleam@string:pad_end(
                                _pipe@1,
                                3,
                                <<"0"/utf8>>
                            ),
                            gleam_stdlib:parse_int(_pipe@2)
                        end}};

            {[_], [Time_string@2, Milli_seconds_string]} ->
                {ok,
                    {Time_string@2,
                        begin
                            _pipe = Milli_seconds_string,
                            _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                            _pipe@2 = gleam@string:pad_end(
                                _pipe@1,
                                3,
                                <<"0"/utf8>>
                            ),
                            gleam_stdlib:parse_int(_pipe@2)
                        end}};

            {_, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Time_string@3, Milli_seconds_result} = _use0,
            case Milli_seconds_result of
                {ok, Milli_seconds} ->
                    gleam@result:then(
                        parse_time_section(Time_string@3),
                        fun(Time_of_day) ->
                            {Hour@1, Minute@1, Second@1} = case Time_of_day of
                                [Hour, Minute, Second] -> {Hour, Minute, Second};
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"birl"/utf8>>,
                                                function => <<"parse_naive_time_of_day"/utf8>>,
                                                line => 506,
                                                value => _assert_fail,
                                                start => 12235,
                                                'end' => 12282,
                                                pattern_start => 12246,
                                                pattern_end => 12268})
                            end,
                            {ok,
                                {{time_of_day,
                                        Hour@1,
                                        Minute@1,
                                        Second@1,
                                        Milli_seconds},
                                    <<"Z"/utf8>>}}
                        end
                    );

                {error, nil} ->
                    {error, nil}
            end
        end
    ).

-file("src/birl.gleam", 1553).
-spec weekday_from_int(integer()) -> {ok, weekday()} | {error, nil}.
weekday_from_int(Weekday) ->
    case Weekday of
        0 ->
            {ok, mon};

        1 ->
            {ok, tue};

        2 ->
            {ok, wed};

        3 ->
            {ok, thu};

        4 ->
            {ok, fri};

        5 ->
            {ok, sat};

        6 ->
            {ok, sun};

        _ ->
            {error, nil}
    end.

-file("src/birl.gleam", 1580).
-spec month_from_int(integer()) -> {ok, month()} | {error, nil}.
month_from_int(Month) ->
    case Month of
        1 ->
            {ok, jan};

        2 ->
            {ok, feb};

        3 ->
            {ok, mar};

        4 ->
            {ok, apr};

        5 ->
            {ok, may};

        6 ->
            {ok, jun};

        7 ->
            {ok, jul};

        8 ->
            {ok, aug};

        9 ->
            {ok, sep};

        10 ->
            {ok, oct};

        11 ->
            {ok, nov};

        12 ->
            {ok, dec};

        _ ->
            {error, nil}
    end.

-file("src/birl.gleam", 82).
?DOC(" use this to get the current time in utc\n").
-spec utc_now() -> time().
utc_now() ->
    Now = birl_ffi:now(),
    Monotonic_now = birl_ffi:monotonic_now(),
    {time, Now, 0, {some, <<"Etc/UTC"/utf8>>}, {some, Monotonic_now}}.

-file("src/birl.gleam", 93).
?DOC(
    " use this to get the current time with a given offset.\n"
    "\n"
    " some examples of acceptable offsets:\n"
    "\n"
    " `\"+330\", \"03:30\", \"-8:00\",\"-7\", \"-0400\", \"03\"`\n"
).
-spec now_with_offset(binary()) -> {ok, time()} | {error, nil}.
now_with_offset(Offset) ->
    gleam@result:then(
        parse_offset(Offset),
        fun(Offset@1) ->
            Now = birl_ffi:now(),
            Monotonic_now = birl_ffi:monotonic_now(),
            _pipe = {time, Now, Offset@1, none, {some, Monotonic_now}},
            {ok, _pipe}
        end
    ).

-file("src/birl.gleam", 101).
-spec now_with_timezone(binary()) -> {ok, time()} | {error, nil}.
now_with_timezone(Timezone) ->
    case gleam@list:key_find(
        [{<<"Africa/Abidjan"/utf8>>, 0},
            {<<"Africa/Algiers"/utf8>>, 3600},
            {<<"Africa/Bissau"/utf8>>, 0},
            {<<"Africa/Cairo"/utf8>>, 7200},
            {<<"Africa/Casablanca"/utf8>>, 3600},
            {<<"Africa/Ceuta"/utf8>>, 3600},
            {<<"Africa/El_Aaiun"/utf8>>, 3600},
            {<<"Africa/Johannesburg"/utf8>>, 7200},
            {<<"Africa/Juba"/utf8>>, 7200},
            {<<"Africa/Khartoum"/utf8>>, 7200},
            {<<"Africa/Lagos"/utf8>>, 3600},
            {<<"Africa/Maputo"/utf8>>, 7200},
            {<<"Africa/Monrovia"/utf8>>, 0},
            {<<"Africa/Nairobi"/utf8>>, 10800},
            {<<"Africa/Ndjamena"/utf8>>, 3600},
            {<<"Africa/Sao_Tome"/utf8>>, 0},
            {<<"Africa/Tripoli"/utf8>>, 7200},
            {<<"Africa/Tunis"/utf8>>, 3600},
            {<<"Africa/Windhoek"/utf8>>, 7200},
            {<<"America/Adak"/utf8>>, -36000},
            {<<"America/Anchorage"/utf8>>, -32400},
            {<<"America/Araguaina"/utf8>>, -10800},
            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
            {<<"America/Argentina/Salta"/utf8>>, -10800},
            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
            {<<"America/Asuncion"/utf8>>, -14400},
            {<<"America/Bahia"/utf8>>, -10800},
            {<<"America/Bahia_Banderas"/utf8>>, -21600},
            {<<"America/Barbados"/utf8>>, -14400},
            {<<"America/Belem"/utf8>>, -10800},
            {<<"America/Belize"/utf8>>, -21600},
            {<<"America/Boa_Vista"/utf8>>, -14400},
            {<<"America/Bogota"/utf8>>, -18000},
            {<<"America/Boise"/utf8>>, -25200},
            {<<"America/Cambridge_Bay"/utf8>>, -25200},
            {<<"America/Campo_Grande"/utf8>>, -14400},
            {<<"America/Cancun"/utf8>>, -18000},
            {<<"America/Caracas"/utf8>>, -14400},
            {<<"America/Cayenne"/utf8>>, -10800},
            {<<"America/Chicago"/utf8>>, -21600},
            {<<"America/Chihuahua"/utf8>>, -21600},
            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
            {<<"America/Costa_Rica"/utf8>>, -21600},
            {<<"America/Cuiaba"/utf8>>, -14400},
            {<<"America/Danmarkshavn"/utf8>>, 0},
            {<<"America/Dawson"/utf8>>, -25200},
            {<<"America/Dawson_Creek"/utf8>>, -25200},
            {<<"America/Denver"/utf8>>, -25200},
            {<<"America/Detroit"/utf8>>, -18000},
            {<<"America/Edmonton"/utf8>>, -25200},
            {<<"America/Eirunepe"/utf8>>, -18000},
            {<<"America/El_Salvador"/utf8>>, -21600},
            {<<"America/Fort_Nelson"/utf8>>, -25200},
            {<<"America/Fortaleza"/utf8>>, -10800},
            {<<"America/Glace_Bay"/utf8>>, -14400},
            {<<"America/Goose_Bay"/utf8>>, -14400},
            {<<"America/Grand_Turk"/utf8>>, -18000},
            {<<"America/Guatemala"/utf8>>, -21600},
            {<<"America/Guayaquil"/utf8>>, -18000},
            {<<"America/Guyana"/utf8>>, -14400},
            {<<"America/Halifax"/utf8>>, -14400},
            {<<"America/Havana"/utf8>>, -18000},
            {<<"America/Hermosillo"/utf8>>, -25200},
            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
            {<<"America/Indiana/Knox"/utf8>>, -21600},
            {<<"America/Indiana/Marengo"/utf8>>, -18000},
            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
            {<<"America/Indiana/Vevay"/utf8>>, -18000},
            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
            {<<"America/Indiana/Winamac"/utf8>>, -18000},
            {<<"America/Inuvik"/utf8>>, -25200},
            {<<"America/Iqaluit"/utf8>>, -18000},
            {<<"America/Jamaica"/utf8>>, -18000},
            {<<"America/Juneau"/utf8>>, -32400},
            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
            {<<"America/La_Paz"/utf8>>, -14400},
            {<<"America/Lima"/utf8>>, -18000},
            {<<"America/Los_Angeles"/utf8>>, -28800},
            {<<"America/Maceio"/utf8>>, -10800},
            {<<"America/Managua"/utf8>>, -21600},
            {<<"America/Manaus"/utf8>>, -14400},
            {<<"America/Martinique"/utf8>>, -14400},
            {<<"America/Matamoros"/utf8>>, -21600},
            {<<"America/Mazatlan"/utf8>>, -25200},
            {<<"America/Menominee"/utf8>>, -21600},
            {<<"America/Merida"/utf8>>, -21600},
            {<<"America/Metlakatla"/utf8>>, -32400},
            {<<"America/Mexico_City"/utf8>>, -21600},
            {<<"America/Miquelon"/utf8>>, -10800},
            {<<"America/Moncton"/utf8>>, -14400},
            {<<"America/Monterrey"/utf8>>, -21600},
            {<<"America/Montevideo"/utf8>>, -10800},
            {<<"America/New_York"/utf8>>, -18000},
            {<<"America/Nome"/utf8>>, -32400},
            {<<"America/Noronha"/utf8>>, -7200},
            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
            {<<"America/North_Dakota/Center"/utf8>>, -21600},
            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
            {<<"America/Nuuk"/utf8>>, -7200},
            {<<"America/Ojinaga"/utf8>>, -21600},
            {<<"America/Panama"/utf8>>, -18000},
            {<<"America/Paramaribo"/utf8>>, -10800},
            {<<"America/Phoenix"/utf8>>, -25200},
            {<<"America/Port-au-Prince"/utf8>>, -18000},
            {<<"America/Porto_Velho"/utf8>>, -14400},
            {<<"America/Puerto_Rico"/utf8>>, -14400},
            {<<"America/Punta_Arenas"/utf8>>, -10800},
            {<<"America/Rankin_Inlet"/utf8>>, -21600},
            {<<"America/Recife"/utf8>>, -10800},
            {<<"America/Regina"/utf8>>, -21600},
            {<<"America/Resolute"/utf8>>, -21600},
            {<<"America/Rio_Branco"/utf8>>, -18000},
            {<<"America/Santarem"/utf8>>, -10800},
            {<<"America/Santiago"/utf8>>, -14400},
            {<<"America/Santo_Domingo"/utf8>>, -14400},
            {<<"America/Sao_Paulo"/utf8>>, -10800},
            {<<"America/Scoresbysund"/utf8>>, -7200},
            {<<"America/Sitka"/utf8>>, -32400},
            {<<"America/St_Johns"/utf8>>, -12600},
            {<<"America/Swift_Current"/utf8>>, -21600},
            {<<"America/Tegucigalpa"/utf8>>, -21600},
            {<<"America/Thule"/utf8>>, -14400},
            {<<"America/Tijuana"/utf8>>, -28800},
            {<<"America/Toronto"/utf8>>, -18000},
            {<<"America/Vancouver"/utf8>>, -28800},
            {<<"America/Whitehorse"/utf8>>, -25200},
            {<<"America/Winnipeg"/utf8>>, -21600},
            {<<"America/Yakutat"/utf8>>, -32400},
            {<<"Antarctica/Casey"/utf8>>, 28800},
            {<<"Antarctica/Davis"/utf8>>, 25200},
            {<<"Antarctica/Macquarie"/utf8>>, 36000},
            {<<"Antarctica/Mawson"/utf8>>, 18000},
            {<<"Antarctica/Palmer"/utf8>>, -10800},
            {<<"Antarctica/Rothera"/utf8>>, -10800},
            {<<"Antarctica/Troll"/utf8>>, 0},
            {<<"Antarctica/Vostok"/utf8>>, 18000},
            {<<"Asia/Almaty"/utf8>>, 18000},
            {<<"Asia/Amman"/utf8>>, 10800},
            {<<"Asia/Anadyr"/utf8>>, 43200},
            {<<"Asia/Aqtau"/utf8>>, 18000},
            {<<"Asia/Aqtobe"/utf8>>, 18000},
            {<<"Asia/Ashgabat"/utf8>>, 18000},
            {<<"Asia/Atyrau"/utf8>>, 18000},
            {<<"Asia/Baghdad"/utf8>>, 10800},
            {<<"Asia/Baku"/utf8>>, 14400},
            {<<"Asia/Bangkok"/utf8>>, 25200},
            {<<"Asia/Barnaul"/utf8>>, 25200},
            {<<"Asia/Beirut"/utf8>>, 7200},
            {<<"Asia/Bishkek"/utf8>>, 21600},
            {<<"Asia/Chita"/utf8>>, 32400},
            {<<"Asia/Colombo"/utf8>>, 19800},
            {<<"Asia/Damascus"/utf8>>, 10800},
            {<<"Asia/Dhaka"/utf8>>, 21600},
            {<<"Asia/Dili"/utf8>>, 32400},
            {<<"Asia/Dubai"/utf8>>, 14400},
            {<<"Asia/Dushanbe"/utf8>>, 18000},
            {<<"Asia/Famagusta"/utf8>>, 7200},
            {<<"Asia/Gaza"/utf8>>, 7200},
            {<<"Asia/Hebron"/utf8>>, 7200},
            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
            {<<"Asia/Hong_Kong"/utf8>>, 28800},
            {<<"Asia/Hovd"/utf8>>, 25200},
            {<<"Asia/Irkutsk"/utf8>>, 28800},
            {<<"Asia/Jakarta"/utf8>>, 25200},
            {<<"Asia/Jayapura"/utf8>>, 32400},
            {<<"Asia/Jerusalem"/utf8>>, 7200},
            {<<"Asia/Kabul"/utf8>>, 16200},
            {<<"Asia/Kamchatka"/utf8>>, 43200},
            {<<"Asia/Karachi"/utf8>>, 18000},
            {<<"Asia/Kathmandu"/utf8>>, 20700},
            {<<"Asia/Khandyga"/utf8>>, 32400},
            {<<"Asia/Kolkata"/utf8>>, 19800},
            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
            {<<"Asia/Kuching"/utf8>>, 28800},
            {<<"Asia/Macau"/utf8>>, 28800},
            {<<"Asia/Magadan"/utf8>>, 39600},
            {<<"Asia/Makassar"/utf8>>, 28800},
            {<<"Asia/Manila"/utf8>>, 28800},
            {<<"Asia/Nicosia"/utf8>>, 7200},
            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
            {<<"Asia/Novosibirsk"/utf8>>, 25200},
            {<<"Asia/Omsk"/utf8>>, 21600},
            {<<"Asia/Oral"/utf8>>, 18000},
            {<<"Asia/Pontianak"/utf8>>, 25200},
            {<<"Asia/Pyongyang"/utf8>>, 32400},
            {<<"Asia/Qatar"/utf8>>, 10800},
            {<<"Asia/Qostanay"/utf8>>, 18000},
            {<<"Asia/Qyzylorda"/utf8>>, 18000},
            {<<"Asia/Riyadh"/utf8>>, 10800},
            {<<"Asia/Sakhalin"/utf8>>, 39600},
            {<<"Asia/Samarkand"/utf8>>, 18000},
            {<<"Asia/Seoul"/utf8>>, 32400},
            {<<"Asia/Shanghai"/utf8>>, 28800},
            {<<"Asia/Singapore"/utf8>>, 28800},
            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
            {<<"Asia/Taipei"/utf8>>, 28800},
            {<<"Asia/Tashkent"/utf8>>, 18000},
            {<<"Asia/Tbilisi"/utf8>>, 14400},
            {<<"Asia/Tehran"/utf8>>, 12600},
            {<<"Asia/Thimphu"/utf8>>, 21600},
            {<<"Asia/Tokyo"/utf8>>, 32400},
            {<<"Asia/Tomsk"/utf8>>, 25200},
            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
            {<<"Asia/Urumqi"/utf8>>, 21600},
            {<<"Asia/Ust-Nera"/utf8>>, 36000},
            {<<"Asia/Vladivostok"/utf8>>, 36000},
            {<<"Asia/Yakutsk"/utf8>>, 32400},
            {<<"Asia/Yangon"/utf8>>, 23400},
            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
            {<<"Asia/Yerevan"/utf8>>, 14400},
            {<<"Atlantic/Azores"/utf8>>, -3600},
            {<<"Atlantic/Bermuda"/utf8>>, -14400},
            {<<"Atlantic/Canary"/utf8>>, 0},
            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
            {<<"Atlantic/Faroe"/utf8>>, 0},
            {<<"Atlantic/Madeira"/utf8>>, 0},
            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
            {<<"Atlantic/Stanley"/utf8>>, -10800},
            {<<"Australia/Adelaide"/utf8>>, 34200},
            {<<"Australia/Brisbane"/utf8>>, 36000},
            {<<"Australia/Broken_Hill"/utf8>>, 34200},
            {<<"Australia/Darwin"/utf8>>, 34200},
            {<<"Australia/Eucla"/utf8>>, 31500},
            {<<"Australia/Hobart"/utf8>>, 36000},
            {<<"Australia/Lindeman"/utf8>>, 36000},
            {<<"Australia/Lord_Howe"/utf8>>, 37800},
            {<<"Australia/Melbourne"/utf8>>, 36000},
            {<<"Australia/Perth"/utf8>>, 28800},
            {<<"Australia/Sydney"/utf8>>, 36000},
            {<<"Etc/GMT"/utf8>>, 0},
            {<<"Etc/GMT+1"/utf8>>, -3600},
            {<<"Etc/GMT+10"/utf8>>, -36000},
            {<<"Etc/GMT+11"/utf8>>, -39600},
            {<<"Etc/GMT+12"/utf8>>, -43200},
            {<<"Etc/GMT+2"/utf8>>, -7200},
            {<<"Etc/GMT+3"/utf8>>, -10800},
            {<<"Etc/GMT+4"/utf8>>, -14400},
            {<<"Etc/GMT+5"/utf8>>, -18000},
            {<<"Etc/GMT+6"/utf8>>, -21600},
            {<<"Etc/GMT+7"/utf8>>, -25200},
            {<<"Etc/GMT+8"/utf8>>, -28800},
            {<<"Etc/GMT+9"/utf8>>, -32400},
            {<<"Etc/GMT-1"/utf8>>, 3600},
            {<<"Etc/GMT-10"/utf8>>, 36000},
            {<<"Etc/GMT-11"/utf8>>, 39600},
            {<<"Etc/GMT-12"/utf8>>, 43200},
            {<<"Etc/GMT-13"/utf8>>, 46800},
            {<<"Etc/GMT-14"/utf8>>, 50400},
            {<<"Etc/GMT-2"/utf8>>, 7200},
            {<<"Etc/GMT-3"/utf8>>, 10800},
            {<<"Etc/GMT-4"/utf8>>, 14400},
            {<<"Etc/GMT-5"/utf8>>, 18000},
            {<<"Etc/GMT-6"/utf8>>, 21600},
            {<<"Etc/GMT-7"/utf8>>, 25200},
            {<<"Etc/GMT-8"/utf8>>, 28800},
            {<<"Etc/GMT-9"/utf8>>, 32400},
            {<<"Etc/UTC"/utf8>>, 0},
            {<<"Europe/Andorra"/utf8>>, 3600},
            {<<"Europe/Astrakhan"/utf8>>, 14400},
            {<<"Europe/Athens"/utf8>>, 7200},
            {<<"Europe/Belgrade"/utf8>>, 3600},
            {<<"Europe/Berlin"/utf8>>, 3600},
            {<<"Europe/Brussels"/utf8>>, 3600},
            {<<"Europe/Bucharest"/utf8>>, 7200},
            {<<"Europe/Budapest"/utf8>>, 3600},
            {<<"Europe/Chisinau"/utf8>>, 7200},
            {<<"Europe/Dublin"/utf8>>, 3600},
            {<<"Europe/Gibraltar"/utf8>>, 3600},
            {<<"Europe/Helsinki"/utf8>>, 7200},
            {<<"Europe/Istanbul"/utf8>>, 10800},
            {<<"Europe/Kaliningrad"/utf8>>, 7200},
            {<<"Europe/Kirov"/utf8>>, 10800},
            {<<"Europe/Kyiv"/utf8>>, 7200},
            {<<"Europe/Lisbon"/utf8>>, 0},
            {<<"Europe/London"/utf8>>, 0},
            {<<"Europe/Madrid"/utf8>>, 3600},
            {<<"Europe/Malta"/utf8>>, 3600},
            {<<"Europe/Minsk"/utf8>>, 10800},
            {<<"Europe/Moscow"/utf8>>, 10800},
            {<<"Europe/Paris"/utf8>>, 3600},
            {<<"Europe/Prague"/utf8>>, 3600},
            {<<"Europe/Riga"/utf8>>, 7200},
            {<<"Europe/Rome"/utf8>>, 3600},
            {<<"Europe/Samara"/utf8>>, 14400},
            {<<"Europe/Saratov"/utf8>>, 14400},
            {<<"Europe/Simferopol"/utf8>>, 10800},
            {<<"Europe/Sofia"/utf8>>, 7200},
            {<<"Europe/Tallinn"/utf8>>, 7200},
            {<<"Europe/Tirane"/utf8>>, 3600},
            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
            {<<"Europe/Vienna"/utf8>>, 3600},
            {<<"Europe/Vilnius"/utf8>>, 7200},
            {<<"Europe/Volgograd"/utf8>>, 10800},
            {<<"Europe/Warsaw"/utf8>>, 3600},
            {<<"Europe/Zurich"/utf8>>, 3600},
            {<<"Indian/Chagos"/utf8>>, 21600},
            {<<"Indian/Maldives"/utf8>>, 18000},
            {<<"Indian/Mauritius"/utf8>>, 14400},
            {<<"Pacific/Apia"/utf8>>, 46800},
            {<<"Pacific/Auckland"/utf8>>, 43200},
            {<<"Pacific/Bougainville"/utf8>>, 39600},
            {<<"Pacific/Chatham"/utf8>>, 45900},
            {<<"Pacific/Easter"/utf8>>, -21600},
            {<<"Pacific/Efate"/utf8>>, 39600},
            {<<"Pacific/Fakaofo"/utf8>>, 46800},
            {<<"Pacific/Fiji"/utf8>>, 43200},
            {<<"Pacific/Galapagos"/utf8>>, -21600},
            {<<"Pacific/Gambier"/utf8>>, -32400},
            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
            {<<"Pacific/Guam"/utf8>>, 36000},
            {<<"Pacific/Honolulu"/utf8>>, -36000},
            {<<"Pacific/Kanton"/utf8>>, 46800},
            {<<"Pacific/Kiritimati"/utf8>>, 50400},
            {<<"Pacific/Kosrae"/utf8>>, 39600},
            {<<"Pacific/Kwajalein"/utf8>>, 43200},
            {<<"Pacific/Marquesas"/utf8>>, -34200},
            {<<"Pacific/Nauru"/utf8>>, 43200},
            {<<"Pacific/Niue"/utf8>>, -39600},
            {<<"Pacific/Norfolk"/utf8>>, 39600},
            {<<"Pacific/Noumea"/utf8>>, 39600},
            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
            {<<"Pacific/Palau"/utf8>>, 32400},
            {<<"Pacific/Pitcairn"/utf8>>, -28800},
            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
            {<<"Pacific/Rarotonga"/utf8>>, -36000},
            {<<"Pacific/Tahiti"/utf8>>, -36000},
            {<<"Pacific/Tarawa"/utf8>>, 43200},
            {<<"Pacific/Tongatapu"/utf8>>, 46800}],
        Timezone
    ) of
        {ok, Offset} ->
            Now = birl_ffi:now(),
            Monotonic_now = birl_ffi:monotonic_now(),
            _pipe = {time,
                Now,
                Offset * 1000000,
                {some, Timezone},
                {some, Monotonic_now}},
            {ok, _pipe};

        {error, nil} ->
            {error, nil}
    end.

-file("src/birl.gleam", 119).
-spec monotonic_now() -> integer().
monotonic_now() ->
    birl_ffi:monotonic_now().

-file("src/birl.gleam", 1320).
-spec to_parts(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer(), integer()},
    binary()}.
to_parts(Value) ->
    case Value of
        {time, T, O, _, _} ->
            {Date, Time} = birl_ffi:to_parts(T, O),
            Offset@1 = case generate_offset(O) of
                {ok, Offset} -> Offset;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl"/utf8>>,
                                function => <<"to_parts"/utf8>>,
                                line => 1324,
                                value => _assert_fail,
                                start => 33803,
                                'end' => 33845,
                                pattern_start => 33814,
                                pattern_end => 33824})
            end,
            {Date, Time, Offset@1}
    end.

-file("src/birl.gleam", 129).
?DOC(" returns a string which is the date part of an ISO8601 string along with the offset\n").
-spec to_date_string(time()) -> binary().
to_date_string(Value) ->
    {{Year, Month, Day}, _, Offset} = to_parts(Value),
    <<<<<<<<<<(erlang:integer_to_binary(Year))/binary, "-"/utf8>>/binary,
                    (begin
                        _pipe = Month,
                        _pipe@1 = erlang:integer_to_binary(_pipe),
                        gleam@string:pad_start(_pipe@1, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "-"/utf8>>/binary,
            (begin
                _pipe@2 = Day,
                _pipe@3 = erlang:integer_to_binary(_pipe@2),
                gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-file("src/birl.gleam", 149).
?DOC(" like `to_date_string` except it does not contain the offset\n").
-spec to_naive_date_string(time()) -> binary().
to_naive_date_string(Value) ->
    {{Year, Month, Day}, _, _} = to_parts(Value),
    <<<<<<<<(erlang:integer_to_binary(Year))/binary, "-"/utf8>>/binary,
                (begin
                    _pipe = Month,
                    _pipe@1 = erlang:integer_to_binary(_pipe),
                    gleam@string:pad_start(_pipe@1, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "-"/utf8>>/binary,
        (begin
            _pipe@2 = Day,
            _pipe@3 = erlang:integer_to_binary(_pipe@2),
            gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
        end)/binary>>.

-file("src/birl.gleam", 168).
?DOC(" returns a string which is the time part of an ISO8601 string along with the offset\n").
-spec to_time_string(time()) -> binary().
to_time_string(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, Offset} = to_parts(Value),
    <<<<<<<<<<<<<<(begin
                                    _pipe = Hour,
                                    _pipe@1 = erlang:integer_to_binary(_pipe),
                                    gleam@string:pad_start(
                                        _pipe@1,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary,
                                ":"/utf8>>/binary,
                            (begin
                                _pipe@2 = Minute,
                                _pipe@3 = erlang:integer_to_binary(_pipe@2),
                                gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@4 = Second,
                        _pipe@5 = erlang:integer_to_binary(_pipe@4),
                        gleam@string:pad_start(_pipe@5, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "."/utf8>>/binary,
            (begin
                _pipe@6 = Milli_second,
                _pipe@7 = erlang:integer_to_binary(_pipe@6),
                gleam@string:pad_start(_pipe@7, 3, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-file("src/birl.gleam", 198).
?DOC(" like `to_time_string` except it does not contain the offset\n").
-spec to_naive_time_string(time()) -> binary().
to_naive_time_string(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, _} = to_parts(Value),
    <<<<<<<<<<<<(begin
                                _pipe = Hour,
                                _pipe@1 = erlang:integer_to_binary(_pipe),
                                gleam@string:pad_start(_pipe@1, 2, <<"0"/utf8>>)
                            end)/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@2 = Minute,
                            _pipe@3 = erlang:integer_to_binary(_pipe@2),
                            gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@4 = Second,
                    _pipe@5 = erlang:integer_to_binary(_pipe@4),
                    gleam@string:pad_start(_pipe@5, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@6 = Milli_second,
            _pipe@7 = erlang:integer_to_binary(_pipe@6),
            gleam@string:pad_start(_pipe@7, 3, <<"0"/utf8>>)
        end)/binary>>.

-file("src/birl.gleam", 226).
-spec to_iso8601(time()) -> binary().
to_iso8601(Value) ->
    {{Year, Month, Day}, {Hour, Minute, Second, Milli_second}, Offset} = to_parts(
        Value
    ),
    <<<<<<<<<<<<<<<<<<<<<<<<<<(erlang:integer_to_binary(Year))/binary,
                                                        "-"/utf8>>/binary,
                                                    (begin
                                                        _pipe = Month,
                                                        _pipe@1 = erlang:integer_to_binary(
                                                            _pipe
                                                        ),
                                                        gleam@string:pad_start(
                                                            _pipe@1,
                                                            2,
                                                            <<"0"/utf8>>
                                                        )
                                                    end)/binary>>/binary,
                                                "-"/utf8>>/binary,
                                            (begin
                                                _pipe@2 = Day,
                                                _pipe@3 = erlang:integer_to_binary(
                                                    _pipe@2
                                                ),
                                                gleam@string:pad_start(
                                                    _pipe@3,
                                                    2,
                                                    <<"0"/utf8>>
                                                )
                                            end)/binary>>/binary,
                                        "T"/utf8>>/binary,
                                    (begin
                                        _pipe@4 = Hour,
                                        _pipe@5 = erlang:integer_to_binary(
                                            _pipe@4
                                        ),
                                        gleam@string:pad_start(
                                            _pipe@5,
                                            2,
                                            <<"0"/utf8>>
                                        )
                                    end)/binary>>/binary,
                                ":"/utf8>>/binary,
                            (begin
                                _pipe@6 = Minute,
                                _pipe@7 = erlang:integer_to_binary(_pipe@6),
                                gleam@string:pad_start(_pipe@7, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@8 = Second,
                        _pipe@9 = erlang:integer_to_binary(_pipe@8),
                        gleam@string:pad_start(_pipe@9, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "."/utf8>>/binary,
            (begin
                _pipe@10 = Milli_second,
                _pipe@11 = erlang:integer_to_binary(_pipe@10),
                gleam@string:pad_start(_pipe@11, 3, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-file("src/birl.gleam", 535).
?DOC(" the naive format is the same as ISO8601 except that it does not contain the offset\n").
-spec to_naive(time()) -> binary().
to_naive(Value) ->
    {{Year, Month, Day}, {Hour, Minute, Second, Milli_second}, _} = to_parts(
        Value
    ),
    <<<<<<<<<<<<<<<<<<<<<<<<(erlang:integer_to_binary(Year))/binary, "-"/utf8>>/binary,
                                                (begin
                                                    _pipe = Month,
                                                    _pipe@1 = erlang:integer_to_binary(
                                                        _pipe
                                                    ),
                                                    gleam@string:pad_start(
                                                        _pipe@1,
                                                        2,
                                                        <<"0"/utf8>>
                                                    )
                                                end)/binary>>/binary,
                                            "-"/utf8>>/binary,
                                        (begin
                                            _pipe@2 = Day,
                                            _pipe@3 = erlang:integer_to_binary(
                                                _pipe@2
                                            ),
                                            gleam@string:pad_start(
                                                _pipe@3,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end)/binary>>/binary,
                                    "T"/utf8>>/binary,
                                (begin
                                    _pipe@4 = Hour,
                                    _pipe@5 = erlang:integer_to_binary(_pipe@4),
                                    gleam@string:pad_start(
                                        _pipe@5,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary>>/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@6 = Minute,
                            _pipe@7 = erlang:integer_to_binary(_pipe@6),
                            gleam@string:pad_start(_pipe@7, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@8 = Second,
                    _pipe@9 = erlang:integer_to_binary(_pipe@8),
                    gleam@string:pad_start(_pipe@9, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@10 = Milli_second,
            _pipe@11 = erlang:integer_to_binary(_pipe@10),
            gleam@string:pad_start(_pipe@11, 3, <<"0"/utf8>>)
        end)/binary>>.

-file("src/birl.gleam", 1107).
-spec month(time()) -> month().
month(Value) ->
    {{_, Month, _}, _, _} = to_parts(Value),
    Month@2 = case month_from_int(Month) of
        {ok, Month@1} -> Month@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"month"/utf8>>,
                        line => 1109,
                        value => _assert_fail,
                        start => 27988,
                        'end' => 28032,
                        pattern_start => 27999,
                        pattern_end => 28008})
    end,
    Month@2.

-file("src/birl.gleam", 1113).
-spec string_month(time()) -> binary().
string_month(Value) ->
    case month(Value) of
        jan ->
            <<"January"/utf8>>;

        feb ->
            <<"February"/utf8>>;

        mar ->
            <<"March"/utf8>>;

        apr ->
            <<"April"/utf8>>;

        may ->
            <<"May"/utf8>>;

        jun ->
            <<"June"/utf8>>;

        jul ->
            <<"July"/utf8>>;

        aug ->
            <<"August"/utf8>>;

        sep ->
            <<"September"/utf8>>;

        oct ->
            <<"October"/utf8>>;

        nov ->
            <<"November"/utf8>>;

        dec ->
            <<"December"/utf8>>
    end.

-file("src/birl.gleam", 1130).
-spec short_string_month(time()) -> binary().
short_string_month(Value) ->
    case month(Value) of
        jan ->
            <<"Jan"/utf8>>;

        feb ->
            <<"Feb"/utf8>>;

        mar ->
            <<"Mar"/utf8>>;

        apr ->
            <<"Apr"/utf8>>;

        may ->
            <<"May"/utf8>>;

        jun ->
            <<"Jun"/utf8>>;

        jul ->
            <<"Jul"/utf8>>;

        aug ->
            <<"Aug"/utf8>>;

        sep ->
            <<"Sep"/utf8>>;

        oct ->
            <<"Oct"/utf8>>;

        nov ->
            <<"Nov"/utf8>>;

        dec ->
            <<"Dec"/utf8>>
    end.

-file("src/birl.gleam", 1225).
-spec get_day(time()) -> day().
get_day(Value) ->
    {{Year, Month, Day}, _, _} = to_parts(Value),
    {day, Year, Month, Day}.

-file("src/birl.gleam", 1244).
-spec get_time_of_day(time()) -> time_of_day().
get_time_of_day(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, _} = to_parts(Value),
    {time_of_day, Hour, Minute, Second, Milli_second}.

-file("src/birl.gleam", 1251).
?DOC(" calculates erlang datetime using the offset in the DateTime value\n").
-spec to_erlang_datetime(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer()}}.
to_erlang_datetime(Value) ->
    {Date, {Hour, Minute, Second, _}, _} = to_parts(Value),
    {Date, {Hour, Minute, Second}}.

-file("src/birl.gleam", 1258).
?DOC(" calculates the universal erlang datetime regardless of the offset in the DateTime value\n").
-spec to_erlang_universal_datetime(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer()}}.
to_erlang_universal_datetime(Value) ->
    Value@2 = case set_offset(Value, <<"Z"/utf8>>) of
        {ok, Value@1} -> Value@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"to_erlang_universal_datetime"/utf8>>,
                        line => 1261,
                        value => _assert_fail,
                        start => 31937,
                        'end' => 31982,
                        pattern_start => 31948,
                        pattern_end => 31957})
    end,
    {Date, {Hour, Minute, Second, _}, _} = to_parts(Value@2),
    {Date, {Hour, Minute, Second}}.

-file("src/birl.gleam", 1309).
-spec from_parts(
    {integer(), integer(), integer()},
    {integer(), integer(), integer(), integer()},
    binary()
) -> {ok, time()} | {error, nil}.
from_parts(Date, Time, Offset) ->
    gleam@result:then(
        parse_offset(Offset),
        fun(Offset_number) ->
            _pipe = birl_ffi:from_parts({Date, Time}, Offset_number),
            _pipe@1 = {time, _pipe, Offset_number, none, none},
            {ok, _pipe@1}
        end
    ).

-file("src/birl.gleam", 297).
?DOC(
    " if you need to parse an `ISO8601` string, this is probably what you're looking for.\n"
    "\n"
    " given the huge surface area that `ISO8601` covers, it does not make sense for `birl`\n"
    " to support all of it in one function, so this function parses only strings for which both\n"
    " day and time of day can be extracted or deduced. Some acceptable examples are given below:\n"
    "\n"
    "   - `2019t14-4` -> `2019-01-01T14:00:00.000-04:00`\n"
    "\n"
    "   - `2019-03-26t14:00.9z` -> `2019-03-26T14:00:00.900Z`\n"
    "\n"
    "   - `2019-03-26+330` -> `2019-03-26T00:00:00.000+03:30`\n"
    "\n"
    "   - `20190326t1400-4` -> `2019-03-26T14:00:00.000-04:00`\n"
    "\n"
    "   - `19051222T16:38-3` -> `1905-12-22T16:38:00.000-03:00`\n"
    "\n"
    "   - `2019-03-26 14:30:00.9Z` -> `2019-03-26T14:30:00.900Z`\n"
    "\n"
    "   - `2019-03-26T14:00:00.9Z` -> `2019-03-26T14:00:00.900Z`\n"
    "\n"
    "   - `1905-12-22 16:38:23-3` -> `1905-12-22T16:38:23.000-03:00`\n"
    "\n"
    "   - `2019-03-26T14:00:00,4999Z` -> `2019-03-26T14:00:00.499Z`\n"
    "\n"
    "   - `1905-12-22T163823+0330` -> `1905-12-22T16:38:23.000+03:30`\n"
    "\n"
    "   - `1905-12-22T16:38:23.000+03:30` -> `1905-12-22T16:38:23.000+03:30`\n"
).
-spec parse(binary()) -> {ok, time()} | {error, nil}.
parse(Value) ->
    Offset_pattern@1 = case gleam@regexp:from_string(<<"(.*)([+|\\-].*)"/utf8>>) of
        {ok, Offset_pattern} -> Offset_pattern;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"parse"/utf8>>,
                        line => 298,
                        value => _assert_fail,
                        start => 6155,
                        'end' => 6224,
                        pattern_start => 6166,
                        pattern_end => 6184})
    end,
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        case {gleam@string:split(Value@1, <<"T"/utf8>>),
            gleam@string:split(Value@1, <<"t"/utf8>>),
            gleam@string:split(Value@1, <<" "/utf8>>)} of
            {[Day_string, Time_string], _, _} ->
                {ok, {Day_string, Time_string}};

            {_, [Day_string, Time_string], _} ->
                {ok, {Day_string, Time_string}};

            {_, _, [Day_string, Time_string]} ->
                {ok, {Day_string, Time_string}};

            {[_], [_], [_]} ->
                {ok, {Value@1, <<"00"/utf8>>}};

            {_, _, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Day_string@1, Offsetted_time_string} = _use0,
            Day_string@2 = gleam@string:trim(Day_string@1),
            Offsetted_time_string@1 = gleam@string:trim(Offsetted_time_string),
            gleam@result:then(
                case gleam_stdlib:string_ends_with(
                    Offsetted_time_string@1,
                    <<"Z"/utf8>>
                )
                orelse gleam_stdlib:string_ends_with(
                    Offsetted_time_string@1,
                    <<"z"/utf8>>
                ) of
                    true ->
                        {ok,
                            {Day_string@2,
                                gleam@string:drop_end(
                                    Offsetted_time_string@1,
                                    1
                                ),
                                <<"+00:00"/utf8>>}};

                    false ->
                        case gleam@regexp:scan(
                            Offset_pattern@1,
                            Offsetted_time_string@1
                        ) of
                            [{match,
                                    _,
                                    [{some, Time_string@1},
                                        {some, Offset_string}]}] ->
                                {ok,
                                    {Day_string@2, Time_string@1, Offset_string}};

                            _ ->
                                case gleam@regexp:scan(
                                    Offset_pattern@1,
                                    Day_string@2
                                ) of
                                    [{match,
                                            _,
                                            [{some, Day_string@3},
                                                {some, Offset_string@1}]}] ->
                                        {ok,
                                            {Day_string@3,
                                                <<"00"/utf8>>,
                                                Offset_string@1}};

                                    _ ->
                                        {error, nil}
                                end
                        end
                end,
                fun(_use0@1) ->
                    {Day_string@4, Time_string@2, Offset_string@2} = _use0@1,
                    Time_string@3 = gleam@string:replace(
                        Time_string@2,
                        <<":"/utf8>>,
                        <<""/utf8>>
                    ),
                    gleam@result:then(
                        case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                            gleam@string:split(Time_string@3, <<","/utf8>>)} of
                            {[_], [_]} ->
                                {ok, {Time_string@3, {ok, 0}}};

                            {[Time_string@4, Milli_seconds_string], [_]} ->
                                {ok,
                                    {Time_string@4,
                                        begin
                                            _pipe = Milli_seconds_string,
                                            _pipe@1 = gleam@string:slice(
                                                _pipe,
                                                0,
                                                3
                                            ),
                                            _pipe@2 = gleam@string:pad_end(
                                                _pipe@1,
                                                3,
                                                <<"0"/utf8>>
                                            ),
                                            gleam_stdlib:parse_int(_pipe@2)
                                        end}};

                            {[_], [Time_string@4, Milli_seconds_string]} ->
                                {ok,
                                    {Time_string@4,
                                        begin
                                            _pipe = Milli_seconds_string,
                                            _pipe@1 = gleam@string:slice(
                                                _pipe,
                                                0,
                                                3
                                            ),
                                            _pipe@2 = gleam@string:pad_end(
                                                _pipe@1,
                                                3,
                                                <<"0"/utf8>>
                                            ),
                                            gleam_stdlib:parse_int(_pipe@2)
                                        end}};

                            {_, _} ->
                                {error, nil}
                        end,
                        fun(_use0@2) ->
                            {Time_string@5, Milli_seconds_result} = _use0@2,
                            case Milli_seconds_result of
                                {ok, Milli_seconds} ->
                                    gleam@result:then(
                                        parse_date_section(Day_string@4),
                                        fun(Day) ->
                                            {Year@1, Month@1, Date@1} = case Day of
                                                [Year, Month, Date] -> {
                                                Year,
                                                    Month,
                                                    Date};
                                                _assert_fail@1 ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                file => <<?FILEPATH/utf8>>,
                                                                module => <<"birl"/utf8>>,
                                                                function => <<"parse"/utf8>>,
                                                                line => 370,
                                                                value => _assert_fail@1,
                                                                start => 8278,
                                                                'end' => 8314,
                                                                pattern_start => 8289,
                                                                pattern_end => 8308}
                                                        )
                                            end,
                                            gleam@result:then(
                                                parse_time_section(
                                                    Time_string@5
                                                ),
                                                fun(Time_of_day) ->
                                                    {
                                                    Hour@1,
                                                        Minute@1,
                                                        Second@1} = case Time_of_day of
                                                        [Hour, Minute, Second] -> {
                                                        Hour,
                                                            Minute,
                                                            Second};
                                                        _assert_fail@2 ->
                                                            erlang:error(
                                                                    #{gleam_error => let_assert,
                                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                        file => <<?FILEPATH/utf8>>,
                                                                        module => <<"birl"/utf8>>,
                                                                        function => <<"parse"/utf8>>,
                                                                        line => 373,
                                                                        value => _assert_fail@2,
                                                                        start => 8392,
                                                                        'end' => 8439,
                                                                        pattern_start => 8403,
                                                                        pattern_end => 8425}
                                                                )
                                                    end,
                                                    from_parts(
                                                        {Year@1,
                                                            Month@1,
                                                            Date@1},
                                                        {Hour@1,
                                                            Minute@1,
                                                            Second@1,
                                                            Milli_seconds},
                                                        Offset_string@2
                                                    )
                                                end
                                            )
                                        end
                                    );

                                {error, nil} ->
                                    {error, nil}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/birl.gleam", 579).
?DOC(" accepts fromats similar to the ones listed for `parse` except that there shoundn't be any offset information\n").
-spec from_naive(binary()) -> {ok, time()} | {error, nil}.
from_naive(Value) ->
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        case {gleam@string:split(Value@1, <<"T"/utf8>>),
            gleam@string:split(Value@1, <<"t"/utf8>>),
            gleam@string:split(Value@1, <<" "/utf8>>)} of
            {[Day_string, Time_string], _, _} ->
                {ok, {Day_string, Time_string}};

            {_, [Day_string, Time_string], _} ->
                {ok, {Day_string, Time_string}};

            {_, _, [Day_string, Time_string]} ->
                {ok, {Day_string, Time_string}};

            {[_], [_], [_]} ->
                {ok, {Value@1, <<"00"/utf8>>}};

            {_, _, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Day_string@1, Time_string@1} = _use0,
            Day_string@2 = gleam@string:trim(Day_string@1),
            Time_string@2 = gleam@string:trim(Time_string@1),
            Time_string@3 = gleam@string:replace(
                Time_string@2,
                <<":"/utf8>>,
                <<""/utf8>>
            ),
            gleam@result:then(
                case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                    gleam@string:split(Time_string@3, <<","/utf8>>)} of
                    {[_], [_]} ->
                        {ok, {Time_string@3, {ok, 0}}};

                    {[Time_string@4, Milli_seconds_string], [_]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_end(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam_stdlib:parse_int(_pipe@2)
                                end}};

                    {[_], [Time_string@4, Milli_seconds_string]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_end(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam_stdlib:parse_int(_pipe@2)
                                end}};

                    {_, _} ->
                        {error, nil}
                end,
                fun(_use0@1) ->
                    {Time_string@5, Milli_seconds_result} = _use0@1,
                    case Milli_seconds_result of
                        {ok, Milli_seconds} ->
                            gleam@result:then(
                                parse_date_section(Day_string@2),
                                fun(Day) ->
                                    {Year@1, Month@1, Date@1} = case Day of
                                        [Year, Month, Date] -> {
                                        Year,
                                            Month,
                                            Date};
                                        _assert_fail ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        file => <<?FILEPATH/utf8>>,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"from_naive"/utf8>>,
                                                        line => 622,
                                                        value => _assert_fail,
                                                        start => 14936,
                                                        'end' => 14972,
                                                        pattern_start => 14947,
                                                        pattern_end => 14966}
                                                )
                                    end,
                                    gleam@result:then(
                                        parse_time_section(Time_string@5),
                                        fun(Time_of_day) ->
                                            {Hour@1, Minute@1, Second@1} = case Time_of_day of
                                                [Hour, Minute, Second] -> {
                                                Hour,
                                                    Minute,
                                                    Second};
                                                _assert_fail@1 ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                file => <<?FILEPATH/utf8>>,
                                                                module => <<"birl"/utf8>>,
                                                                function => <<"from_naive"/utf8>>,
                                                                line => 625,
                                                                value => _assert_fail@1,
                                                                start => 15050,
                                                                'end' => 15097,
                                                                pattern_start => 15061,
                                                                pattern_end => 15083}
                                                        )
                                            end,
                                            from_parts(
                                                {Year@1, Month@1, Date@1},
                                                {Hour@1,
                                                    Minute@1,
                                                    Second@1,
                                                    Milli_seconds},
                                                <<"Z"/utf8>>
                                            )
                                        end
                                    )
                                end
                            );

                        {error, nil} ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-file("src/birl.gleam", 1212).
-spec set_day(time(), day()) -> time().
set_day(Value, Day) ->
    {_, Time, Offset} = to_parts(Value),
    {day, Year, Month, Date} = Day,
    New_value@1 = case from_parts({Year, Month, Date}, Time, Offset) of
        {ok, New_value} -> New_value;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"set_day"/utf8>>,
                        line => 1215,
                        value => _assert_fail,
                        start => 30617,
                        'end' => 30690,
                        pattern_start => 30628,
                        pattern_end => 30641})
    end,
    {time,
        erlang:element(2, New_value@1),
        erlang:element(3, New_value@1),
        erlang:element(4, Value),
        erlang:element(5, Value)}.

-file("src/birl.gleam", 1230).
-spec set_time_of_day(time(), time_of_day()) -> time().
set_time_of_day(Value, Time) ->
    {Date, _, Offset} = to_parts(Value),
    {time_of_day, Hour, Minute, Second, Milli_second} = Time,
    New_value@1 = case from_parts(
        Date,
        {Hour, Minute, Second, Milli_second},
        Offset
    ) of
        {ok, New_value} -> New_value;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"set_time_of_day"/utf8>>,
                        line => 1233,
                        value => _assert_fail,
                        start => 31084,
                        'end' => 31178,
                        pattern_start => 31095,
                        pattern_end => 31108})
    end,
    {time,
        erlang:element(2, New_value@1),
        erlang:element(3, New_value@1),
        erlang:element(4, Value),
        erlang:element(5, Value)}.

-file("src/birl.gleam", 1040).
-spec weekday(time()) -> weekday().
weekday(Value) ->
    case Value of
        {time, T, O, _, _} ->
            Weekday@1 = case weekday_from_int(birl_ffi:weekday(T, O)) of
                {ok, Weekday} -> Weekday;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl"/utf8>>,
                                function => <<"weekday"/utf8>>,
                                line => 1043,
                                value => _assert_fail,
                                start => 26474,
                                'end' => 26534,
                                pattern_start => 26485,
                                pattern_end => 26496})
            end,
            Weekday@1
    end.

-file("src/birl.gleam", 1049).
-spec string_weekday(time()) -> binary().
string_weekday(Value) ->
    _pipe = weekday(Value),
    weekday_to_string(_pipe).

-file("src/birl.gleam", 1054).
-spec short_string_weekday(time()) -> binary().
short_string_weekday(Value) ->
    _pipe = weekday(Value),
    weekday_to_short_string(_pipe).

-file("src/birl.gleam", 639).
?DOC(" see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)\n").
-spec to_http(time()) -> binary().
to_http(Value) ->
    Value@2 = case set_offset(Value, <<"Z"/utf8>>) of
        {ok, Value@1} -> Value@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"to_http"/utf8>>,
                        line => 640,
                        value => _assert_fail,
                        start => 15380,
                        'end' => 15425,
                        pattern_start => 15391,
                        pattern_end => 15400})
    end,
    {{Year, _, Day}, {Hour, Minute, Second, _}, _} = to_parts(Value@2),
    Short_weekday = short_string_weekday(Value@2),
    Short_month = short_string_month(Value@2),
    <<<<<<<<<<<<<<<<<<<<<<<<<<Short_weekday/binary, ", "/utf8>>/binary,
                                                    (begin
                                                        _pipe = Day,
                                                        _pipe@1 = erlang:integer_to_binary(
                                                            _pipe
                                                        ),
                                                        gleam@string:pad_start(
                                                            _pipe@1,
                                                            2,
                                                            <<"0"/utf8>>
                                                        )
                                                    end)/binary>>/binary,
                                                " "/utf8>>/binary,
                                            Short_month/binary>>/binary,
                                        " "/utf8>>/binary,
                                    (erlang:integer_to_binary(Year))/binary>>/binary,
                                " "/utf8>>/binary,
                            (begin
                                _pipe@2 = Hour,
                                _pipe@3 = erlang:integer_to_binary(_pipe@2),
                                gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@4 = Minute,
                        _pipe@5 = erlang:integer_to_binary(_pipe@4),
                        gleam@string:pad_start(_pipe@5, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                ":"/utf8>>/binary,
            (begin
                _pipe@6 = Second,
                _pipe@7 = erlang:integer_to_binary(_pipe@6),
                gleam@string:pad_start(_pipe@7, 2, <<"0"/utf8>>)
            end)/binary>>/binary,
        " GMT"/utf8>>.

-file("src/birl.gleam", 678).
?DOC(" like `to_http` but assumes the offset in the DateTime value instead of `GMT`\n").
-spec to_http_with_offset(time()) -> binary().
to_http_with_offset(Value) ->
    {{Year, _, Day}, {Hour, Minute, Second, _}, Offset} = to_parts(Value),
    Short_weekday = short_string_weekday(Value),
    Short_month = short_string_month(Value),
    Offset@1 = case Offset of
        <<"Z"/utf8>> ->
            <<"GMT"/utf8>>;

        _ ->
            Offset
    end,
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<Short_weekday/binary, ", "/utf8>>/binary,
                                                        (begin
                                                            _pipe = Day,
                                                            _pipe@1 = erlang:integer_to_binary(
                                                                _pipe
                                                            ),
                                                            gleam@string:pad_start(
                                                                _pipe@1,
                                                                2,
                                                                <<"0"/utf8>>
                                                            )
                                                        end)/binary>>/binary,
                                                    " "/utf8>>/binary,
                                                Short_month/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (erlang:integer_to_binary(Year))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (begin
                                    _pipe@2 = Hour,
                                    _pipe@3 = erlang:integer_to_binary(_pipe@2),
                                    gleam@string:pad_start(
                                        _pipe@3,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary>>/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@4 = Minute,
                            _pipe@5 = erlang:integer_to_binary(_pipe@4),
                            gleam@string:pad_start(_pipe@5, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@6 = Second,
                    _pipe@7 = erlang:integer_to_binary(_pipe@6),
                    gleam@string:pad_start(_pipe@7, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            " "/utf8>>/binary,
        Offset@1/binary>>.

-file("src/birl.gleam", 61).
?DOC(" use this to get the current time in the local timezone offset\n").
-spec now() -> time().
now() ->
    Now = birl_ffi:now(),
    Offset_in_minutes = birl_ffi:local_offset(),
    Monotonic_now = birl_ffi:monotonic_now(),
    Timezone = birl_ffi:local_timezone(),
    {time,
        Now,
        Offset_in_minutes * 60000000,
        begin
            _pipe = gleam@option:map(
                Timezone,
                fun(Tz) ->
                    case gleam@list:any(
                        [{<<"Africa/Abidjan"/utf8>>, 0},
                            {<<"Africa/Algiers"/utf8>>, 3600},
                            {<<"Africa/Bissau"/utf8>>, 0},
                            {<<"Africa/Cairo"/utf8>>, 7200},
                            {<<"Africa/Casablanca"/utf8>>, 3600},
                            {<<"Africa/Ceuta"/utf8>>, 3600},
                            {<<"Africa/El_Aaiun"/utf8>>, 3600},
                            {<<"Africa/Johannesburg"/utf8>>, 7200},
                            {<<"Africa/Juba"/utf8>>, 7200},
                            {<<"Africa/Khartoum"/utf8>>, 7200},
                            {<<"Africa/Lagos"/utf8>>, 3600},
                            {<<"Africa/Maputo"/utf8>>, 7200},
                            {<<"Africa/Monrovia"/utf8>>, 0},
                            {<<"Africa/Nairobi"/utf8>>, 10800},
                            {<<"Africa/Ndjamena"/utf8>>, 3600},
                            {<<"Africa/Sao_Tome"/utf8>>, 0},
                            {<<"Africa/Tripoli"/utf8>>, 7200},
                            {<<"Africa/Tunis"/utf8>>, 3600},
                            {<<"Africa/Windhoek"/utf8>>, 7200},
                            {<<"America/Adak"/utf8>>, -36000},
                            {<<"America/Anchorage"/utf8>>, -32400},
                            {<<"America/Araguaina"/utf8>>, -10800},
                            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
                            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
                            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
                            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
                            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
                            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
                            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
                            {<<"America/Argentina/Salta"/utf8>>, -10800},
                            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
                            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
                            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
                            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
                            {<<"America/Asuncion"/utf8>>, -14400},
                            {<<"America/Bahia"/utf8>>, -10800},
                            {<<"America/Bahia_Banderas"/utf8>>, -21600},
                            {<<"America/Barbados"/utf8>>, -14400},
                            {<<"America/Belem"/utf8>>, -10800},
                            {<<"America/Belize"/utf8>>, -21600},
                            {<<"America/Boa_Vista"/utf8>>, -14400},
                            {<<"America/Bogota"/utf8>>, -18000},
                            {<<"America/Boise"/utf8>>, -25200},
                            {<<"America/Cambridge_Bay"/utf8>>, -25200},
                            {<<"America/Campo_Grande"/utf8>>, -14400},
                            {<<"America/Cancun"/utf8>>, -18000},
                            {<<"America/Caracas"/utf8>>, -14400},
                            {<<"America/Cayenne"/utf8>>, -10800},
                            {<<"America/Chicago"/utf8>>, -21600},
                            {<<"America/Chihuahua"/utf8>>, -21600},
                            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
                            {<<"America/Costa_Rica"/utf8>>, -21600},
                            {<<"America/Cuiaba"/utf8>>, -14400},
                            {<<"America/Danmarkshavn"/utf8>>, 0},
                            {<<"America/Dawson"/utf8>>, -25200},
                            {<<"America/Dawson_Creek"/utf8>>, -25200},
                            {<<"America/Denver"/utf8>>, -25200},
                            {<<"America/Detroit"/utf8>>, -18000},
                            {<<"America/Edmonton"/utf8>>, -25200},
                            {<<"America/Eirunepe"/utf8>>, -18000},
                            {<<"America/El_Salvador"/utf8>>, -21600},
                            {<<"America/Fort_Nelson"/utf8>>, -25200},
                            {<<"America/Fortaleza"/utf8>>, -10800},
                            {<<"America/Glace_Bay"/utf8>>, -14400},
                            {<<"America/Goose_Bay"/utf8>>, -14400},
                            {<<"America/Grand_Turk"/utf8>>, -18000},
                            {<<"America/Guatemala"/utf8>>, -21600},
                            {<<"America/Guayaquil"/utf8>>, -18000},
                            {<<"America/Guyana"/utf8>>, -14400},
                            {<<"America/Halifax"/utf8>>, -14400},
                            {<<"America/Havana"/utf8>>, -18000},
                            {<<"America/Hermosillo"/utf8>>, -25200},
                            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
                            {<<"America/Indiana/Knox"/utf8>>, -21600},
                            {<<"America/Indiana/Marengo"/utf8>>, -18000},
                            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
                            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
                            {<<"America/Indiana/Vevay"/utf8>>, -18000},
                            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
                            {<<"America/Indiana/Winamac"/utf8>>, -18000},
                            {<<"America/Inuvik"/utf8>>, -25200},
                            {<<"America/Iqaluit"/utf8>>, -18000},
                            {<<"America/Jamaica"/utf8>>, -18000},
                            {<<"America/Juneau"/utf8>>, -32400},
                            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
                            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
                            {<<"America/La_Paz"/utf8>>, -14400},
                            {<<"America/Lima"/utf8>>, -18000},
                            {<<"America/Los_Angeles"/utf8>>, -28800},
                            {<<"America/Maceio"/utf8>>, -10800},
                            {<<"America/Managua"/utf8>>, -21600},
                            {<<"America/Manaus"/utf8>>, -14400},
                            {<<"America/Martinique"/utf8>>, -14400},
                            {<<"America/Matamoros"/utf8>>, -21600},
                            {<<"America/Mazatlan"/utf8>>, -25200},
                            {<<"America/Menominee"/utf8>>, -21600},
                            {<<"America/Merida"/utf8>>, -21600},
                            {<<"America/Metlakatla"/utf8>>, -32400},
                            {<<"America/Mexico_City"/utf8>>, -21600},
                            {<<"America/Miquelon"/utf8>>, -10800},
                            {<<"America/Moncton"/utf8>>, -14400},
                            {<<"America/Monterrey"/utf8>>, -21600},
                            {<<"America/Montevideo"/utf8>>, -10800},
                            {<<"America/New_York"/utf8>>, -18000},
                            {<<"America/Nome"/utf8>>, -32400},
                            {<<"America/Noronha"/utf8>>, -7200},
                            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
                            {<<"America/North_Dakota/Center"/utf8>>, -21600},
                            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
                            {<<"America/Nuuk"/utf8>>, -7200},
                            {<<"America/Ojinaga"/utf8>>, -21600},
                            {<<"America/Panama"/utf8>>, -18000},
                            {<<"America/Paramaribo"/utf8>>, -10800},
                            {<<"America/Phoenix"/utf8>>, -25200},
                            {<<"America/Port-au-Prince"/utf8>>, -18000},
                            {<<"America/Porto_Velho"/utf8>>, -14400},
                            {<<"America/Puerto_Rico"/utf8>>, -14400},
                            {<<"America/Punta_Arenas"/utf8>>, -10800},
                            {<<"America/Rankin_Inlet"/utf8>>, -21600},
                            {<<"America/Recife"/utf8>>, -10800},
                            {<<"America/Regina"/utf8>>, -21600},
                            {<<"America/Resolute"/utf8>>, -21600},
                            {<<"America/Rio_Branco"/utf8>>, -18000},
                            {<<"America/Santarem"/utf8>>, -10800},
                            {<<"America/Santiago"/utf8>>, -14400},
                            {<<"America/Santo_Domingo"/utf8>>, -14400},
                            {<<"America/Sao_Paulo"/utf8>>, -10800},
                            {<<"America/Scoresbysund"/utf8>>, -7200},
                            {<<"America/Sitka"/utf8>>, -32400},
                            {<<"America/St_Johns"/utf8>>, -12600},
                            {<<"America/Swift_Current"/utf8>>, -21600},
                            {<<"America/Tegucigalpa"/utf8>>, -21600},
                            {<<"America/Thule"/utf8>>, -14400},
                            {<<"America/Tijuana"/utf8>>, -28800},
                            {<<"America/Toronto"/utf8>>, -18000},
                            {<<"America/Vancouver"/utf8>>, -28800},
                            {<<"America/Whitehorse"/utf8>>, -25200},
                            {<<"America/Winnipeg"/utf8>>, -21600},
                            {<<"America/Yakutat"/utf8>>, -32400},
                            {<<"Antarctica/Casey"/utf8>>, 28800},
                            {<<"Antarctica/Davis"/utf8>>, 25200},
                            {<<"Antarctica/Macquarie"/utf8>>, 36000},
                            {<<"Antarctica/Mawson"/utf8>>, 18000},
                            {<<"Antarctica/Palmer"/utf8>>, -10800},
                            {<<"Antarctica/Rothera"/utf8>>, -10800},
                            {<<"Antarctica/Troll"/utf8>>, 0},
                            {<<"Antarctica/Vostok"/utf8>>, 18000},
                            {<<"Asia/Almaty"/utf8>>, 18000},
                            {<<"Asia/Amman"/utf8>>, 10800},
                            {<<"Asia/Anadyr"/utf8>>, 43200},
                            {<<"Asia/Aqtau"/utf8>>, 18000},
                            {<<"Asia/Aqtobe"/utf8>>, 18000},
                            {<<"Asia/Ashgabat"/utf8>>, 18000},
                            {<<"Asia/Atyrau"/utf8>>, 18000},
                            {<<"Asia/Baghdad"/utf8>>, 10800},
                            {<<"Asia/Baku"/utf8>>, 14400},
                            {<<"Asia/Bangkok"/utf8>>, 25200},
                            {<<"Asia/Barnaul"/utf8>>, 25200},
                            {<<"Asia/Beirut"/utf8>>, 7200},
                            {<<"Asia/Bishkek"/utf8>>, 21600},
                            {<<"Asia/Chita"/utf8>>, 32400},
                            {<<"Asia/Colombo"/utf8>>, 19800},
                            {<<"Asia/Damascus"/utf8>>, 10800},
                            {<<"Asia/Dhaka"/utf8>>, 21600},
                            {<<"Asia/Dili"/utf8>>, 32400},
                            {<<"Asia/Dubai"/utf8>>, 14400},
                            {<<"Asia/Dushanbe"/utf8>>, 18000},
                            {<<"Asia/Famagusta"/utf8>>, 7200},
                            {<<"Asia/Gaza"/utf8>>, 7200},
                            {<<"Asia/Hebron"/utf8>>, 7200},
                            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
                            {<<"Asia/Hong_Kong"/utf8>>, 28800},
                            {<<"Asia/Hovd"/utf8>>, 25200},
                            {<<"Asia/Irkutsk"/utf8>>, 28800},
                            {<<"Asia/Jakarta"/utf8>>, 25200},
                            {<<"Asia/Jayapura"/utf8>>, 32400},
                            {<<"Asia/Jerusalem"/utf8>>, 7200},
                            {<<"Asia/Kabul"/utf8>>, 16200},
                            {<<"Asia/Kamchatka"/utf8>>, 43200},
                            {<<"Asia/Karachi"/utf8>>, 18000},
                            {<<"Asia/Kathmandu"/utf8>>, 20700},
                            {<<"Asia/Khandyga"/utf8>>, 32400},
                            {<<"Asia/Kolkata"/utf8>>, 19800},
                            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
                            {<<"Asia/Kuching"/utf8>>, 28800},
                            {<<"Asia/Macau"/utf8>>, 28800},
                            {<<"Asia/Magadan"/utf8>>, 39600},
                            {<<"Asia/Makassar"/utf8>>, 28800},
                            {<<"Asia/Manila"/utf8>>, 28800},
                            {<<"Asia/Nicosia"/utf8>>, 7200},
                            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
                            {<<"Asia/Novosibirsk"/utf8>>, 25200},
                            {<<"Asia/Omsk"/utf8>>, 21600},
                            {<<"Asia/Oral"/utf8>>, 18000},
                            {<<"Asia/Pontianak"/utf8>>, 25200},
                            {<<"Asia/Pyongyang"/utf8>>, 32400},
                            {<<"Asia/Qatar"/utf8>>, 10800},
                            {<<"Asia/Qostanay"/utf8>>, 18000},
                            {<<"Asia/Qyzylorda"/utf8>>, 18000},
                            {<<"Asia/Riyadh"/utf8>>, 10800},
                            {<<"Asia/Sakhalin"/utf8>>, 39600},
                            {<<"Asia/Samarkand"/utf8>>, 18000},
                            {<<"Asia/Seoul"/utf8>>, 32400},
                            {<<"Asia/Shanghai"/utf8>>, 28800},
                            {<<"Asia/Singapore"/utf8>>, 28800},
                            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
                            {<<"Asia/Taipei"/utf8>>, 28800},
                            {<<"Asia/Tashkent"/utf8>>, 18000},
                            {<<"Asia/Tbilisi"/utf8>>, 14400},
                            {<<"Asia/Tehran"/utf8>>, 12600},
                            {<<"Asia/Thimphu"/utf8>>, 21600},
                            {<<"Asia/Tokyo"/utf8>>, 32400},
                            {<<"Asia/Tomsk"/utf8>>, 25200},
                            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
                            {<<"Asia/Urumqi"/utf8>>, 21600},
                            {<<"Asia/Ust-Nera"/utf8>>, 36000},
                            {<<"Asia/Vladivostok"/utf8>>, 36000},
                            {<<"Asia/Yakutsk"/utf8>>, 32400},
                            {<<"Asia/Yangon"/utf8>>, 23400},
                            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
                            {<<"Asia/Yerevan"/utf8>>, 14400},
                            {<<"Atlantic/Azores"/utf8>>, -3600},
                            {<<"Atlantic/Bermuda"/utf8>>, -14400},
                            {<<"Atlantic/Canary"/utf8>>, 0},
                            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
                            {<<"Atlantic/Faroe"/utf8>>, 0},
                            {<<"Atlantic/Madeira"/utf8>>, 0},
                            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
                            {<<"Atlantic/Stanley"/utf8>>, -10800},
                            {<<"Australia/Adelaide"/utf8>>, 34200},
                            {<<"Australia/Brisbane"/utf8>>, 36000},
                            {<<"Australia/Broken_Hill"/utf8>>, 34200},
                            {<<"Australia/Darwin"/utf8>>, 34200},
                            {<<"Australia/Eucla"/utf8>>, 31500},
                            {<<"Australia/Hobart"/utf8>>, 36000},
                            {<<"Australia/Lindeman"/utf8>>, 36000},
                            {<<"Australia/Lord_Howe"/utf8>>, 37800},
                            {<<"Australia/Melbourne"/utf8>>, 36000},
                            {<<"Australia/Perth"/utf8>>, 28800},
                            {<<"Australia/Sydney"/utf8>>, 36000},
                            {<<"Etc/GMT"/utf8>>, 0},
                            {<<"Etc/GMT+1"/utf8>>, -3600},
                            {<<"Etc/GMT+10"/utf8>>, -36000},
                            {<<"Etc/GMT+11"/utf8>>, -39600},
                            {<<"Etc/GMT+12"/utf8>>, -43200},
                            {<<"Etc/GMT+2"/utf8>>, -7200},
                            {<<"Etc/GMT+3"/utf8>>, -10800},
                            {<<"Etc/GMT+4"/utf8>>, -14400},
                            {<<"Etc/GMT+5"/utf8>>, -18000},
                            {<<"Etc/GMT+6"/utf8>>, -21600},
                            {<<"Etc/GMT+7"/utf8>>, -25200},
                            {<<"Etc/GMT+8"/utf8>>, -28800},
                            {<<"Etc/GMT+9"/utf8>>, -32400},
                            {<<"Etc/GMT-1"/utf8>>, 3600},
                            {<<"Etc/GMT-10"/utf8>>, 36000},
                            {<<"Etc/GMT-11"/utf8>>, 39600},
                            {<<"Etc/GMT-12"/utf8>>, 43200},
                            {<<"Etc/GMT-13"/utf8>>, 46800},
                            {<<"Etc/GMT-14"/utf8>>, 50400},
                            {<<"Etc/GMT-2"/utf8>>, 7200},
                            {<<"Etc/GMT-3"/utf8>>, 10800},
                            {<<"Etc/GMT-4"/utf8>>, 14400},
                            {<<"Etc/GMT-5"/utf8>>, 18000},
                            {<<"Etc/GMT-6"/utf8>>, 21600},
                            {<<"Etc/GMT-7"/utf8>>, 25200},
                            {<<"Etc/GMT-8"/utf8>>, 28800},
                            {<<"Etc/GMT-9"/utf8>>, 32400},
                            {<<"Etc/UTC"/utf8>>, 0},
                            {<<"Europe/Andorra"/utf8>>, 3600},
                            {<<"Europe/Astrakhan"/utf8>>, 14400},
                            {<<"Europe/Athens"/utf8>>, 7200},
                            {<<"Europe/Belgrade"/utf8>>, 3600},
                            {<<"Europe/Berlin"/utf8>>, 3600},
                            {<<"Europe/Brussels"/utf8>>, 3600},
                            {<<"Europe/Bucharest"/utf8>>, 7200},
                            {<<"Europe/Budapest"/utf8>>, 3600},
                            {<<"Europe/Chisinau"/utf8>>, 7200},
                            {<<"Europe/Dublin"/utf8>>, 3600},
                            {<<"Europe/Gibraltar"/utf8>>, 3600},
                            {<<"Europe/Helsinki"/utf8>>, 7200},
                            {<<"Europe/Istanbul"/utf8>>, 10800},
                            {<<"Europe/Kaliningrad"/utf8>>, 7200},
                            {<<"Europe/Kirov"/utf8>>, 10800},
                            {<<"Europe/Kyiv"/utf8>>, 7200},
                            {<<"Europe/Lisbon"/utf8>>, 0},
                            {<<"Europe/London"/utf8>>, 0},
                            {<<"Europe/Madrid"/utf8>>, 3600},
                            {<<"Europe/Malta"/utf8>>, 3600},
                            {<<"Europe/Minsk"/utf8>>, 10800},
                            {<<"Europe/Moscow"/utf8>>, 10800},
                            {<<"Europe/Paris"/utf8>>, 3600},
                            {<<"Europe/Prague"/utf8>>, 3600},
                            {<<"Europe/Riga"/utf8>>, 7200},
                            {<<"Europe/Rome"/utf8>>, 3600},
                            {<<"Europe/Samara"/utf8>>, 14400},
                            {<<"Europe/Saratov"/utf8>>, 14400},
                            {<<"Europe/Simferopol"/utf8>>, 10800},
                            {<<"Europe/Sofia"/utf8>>, 7200},
                            {<<"Europe/Tallinn"/utf8>>, 7200},
                            {<<"Europe/Tirane"/utf8>>, 3600},
                            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
                            {<<"Europe/Vienna"/utf8>>, 3600},
                            {<<"Europe/Vilnius"/utf8>>, 7200},
                            {<<"Europe/Volgograd"/utf8>>, 10800},
                            {<<"Europe/Warsaw"/utf8>>, 3600},
                            {<<"Europe/Zurich"/utf8>>, 3600},
                            {<<"Indian/Chagos"/utf8>>, 21600},
                            {<<"Indian/Maldives"/utf8>>, 18000},
                            {<<"Indian/Mauritius"/utf8>>, 14400},
                            {<<"Pacific/Apia"/utf8>>, 46800},
                            {<<"Pacific/Auckland"/utf8>>, 43200},
                            {<<"Pacific/Bougainville"/utf8>>, 39600},
                            {<<"Pacific/Chatham"/utf8>>, 45900},
                            {<<"Pacific/Easter"/utf8>>, -21600},
                            {<<"Pacific/Efate"/utf8>>, 39600},
                            {<<"Pacific/Fakaofo"/utf8>>, 46800},
                            {<<"Pacific/Fiji"/utf8>>, 43200},
                            {<<"Pacific/Galapagos"/utf8>>, -21600},
                            {<<"Pacific/Gambier"/utf8>>, -32400},
                            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
                            {<<"Pacific/Guam"/utf8>>, 36000},
                            {<<"Pacific/Honolulu"/utf8>>, -36000},
                            {<<"Pacific/Kanton"/utf8>>, 46800},
                            {<<"Pacific/Kiritimati"/utf8>>, 50400},
                            {<<"Pacific/Kosrae"/utf8>>, 39600},
                            {<<"Pacific/Kwajalein"/utf8>>, 43200},
                            {<<"Pacific/Marquesas"/utf8>>, -34200},
                            {<<"Pacific/Nauru"/utf8>>, 43200},
                            {<<"Pacific/Niue"/utf8>>, -39600},
                            {<<"Pacific/Norfolk"/utf8>>, 39600},
                            {<<"Pacific/Noumea"/utf8>>, 39600},
                            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
                            {<<"Pacific/Palau"/utf8>>, 32400},
                            {<<"Pacific/Pitcairn"/utf8>>, -28800},
                            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
                            {<<"Pacific/Rarotonga"/utf8>>, -36000},
                            {<<"Pacific/Tahiti"/utf8>>, -36000},
                            {<<"Pacific/Tarawa"/utf8>>, 43200},
                            {<<"Pacific/Tongatapu"/utf8>>, 46800}],
                        fun(Item) -> erlang:element(1, Item) =:= Tz end
                    ) of
                        true ->
                            {some, Tz};

                        false ->
                            none
                    end
                end
            ),
            gleam@option:flatten(_pipe)
        end,
        {some, Monotonic_now}}.

-file("src/birl.gleam", 124).
?DOC(" returns if the time has passed\n").
-spec has_occured(time()) -> boolean().
has_occured(Value) ->
    compare(now(), Value) =:= gt.

-file("src/birl.gleam", 1268).
?DOC(" calculates the DateTime value from the erlang datetime using the local offset of the system\n").
-spec from_erlang_local_datetime(
    {{integer(), integer(), integer()}, {integer(), integer(), integer()}}
) -> time().
from_erlang_local_datetime(Erlang_datetime) ->
    {Date, Time} = Erlang_datetime,
    Offset_in_minutes = birl_ffi:local_offset(),
    Wall_time@1 = case begin
        _pipe = {time, 0, 0, none, none},
        _pipe@1 = set_day(
            _pipe,
            {day,
                erlang:element(1, Date),
                erlang:element(2, Date),
                erlang:element(3, Date)}
        ),
        set_time_of_day(
            _pipe@1,
            {time_of_day,
                erlang:element(1, Time),
                erlang:element(2, Time),
                erlang:element(3, Time),
                0}
        )
    end of
        {time, Wall_time, _, none, none} -> Wall_time;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"from_erlang_local_datetime"/utf8>>,
                        line => 1274,
                        value => _assert_fail,
                        start => 32387,
                        'end' => 32564,
                        pattern_start => 32398,
                        pattern_end => 32442})
    end,
    Timezone = birl_ffi:local_timezone(),
    {time,
        Wall_time@1,
        Offset_in_minutes * 60000000,
        begin
            _pipe@2 = gleam@option:map(
                Timezone,
                fun(Tz) ->
                    case gleam@list:any(
                        [{<<"Africa/Abidjan"/utf8>>, 0},
                            {<<"Africa/Algiers"/utf8>>, 3600},
                            {<<"Africa/Bissau"/utf8>>, 0},
                            {<<"Africa/Cairo"/utf8>>, 7200},
                            {<<"Africa/Casablanca"/utf8>>, 3600},
                            {<<"Africa/Ceuta"/utf8>>, 3600},
                            {<<"Africa/El_Aaiun"/utf8>>, 3600},
                            {<<"Africa/Johannesburg"/utf8>>, 7200},
                            {<<"Africa/Juba"/utf8>>, 7200},
                            {<<"Africa/Khartoum"/utf8>>, 7200},
                            {<<"Africa/Lagos"/utf8>>, 3600},
                            {<<"Africa/Maputo"/utf8>>, 7200},
                            {<<"Africa/Monrovia"/utf8>>, 0},
                            {<<"Africa/Nairobi"/utf8>>, 10800},
                            {<<"Africa/Ndjamena"/utf8>>, 3600},
                            {<<"Africa/Sao_Tome"/utf8>>, 0},
                            {<<"Africa/Tripoli"/utf8>>, 7200},
                            {<<"Africa/Tunis"/utf8>>, 3600},
                            {<<"Africa/Windhoek"/utf8>>, 7200},
                            {<<"America/Adak"/utf8>>, -36000},
                            {<<"America/Anchorage"/utf8>>, -32400},
                            {<<"America/Araguaina"/utf8>>, -10800},
                            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
                            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
                            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
                            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
                            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
                            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
                            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
                            {<<"America/Argentina/Salta"/utf8>>, -10800},
                            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
                            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
                            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
                            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
                            {<<"America/Asuncion"/utf8>>, -14400},
                            {<<"America/Bahia"/utf8>>, -10800},
                            {<<"America/Bahia_Banderas"/utf8>>, -21600},
                            {<<"America/Barbados"/utf8>>, -14400},
                            {<<"America/Belem"/utf8>>, -10800},
                            {<<"America/Belize"/utf8>>, -21600},
                            {<<"America/Boa_Vista"/utf8>>, -14400},
                            {<<"America/Bogota"/utf8>>, -18000},
                            {<<"America/Boise"/utf8>>, -25200},
                            {<<"America/Cambridge_Bay"/utf8>>, -25200},
                            {<<"America/Campo_Grande"/utf8>>, -14400},
                            {<<"America/Cancun"/utf8>>, -18000},
                            {<<"America/Caracas"/utf8>>, -14400},
                            {<<"America/Cayenne"/utf8>>, -10800},
                            {<<"America/Chicago"/utf8>>, -21600},
                            {<<"America/Chihuahua"/utf8>>, -21600},
                            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
                            {<<"America/Costa_Rica"/utf8>>, -21600},
                            {<<"America/Cuiaba"/utf8>>, -14400},
                            {<<"America/Danmarkshavn"/utf8>>, 0},
                            {<<"America/Dawson"/utf8>>, -25200},
                            {<<"America/Dawson_Creek"/utf8>>, -25200},
                            {<<"America/Denver"/utf8>>, -25200},
                            {<<"America/Detroit"/utf8>>, -18000},
                            {<<"America/Edmonton"/utf8>>, -25200},
                            {<<"America/Eirunepe"/utf8>>, -18000},
                            {<<"America/El_Salvador"/utf8>>, -21600},
                            {<<"America/Fort_Nelson"/utf8>>, -25200},
                            {<<"America/Fortaleza"/utf8>>, -10800},
                            {<<"America/Glace_Bay"/utf8>>, -14400},
                            {<<"America/Goose_Bay"/utf8>>, -14400},
                            {<<"America/Grand_Turk"/utf8>>, -18000},
                            {<<"America/Guatemala"/utf8>>, -21600},
                            {<<"America/Guayaquil"/utf8>>, -18000},
                            {<<"America/Guyana"/utf8>>, -14400},
                            {<<"America/Halifax"/utf8>>, -14400},
                            {<<"America/Havana"/utf8>>, -18000},
                            {<<"America/Hermosillo"/utf8>>, -25200},
                            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
                            {<<"America/Indiana/Knox"/utf8>>, -21600},
                            {<<"America/Indiana/Marengo"/utf8>>, -18000},
                            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
                            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
                            {<<"America/Indiana/Vevay"/utf8>>, -18000},
                            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
                            {<<"America/Indiana/Winamac"/utf8>>, -18000},
                            {<<"America/Inuvik"/utf8>>, -25200},
                            {<<"America/Iqaluit"/utf8>>, -18000},
                            {<<"America/Jamaica"/utf8>>, -18000},
                            {<<"America/Juneau"/utf8>>, -32400},
                            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
                            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
                            {<<"America/La_Paz"/utf8>>, -14400},
                            {<<"America/Lima"/utf8>>, -18000},
                            {<<"America/Los_Angeles"/utf8>>, -28800},
                            {<<"America/Maceio"/utf8>>, -10800},
                            {<<"America/Managua"/utf8>>, -21600},
                            {<<"America/Manaus"/utf8>>, -14400},
                            {<<"America/Martinique"/utf8>>, -14400},
                            {<<"America/Matamoros"/utf8>>, -21600},
                            {<<"America/Mazatlan"/utf8>>, -25200},
                            {<<"America/Menominee"/utf8>>, -21600},
                            {<<"America/Merida"/utf8>>, -21600},
                            {<<"America/Metlakatla"/utf8>>, -32400},
                            {<<"America/Mexico_City"/utf8>>, -21600},
                            {<<"America/Miquelon"/utf8>>, -10800},
                            {<<"America/Moncton"/utf8>>, -14400},
                            {<<"America/Monterrey"/utf8>>, -21600},
                            {<<"America/Montevideo"/utf8>>, -10800},
                            {<<"America/New_York"/utf8>>, -18000},
                            {<<"America/Nome"/utf8>>, -32400},
                            {<<"America/Noronha"/utf8>>, -7200},
                            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
                            {<<"America/North_Dakota/Center"/utf8>>, -21600},
                            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
                            {<<"America/Nuuk"/utf8>>, -7200},
                            {<<"America/Ojinaga"/utf8>>, -21600},
                            {<<"America/Panama"/utf8>>, -18000},
                            {<<"America/Paramaribo"/utf8>>, -10800},
                            {<<"America/Phoenix"/utf8>>, -25200},
                            {<<"America/Port-au-Prince"/utf8>>, -18000},
                            {<<"America/Porto_Velho"/utf8>>, -14400},
                            {<<"America/Puerto_Rico"/utf8>>, -14400},
                            {<<"America/Punta_Arenas"/utf8>>, -10800},
                            {<<"America/Rankin_Inlet"/utf8>>, -21600},
                            {<<"America/Recife"/utf8>>, -10800},
                            {<<"America/Regina"/utf8>>, -21600},
                            {<<"America/Resolute"/utf8>>, -21600},
                            {<<"America/Rio_Branco"/utf8>>, -18000},
                            {<<"America/Santarem"/utf8>>, -10800},
                            {<<"America/Santiago"/utf8>>, -14400},
                            {<<"America/Santo_Domingo"/utf8>>, -14400},
                            {<<"America/Sao_Paulo"/utf8>>, -10800},
                            {<<"America/Scoresbysund"/utf8>>, -7200},
                            {<<"America/Sitka"/utf8>>, -32400},
                            {<<"America/St_Johns"/utf8>>, -12600},
                            {<<"America/Swift_Current"/utf8>>, -21600},
                            {<<"America/Tegucigalpa"/utf8>>, -21600},
                            {<<"America/Thule"/utf8>>, -14400},
                            {<<"America/Tijuana"/utf8>>, -28800},
                            {<<"America/Toronto"/utf8>>, -18000},
                            {<<"America/Vancouver"/utf8>>, -28800},
                            {<<"America/Whitehorse"/utf8>>, -25200},
                            {<<"America/Winnipeg"/utf8>>, -21600},
                            {<<"America/Yakutat"/utf8>>, -32400},
                            {<<"Antarctica/Casey"/utf8>>, 28800},
                            {<<"Antarctica/Davis"/utf8>>, 25200},
                            {<<"Antarctica/Macquarie"/utf8>>, 36000},
                            {<<"Antarctica/Mawson"/utf8>>, 18000},
                            {<<"Antarctica/Palmer"/utf8>>, -10800},
                            {<<"Antarctica/Rothera"/utf8>>, -10800},
                            {<<"Antarctica/Troll"/utf8>>, 0},
                            {<<"Antarctica/Vostok"/utf8>>, 18000},
                            {<<"Asia/Almaty"/utf8>>, 18000},
                            {<<"Asia/Amman"/utf8>>, 10800},
                            {<<"Asia/Anadyr"/utf8>>, 43200},
                            {<<"Asia/Aqtau"/utf8>>, 18000},
                            {<<"Asia/Aqtobe"/utf8>>, 18000},
                            {<<"Asia/Ashgabat"/utf8>>, 18000},
                            {<<"Asia/Atyrau"/utf8>>, 18000},
                            {<<"Asia/Baghdad"/utf8>>, 10800},
                            {<<"Asia/Baku"/utf8>>, 14400},
                            {<<"Asia/Bangkok"/utf8>>, 25200},
                            {<<"Asia/Barnaul"/utf8>>, 25200},
                            {<<"Asia/Beirut"/utf8>>, 7200},
                            {<<"Asia/Bishkek"/utf8>>, 21600},
                            {<<"Asia/Chita"/utf8>>, 32400},
                            {<<"Asia/Colombo"/utf8>>, 19800},
                            {<<"Asia/Damascus"/utf8>>, 10800},
                            {<<"Asia/Dhaka"/utf8>>, 21600},
                            {<<"Asia/Dili"/utf8>>, 32400},
                            {<<"Asia/Dubai"/utf8>>, 14400},
                            {<<"Asia/Dushanbe"/utf8>>, 18000},
                            {<<"Asia/Famagusta"/utf8>>, 7200},
                            {<<"Asia/Gaza"/utf8>>, 7200},
                            {<<"Asia/Hebron"/utf8>>, 7200},
                            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
                            {<<"Asia/Hong_Kong"/utf8>>, 28800},
                            {<<"Asia/Hovd"/utf8>>, 25200},
                            {<<"Asia/Irkutsk"/utf8>>, 28800},
                            {<<"Asia/Jakarta"/utf8>>, 25200},
                            {<<"Asia/Jayapura"/utf8>>, 32400},
                            {<<"Asia/Jerusalem"/utf8>>, 7200},
                            {<<"Asia/Kabul"/utf8>>, 16200},
                            {<<"Asia/Kamchatka"/utf8>>, 43200},
                            {<<"Asia/Karachi"/utf8>>, 18000},
                            {<<"Asia/Kathmandu"/utf8>>, 20700},
                            {<<"Asia/Khandyga"/utf8>>, 32400},
                            {<<"Asia/Kolkata"/utf8>>, 19800},
                            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
                            {<<"Asia/Kuching"/utf8>>, 28800},
                            {<<"Asia/Macau"/utf8>>, 28800},
                            {<<"Asia/Magadan"/utf8>>, 39600},
                            {<<"Asia/Makassar"/utf8>>, 28800},
                            {<<"Asia/Manila"/utf8>>, 28800},
                            {<<"Asia/Nicosia"/utf8>>, 7200},
                            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
                            {<<"Asia/Novosibirsk"/utf8>>, 25200},
                            {<<"Asia/Omsk"/utf8>>, 21600},
                            {<<"Asia/Oral"/utf8>>, 18000},
                            {<<"Asia/Pontianak"/utf8>>, 25200},
                            {<<"Asia/Pyongyang"/utf8>>, 32400},
                            {<<"Asia/Qatar"/utf8>>, 10800},
                            {<<"Asia/Qostanay"/utf8>>, 18000},
                            {<<"Asia/Qyzylorda"/utf8>>, 18000},
                            {<<"Asia/Riyadh"/utf8>>, 10800},
                            {<<"Asia/Sakhalin"/utf8>>, 39600},
                            {<<"Asia/Samarkand"/utf8>>, 18000},
                            {<<"Asia/Seoul"/utf8>>, 32400},
                            {<<"Asia/Shanghai"/utf8>>, 28800},
                            {<<"Asia/Singapore"/utf8>>, 28800},
                            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
                            {<<"Asia/Taipei"/utf8>>, 28800},
                            {<<"Asia/Tashkent"/utf8>>, 18000},
                            {<<"Asia/Tbilisi"/utf8>>, 14400},
                            {<<"Asia/Tehran"/utf8>>, 12600},
                            {<<"Asia/Thimphu"/utf8>>, 21600},
                            {<<"Asia/Tokyo"/utf8>>, 32400},
                            {<<"Asia/Tomsk"/utf8>>, 25200},
                            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
                            {<<"Asia/Urumqi"/utf8>>, 21600},
                            {<<"Asia/Ust-Nera"/utf8>>, 36000},
                            {<<"Asia/Vladivostok"/utf8>>, 36000},
                            {<<"Asia/Yakutsk"/utf8>>, 32400},
                            {<<"Asia/Yangon"/utf8>>, 23400},
                            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
                            {<<"Asia/Yerevan"/utf8>>, 14400},
                            {<<"Atlantic/Azores"/utf8>>, -3600},
                            {<<"Atlantic/Bermuda"/utf8>>, -14400},
                            {<<"Atlantic/Canary"/utf8>>, 0},
                            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
                            {<<"Atlantic/Faroe"/utf8>>, 0},
                            {<<"Atlantic/Madeira"/utf8>>, 0},
                            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
                            {<<"Atlantic/Stanley"/utf8>>, -10800},
                            {<<"Australia/Adelaide"/utf8>>, 34200},
                            {<<"Australia/Brisbane"/utf8>>, 36000},
                            {<<"Australia/Broken_Hill"/utf8>>, 34200},
                            {<<"Australia/Darwin"/utf8>>, 34200},
                            {<<"Australia/Eucla"/utf8>>, 31500},
                            {<<"Australia/Hobart"/utf8>>, 36000},
                            {<<"Australia/Lindeman"/utf8>>, 36000},
                            {<<"Australia/Lord_Howe"/utf8>>, 37800},
                            {<<"Australia/Melbourne"/utf8>>, 36000},
                            {<<"Australia/Perth"/utf8>>, 28800},
                            {<<"Australia/Sydney"/utf8>>, 36000},
                            {<<"Etc/GMT"/utf8>>, 0},
                            {<<"Etc/GMT+1"/utf8>>, -3600},
                            {<<"Etc/GMT+10"/utf8>>, -36000},
                            {<<"Etc/GMT+11"/utf8>>, -39600},
                            {<<"Etc/GMT+12"/utf8>>, -43200},
                            {<<"Etc/GMT+2"/utf8>>, -7200},
                            {<<"Etc/GMT+3"/utf8>>, -10800},
                            {<<"Etc/GMT+4"/utf8>>, -14400},
                            {<<"Etc/GMT+5"/utf8>>, -18000},
                            {<<"Etc/GMT+6"/utf8>>, -21600},
                            {<<"Etc/GMT+7"/utf8>>, -25200},
                            {<<"Etc/GMT+8"/utf8>>, -28800},
                            {<<"Etc/GMT+9"/utf8>>, -32400},
                            {<<"Etc/GMT-1"/utf8>>, 3600},
                            {<<"Etc/GMT-10"/utf8>>, 36000},
                            {<<"Etc/GMT-11"/utf8>>, 39600},
                            {<<"Etc/GMT-12"/utf8>>, 43200},
                            {<<"Etc/GMT-13"/utf8>>, 46800},
                            {<<"Etc/GMT-14"/utf8>>, 50400},
                            {<<"Etc/GMT-2"/utf8>>, 7200},
                            {<<"Etc/GMT-3"/utf8>>, 10800},
                            {<<"Etc/GMT-4"/utf8>>, 14400},
                            {<<"Etc/GMT-5"/utf8>>, 18000},
                            {<<"Etc/GMT-6"/utf8>>, 21600},
                            {<<"Etc/GMT-7"/utf8>>, 25200},
                            {<<"Etc/GMT-8"/utf8>>, 28800},
                            {<<"Etc/GMT-9"/utf8>>, 32400},
                            {<<"Etc/UTC"/utf8>>, 0},
                            {<<"Europe/Andorra"/utf8>>, 3600},
                            {<<"Europe/Astrakhan"/utf8>>, 14400},
                            {<<"Europe/Athens"/utf8>>, 7200},
                            {<<"Europe/Belgrade"/utf8>>, 3600},
                            {<<"Europe/Berlin"/utf8>>, 3600},
                            {<<"Europe/Brussels"/utf8>>, 3600},
                            {<<"Europe/Bucharest"/utf8>>, 7200},
                            {<<"Europe/Budapest"/utf8>>, 3600},
                            {<<"Europe/Chisinau"/utf8>>, 7200},
                            {<<"Europe/Dublin"/utf8>>, 3600},
                            {<<"Europe/Gibraltar"/utf8>>, 3600},
                            {<<"Europe/Helsinki"/utf8>>, 7200},
                            {<<"Europe/Istanbul"/utf8>>, 10800},
                            {<<"Europe/Kaliningrad"/utf8>>, 7200},
                            {<<"Europe/Kirov"/utf8>>, 10800},
                            {<<"Europe/Kyiv"/utf8>>, 7200},
                            {<<"Europe/Lisbon"/utf8>>, 0},
                            {<<"Europe/London"/utf8>>, 0},
                            {<<"Europe/Madrid"/utf8>>, 3600},
                            {<<"Europe/Malta"/utf8>>, 3600},
                            {<<"Europe/Minsk"/utf8>>, 10800},
                            {<<"Europe/Moscow"/utf8>>, 10800},
                            {<<"Europe/Paris"/utf8>>, 3600},
                            {<<"Europe/Prague"/utf8>>, 3600},
                            {<<"Europe/Riga"/utf8>>, 7200},
                            {<<"Europe/Rome"/utf8>>, 3600},
                            {<<"Europe/Samara"/utf8>>, 14400},
                            {<<"Europe/Saratov"/utf8>>, 14400},
                            {<<"Europe/Simferopol"/utf8>>, 10800},
                            {<<"Europe/Sofia"/utf8>>, 7200},
                            {<<"Europe/Tallinn"/utf8>>, 7200},
                            {<<"Europe/Tirane"/utf8>>, 3600},
                            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
                            {<<"Europe/Vienna"/utf8>>, 3600},
                            {<<"Europe/Vilnius"/utf8>>, 7200},
                            {<<"Europe/Volgograd"/utf8>>, 10800},
                            {<<"Europe/Warsaw"/utf8>>, 3600},
                            {<<"Europe/Zurich"/utf8>>, 3600},
                            {<<"Indian/Chagos"/utf8>>, 21600},
                            {<<"Indian/Maldives"/utf8>>, 18000},
                            {<<"Indian/Mauritius"/utf8>>, 14400},
                            {<<"Pacific/Apia"/utf8>>, 46800},
                            {<<"Pacific/Auckland"/utf8>>, 43200},
                            {<<"Pacific/Bougainville"/utf8>>, 39600},
                            {<<"Pacific/Chatham"/utf8>>, 45900},
                            {<<"Pacific/Easter"/utf8>>, -21600},
                            {<<"Pacific/Efate"/utf8>>, 39600},
                            {<<"Pacific/Fakaofo"/utf8>>, 46800},
                            {<<"Pacific/Fiji"/utf8>>, 43200},
                            {<<"Pacific/Galapagos"/utf8>>, -21600},
                            {<<"Pacific/Gambier"/utf8>>, -32400},
                            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
                            {<<"Pacific/Guam"/utf8>>, 36000},
                            {<<"Pacific/Honolulu"/utf8>>, -36000},
                            {<<"Pacific/Kanton"/utf8>>, 46800},
                            {<<"Pacific/Kiritimati"/utf8>>, 50400},
                            {<<"Pacific/Kosrae"/utf8>>, 39600},
                            {<<"Pacific/Kwajalein"/utf8>>, 43200},
                            {<<"Pacific/Marquesas"/utf8>>, -34200},
                            {<<"Pacific/Nauru"/utf8>>, 43200},
                            {<<"Pacific/Niue"/utf8>>, -39600},
                            {<<"Pacific/Norfolk"/utf8>>, 39600},
                            {<<"Pacific/Noumea"/utf8>>, 39600},
                            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
                            {<<"Pacific/Palau"/utf8>>, 32400},
                            {<<"Pacific/Pitcairn"/utf8>>, -28800},
                            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
                            {<<"Pacific/Rarotonga"/utf8>>, -36000},
                            {<<"Pacific/Tahiti"/utf8>>, -36000},
                            {<<"Pacific/Tarawa"/utf8>>, 43200},
                            {<<"Pacific/Tongatapu"/utf8>>, 46800}],
                        fun(Item) -> erlang:element(1, Item) =:= Tz end
                    ) of
                        true ->
                            {some, Tz};

                        false ->
                            none
                    end
                end
            ),
            gleam@option:flatten(_pipe@2)
        end,
        none}.

-file("src/birl.gleam", 1297).
?DOC(" calculates the DateTime value from the erlang datetime in UTC\n").
-spec from_erlang_universal_datetime(
    {{integer(), integer(), integer()}, {integer(), integer(), integer()}}
) -> time().
from_erlang_universal_datetime(Erlang_datetime) ->
    {Date, Time} = Erlang_datetime,
    New_value@1 = case begin
        _pipe = {time, 0, 0, none, none},
        _pipe@1 = set_day(
            _pipe,
            {day,
                erlang:element(1, Date),
                erlang:element(2, Date),
                erlang:element(3, Date)}
        ),
        _pipe@2 = set_time_of_day(
            _pipe@1,
            {time_of_day,
                erlang:element(1, Time),
                erlang:element(2, Time),
                erlang:element(3, Time),
                0}
        ),
        set_timezone(_pipe@2, <<"Etc/UTC"/utf8>>)
    end of
        {ok, New_value} -> New_value;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"birl"/utf8>>,
                        function => <<"from_erlang_universal_datetime"/utf8>>,
                        line => 1301,
                        value => _assert_fail,
                        start => 33111,
                        'end' => 33288,
                        pattern_start => 33122,
                        pattern_end => 33135})
    end,
    New_value@1.

-file("src/birl.gleam", 922).
?DOC(
    " you could say this is the opposite of `legible_difference`\n"
    "\n"
    " ```gleam\n"
    " > parse_relative(birl.now(), \"8 minutes ago\")\n"
    " ```\n"
).
-spec parse_relative(time(), binary()) -> {ok, time()} | {error, nil}.
parse_relative(Origin, Legible_difference) ->
    case gleam@string:split(Legible_difference, <<" "/utf8>>) of
        [<<"in"/utf8>>, Amount_string, Unit] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"from now"/utf8>>] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"later"/utf8>>] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"ahead"/utf8>>] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"in the future"/utf8>>] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"hence"/utf8>>] ->
            Unit@1 = case gleam_stdlib:string_ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_end(Unit, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"ago"/utf8>>] ->
            Unit@4 = case gleam_stdlib:string_ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_end(Unit@3, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"before"/utf8>>] ->
            Unit@4 = case gleam_stdlib:string_ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_end(Unit@3, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"earlier"/utf8>>] ->
            Unit@4 = case gleam_stdlib:string_ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_end(Unit@3, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"since"/utf8>>] ->
            Unit@4 = case gleam_stdlib:string_ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_end(Unit@3, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"in the past"/utf8>>] ->
            Unit@4 = case gleam_stdlib:string_ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_end(Unit@3, 1)
            end,
            gleam@result:then(
                gleam_stdlib:parse_int(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        _ ->
            {error, nil}
    end.

-file("src/birl.gleam", 965).
-spec legible_difference(time(), time()) -> binary().
legible_difference(A, B) ->
    case begin
        _pipe = difference(A, B),
        birl@duration:blur(_pipe)
    end of
        {_, micro_second} ->
            <<"just now"/utf8>>;

        {_, milli_second} ->
            <<"just now"/utf8>>;

        {Amount, Unit} ->
            Unit@2 = case gleam@list:key_find(
                [{year, <<"year"/utf8>>},
                    {month, <<"month"/utf8>>},
                    {week, <<"week"/utf8>>},
                    {day, <<"day"/utf8>>},
                    {hour, <<"hour"/utf8>>},
                    {minute, <<"minute"/utf8>>},
                    {second, <<"second"/utf8>>}],
                Unit
            ) of
                {ok, Unit@1} -> Unit@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"birl"/utf8>>,
                                function => <<"legible_difference"/utf8>>,
                                line => 973,
                                value => _assert_fail,
                                start => 24758,
                                'end' => 24816,
                                pattern_start => 24769,
                                pattern_end => 24777})
            end,
            Is_negative = Amount < 0,
            Amount@1 = gleam@int:absolute_value(Amount),
            Unit@3 = case Amount@1 of
                1 ->
                    Unit@2;

                _ ->
                    <<Unit@2/binary, "s"/utf8>>
            end,
            case Is_negative of
                true ->
                    <<<<<<"in "/utf8,
                                (erlang:integer_to_binary(Amount@1))/binary>>/binary,
                            " "/utf8>>/binary,
                        Unit@3/binary>>;

                false ->
                    <<<<<<(begin
                                    _pipe@1 = Amount@1,
                                    _pipe@2 = gleam@int:absolute_value(_pipe@1),
                                    erlang:integer_to_binary(_pipe@2)
                                end)/binary,
                                " "/utf8>>/binary,
                            Unit@3/binary>>/binary,
                        " ago"/utf8>>
            end
    end.

-file("src/birl.gleam", 1083).
-spec parse_weekday(binary()) -> {ok, weekday()} | {error, nil}.
parse_weekday(Value) ->
    Lowercase = string:lowercase(Value),
    Weekday = gleam@list:find(
        [{mon, {<<"Monday"/utf8>>, <<"Mon"/utf8>>}},
            {tue, {<<"Tuesday"/utf8>>, <<"Tue"/utf8>>}},
            {wed, {<<"Wednesday"/utf8>>, <<"Wed"/utf8>>}},
            {thu, {<<"Thursday"/utf8>>, <<"Thu"/utf8>>}},
            {fri, {<<"Friday"/utf8>>, <<"Fri"/utf8>>}},
            {sat, {<<"Saturday"/utf8>>, <<"Sat"/utf8>>}},
            {sun, {<<"Sunday"/utf8>>, <<"Sun"/utf8>>}}],
        fun(Weekday_string) ->
            {_, {Long, Short}} = Weekday_string,
            (Lowercase =:= string:lowercase(Short)) orelse (Lowercase =:= string:lowercase(
                Long
            ))
        end
    ),
    _pipe = Weekday,
    gleam@result:map(_pipe, fun(Weekday@1) -> erlang:element(1, Weekday@1) end).

-file("src/birl.gleam", 734).
?DOC(
    " see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)\n"
    "\n"
    " also supports other similar formats:\n"
    "\n"
    "   - `Tue, 01-Nov-2016 08:49:37 GMT`\n"
    "\n"
    "   - `Tue, 01 Nov 2016 08:49:37 +0630`\n"
    "\n"
    "   - `Tue, 01-November-2016 08:49:37 Z`\n"
    "\n"
    "   - `Tuesday, 01-Nov-2016 08:49:37 +330`\n"
    "\n"
    "   - `Tuesday, 01 November 2016 08:49:37 +06:30`\n"
).
-spec from_http(binary()) -> {ok, time()} | {error, nil}.
from_http(Value) ->
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        gleam@string:split_once(Value@1, <<","/utf8>>),
        fun(_use0) ->
            {Weekday, Rest} = _use0,
            gleam@bool:guard(
                not gleam@list:any(
                    [{mon, {<<"Monday"/utf8>>, <<"Mon"/utf8>>}},
                        {tue, {<<"Tuesday"/utf8>>, <<"Tue"/utf8>>}},
                        {wed, {<<"Wednesday"/utf8>>, <<"Wed"/utf8>>}},
                        {thu, {<<"Thursday"/utf8>>, <<"Thu"/utf8>>}},
                        {fri, {<<"Friday"/utf8>>, <<"Fri"/utf8>>}},
                        {sat, {<<"Saturday"/utf8>>, <<"Sat"/utf8>>}},
                        {sun, {<<"Sunday"/utf8>>, <<"Sun"/utf8>>}}],
                    fun(Weekday_item) ->
                        Strings = erlang:element(2, Weekday_item),
                        (erlang:element(1, Strings) =:= Weekday) orelse (erlang:element(
                            2,
                            Strings
                        )
                        =:= Weekday)
                    end
                ),
                {error, nil},
                fun() ->
                    Rest@1 = gleam@string:trim(Rest),
                    Whitespace_pattern@1 = case gleam@regexp:from_string(
                        <<"\\s+"/utf8>>
                    ) of
                        {ok, Whitespace_pattern} -> Whitespace_pattern;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"birl"/utf8>>,
                                        function => <<"from_http"/utf8>>,
                                        line => 747,
                                        value => _assert_fail,
                                        start => 17571,
                                        'end' => 17633,
                                        pattern_start => 17582,
                                        pattern_end => 17604})
                    end,
                    case gleam@regexp:split(Whitespace_pattern@1, Rest@1) of
                        [Day_string,
                            Month_string,
                            Year_string,
                            Time_string,
                            Offset_string] ->
                            Time_string@1 = gleam@string:replace(
                                Time_string,
                                <<":"/utf8>>,
                                <<""/utf8>>
                            ),
                            case {gleam_stdlib:parse_int(Day_string),
                                begin
                                    _pipe = gleam@list:index_map(
                                        [{jan,
                                                {<<"January"/utf8>>,
                                                    <<"Jan"/utf8>>}},
                                            {feb,
                                                {<<"February"/utf8>>,
                                                    <<"Feb"/utf8>>}},
                                            {mar,
                                                {<<"March"/utf8>>,
                                                    <<"Mar"/utf8>>}},
                                            {apr,
                                                {<<"April"/utf8>>,
                                                    <<"Apr"/utf8>>}},
                                            {may,
                                                {<<"May"/utf8>>, <<"May"/utf8>>}},
                                            {jun,
                                                {<<"June"/utf8>>,
                                                    <<"Jun"/utf8>>}},
                                            {jul,
                                                {<<"July"/utf8>>,
                                                    <<"Jul"/utf8>>}},
                                            {aug,
                                                {<<"August"/utf8>>,
                                                    <<"Aug"/utf8>>}},
                                            {sep,
                                                {<<"September"/utf8>>,
                                                    <<"Sep"/utf8>>}},
                                            {oct,
                                                {<<"October"/utf8>>,
                                                    <<"Oct"/utf8>>}},
                                            {nov,
                                                {<<"November"/utf8>>,
                                                    <<"Nov"/utf8>>}},
                                            {dec,
                                                {<<"December"/utf8>>,
                                                    <<"Dec"/utf8>>}}],
                                        fun(Month, Index) ->
                                            Strings@1 = erlang:element(2, Month),
                                            {Index,
                                                erlang:element(1, Strings@1),
                                                erlang:element(2, Strings@1)}
                                        end
                                    ),
                                    gleam@list:find(
                                        _pipe,
                                        fun(Month@1) ->
                                            (erlang:element(2, Month@1) =:= Month_string)
                                            orelse (erlang:element(3, Month@1)
                                            =:= Month_string)
                                        end
                                    )
                                end,
                                gleam_stdlib:parse_int(Year_string),
                                parse_time_section(Time_string@1)} of
                                {{ok, Day},
                                    {ok, {Month_index, _, _}},
                                    {ok, Year},
                                    {ok, [Hour, Minute, Second]}} ->
                                    case from_parts(
                                        {Year, Month_index + 1, Day},
                                        {Hour, Minute, Second, 0},
                                        case Offset_string of
                                            <<"GMT"/utf8>> ->
                                                <<"Z"/utf8>>;

                                            _ ->
                                                Offset_string
                                        end
                                    ) of
                                        {ok, Value@2} ->
                                            Correct_weekday = string_weekday(
                                                Value@2
                                            ),
                                            Correct_short_weekday = short_string_weekday(
                                                Value@2
                                            ),
                                            case gleam@list:contains(
                                                [Correct_weekday,
                                                    Correct_short_weekday],
                                                Weekday
                                            ) of
                                                true ->
                                                    {ok, Value@2};

                                                false ->
                                                    {error, nil}
                                            end;

                                        {error, nil} ->
                                            {error, nil}
                                    end;

                                {_, _, _, _} ->
                                    {error, nil}
                            end;

                        [Day_string@1, Time_string@2, Offset_string@1] ->
                            case gleam@string:split(Day_string@1, <<"-"/utf8>>) of
                                [Day_string@2, Month_string@1, Year_string@1] ->
                                    Time_string@3 = gleam@string:replace(
                                        Time_string@2,
                                        <<":"/utf8>>,
                                        <<""/utf8>>
                                    ),
                                    case {gleam_stdlib:parse_int(Day_string@2),
                                        begin
                                            _pipe@1 = gleam@list:index_map(
                                                [{jan,
                                                        {<<"January"/utf8>>,
                                                            <<"Jan"/utf8>>}},
                                                    {feb,
                                                        {<<"February"/utf8>>,
                                                            <<"Feb"/utf8>>}},
                                                    {mar,
                                                        {<<"March"/utf8>>,
                                                            <<"Mar"/utf8>>}},
                                                    {apr,
                                                        {<<"April"/utf8>>,
                                                            <<"Apr"/utf8>>}},
                                                    {may,
                                                        {<<"May"/utf8>>,
                                                            <<"May"/utf8>>}},
                                                    {jun,
                                                        {<<"June"/utf8>>,
                                                            <<"Jun"/utf8>>}},
                                                    {jul,
                                                        {<<"July"/utf8>>,
                                                            <<"Jul"/utf8>>}},
                                                    {aug,
                                                        {<<"August"/utf8>>,
                                                            <<"Aug"/utf8>>}},
                                                    {sep,
                                                        {<<"September"/utf8>>,
                                                            <<"Sep"/utf8>>}},
                                                    {oct,
                                                        {<<"October"/utf8>>,
                                                            <<"Oct"/utf8>>}},
                                                    {nov,
                                                        {<<"November"/utf8>>,
                                                            <<"Nov"/utf8>>}},
                                                    {dec,
                                                        {<<"December"/utf8>>,
                                                            <<"Dec"/utf8>>}}],
                                                fun(Month@2, Index@1) ->
                                                    Strings@2 = erlang:element(
                                                        2,
                                                        Month@2
                                                    ),
                                                    {Index@1,
                                                        erlang:element(
                                                            1,
                                                            Strings@2
                                                        ),
                                                        erlang:element(
                                                            2,
                                                            Strings@2
                                                        )}
                                                end
                                            ),
                                            gleam@list:find(
                                                _pipe@1,
                                                fun(Month@3) ->
                                                    (erlang:element(2, Month@3)
                                                    =:= Month_string@1)
                                                    orelse (erlang:element(
                                                        3,
                                                        Month@3
                                                    )
                                                    =:= Month_string@1)
                                                end
                                            )
                                        end,
                                        gleam_stdlib:parse_int(Year_string@1),
                                        parse_time_section(Time_string@3)} of
                                        {{ok, Day@1},
                                            {ok, {Month_index@1, _, _}},
                                            {ok, Year@1},
                                            {ok, [Hour@1, Minute@1, Second@1]}} ->
                                            case from_parts(
                                                {Year@1,
                                                    Month_index@1 + 1,
                                                    Day@1},
                                                {Hour@1, Minute@1, Second@1, 0},
                                                case Offset_string@1 of
                                                    <<"GMT"/utf8>> ->
                                                        <<"Z"/utf8>>;

                                                    _ ->
                                                        Offset_string@1
                                                end
                                            ) of
                                                {ok, Value@3} ->
                                                    Correct_weekday@1 = string_weekday(
                                                        Value@3
                                                    ),
                                                    Correct_short_weekday@1 = short_string_weekday(
                                                        Value@3
                                                    ),
                                                    case gleam@list:contains(
                                                        [Correct_weekday@1,
                                                            Correct_short_weekday@1],
                                                        Weekday
                                                    ) of
                                                        true ->
                                                            {ok, Value@3};

                                                        false ->
                                                            {error, nil}
                                                    end;

                                                {error, nil} ->
                                                    {error, nil}
                                            end;

                                        {_, _, _, _} ->
                                            {error, nil}
                                    end;

                                _ ->
                                    {error, nil}
                            end;

                        _ ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-file("src/birl.gleam", 1095).
-spec parse_month(binary()) -> {ok, month()} | {error, nil}.
parse_month(Value) ->
    Lowercase = string:lowercase(Value),
    Month = gleam@list:find(
        [{jan, {<<"January"/utf8>>, <<"Jan"/utf8>>}},
            {feb, {<<"February"/utf8>>, <<"Feb"/utf8>>}},
            {mar, {<<"March"/utf8>>, <<"Mar"/utf8>>}},
            {apr, {<<"April"/utf8>>, <<"Apr"/utf8>>}},
            {may, {<<"May"/utf8>>, <<"May"/utf8>>}},
            {jun, {<<"June"/utf8>>, <<"Jun"/utf8>>}},
            {jul, {<<"July"/utf8>>, <<"Jul"/utf8>>}},
            {aug, {<<"August"/utf8>>, <<"Aug"/utf8>>}},
            {sep, {<<"September"/utf8>>, <<"Sep"/utf8>>}},
            {oct, {<<"October"/utf8>>, <<"Oct"/utf8>>}},
            {nov, {<<"November"/utf8>>, <<"Nov"/utf8>>}},
            {dec, {<<"December"/utf8>>, <<"Dec"/utf8>>}}],
        fun(Month_string) ->
            {_, {Long, Short}} = Month_string,
            (Lowercase =:= string:lowercase(Short)) orelse (Lowercase =:= string:lowercase(
                Long
            ))
        end
    ),
    _pipe = Month,
    gleam@result:map(_pipe, fun(Month@1) -> erlang:element(1, Month@1) end).
