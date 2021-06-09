-module(iconverl_tests).

-include_lib("eunit/include/eunit.hrl").

open_test() ->
    ?assertEqual({error, unsupported}, iconverl:open("utf-99", "ucs-34")),
    ?assertEqual({ok, <<>>}, iconverl:conv("ascii", "ascii", <<>>)).

conv2_test() ->
    CD = iconverl:open("ucs-2be", "utf-8"),
    ?assertEqual({ok, <<0,$t,0,$e,0,$s,0,$t>>}, iconverl:conv(CD, <<"test">>)),
    ?assertEqual({error, eilseq}, iconverl:conv(CD, <<129,129>>)).

conv3_test() ->
    ?assertEqual({ok, <<0,$t,0,$e,0,$s,0,$t>>},
                 iconverl:conv("ucs-2be", "utf-8", <<"test">>)).
conv4_test() ->
    ?assertEqual({error, eilseq}, iconverl:conv("ucs-2be", "utf-8", <<129,129>>)).

conv5_test() ->
    ?assertEqual({ok, <<"ab">>}, iconverl:conv("ASCII//ignore", "UTF-8",<<97,226,152,160,98>>)).

conv6_test() ->
    ?assertEqual({ok,<<"e8E">>}, iconverl:conv("ascii//translit//ignore", "utf-8", <<195, 168, 56, 195, 139>>)).

conv7_test() ->
    erlang:process_flag(max_heap_size, #{size => 1000000, kill => true, error_looger => true}),
    application:ensure_all_started(crypto),
    ?assertMatch(
         {ok, _},
         iconverl:conv("latin1//translit//ignore", "utf-8", crypto:strong_rand_bytes(32768))
    ).

conv8_test() ->
    erlang:process_flag(max_heap_size, #{size => 1000000, kill => true, error_looger => true}),
    application:ensure_all_started(crypto),
    Input = crypto:strong_rand_bytes(32768),
    {ok, Result} = iconverl:conv("latin1//translit//ignore", "utf-8", Input),
    io:format("~p:~p~n", [byte_size(Result), byte_size(Input)]),
    ?assert(byte_size(Result) =< byte_size(Input)).

chunk_test() ->
    {ok, Utf8Bin1} = iconverl:conv("utf-8", "latin1", <<"test", 16#e9, "test">>),
    Utf8Bin2 = binary:part(Utf8Bin1, 0, 5),
    CD = iconverl:open("latin1", "utf-8"),
    ?assertMatch({more, _}, iconverl:chunk(CD, Utf8Bin2)),
    ?assertMatch({done, _}, iconverl:chunk(CD, Utf8Bin1)).
