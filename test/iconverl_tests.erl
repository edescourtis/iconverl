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
    ?assertEqual({error, unsupported}, iconverl:open("utf-99", "ucs-34")),
    ?assertEqual({ok, <<0,$t,0,$e,0,$s,0,$t>>},
                 iconverl:conv("ucs-2be", "utf-8", <<"test">>)),
    ?assertEqual({error, eilseq}, iconverl:conv("ucs-2be", "utf-8", <<129,129>>)).

chunk_test() ->
    {ok, Utf8Bin1} = iconverl:conv("utf-8", "latin1", <<"test", 16#e9, "test">>),
    Utf8Bin2 = binary:part(Utf8Bin1, 0, 5),
    CD = iconverl:open("latin1", "utf-8"),
    ?assertMatch({more, _}, iconverl:chunk(CD, Utf8Bin2)),
    ?assertMatch({done, _}, iconverl:chunk(CD, Utf8Bin1)).
