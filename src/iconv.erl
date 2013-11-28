%%% Copyright (c) 2013 Eric des Courtis <eric.des.courtis@benbria.ca>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%Standard interface for iconv libraries (to be used as a drop in replacement)
-module(iconv).

-export([open/2, conv/2, close/1]).

-spec open(string() | binary(), string() | binary()) -> {ok, any()} | {error, any()}.
open(ToCode, FromCode) when is_list(ToCode) or is_binary(ToCode),
    is_list(FromCode) or is_binary(FromCode) ->
    case iconverl:open(ToCode, FromCode) of
        {error, unsupported} ->
            {error, einval};
        T when is_tuple(T) ->
            T;
        X ->
            {ok, X}
    end.

-spec conv(any(), binary()) -> {ok, binary()} | {error, any()}.
conv(Cd, Input) ->
    iconverl:conv(Cd, Input).

-spec close(any()) -> ok | {error, any()}.
close(_Cd) ->
    ok.
