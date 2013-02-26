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

-module(eiconv).

-export([convert/2, convert/3]).
-export([open/2, conv/2, conv/3, close/1]).
-export([chunk/2, finalize/1]).


-spec open(iodata() | string(), iodata() | string()) -> {ok, any()} | {error, any()}.
open(ToCode, FromCode) ->
    case iconverl:open(unicode:characters_to_list(ToCode), unicode:characters_to_list(FromCode)) of
        Error when is_tuple(Error) ->
            Error;
        Cd ->
            {ok, Cd} 
    end.

-spec conv(any(), iodata()) -> {ok, binary()} | {error, any()}.
conv(Cd, Input) ->
    iconverl:conv(Cd, Input).

-spec conv(any(), iodata(), integer()) -> {ok, binary()} | {error, any()}.
conv(Cd, Input, ChunkSize) when is_integer(ChunkSize) ->
    iconverl:conv(Cd, Input).

-spec close(any()) -> ok.
close(_Cd) ->
    ok.

-spec convert(string() | iodata(), iodata()) -> {ok, binary()} | {error, any()}.
convert(FromEncoding, Input) ->
    convert(FromEncoding, "utf-8", Input).

-spec convert(string() | iodata(), string() | iodata(), iodata()) -> {ok, binary()} | {error, any()}.
convert(FromEncoding, ToEncoding, Input) ->
    iconverl:conv(ToEncoding, FromEncoding, Input). 

-spec chunk(any(), iodata()) -> {ok, binary()} | {error, any()}. 
chunk(Cd, Input) when is_binary(Input) ->
    iconverl:chunk(Cd, Input);
chunk(Cd, Input) when is_list(Input) ->
    iconverl:chunk(Cd, iolist_to_binary(Input)).

-spec finalize(any()) -> ok | {rest, binary()} | {error, any()}.
finalize(Cd) ->
    iconverl:reset(Cd).
