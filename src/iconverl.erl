%%% Copyright (c) 2013-2014 Eric des Courtis <eric.des.courtis@benbria.ca>
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

-module(iconverl).

-on_load(load_nif/0).

-export([open/2, iconv/2, reset/1, ignores/1, chunk/2, conv/2, conv/3]).

-opaque cd() :: binary().
-export_type([cd/0]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------
-spec open(string() | binary(), string() | binary()) -> cd() | {error, unsupported | any()}.
open(To, From) when is_binary(To), is_binary(From) ->
    open(unicode:characters_to_list(To), unicode:characters_to_list(From));
open(To, From) when is_list(To), is_list(From) ->
    open_priv(To, From);
open(To, From) when is_list(To), is_binary(From) ->
    open(To, unicode:characters_to_list(From));
open(To, From) when is_binary(To), is_list(From) ->
    open(unicode:characters_to_list(To), From).


open_priv(To, From) when is_list(To), is_list(From) ->
    erlang:nif_error(not_loaded).

-spec iconv(cd(), binary()) -> 
    {ok,    e2big,     integer(), binary()} |
    {ok,    eilseq,    integer(), binary()} |
    {ok,    einval,    integer(), binary()} |
    {ok,    integer(), binary()           } |
    {error, atom()                        } |
    {error, integer()                     }.
iconv(_Cd, _Binary) ->
    erlang:nif_error(not_loaded).

-spec ignores(cd()) -> true | false.
ignores(_Cd) ->
    erlang:nif_error(not_loaded).


chunk(_CD, Acc, Status) ->
    {Status, Acc}.

chunk(CD, Data, Acc, PreviousOffset) when is_binary(Data), is_list(Acc) ->
    case iconverl:iconv(CD, Data) of
        {ok, e2big, Offset0, OutputData} ->
            Offset1 = case (PreviousOffset =:= Offset0) and ignores(CD) =:= true of
              true -> Offset0 + 1;
              false -> Offset0
            end,
            chunk(
                CD,
                binary:part(Data, byte_size(Data) - Offset1, Offset1),
                [Acc | [OutputData | []]],
                Offset1
            );
        {ok, einval, _Offset, OutputData} ->
            chunk(CD, [Acc | [OutputData | []]], more);
        {ok, 0, OutputData} ->
            chunk(CD, [Acc | [OutputData | []]], done);
        {ok, Offset, OutputData} ->
            chunk(
                CD,
                binary:part(Data, byte_size(Data) - Offset, Offset),
                [Acc | [OutputData | []]],
                Offset
            );
        Other ->
            Other
    end.

%% more means try again with the same data concatenated with more input
-spec chunk(cd(), iodata()) ->
    {done, iodata()} | {more, iodata()} | {error, atom()} | {ok, eilseq, integer(), binary()}.
chunk(CD, Data) when is_binary(Data) or is_list(Data)  ->
    chunk(CD, iolist_to_binary(Data), [], 0).


-spec conv(cd(), iodata()) -> {ok, binary()} | {error, atom()}.
conv(CD, Data) when is_binary(Data) or is_list(Data)  ->
    case chunk(CD, Data) of
        {done, Result} ->
            {ok, iolist_to_binary(Result)};
        {more, _Result} ->
            {error, einval};
        {ok,eilseq, _, _} ->
            {error, eilseq};
        {error, Reason} ->
            {error, Reason}
    end.

-spec conv(string() | binary(), string() | binary(), iodata()) -> {ok, binary()} | {error, atom()}.
conv(To, From, Data) when is_list(To) or is_binary(To),
    is_list(From) or is_binary(From), is_list(Data) or is_binary(Data) ->
    case open(To, From) of
        Error when is_tuple(Error) ->
            Error;
        CD ->
            conv(CD, Data)
    end.


-spec reset(cd()) -> ok | {error, atom()} | {error, integer()}.
reset(_Cd) ->
    erlang:nif_error(not_loaded).

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------

load_nif() ->
    erlang:load_nif(filename:join(code:priv_dir(iconverl), "iconverl"), 0).
