%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_realtime_online_sup).

-behaviour(gen_server).

-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/0, stop/0, publish/2, start_tick/1, stop_tick/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {tick}).

-define(ONLINE_TAB, mqtt_online).

-define(SERVER, ?MODULE).


%% @doc Start stats server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).


start_tick(Msg) ->
    {ok, TRef} = timer:send_interval(emqttd:env(interval, 3000), Msg), TRef.

stop_tick(TRef) ->
        timer:cancel(TRef).


%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([]) ->
    emqttd_time:seed(),
    ets:new(?ONLINE_TAB, [set, public, named_table, {write_concurrency, true}]),
    {ok, #state{tick = start_tick(tick)}, hibernate}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};    
handle_call(_Request, _From, State) ->
    {reply, error, State}.

%% atomic
handle_cast({setstats, Stat, MaxStat, Val}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Interval Tick.
handle_info(tick, State) ->
    [publish(Stat, Val) || {Stat, Val} <- ets:tab2list(?ONLINE_TAB)],
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tick = TRef}) ->
    stop_tick(TRef).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

publish(Topic, Val) ->
    ClientId = online,
    Payload = list_to_binary(io_lib:format("{\"typ\": \"online\", \"value\": \"~w\"}", [Val])),
    emqttd_mgmt:publish({ClientId, Topic, Payload, 0, false}).

