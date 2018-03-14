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

-module(emq_realtime_online).

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).


-define(ONLINE_TAB, mqtt_online).


%% Hooks functions
-export([on_client_subscribe/4, on_client_unsubscribe/4]).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]).
    

on_client_subscribe(ClientId, Username, TopicTable, _Env) ->
    [H | _] = proplists:get_keys(TopicTable),
    case ets:lookup(?ONLINE_TAB, H) of
        [{_, Val}] -> ets:update_element(?ONLINE_TAB, H, {2, Val+1});
        [] -> ets:insert(?ONLINE_TAB, {H, 1})
    end,
    {ok, TopicTable}.
    
on_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
    [H | _] = proplists:get_keys(TopicTable),
    case ets:lookup(?ONLINE_TAB, H) of
        [{_, Val}] -> ets:update_element(?ONLINE_TAB, H, {2, Val-1});
        [] -> ets:delete(?ONLINE_TAB, H)
    end,
    {ok, TopicTable}.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4).

