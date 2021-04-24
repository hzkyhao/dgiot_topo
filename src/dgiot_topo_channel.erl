%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_topo_channel).
-behavior(shuwa_channelx).
-define(TYPE, <<"DGIOTTOPO">>).
-author("johnliu").
-record(state, {id, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 2,
    title => #{
        zh => <<"TOPO组态通道"/utf8>>
    },
    description => #{
        zh => <<"TOPO组态通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"TOPOBRAND">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"dgiottopo"/utf8>>,
        title => #{
            zh => <<"DGIOT厂商"/utf8>>
        },
        description => #{
            zh => <<"DGIOT厂商"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    shuwa_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"product">> := Products} = ChannelArgs) ->
    NewEnv = get_newenv(ChannelArgs),
    [{ProductId, App} | _] = get_app(Products),
    State = #state{
        id = ChannelId,
        env = NewEnv#{
            <<"app">> => App,
            <<"product">> => ProductId
        }
    },
    {ok, State}.

%% 初始化池子
handle_init( State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    lager:info("channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(Message, State) ->
    lager:info("channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    lager:info("channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

get_app(Products) ->
    lists:map(fun({_ProdcutId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        [<<"role:", _App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
        {_ProdcutId, _App}
              end, Products).

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).