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

%% @doc dgiot_topo Application
-module(dgiot_topo_app).
-include("dgiot_topo.hrl").
-behaviour(application).
-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).


%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_topo_sup:start_link(),
    Spec =  dgiot_topo:start_http(),
    {ok, _} = supervisor:start_child(Sup, Spec),
    {ok, Sup}.

stop(_State) ->
    ok.

