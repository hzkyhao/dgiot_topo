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
-module(dgiot_topo_handler).
-author("johnliu").
-behavior(shuwa_rest).
-shuwa_rest(all).
-compile([{parse_transform, lager_transform}]).

%% API
-export([handle/4]).
-export([swagger_topo/0]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    shuwa_http_server:bind(<<"/opc">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    shuwa_http_server:bind(<<"/swagger_topo.json">>, ?MODULE, [], priv)
swagger_topo() ->
    [
        shuwa_http_server:bind(<<"/swagger_topo.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: shuwa_req:req()) ->
    {Status :: shuwa_req:http_status(), Body :: map()} |
    {Status :: shuwa_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: shuwa_req:http_status(), Headers :: map(), Body :: map(), Req :: shuwa_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            lager:info("do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> shuwa_framework:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            lager:debug("do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            lager:info("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            lager:info("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            lager:info("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% topo 概要: 组态
%% OperationId:topo
%% 请求:GET topo
do_request(get_topo, Arg, Context, _Req) ->
    case dgiot_topo:get_topo(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;


%% topo 概要: 组态
%% OperationId:topo
%% 请求:post post_send_topo
do_request(post_send_topo, Arg, Context, _Req) ->
    lager:info("Arg ~p", [Arg]),
    case dgiot_topo:put_topo(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
