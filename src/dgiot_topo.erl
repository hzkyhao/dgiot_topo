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
-module(dgiot_topo).
-author("johnliu").

-export([start_http/0, docroot/0, get_topo/2, send_topo/3, get_Product/0, get_name/3]).

start_http() ->
    Port = application:get_env(?MODULE, port, 6081),
    DocRoot = docroot(),
    shuwa_http_server:start_http(?MODULE, Port, DocRoot).


docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = shuwa_httpc:url_join([Dir, "/priv/"]),
    Root ++ "www".


get_topo(Arg, _Context) ->
    #{<<"productid">> := ProductId,
        <<"devaddr">> := Devaddr
    } = Arg,
    case shuwa_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"konva">> := #{<<"Shape">> := Shape} = Konva}}} when length(Shape) > 0 ->
            case Devaddr of
                undefined ->
%%                    product
                    Topic = <<"thing/", ProductId/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(Konva)),
                    shuwa_mqtt:publish(self(), Topic, Base64),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva}};
                _ ->
%%                    device
                    DeviceId = shuwa_parse:get_deviceid(ProductId, Devaddr),
                    NewShape =
                        lists:foldl(fun(X, Acc) ->
                            Acc ++ [X#{<<"id">> => shuwa_parse:get_shapeid(DeviceId, maps:get(<<"id">>, X))}]
                                    end, [], Shape),
                    Topic = <<"thing/", DeviceId/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(Konva#{<<"Shape">> := NewShape})),
                    shuwa_mqtt:publish(self(), Topic, Base64),
                    {ok, #{<<"code">> => 200, <<"message">> => <<"SUCCESS">>, <<"data">> => Konva#{<<"Shape">> := NewShape}}}
            end;
        _ ->
            {ok, #{<<"code">> => 204, <<"message">> => <<"没有组态"/utf8>>}}
    end.

%% #{<<"Arcel">>=> 1,<<"Flow">> => 1.2} => ShapeId = md5(<<DeviceId/binary,"Arcel">>)
%%{
%%"konva":{
%%    "Shape":[
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                },
%%                {
%%                "id":[shapeid],
%%                "text":"16",
%%                }
%%        ]
%%    }
%%}  shuwa_data:get({product, <<"16cf2bf9f7energy">>})
%% dgiot_topo:send_topo(<<"16cf2bf9f7">>, <<"1">>, #{<<"energy">> => 1.2}).
send_topo(ProductId, Devaddr, Payload) ->
    DeviceId = shuwa_parse:get_deviceid(ProductId, Devaddr),
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Text = get_name(ProductId, K, shuwa_utils:to_binary(V)),
            Acc ++ [#{<<"id">> => shuwa_parse:get_shapeid(DeviceId, K), <<"text">> => Text}]
                  end, [], Payload),
    Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
    Base64 = base64:encode(jsx:encode(#{<<"konva">> => Shape})),
    shuwa_mqtt:publish(self(), Pubtopic, Base64).

get_name(ProductId, K, V) ->
    case shuwa_data:get({product, <<ProductId/binary, K/binary>>}) of
        not_find ->
            V;
        Name ->
            <<Name/binary, ":", V/binary>>
    end.

get_Product() ->
    case shuwa_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, _Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"thing">> := #{<<"properties">> := Properties}} ->
                        lists:map(fun(P) ->
                            Identifier = maps:get(<<"identifier">>, P),
                            Name = maps:get(<<"name">>, P),
                            shuwa_data:insert({product, <<ObjectId/binary, Identifier/binary>>}, Name)
                                  end, Properties);
                    _ ->
                        pass
                end
                        end, [], Results);
        _ ->
            pass
    end.