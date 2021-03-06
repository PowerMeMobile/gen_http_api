-module(gen_http_api).

-behaviour(cowboy_http_handler).

-export([compile_routes/1]).
-export([init/3, handle/2, terminate/3]).

-include("logging.hrl").
-include("crud.hrl").

%-define(TEST, 1).
-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.

%% deprecated, do not use.
-record(addr, {
    addr :: string(),
    ton :: integer(),
    npi :: integer()
}).

-record(state, {
    handler           :: module(),
    req               :: cowboy_req:req(),
    handler_spec      :: #specs{},
    method_params     :: [#param{}] | undefined,
    req_params        :: [{binary(), binary()}],
    handler_params    :: [{atom(), term()}],
    view              :: term(),
    handler_func      :: create | read | update | delete
}).

%% ===================================================================
%% Callbacks
%% ===================================================================

-callback init() -> {ok, #specs{}}.

-callback create(params()) -> response().

-callback read(params()) -> response().

-callback update(params()) -> response().

-callback delete(params()) -> response().

%% ===================================================================
%% API
%% ===================================================================

-spec compile_routes(Handlers :: [module()]) ->
    [{Path :: list(), ?MODULE, [Handler :: module()]}].
compile_routes(Handlers) ->
    compile_routes(Handlers, []).
compile_routes([], Acc) ->
    Routes = lists:reverse(Acc),
    io:format("gen_http_api dispatch rules: ~n~p~n", [Routes]),
    Routes;
compile_routes([Handler | Rest], Acc) ->
    {ok, Specs} = Handler:init(),
    compile_routes(Rest, [{Specs#specs.route, ?MODULE, [Handler]} | Acc]).

%% ===================================================================
%% Cowboy Callback Functions
%% ===================================================================

-spec init({tcp, http}, cowboy_req:req(), [module()]) ->
    {ok, cowboy_req:req(), #state{}}.
init({tcp, http}, Req, [Handler]) ->
    ?log_debug("Req: ~p", [Req]),
    ?log_debug("Handler: ~p", [Handler]),
    {View, _} = cowboy_req:qs_val(<<"view">>, Req),
    {ok, Req, #state{view = View, handler = Handler}}.

-spec handle(cowboy_req:req(), #state{}) ->
    {ok, cowboy_req:req(), #state{}}.
handle(Req1, State = #state{handler = Handler}) ->
    {ok, HandlerSpec} = Handler:init(),
    {Method, Req2} = cowboy_req:method(Req1),
    {ReqParameters, Req3} = get_requests_parameters(Method, Req2),
    ?log_debug("ReqParameters: ~p", [ReqParameters]),
    NewState = State#state{
        req = Req3,
        handler_spec = HandlerSpec,
        req_params = ReqParameters},
    get_method_spec(Method, HandlerSpec, NewState).

-spec terminate(any(), cowboy_req:req(), #state{}) -> ok.
terminate(_Reason, _Req, #state{}) ->
    ok.

%% ===================================================================
%% Local Functions
%% ===================================================================

get_method_spec(<<"GET">>, #specs{read = Params}, State) when Params =/= undefined ->
    process_path(State#state{method_params = Params, handler_func = read});
get_method_spec(<<"POST">>, #specs{create = Params}, State) when Params =/= undefined ->
    process_path(State#state{method_params = Params, handler_func = create});
get_method_spec(<<"PUT">>, #specs{update = Params}, State) when Params =/= undefined ->
    process_path(State#state{method_params = Params, handler_func = update});
get_method_spec(<<"DELETE">>, #specs{delete = Params}, State) when Params =/= undefined ->
    process_path(State#state{method_params = Params, handler_func = delete});
get_method_spec(Method, _, State = #state{req = Req}) ->
    ?log_debug("[~p] method not supported", [Method]),
    exception('svc0006', [Method], Req, State).

process_path(State = #state{req = Req0}) ->
    {AtomBindings, Req1} = cowboy_req:bindings(Req0),
    Bindings = [{atom_to_binary(AtomName, utf8), Value} || {AtomName, Value} <- AtomBindings],
    #state{req_params = Params} = State,
    NewReqParams = lists:flatten([Bindings, Params]),
    ?log_debug("Params with bindings: ~p", [NewReqParams]),
    process_method_params(State#state{req = Req1, req_params = NewReqParams}).


process_method_params(State = #state{method_params = Params, req_params = ReqParamsPL}) ->
    process_method_params(Params, ReqParamsPL, [], State).

process_method_params([], _, Acc, State = #state{}) ->
    ?log_debug("Processed parameters: ~p", [Acc]),
    process_req(State#state{handler_params = Acc});
process_method_params([ParamSpec | Tail], ReqParamsPL, Acc, State = #state{req = Req}) ->
    ?log_debug("ParamSpec: ~p", [ParamSpec]),
    case get_parameter(ParamSpec, ReqParamsPL) of
        {ok, KeyValue} ->
            process_method_params(Tail, ReqParamsPL, [KeyValue | Acc], State);
        {error, missing, Name} ->
            exception('svc0005', [Name], Req, State);
        {error, invalid, Name} ->
            exception('svc0001', [Name], Req, State);
        {error, invalid, Name, Range} ->
            exception('svc0002', [Name, Range], Req, State);
        {error, disabled, <<"rps">>} ->
            exception('svc0008', [], Req, State)
    end.

get_parameter(Spec = #param{name = Name, repeated = true}, ReqParamsPL) ->
    ValuesDelimited = proplists:get_value(atom_to_binary(Name, utf8), ReqParamsPL),
    validate_repeated(ValuesDelimited, Spec);
get_parameter(Spec = #param{name = Name, repeated = false}, ReqParamsPL) ->
    Value = proplists:get_value(atom_to_binary(Name, utf8), ReqParamsPL),
    validate(Value, Spec).

validate_repeated(undefined, Spec = #param{mandatory = true}) ->
    ?log_error("Bad request. Missing mandatory parameter [~p].", [Spec#param.name]),
    {error, missing, atom_to_binary(Spec#param.name, utf8)};
validate_repeated(undefined, Spec) ->
    {ok, {Spec#param.name, undefined}};
validate_repeated(ValuesDelimited, Spec) ->
    Values = binary:split(ValuesDelimited, [<<";">>], [global, trim]),
    validate_repeated(Values, [], Spec).

validate_repeated([], Acc, Spec) ->
    {ok, {Spec#param.name, lists:reverse(Acc)}};
validate_repeated([RawValue | Tail], Acc, Spec) ->
    case validate(RawValue, Spec) of
        {ok, {_Key, Value}} ->
            validate_repeated(Tail, [Value | Acc], Spec);
        Any ->
            Any
    end.

validate(undefined, Spec = #param{mandatory = true}) ->
    ?log_error("Bad request. Missing mandatory parameter [~p].", [Spec#param.name]),
    {error, missing, Spec#param.name};
validate(Value, Spec) ->
    try
        Converted = convert(Value, Spec#param.type),
        {ok, {Spec#param.name, Converted}}
    catch
        error:disabled ->
            ?log_warn("Disabled parameter found [~p]", [Spec#param.name]),
            {error, disabled, atom_to_binary(Spec#param.name, utf8)};
        error:_ ->
            ?log_warn("Invalid [~s] parameter value", [Spec#param.name]),
            {error, invalid, atom_to_binary(Spec#param.name, utf8)}
    end.


process_req(State = #state{req = Req, handler_params = Params, view = V, handler = Handler, handler_func = Function}) ->
    case Handler:Function(Params) of
        {http_code, Code} ->
            http_code(Code, Req, State);
        {http_code, Code, Response} ->
            {ok, Body} = gen_http_api_converter:process(Response, V),
            http_code(Code, Body, Req, State);
        {exception, Code} ->
            exception(Code, [], Req, State);
        {exception, Code, Variables} ->
            exception(Code, Variables, Req, State);
        {ok, Response} ->
            {ok, Body} = gen_http_api_converter:process(Response, V),
            http_code(200, Body, Req, State);
        Any ->
            ?log_warn("Unexpected result: ~p", [Any]),
            http_code(500, Req, State)
    end.

%% deprecated, use {custom, Fun} instead.
convert(State, customer_state) ->
    case State of
        <<"0">> -> 0;
        <<"1">> -> 1
    end;
%% deprecated, use {custom, Fun} instead.
convert(SMPPType, smpp_type) ->
    convert_smpp_type(SMPPType);
%% deprecated, use {custom, Fun} instead.
convert(Value, addr) ->
    ?log_debug("Addr: ~p", [Value]),
    decode_address(Value);

convert(undefined, _Type) ->
    undefined;
convert(Value, {custom, Fun}) ->
    Fun(Value);
convert(_Value, disabled) ->
    erlang:error(disabled);
convert(Value, atom) ->
    list_to_existing_atom(binary_to_list(Value));
convert(Any, boolean) ->
    convert_boolean(Any);
convert(UUID, uuid) ->
    validate_uuid(UUID);
convert(Value, binary) ->
    Value;
convert(Value, string) ->
    binary_to_list(Value);
convert(Value, integer) ->
    list_to_integer(binary_to_list(Value));
convert(Value, float) ->
    convert_float(Value).

convert_boolean(<<"true">>) ->
    true;
convert_boolean(<<"false">>) ->
    false;
convert_boolean(<<"True">>) ->
    true;
convert_boolean(<<"False">>) ->
    false;
convert_boolean(Any) ->
    erlang:error({not_boolean, Any}).

convert_float(Bin) ->
    List = binary_to_list(Bin),
    try list_to_float(List)
    catch
        error:badarg ->
            try convert(Bin, integer) of
                Int -> Int * 1.0
            catch
                error:badarg ->
                    case lists:suffix(".", List) of
                        true ->
                            convert_float(list_to_binary(List ++ "0"));
                        false ->
                            case lists:prefix(".", List) of
                                true ->
                                    convert_float(list_to_binary("0" ++ List));
                                false ->
                                    erlang:error({bad_float, Bin})
                            end
                    end
            end
    end.

validate_uuid(UUID) ->
    try uuid:parse(UUID) of
        _ -> UUID
    catch
        error:badarg ->
            erlang:error({bad_uuid, UUID})
    end.

%% deprecated, do not use.
decode_address(AddrBin) ->
    AddrString = binary_to_list(AddrBin),
    [Addr, Ton, Npi] = string:tokens(AddrString, ","),
    #addr{
        addr = list_to_binary(Addr),
        ton = list_to_integer(Ton),
        npi = list_to_integer(Npi)
    }.

%% deprecated, do not use.
convert_smpp_type(Type) ->
    case Type of
        <<"transmitter">> -> transmitter;
        <<"receiver">> -> receiver;
        <<"transceiver">> -> transceiver
    end.

%% ===================================================================
%% HTTP Response Codes
%% ===================================================================

http_code(Code, Req, State) ->
    http_code(Code, undefined, Req, State).

http_code(500, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Internal Server Error">>),
    http_reply(500, [], Body, Req, State);
http_code(400, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Bad request">>),
    http_reply(400, [], Body, Req, State);
http_code(401, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Authentication failure, check your authentication details">>),
    Headers = [{<<"Www-Authenticate">>, <<"Basic">>}],
    http_reply(401, Headers, Body, Req, State);
http_code(403, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Forbidden">>),
    http_reply(403, [], Body, Req, State);
http_code(404, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Not found: mistake in the host or path of the service URI">>),
    http_reply(404, [], Body, Req, State);
http_code(409, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Conflict">>),
    http_reply(409, [], Body, Req, State);
http_code(204, _ExtBody, Req, State) ->
    http_reply(204, [], <<>>, Req, State);
http_code(201, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"Created">>),
    http_reply(201, [], Body, Req, State);
http_code(200, ExtBody, Req, State) ->
    Body = resolve_body(ExtBody, <<"OK">>),
    http_reply(200, [], Body, Req, State).

http_reply(Code, Headers, Body, Req, State) ->
    {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, Req2, State}.

resolve_body(undefined, DefaultBody) ->
    DefaultBody;
resolve_body(ExternalBody, _DefaultBody) ->
    ExternalBody.

%% ===================================================================
%% Exceptions
%% ===================================================================

-spec exception(ExceptionTag :: atom(), Variables :: [term()], Req :: term(), State :: term()) ->
    {ok, Req2 :: term(), State :: term()}.
exception(Code, Variables, Req, State) ->
    ?log_debug("Code: ~p, Variables: ~p", [Code, Variables]),
    {ok, Body, HttpCode} = exception_body_and_code(Code, Variables),
    ContentType = <<"application/json">>,
    Headers = [{<<"Content-Type">>, ContentType}],
    http_reply(HttpCode, Headers, Body, Req, State).

%%
%% Service exceptions
%%
exception_body_and_code('svc0001', Variables) ->
    MessageID = <<"SVC0001">>,
    Text = <<"Invalid input %1 parameter value.">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0002', Variables) ->
    MessageID = <<"SVC0002">>,
    Text = <<"Invalid input %1 parameter value, valid values are %2">>,
    2 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0003', Variables) ->
    MessageID = <<"SVC0003">>,
    Text = <<"Resource not found.">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 404};

exception_body_and_code('svc0004', Variables) ->
    MessageID = <<"SVC0004">>,
    Text = <<"Resource already exists.">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0005', Variables) ->
    MessageID = <<"SVC0005">>,
    Text = <<"Missing mandatory %1 parameter.">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0006', Variables) ->
    MessageID = <<"SVC0006">>,
    Text = <<"Http method %1 not supported.">>,
    1 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0007', Variables) ->
    MessageID = <<"SVC0007">>,
    Text = <<"Error in path">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code('svc0008', Variables) ->
    MessageID = <<"SVC0008">>,
    Text = <<"Customer's \"rps\" setting is disabled. See http://extranet.powermemobile.com/issues/17465 for detail.">>,
    0 = length(Variables),
    {ok, Body} = exception_body(MessageID, Text, Variables),
    {ok, Body, 400};

exception_body_and_code(Exception, _Variables) ->
    ?log_error("", []),
    {error, {no_such_exception, Exception}}.

exception_body(MessageID, Text, Variables) ->
    Body = [
        {<<"request_error">>, [
            {<<"service_exception">>, [
                {<<"message_id">>, MessageID},
                {<<"text">>, Text},
                {<<"variables">>, Variables}
            ]}
        ]}
    ],
    {ok, Json} = gen_http_api_converter:process(Body, <<"json">>),
    ?log_warn("Exception json body: ~p", [Body]),
    {ok, Json}.

get_requests_parameters(Method, Req0) ->
    case Method of
        Method when Method =:= <<"POST">> orelse Method =:= <<"PUT">> ->
            {ok, Body, Req1} = cowboy_req:body(Req0, [{length, 800000}]),
            BodyQs = cow_qs:parse_qs(Body),
            {QsVals, _} = cowboy_req:qs_vals(Req1),
            {BodyQs ++ QsVals, Req1};
        _Any ->
            cowboy_req:qs_vals(Req0)
    end.

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

convert_float_test() ->
    ?assertEqual(1.0, convert(<<"1.0">>, float)),
    ?assertEqual(1.0, convert(<<"1.00">>, float)),
    ?assertEqual(1.0, convert(<<"1.">>, float)),
    ?assertEqual(0.1, convert(<<".1">>, float)),
    ?assertEqual(0.1, convert(<<"00.1">>, float)),
    ?assertEqual(1.0, convert(<<"1">>, float)),
    ?assertException(error, {bad_float, <<"float">>}, convert(<<"float">>, float)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
