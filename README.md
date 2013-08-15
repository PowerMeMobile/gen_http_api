Gen HTTP API
===========

gen_http_api is an Erlang behaviour (cowboy based) that allows to quckly write simple REST (CRUD) handlers.

Using
-----

Add dependency into your rebar.config:

``` erlang
{deps, [
    {gen_http_api, ".*", {git, "https://github.com/PowerMeMobile/gen_http_api.git", {branch, "develop"}}}
]}.
```

Write simple handler:

``` erlang
-module(my_handler).
-behaviour(gen_http_api).

-export([
    init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include_lib("gen_http_api/include/crud_specs.hrl").

init() ->
    {ok, #specs{
        %% create method has not any parameters
        create = [],

        %% read method has 'id' mandatory parameter
        read = [#param{name = id, mandatory = false, repeated = false, type = binary}],

        %% update method is not supported
        %% behaviour respond with svc0006
        update = undefined,

        %% delete method is not supported
        delete = undefined,

        %% rout to resource
        route = "/customers/[:id]"
    }}.

%% Params variable is a proplist. For example: [{id, <<"CustomerID">>}]
create(Params) ->
    {http_code, 500}.
read(Params) ->
    {ok, <<"Any message">>}.
update(Params) ->
    {exception, 'svc0001', [<<"id">>]}.
delete(Params) ->
    {http_code, 204}.

Also your response can be (see crud.hrl):
-type response() ::
    {ok, body()} |
    {http_code, http_code()} |
	{http_code, http_code(), body()} |
	{exception, exception_id()} |
	{exception, exception_id(), [term()]} |
	{error, term()}.

%% {ok, body()} is equvalent to {http_code, 200, body()}

```

Then on your app start:
``` erlang
Routes = gen_http_api:compile_routes([my_handler]),
%% Routes =
%% [{"/customers/[:id]", gen_http_api, [my_handler]}]
DispatchRules = cowboy_router:compile({'_', Routes}).
```
And then start cowboy listener (read cowboy docs).

Response content type
---------------------
Instead of binary body, you can use an Erlang proplist that will be converted to JSON.
For example:
``` erlang
read(_Params) ->
    {ok, [{field1, <<"value1">>}, {field2, <<"value2">>}]}.
```

Supported type validations
--------------------------
By mandatory/optional:
- mandatory: if not presented in request, behaviour responds with svc0001 exception
- optional

By type:
- binary
- atom (list_to_existing_atom)
- boolean
- uuid
- string (list)
- integer
- {custom, fun(B::binary()) -> T::term()} %% for custom validation

By repeated:
- non repeated
- repeated (values delimited with semicolons <;>)

Supported http codes
--------------------
You handler can return http code with response body (optional).
Examples:
``` erlang
%% return http code 200 and default response body "OK"
read(_Params) ->
    {http_code, 200}.

%% return http code 200 and specified respose body
read(_Params) ->
    {http_code, 200, <<"My info">>}.
```

Supported http codes and default response body:

- 500 "Internal Server Error"
- 400 "Bad request"
- 401 "Authentication failure, check your authentication details" (also
response headers are presented: "Www-Authenticate: Basic")
- 404 "Not found: mistake in the host or path of the service URI"
- 204 "" (hardcoded to support [rfc2616])
- 201 "Created"
- 200 "OK"

[rfc2616]: http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

Supported exceptions
--------------------
The idea about exceptions was inspired from OneAPI [policy] and
[service] exceptions.
[service]: http://www.gsma.com/oneapi/common-service-exceptions
[policy]: http://www.gsma.com/oneapi/common-policy-exceptions

Available exceptions:

- svc0001 Text: "Invalid input %1 parameter value."; HTTP code: 400; Variables: 1
- svc0002 Text: "Invalid input %1 parameter value, valid values are %2"; HTTP code: 400; Variables: 2
- svc0003 Text: "Resource not found."; HTTP code: 404; Variables: 0
- svc0004 Text: "Resource already exist."; HTTP code: 400; Variables: 0
- svc0005 Text: "Missing mandatory %1 parameter."; HTTP code: 400; Variables: 1
- svc0006 Text: "Http method %1 not supported."; HTTP code: 400; Variables: 1
- svc0007 Text: <<"Error in path">>; HTTP code: 400; Variables: 0
- svc0008 Text: <<"Customer's \"rps\" setting is disabled.
See http://extranet.powermemobile.com/issues/17465 for detail.">> (for PMM only use)

Response example:
"Content-Type": "application/json"
``` js
{request_error:
    {service_exception:
        {message_id: "SVC0001",
        text: "Invalid input %1 parameter value.",
        variables: ["id"]}
    }
}
```

More examples
-------------
More examples you can find at https://github.com/PowerMeMobile/kelly/tree/develop/subapps/k_http_api/src

TODO
----
- avoid ignore cowboy Req in returns
- add ability to return any valid http code
- resource inheritance
- arbitrary actions for resource
