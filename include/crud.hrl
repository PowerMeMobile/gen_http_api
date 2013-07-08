-ifndef(crud_hrl).
-define(crud_hrl, included).

-type params() :: [{atom(), term()}].

-type response() ::
	{ok, term()} |
	{http_code, pos_integer(), term()} |
	{exception, atom()} |
	{exception, atom(), [term()]} |
	{error, term()}.

-record(param, {
	name	 			:: atom(),
	mandatory = false 	:: boolean(),
	repeated = false 	:: boolean(),
	type				:: atom()
}).

-record(method_spec, {
	path 	:: [binary() | atom()],
	params 	:: [#param{}]
}).

-record(specs, {
	create 	:: #method_spec{} | undefined,
	read 	:: #method_spec{} | undefined,
	update 	:: #method_spec{} | undefined,
	delete 	:: #method_spec{} | undefined
}).

-endif.
