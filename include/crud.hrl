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
    name              :: atom(),
    mandatory = false :: boolean(),
    repeated = false  :: boolean(),
    type              :: atom()
}).

-record(specs, {
    create    :: [#param{}] | undefined,
    read      :: [#param{}] | undefined,
    update    :: [#param{}] | undefined,
    delete    :: [#param{}] | undefined,
    route     :: list()
}).

-endif.
