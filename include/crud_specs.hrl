-ifndef(crud_specs_hrl).
-define(crud_specs_hrl, included).

-include("crud.hrl").

-spec init() -> {ok, #specs{}}.

-spec create(params()) -> response().

-spec read(params()) -> response().

-spec update(params()) -> response().

-spec delete(params()) -> response().

-endif.
