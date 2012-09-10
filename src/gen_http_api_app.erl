-module(gen_http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("logging.hrl").

%% ==============================================================
%% Application callbacks
%% ==============================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	ok.
	%% gen_http_api_sup:start_link().

stop(_State) ->
    ok.
