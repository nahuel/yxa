%%%-------------------------------------------------------------------
%%% File    : yxa_yaws_sup.erl
%%% @author   Mikael Magnusson <mikma@users.sourceforge.net>
%%% @doc      YXA embedded Yaws web server supervisor
%%% @since    5 Aug 2006
%%%           by Mikael Magnusson <mikma@users.sourceforge.net>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_yaws_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------
-export([init/1]).

%%--------------------------------------------------------------------
%% API:s
%%--------------------------------------------------------------------
-export([start_link/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @spec    () -> {ok, Pid}
%%
%% @doc     Starts the supervisor
%% @hidden
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 20, 60}, []}}.
