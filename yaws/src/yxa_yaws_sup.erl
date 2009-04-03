%%%-------------------------------------------------------------------
%%% File    : yxa_yaws_sup.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Descrip.: YXA embedded Yaws web server supervisor
%%% Created : 5 Aug 2006
%%%           by Mikael Magnusson <mikma@users.sourceforge.net>
%%%-------------------------------------------------------------------
-module(yxa_yaws_sup).

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1]).

%% api:s
-export([start_link/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 20, 60}, []}}.
