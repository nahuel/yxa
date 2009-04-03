%%%-------------------------------------------------------------------
%%% File    : yxa_yaws_app.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Descrip.: YXA embedded Yaws web server application.
%%% Created : 1 Aug 2006
%%%           by Mikael Magnusson <mikma@users.sourceforge.net>
%%%-------------------------------------------------------------------
-module(yxa_yaws_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0, reload/0, stop/0, restart/0, status/0]).

-include_lib("yaws/include/yaws.hrl").

%% application callbacks
start(normal, []) ->
    ok = start_yaws(),
    yxa_yaws_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: start
%% Descrip.: Start and configure the yaws web server application
%% Returns : ok
%%--------------------------------------------------------------------
start() ->
    start_yaws().

start_yaws() ->
    ok = application:set_env(yaws, embedded, true),
%%    yaws:start(),
    set_yaws_config().


%%--------------------------------------------------------------------
%% Function: reload
%% Descrip.: Reload configuration of the yaws web server application
%% Returns : ok
%%--------------------------------------------------------------------
reload() ->
    set_yaws_config().


set_yaws_config() ->
    {ok, DefaultIncdir} = inc_dir(?MODULE),
    Includedir = get_env_default(includedir, DefaultIncdir),
    Docroot = get_env_default(docroot, ?DOCROOT),
    Tmpdir = get_env_default(tmpdir, ?CACHEDIR),
    Logdir = get_env_default(logdir, ?LOGDIR),
    GC = yaws_config:make_default_gconf(false, "yxa"),
    SC = #sconf{port = 8888,
		servername = "localhost",
		listen = {127,0,0,1},
		docroot = Docroot},
    MyGC = GC#gconf{logdir=Logdir,
		    include_dir=GC#gconf.include_dir ++ [Includedir]},
    ok = yaws_api:setconf(MyGC, [[SC]]).

get_env_default(Param, Default) ->
    case application:get_env(yxa_yaws, Param) of
	{ok, Value} ->
	    Value;
	undefined ->
	    Default
    end.

inc_dir(Module) ->
    Filename = code:which(Module),
    Dir = filename:dirname(Filename),
    IncDir = filename:join(Dir, "include"),
    case filelib:is_dir(IncDir) of
	true ->
	    {ok, IncDir};
	false ->
	    {ok, filename:join(filename:dirname(Dir), "include")}
    end.


%%--------------------------------------------------------------------
%% Function: stop
%% Descrip.: Stop yaws web server application and terminate system
%% Returns : void
%%--------------------------------------------------------------------
stop() ->
    %%stop_yaws(),
    %% Need to stop yxa_yaws_app manually
    %%application:stop(yxa_yaws_app),
    init:stop().

%% stop_yaws() ->
%%    application:stop(yaws).


%%--------------------------------------------------------------------
%% Function: restart()
%% Descrip.: restart application.
%% Returns : does not return
%%--------------------------------------------------------------------
restart() ->
    %%    logger:log(normal, "yxa_yaws: restarting"),
    init:restart().


%%--------------------------------------------------------------------
%% Function: status
%% Descrip.: Check if yaws is running
%% Returns : ok | {error, {not_started, yaws}}
%%--------------------------------------------------------------------
status() ->
    check_running(yaws).


%%--------------------------------------------------------------------
%% Function: check_running(AppName)
%%           AppName = atom(), Application name
%% Descrip.: Check if an application is running
%% Returns : ok | {error, {not_started, AppName}}
%%--------------------------------------------------------------------
check_running(AppName) ->
    check_running(AppName, application:which_applications()).

check_running(AppName, [{AppName, _Desc, _Vsn}|_R]) ->
    ok;
check_running(AppName, []) ->
    {error, {not_started, AppName}};
check_running(_AppName, [_AppName2|R]) ->
    check_running(_AppName, R).
