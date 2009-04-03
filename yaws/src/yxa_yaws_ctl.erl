%%%-------------------------------------------------------------------
%%% File    : yxa_yaws_ctl.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Descrip.: YXA embedded Yaws web server control
%%% Created : 1 Aug 2006
%%%           by Mikael Magnusson <mikma@users.sourceforge.net>
%%%           based on yxa_ctl.erl
%%%-------------------------------------------------------------------

-module(yxa_yaws_ctl).

%% api:s
-export([start/0]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(EXIT_OK, 0).
-define(EXIT_ERROR, 1).
-define(EXIT_NODEDOWN, 2).
-define(EXIT_USAGE, 3).

process(Node, ["start"]) ->
        case rpc:call(Node, yxa_yaws_app, start, []) of
	ok ->
	    io:format("Node ~p started~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(Node, ["reload"]) ->
    case rpc:call(Node, yxa_yaws_app, reload, []) of
	ok ->
	    io:format("Node ~p reloaded~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(Node, ["stop"]) ->
        case rpc:call(Node, yxa_yaws_app, stop, []) of
	ok ->
	    io:format("Node ~p stopped~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(Node, ["restart"]) ->
    case rpc:call(Node, yxa_yaws_app, restart, []) of
	ok ->
	    io:format("Node ~p restarted~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(Node, ["status"]) ->
        case rpc:call(Node, yxa_yaws_app, status, []) of
	ok ->
	    io:format("Node ~p running~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(_Node, [Cmd]) ->
    io:format("Invalid command ~p~n", [Cmd]),
    error.


start() ->
    case init:get_plain_arguments() of
	[NodeStr | Args] ->
	    Node = list_to_atom(NodeStr),
	    try process(Node, Args) of
		ok ->
		    erlang:halt(?EXIT_OK);
		error ->
		    erlang:halt(?EXIT_ERROR);
		{badrpc, nodedown} ->
		    io:format("Error: Node ~p not responding~n", [Node]),
		    erlang:halt(?EXIT_NODEDOWN);
		Unknown ->
		    io:format("Yxa_yaws_app RPC returned unknown result : ~p~n", [Unknown]),
		    erlang:halt(?EXIT_ERROR)
	    catch
		error: Y ->
		    ST = erlang:get_stacktrace(),
		    io:format("Yxa_yaws_ctl failed : error ~p ~p~n", [Y, ST]),
		    erlang:halt(?EXIT_ERROR);
		X: Y ->
		    io:format("Yxa_yaws_ctl failed : ~p ~p~n", [X, Y]),
		    erlang:halt(?EXIT_ERROR)
	    end;
	_ ->
	    io:format("Invalid arguments~n"),
	    erlang:halt(?EXIT_USAGE)
    end.
