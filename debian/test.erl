-module(test).

-export([start/0, stop/0, run/0]).

start() ->
    xref:start(s),
%    xref:add_release(s, code:lib_dir(), {name, otp}),
%    xref:add_directory(s, "/home/mikael/svn/alioth/pkg-voip/build-area/yxa-1.0~rc1/debian/yxa", [{recurse,true}]).
    xref:add_directory(s, "/usr/lib/yxa/ebin", [{recurse,true}]).

stop() ->
    xref:stop(s).
    
run() ->
    Apps=lists:nth(2, erlang:tuple_to_list(xref:q(s, "(App)range ME"))),
    Search=lists:map(fun(E) -> os:cmd("dpkg --search " ++ filename:join(code:lib_dir(E), "ebin")) end, Apps),
    Pkgs=lists:usort(lists:map(fun(E) -> lists:nth(1, string:tokens(E, ":")) end, Search)),
    Versions = lists:map(fun(E) -> Version=os:cmd("dpkg-query --show --showformat='${Version}' " ++ E), {E, Version} end, Pkgs),
    


%%     Ver2 = string:substr(Version, string:chr(Version, $:)+1),
%%     Upstream = string:substr(Ver2, 1, string:rchr(Ver2, $-)-1),
%%     io:format("Ver ~p, Upstream ~p~n", [Ver2, Upstream]),

    Versions.
