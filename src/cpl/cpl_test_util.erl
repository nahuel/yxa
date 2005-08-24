%% 
%%--------------------------------------------------------------------

-module(cpl_test_util).

%% -behaviour().

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 timezone_offset/0,
	 dst_offset/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([

        ]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").

%% -include_lib("").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: timezone_offset()
%% Descrip.: determine the hour offset +/-N hours from UTC
%% Returns : integer(), the hourly offset
%% Notes   : +/-N:30 timezones are unsupported
%%--------------------------------------------------------------------
timezone_offset() ->
    Date = {2004, 1, 1},
    Time = {0, 0, 0},
    [{D_UTC, T_UTC}] = calendar:local_time_to_universal_time_dst({Date,Time}),
    Timezone = dummy,
    HourDiff = ts_datetime:diff_datetime(Timezone, 
					 #date_time{date = Date, time = Time}, 
					 #date_time{date = D_UTC, time = T_UTC}, hourly),
    case {D_UTC, T_UTC} < {Date, Time} of
	%% local time is ahead
	true ->
	    HourDiff;
	%% local time behind
	false ->
	    -HourDiff
    end.


%%--------------------------------------------------------------------
%% Function: dst_offset()
%% Descrip.: determine the DST offset used in the current _local_  
%% Returns : 0 (DST not used) | 1 (DST used) 
%%--------------------------------------------------------------------
%% "DST commonly begins in the Northern Hemisphere at 2:00 AM on 
%%  either the first Sunday in April or the last Sunday in March, and 
%%  ends at 2:00 AM on the last Sunday in October. In the Southern 
%%  Hemisphere, the beginning and ending dates are switched" 
%% - wikipedia
%%--------------------------------------------------------------------
dst_offset() ->
    %% no DST in effect (in northern hemisphere)
    [{_, T1}] = calendar:local_time_to_universal_time_dst({{2004, 1, 1}, {13, 0, 0}}),
    %% DST in effect (in northern hemisphere)
    [{_, T2}] = calendar:local_time_to_universal_time_dst({{2004, 6, 1}, {13, 0, 0}}),
    %% hour diff -1 / 1 indicates that DST is used 
    {H1,_,_} = T1,
    {H2,_,_} = T2,
    abs(H2 - H1).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------

%%====================================================================
%% Test functions
%%====================================================================



