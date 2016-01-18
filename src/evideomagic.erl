%% @author igorzinin
%% @doc @todo Add description to evideomagic.


-module(evideomagic).
-behaviour(gen_server).

-include("evideomagic.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         stop/0,
         makeThumbnail/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% makeThumbnail/2
-spec makeThumbnail(VideoFname, Pid) -> ok when
    VideoFname::string(),
    Pid::pid().
%% ====================================================================
%% @doc creates video thumbnail with the following parameters:
%%      VideoFname - input video file
%%      Pid        - calback process will receive base64 encoded thumbnail
%% @end
makeThumbnail(VideoFname, Pid) ->
    WorkDir = application:get_env(?APPNAME, workdir, ?DEFAULT_WORKDIR),
    ok = filelib:ensure_dir(WorkDir ++ "/"),
    OutFname = WorkDir ++ "/" ++ uuid:to_string(uuid:uuid1())  ++ ".png",
    Request = #make_thumbnail{fnamein=VideoFname, fnameout=OutFname, pid=Pid},
    gen_server:cast(?MODULE, Request).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({make_thumbnail, VideoFname, OutFname, Pid}=Req, State) ->
    Cmd = "avconv -i "++ VideoFname ++ " -ss 00:00:00 -vsync 1 -qscale 1 -vframes 1 " ++ OutFname,
    Port = erlang:open_port({spawn, Cmd}, [stream, use_stdio, exit_status, binary]),
    {ok, Data, 0} = wait(Port, []),
    case erlang:port_info(Port) of
        undefined -> ok;
        _ -> true = erlang:port_close(Port)
    end,
    Pid ! Req,
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% -----------------------------------------------------------------------------
-spec wait(Port, InitBuffer) -> {ok, Data, Status}
    when Port          :: port(),
         InitBuffer    :: list(iolist()),
         Data          :: binary(),
         Status        :: non_neg_integer().
%%
%% @doc
%%      Receives until the given port exits.
%% @end
%% -----------------------------------------------------------------------------
wait(Port, Buff) ->
    receive
        {Port, {exit_status, Status}} ->
            {ok, iolist_to_binary(Buff), Status};
        {Port, {data, Data}} ->
            wait(Port, [Buff, Data])
    end.

