%%%-------------------------------------------------------------------
%%% @author dem <dvarkin@gmail.com>
%%% @copyright (C) 2014, dem
%%% @doc
%%% Main module for start and control Port with Java driver
%%% @end
%%% Created : 13 May 2014 by dem <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(hzerl).

-behaviour(gen_server).

%% API
-export([start_link/0, cmd/1, reload/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(JAR_PATH, "/Users/dem/projecs/hzerl/target/hz.jar").

-record(state, {port, cmd, node_drv}).

%%%===================================================================
%%% API
%%%===================================================================
reload() ->
	code:purge(?MODULE),
	code:load_file(?MODULE).
	
cmd(Cmd) ->
	gen_server:cast(?SERVER, {cmd, Cmd}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	start_link(?JAR_PATH).
start_link(JarPath) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [JarPath], []).

stop() ->
	gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([JarPath]) ->
	Node = string:join(["-Derlang.node=", atom_to_list(node())], ""),
	Cmd = string:join(["java -jar", Node, JarPath], " "),
	io:format("start jar: ~s~n", [Cmd]),
	Port = open_port({spawn, Cmd}, [binary, eof]),
	{ok, #state{port = Port, cmd = Cmd}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	io:format("close driver~n"),
	{stop, normal, State};
handle_cast({cmd, Cmd}, #state{node_drv = Node} = State) ->
	io:format("send to HZ ~p~n", [Cmd]),
	Node ! Cmd,
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, eof}, #state{port = Port} = State) ->
	io:format("dirver closed!~n"),
	{stop, dirver_closed,State};
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
	io:format("data from port driver: ~p", [Data]),
	{noreply, State};
handle_info(_Info, State) ->
	io:format("info in ctrl: ~p", [_Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
