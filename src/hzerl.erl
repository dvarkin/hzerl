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
-export([start_link/0,
		 hz_sync/1,
		 hz_async/1,
		 reload/0,
		 stop/0,
		 state/0,
		 connect/0,
		 connect/1,
		 make_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(JAR_PATH, "/Users/dem/projecs/hzerl/target/hz.jar").

-record(state, {port, cmd, hzerl_node, hzerl_mbox, hzerl_pid}).

%%%===================================================================
%%% API
%%%===================================================================
reload() ->
	code:purge(?MODULE),
	code:load_file(?MODULE).

stop_jar(Pid) when is_list(Pid) ->
	KillCmd = "kill -9 " ++ Pid,
	os:cmd(KillCmd);
stop_jar(_) ->
	ok.

make_request(Pid, {cmd, _, _, _} = Cmd) when is_tuple(Cmd) ->
	R = list_to_tuple([pid, Pid] ++ tuple_to_list(Cmd)),
%	io:format("request: ~p~n", [R]),
	R.
	

%%--------------------------------------------------------------------
%% @doc Config = {user, UserName,
%%                password, UserPassword,
%%                hosts, ["localhost", "10.1.1.1"]
%%                connAtemptLimit, 5 , %% unlimited by default
%%                connAtemptPeriod, 3000,
%%                connTimeout, 5000}
%% @spec
%% @end
%%--------------------------------------------------------------------
connect() ->
	gen_server:call(?SERVER,{cmd, connect, config, {}}).

connect([_|_] = Config) ->
	gen_server:call(?SERVER,{cmd, connect, config, Config});
connect(Config) ->
	{error, {"not an emtpy list", Config}}.

hz_sync(Cmd) ->
	gen_server:call(?SERVER, {cmd, sync, args, Cmd}).

hz_async(Cmd) ->
	gen_server:cast(?SERVER, {cmd, async, args, Cmd}).

state() ->
	gen_server:call(?SERVER, state).

stop() ->
	gen_server:cast(?SERVER, stop).

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
	{registered_name, SelfName} = erlang:process_info(self(), registered_name),
	Node = string:join(["-Derlang.node=", atom_to_list(node())], ""),
	Mbox = string:join(["-Derlang.mbox=", atom_to_list(SelfName)], ""),
	Cmd = string:join(["java -jar", "-Xmx5g", Mbox, Node, JarPath], " "),
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
handle_call({cmd, _IntCmd, _HzCmd, _Args} = Cmd, From,
			#state{hzerl_node = Node, hzerl_mbox = Mbox, hzerl_pid = Pid} = State)
  when
	  Node =/= undefined andalso
	  Mbox =/= undefined andalso
	  Pid  =/= undefined
	  ->
	Fn = fun() ->
				 {Mbox, Node} ! make_request(self(), Cmd),
				 receive
					 Response ->
						 Reply = Response,
						 gen_server:reply(From, Reply)
				 after 5000 ->
						 gen_server:reply(From, timeout)
				 end
		 end,
	spawn(Fn),
	{noreply,State};
handle_call({cmd, _Cmd}, _From, State) ->
	Reply = {error, driver_not_ready},
	{reply, Reply, State};

handle_call(state, _From, State) ->
	Reply = State,
	{reply, Reply, State};
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
handle_cast({cmd, _IntCmd, _HzCmd, _Args} = Cmd,
			#state{hzerl_node = Node, hzerl_mbox = Mbox, hzerl_pid = Pid} = State)
  when
	  Node =/= undefined andalso
	  Mbox =/= undefined andalso
	  Pid  =/= undefined
	  ->
	{Mbox, Node} ! make_request(self(), Cmd),
	{noreply,State};
handle_cast(stop , #state{hzerl_pid = Pid} = State)	->
	stop_jar(Pid),
	{stop, normal, State};
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
	{stop, dirver_closed,State};
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
	io:format("data from port driver: ~p", [Data]),
	{noreply, State};
handle_info({hzerl_node, Node}, State) ->
	{noreply, State#state{hzerl_node = Node}};
handle_info({hzerl_mbox, Mbox}, State) ->
	{noreply, State#state{hzerl_mbox = Mbox}};
handle_info({hzerl_pid, Pid}, State) ->
	io:format("pid ~p~n", [Pid]),
	{noreply, State#state{hzerl_pid = Pid}};
handle_info(_Info, State) ->
%	io:format("unexpected info in ctrl: ~p", [_Info]),
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
terminate(_Reason, #state{hzerl_pid = Pid} ) ->
	stop_jar(Pid),
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
