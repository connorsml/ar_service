-module(ar_service).

-behaviour(gen_server).

-export([start_link/0, run/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-define(APP, ar_service).

start_link() ->
    gen_server:start_link({local, ar_service}, ar_service,
			  [], []).

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

loop() ->
    receive
      {FromPid, Msg} ->
	  put("models", FromPid),
	  io:format("From:[~w], MSG From Ruby:[~p] ~n",
		    [FromPid, Msg]),
	  loop()
    end.

run(Models) ->
    PrivDir = code:priv_dir(?APP),
    Command = io_lib:format("export RUBYLIB=~s/;ruby -S ~s",
			    [PrivDir, Models]),
    F = fun () -> os:cmd(Command) end,
    spawn(F),
    Pid = spawn(fun loop/0),
    register(my_pid, Pid).


