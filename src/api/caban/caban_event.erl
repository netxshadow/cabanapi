%%%-------------------------------------------------------------------
%%% @author Caban
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2022 10:00 AM
%%%-------------------------------------------------------------------
-module(caban_event).
-author("Caban").

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(caban_event_state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @doc Creates an event manager
-spec(start_link() -> {ok, pid()} | {error, {already_started, pid()}}).
start_link() ->
  gen_event:start_link({local, ?SERVER}).

%% @doc Adds an event handler
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).
add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
-spec(init(InitArgs :: term()) ->
  {ok, State :: #caban_event_state{}} |
  {ok, State :: #caban_event_state{}, hibernate} |
  {error, Reason :: term()}).
init([]) ->
  {ok, #caban_event_state{}}.

%% @private
%% @doc Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
-spec(handle_event(Event :: term(), State :: #caban_event_state{}) ->
  {ok, NewState :: #caban_event_state{}} |
  {ok, NewState :: #caban_event_state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #caban_event_state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).
handle_event(_Event, State = #caban_event_state{}) ->
  {ok, State}.

%% @private
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
-spec(handle_call(Request :: term(), State :: #caban_event_state{}) ->
  {ok, Reply :: term(), NewState :: #caban_event_state{}} |
  {ok, Reply :: term(), NewState :: #caban_event_state{}, hibernate} |
  {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #caban_event_state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  {remove_handler, Reply :: term()}).
handle_call(_Request, State = #caban_event_state{}) ->
  Reply = ok,
  {ok, Reply, State}.

%% @private
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
-spec(handle_info(Info :: term(), State :: #caban_event_state{}) ->
  {ok, NewState :: #caban_event_state{}} |
  {ok, NewState :: #caban_event_state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #caban_event_state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).
handle_info(_Info, State = #caban_event_state{}) ->
  {ok, State}.

%% @private
%% @doc Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
remove_handler | {error, {'EXIT', Reason :: term()}} |
{error, term()}), State :: term()) -> term()).
terminate(_Arg, _State = #caban_event_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #caban_event_state{},
    Extra :: term()) ->
  {ok, NewState :: #caban_event_state{}}).
code_change(_OldVsn, State = #caban_event_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


