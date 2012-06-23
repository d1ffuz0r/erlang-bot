-module(bot_wrk).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Settings

-define(USERNAME, "username").
-define(JSERVER, "jabber.ru").
-define(PASSWORD, "password").

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

%% Records

-record(state, {session, name, rooms=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, join/1, rooms/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    MySession = exmpp_session:start(),
    MyJID = exmpp_jid:make(?USERNAME, ?JSERVER, random),
    exmpp_session:auth_basic_digest(MySession, MyJID, ?PASSWORD),
    _StreamId = exmpp_session:connect_TCP(MySession, ?JSERVER, 5222),
    exmpp_session:login(MySession),
    {ok, #state{session=MySession}}.

handle_call(stop, _From, State) ->
    exmpp_component:stop(State#state.session),
    {stop, normal, ok, State};

handle_call(listrooms, _From, State) ->
    [io:format("Room: ~s~n", [Room]) || Room <- State#state.rooms, Room /= []],
    {reply, ok, State};

handle_call({join, Room}, _From, State) ->
    spawn_link(fun() ->
        exmpp_session:send_packet(State#state.session,
            exmpp_stanza:set_recipient(exmpp_presence:available(), Room ++ "/erlbot")
        )
    end),
    io:format("Joined to ~s~n", [Room]),
    {reply, ok, State#state{session = State#state.session,
                            rooms = [State#state.rooms, Room],
                            name = "erlbot:"}};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_info(#received_packet{} = Packet, State) ->
    spawn_link(fun() -> process_received_packet(State, Packet) end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
process_message("ping" = Message, To, State) ->
    io:format("You receiveds: ~s: ~s~n",[To, Message]),
    case string:tokens(To, "/") of
	[Conf, Nick] ->
	    exmpp_session:send_packet(State#state.session,
		exmpp_stanza:set_recipient(exmpp_message:groupchat(
		    erlang:list_to_binary(Nick ++ ": pong")),Conf));
	_ ->
	    ok
    end;
process_message(_Message, _To, _State) ->
    ok.

process_received_packet(#state{name=Name} = State, #received_packet{packet_type=message, raw_packet=Packet}) ->
    From = exmpp_stanza:get_sender(Packet),
    Message = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'body')),
    case string:tokens(Message, " ") of
	[Name, Msg] -> process_message(Msg, erlang:binary_to_list(From), State);
        _ ->           io:format("~s: ~s~n", [From, Message])
    end;

process_received_packet(_State, _Packet) ->
    ok.

stop() ->
    gen_server:call(?MODULE, stop).

join(Room) ->
    gen_server:call(?MODULE, {join, Room}).

rooms() ->
    gen_server:call(?MODULE, listrooms).
