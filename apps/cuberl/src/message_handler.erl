%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 01.12.2013
-module(message_handler).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([handle_message/1]).

handle_message(<<"H:", Rest/binary>> = Message) ->
	Msg = message_decoder:decode(Message);

handle_message(<<"M:", Rest/binary>> = Message) ->
	[{rooms, Rooms}, {devices, Devices}] = message_decoder:decode(Message),
	[house:add_room(Room) || Room <- Rooms],
	[house_sup:start_device(Device) || {room_id, Room_id, Device} <- Devices],
	[room:add_device(Room_id, Device)  || {room_id, Room_id, Device} <- Devices];
handle_message(<<"L:", Rest/binary>> = Message) ->
	Msg = message_decoder:decode(Message),
	lager:info("l-data : ~p", [Msg]),
	[device:set_l_data(RF_address, L_data) || {rf_address, RF_address, L_data} <- Msg];
handle_message(<<"C:", Rest/binary>> = Message) ->
	Msg = message_decoder:decode(Message),
	[device:set_c_data(RF_address, C_data) || {rf_address, RF_address, C_data} <- Msg];
handle_message(Message) ->
	lager:warning("What kind of message is it? ~p", [Message]),
	ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
