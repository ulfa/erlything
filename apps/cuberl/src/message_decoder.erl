%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 16.11.2013
%%% -------------------------------------------------------------------
-module(message_decoder).
-define(BYTE, 1/little-signed-integer-unit:8).
-define(RF_ADDRESS, 3/little-unsigned-integer-unit:8).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(values, [value/2, value/3]).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([decode/1]).

%%	  IEQ0123456  Serial number
%%    00b3b4      RF address, hexadecimal
%%    0102        Firmware version: 258, Other values 010e = 270,  Other values 010e = version 270,
%%                0111 = version 273
%%    00000000    ?
%%    355df98a    Connection IP
%%    03          Duty cycle in %. 03 (hex) means 3(dec)  %. Other collected values after
%%                sending some commands to cube: 1a = 26(dec) in  %, 1d(hex) = 29(dec) %, 2a = 42(dec)  %
%%    32          ?
%%    0c0a1f      Systemdate: 0C = Year (Year 2000), 0A = Month, 1F = Day --> 2012.10.31
%%    0837        Systemtime: 08(hex) = 8 hours, 37(hex) = 55 minutes --> 08:55
%%    03          ? Could be the HW-Revision of Cube
%%    0000        ?
decode(<<"H:", Rest/binary>> = Message) ->
	lager:debug("H - Message : ~p", [Message]),
	M_1 =  binary:replace(Rest, <<"\r\n">>, <<>>, []),
	M_2 = binary:split(M_1, <<",">>, [global]),
	Fields = [serial, rf_address, firmware, unknown_1, connection_ip, cycle, unknown_2, system_date, system_time, hw_revision, unknown_3],
	[decode(Step, Entry) || {Step, Entry} <- lists:zip(Fields, M_2)];
decode(<<"M:", Unknown:6/binary, Rest/binary>> = Message) ->
	lager:debug("M - Message : ~p", [Message]),
	{Rooms, Devices_encoded} = decode_room(base64:decode(Rest)),
	Devices = decode_devices(Devices_encoded),
	[{rooms, Rooms}, {devices, Devices}];
decode(<<"C:",RF_address:6/binary, "," ,Rest/binary>> = Message) ->
	lager:info("C - Message : ~p", [Message]),	
	Messages = binary:split(Message, <<"\r\n">>, [global]), 
	[decode_c_m(<<"C:", RF_address/binary, ",", (base64:decode(Body))/binary>>) || <<"C:",RF_address:6/binary, "," , Body/binary>> <- Messages];
decode(<<"L:", Rest/binary>> = Message) ->
	lager:debug("L - Message : ~p", [Message]),
	decode_l(base64:decode(Rest), []);
decode(Message) ->
	lager:info("unknown Message : ~p", [Message]).

%%VgIBAgpXb2huemltbWVyCyORAgELI5FLRVEwNTU4NjU1EFRoZXJtb3N0YXQgbGlua3MCAQlCUktFUTA1MjUxMDMRVGhlcm1vc3RhdCByZWNodHMCAQ==\r\n
%% Before we analyze this list, we have to decode it, because it is base64 encoded.
%% After this, we can't handle the first to binaries, so we ignore them.
%% We need the Room_count, because we have to iterate over the room list
%%
%%Description        		Startpos    Length      Example Value
%%=====================================================================
%%Room id            		00          1           1
%%Room name length   		01          1           0A
%%Room name          		02          variable    Hobbykamer
%%masterDeviceRadioAddress  3           003508
decode_room(<<Unkown1:1/binary, Unkown2:1/binary, Room_count:?BYTE, Rest/binary>>=Message)->
	lager:info("Room_count : ~p", [Room_count])	,
	decode_room(Room_count, 0, Rest, []).
	
decode_room(Room_count, Room_count, Rest, Rooms) ->
	{Rooms, Rest};

decode_room(Room_count, Count, <<Room_id:?BYTE, 
								 Room_name_length:?BYTE, 
								 Room_name:Room_name_length/binary, 
								 Room_address:?RF_ADDRESS, 
								 Rest/binary>>, Acc) ->
	Room = [{room_id, Room_id}, {room_name, decode(room_name, Room_name)}, {room_address, Room_address},{room_name_length, Room_name_length}],
	decode_room(Room_count, Count + 1, Rest, [Room|Acc]).

%%Description        Startpos    Length      Example Value
%%=====================================================================
%%Device type        00          1           1
%%Address            01          3           003508
%%Serial Number      04          10          IEQ0109125
%%Name length        0E          1           0C
%%Name               0F          variable    Thermostat 1
%%Room id                        1           01
decode_devices(<<Device_count:?BYTE, Rest/binary>>) ->
	decode_device(Device_count, 0, Rest, []).

decode_device(Device_count, Device_count, Rest, Acc) ->
	Acc;	
decode_device(Device_count, Count, <<Device_type:?BYTE, 
									RF_Address:?RF_ADDRESS, 
									Serial:10/binary, 
									Device_name_length:?BYTE, 
									Device_name:Device_name_length/binary, 
									Room_id:?BYTE, 
									Rest/binary>>, Acc) ->
	Device = {room_id, Room_id,[{device_type, Device_type}, {serial, Serial}, {device_name, Device_name}, {rf_address, RF_Address}]},
	decode_device(Device_count, Count + 1, Rest, [Device|Acc]).

%% L - Message
%% Start Length  Value       Description
%% ==================================================================
%% 0          1  0B          Length of data: 0B = 11(decimal) = 11 bytes
%% 1          3  003508      RF address
%% 4          1  00          ?
%% 5          1  12          bit 4     Valid              0=invalid;1=information provided is valid
%%                           bit 3     Error              0=no; 1=Error occurred
%%                           bit 2     Answer             0=an answer to a command,1=not an answer to a command
%%                           bit 1     Status initialized 0=not initialized, 1=yes
%%                                
%%                           12  = 00010010b
%%                               = Valid, Initialized
%%                           
%%6        1     1A          bit 7     Battery       1=Low
%%                           bit 6     Linkstatus    0=OK,1=error
%%                           bit 5     Panel         0=unlocked,1=locked
%%                           bit 4     Gateway       0=unknown,1=known
%%                           bit 3     DST setting   0=inactive,1=active
%%                           bit 2     Not used
%%                           bit 1,0   Mode         00=auto/week schedule
%%                                                  01=Manual
%%                                                  10=Vacation
%%                                                  11=Boost   
%%                           1A  = 00011010b
%%                               = Battery OK, Linkstatus OK, Panel unlocked, Gateway known, DST active, Mode Vacation.
%%
%% 7       1     20          Valve position in %
%% 8       1     2C          Temperature setpoint, 2Ch = 44d; 44/2=22 deg. C
%% 9       2     858B        Date until (05-09-2011) (see Encoding/Decoding date/time)
%% B       1     2E          Time until (23:00) (see Encoding/Decoding date/time)
decode_l(<<>>, Acc) ->
	Acc;
decode_l(<<11,
		   RF_address:?RF_ADDRESS, 
		   Unknown:1/binary, 
		   Status_1:1/binary, 
		   Status_2:1/binary, 
		   Value:?BYTE, 
		   Temp:?BYTE, 
		   Date:2/binary, 
		   Time:1/binary,	
		   Rest/binary>>, Acc) ->
	<<U4:1, U3:1,U2:1, Valid:1, Error:1, Answer:1, State:1, U1:1>> = Status_1, 
	<<Battery:1, Linkstatus:1, Panel:1, Gateway:1, Dst:1, U:1, Mode_1:1, Mode_0:1>> = Status_2,
	Result = {rf_address, RF_address, [{rf_address, RF_address},{temp, Temp/2}, {value, Value}, {date, Date}, {time, Time},
							 {error, value(error,Error)}, {answer,value(answer,Answer)},{state,value(state,State)}, 
							 {battery, value(battery, Battery)}, {linkstatus, value(linkstatus,Linkstatus)}, 
							 {panel, value(panel,Panel)}, {gateway, value(gateway,Gateway)}, {dst, value(dst,Dst)},
							 {mode, value(mode, Mode_1, Mode_0)}]},
	decode_l(Rest, [Result|Acc]);

decode_l(<<12,
		   RF_address:?RF_ADDRESS, 
		   Unknown:1/binary, 
		   Status_1:1/binary, 
		   Status_2:1/binary, 
		   Value:?BYTE, 
		   Temp:?BYTE, 
		   Date:2/binary, 
		   Time:1/binary,
		   Temp1/integer,	
		   Rest/binary>>, Acc) ->
	<<U4:1, U3:1,U2:1, Valid:1, Error:1, Answer:1, State:1, U1:1>> = Status_1, 
	<<Battery:1, Linkstatus:1, Panel:1, Gateway:1, Dst:1, U:1, Mode_1:1, Mode_0:1>> = Status_2,
	Result = {rf_address, RF_address, [{rf_address, RF_address}, {temp, Temp / 2.0}, {value, Value}, {date, Date}, {time, Time},
							 {error, value(error,Error)}, {answer,value(answer,Answer)},{state,value(state,State)}, 
							 {battery, value(battery, Battery)}, {linkstatus, value(linkstatus,Linkstatus)},
							 {panel, value(panel,Panel)}, {gateway, value(gateway,Gateway)}, {dst, value(dst,Dst)},
							 {mode, value(mode, Mode_1, Mode_0)}, {temp, Temp1 / 10}]},	
	decode_l(Rest, [Result|Acc]);
%%
%% Handling of the shuttercontact live data
%%
decode_l(<<6,
		   RF_address:?RF_ADDRESS, 
		   Unknown:1/binary, 
		   Status_1:1/binary, 
		   Status_2:1/binary, 
		   Rest/binary>>, Acc) ->
	<<U4:1, U3:1,U2:1, Valid:1, Error:1, Answer:1, State:1, U1:1>> = Status_1, 
	<<Battery:1, Linkstatus:1, Panel:1, Gateway:1, Dst:1, U:1, Window:1, Mode_0:1>> = Status_2,
	Result = {rf_address, RF_address, [{rf_address, RF_address},
							 {error, value(error,Error)}, {answer,value(answer,Answer)},{state,value(state,State)}, 
							 {battery, value(battery, Battery)}, {linkstatus, value(linkstatus,Linkstatus)}, 
							 {panel, value(panel,Panel)}, {gateway, value(gateway,Gateway)}, {dst, value(dst,Dst)},
							 {window, value(window,Window)}]},
	decode_l(Rest, [Result|Acc]).


%% C- Message
%%       Start Length  Value       Description
%%        ==================================================================
%%        00         1  D2          Length of data: D2 = 210(decimal) = 210 bytes
%%        01         3  003508      RF address
%%        04         1  01          Device Type
%%        05         3  0114FF      01 = Room ID
%%                                  14 = Firmware = 20(decimal) = V1.4 or 15 = 21(decimal) = V1.5 for heating thermostates
%%                                  13 = Firmware = 19(decimal) for Shuttercontacts
%%                                  FF = ??
%%        08        10  IEQ0109125  Serial Number       
%%        12         1  28          Comfort Temperature     
%%        13         1  28          Eco Temperature         
%%        14         1  3D          MaxSetPointTemperature
%%        15         1  09          MinSetPointTemperature
%%        16         1  07          Temperature Offset * 2
%%                                  The default value is 3,5, which means the offset = 0 degrees.
%%                                  The offset is adjustable between -3,5 and +3,5 degrees,
%%                                  which results in a value in this response between 0 and 7 (decoded already)       
%%        17         1  28          Window Open Temperature   
%%        18         1  03          Window  Open Duration     
%%        19         1  30          Boost Duration and Boost Valve Value
%%                                  The 3 MSB bits gives the duration, the 5 LSB bits the Valve Value%.
%%                                  Duration: With 3 bits, the possible values (Dec) are 0 to 7, 0 is not used.
%%                                  The duration in Minutes is: if Dec value = 7, then 30 minutes, else Dec value * 5 minutes
%%                                  Valve Value: dec value 5 LSB bits * 5 gives Valve Value in %
%%        1A         1  0C          Decalcification: Day of week and Time
%%                                  In bits: DDDHHHHH
%%                                  The three most significant bits (MSB) are presenting the day, Saturday = 1, Friday = 7
%%                                  The five least significant bits (LSB) are presenting the time (in hours)     
%%        1B         1  FF          Maximum Valve setting; *(100/255) to get in %
%%        1C         1  00          Valve Offset ; *(100/255) to get in %
%%        1D         ?  44 48 ...   Weekly program (see The weekly program)


decode_c_m(<<C:2/binary, 
			 RF_address:6/binary, 
			 _Comma:1/binary, 
			 Length/integer, 
			 Message/binary>>) when Length > byte_size(Message) ->
	S = byte_size(Message),
	lager:info("1 : ~p", [S]),
	lager:info("2 : ~p", [Length]),
	decode_c_m1(Message);
	

decode_c_m(<<C:2/binary, 
			 RF_address:6/binary, 
			 _Comma:1/binary, 
			 Length/integer, 
			 Message:Length/binary>>) ->
	lager:info("~p", [Length]),
	decode_c_m1(Message).


decode_c_m1(<<RF_address:?RF_ADDRESS, 
			  Device_type:?BYTE, 
			  Room_id:?BYTE, 
			  Firmware:?BYTE,
			  Unknown:?BYTE,
			  Serial:10/binary, 
			  Com_temp:?BYTE, 
			  Eco_temp:?BYTE,
			  Max_set_point_temp:?BYTE, 
			  Min_set_point_temp:?BYTE, 
			  Temp_offset:?BYTE, 
			  Window_open_temp:?BYTE, 
			  Window_open_duration:?BYTE,
			  Boost_duration_value:1/binary, 
			  Day_of_week_and_time:1/binary, 
			  Max_value:?BYTE, 
			  Value_offset:?BYTE, 
			  Rest/binary>>) ->

	{rf_address, RF_address, [{device_type, Device_type}, {room_id, Room_id}, {serial, Serial}, {com_temp, Com_temp}, {eco_temp, Eco_temp},
	{max_set_point, Max_set_point_temp}, {min_set_point_temp, Min_set_point_temp},{temp_offset, Temp_offset}, {Window_open_temp, Window_open_temp},
	{window_open_duration, Window_open_duration}, {boost_duration_value, Boost_duration_value}, {day_of_week_and_time, Day_of_week_and_time}, {max_value, Max_value},
	{value_offset, Value_offset}]};

decode_c_m1(<<RF_address:?RF_ADDRESS, 
			  Device_type:?BYTE, 
			  Room_id:?BYTE, 
			  Firmware:?BYTE,
			  Unknown:?BYTE,
			  Serial:10/binary, 
			  Rest/binary>>) ->
	{rf_address, RF_address, [{device_type, Device_type}, {room_id, Room_id}, {serial, Serial}]}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
decode(serial, Bin) ->
	{serial, binary_to_list(Bin)};
decode(rf_address, Bin) ->
	{rf_address, Bin};
decode(firmware, Bin) ->
	{firmware, binary_to_list(Bin)};
decode(unknown_1, Bin) ->
	{unknown_1, binary_to_list(Bin)};
decode(unknown_2, Bin) ->
	{unknown_2, binary_to_list(Bin)};
decode(unknown_3, Bin) ->
	{unknown_3, binary_to_list(Bin)};
decode(connection_ip, Bin) ->
	{connection_ip, binary_to_list(Bin)};
decode(cycle, Bin) ->
	{cycle, bin_to_dez(Bin)};
decode(system_date, Bin) ->
	lager:info("Date : ~p", [Bin]),
	S = binary_to_list(Bin),
	Year = string:sub_string(S, 1, 2),
	Month = string:sub_string(S, 3, 4),
	Day = string:sub_string(S, 5, 7),
	{system_date, {list_to_integer(Year, 16), list_to_integer(Month, 16), list_to_integer(Day,16)}};
decode(system_time, Bin) ->
	T = binary_to_list(Bin),
	Hours =  string:sub_string(T, 1, 2),
	Minutes = string:sub_string(T, 3, 4),
	{system_time, {list_to_integer(Hours, 16), list_to_integer(Minutes, 16)}};
decode(hw_revision, Bin) ->
	{hw_revision, bin_to_dez(Bin)};
decode(room_name, String) ->
	unicode:characters_to_list(String, utf8);
decode(date, Date) ->
	<<M1:3, Day:5, M4:1,U:1, Y:6>> = Date,
	<<Month:4>> = <<M1:3, M4:1>>,
	Year = 2000 + Y,
	lists:concat([Day, ".", Month, ".", Year]);
decode(time, <<Time>>) ->
	T = Time / 2,
	Hour = trunc(T),
	Minute = trunc(T * 60) rem 60,
	lists:flatten(io_lib:format("~2..0w:~2..0w", [Hour,Minute])).
decode(date_time, Date, Time) ->
	lists:concat([decode(date, Date)," ",decode(time,Time)]).

encode(command, Room_id, RF_address, Mode, Temp, Date, Time) ->
	Start = <<00,04,64,00,00,00>>,
	RID = encode(room_id, Room_id),
	RF = encode(rf_address, RF_address),
	Te = encode(temp, Mode, Temp),
	D = encode(date, Date),
	T = encode(time, Time),
	C = base64:encode(<<Start/binary, RF/binary, RID/binary, Te/binary, D/binary, T/binary>>),
	"s:" ++ binary_to_list(C) ++ "\r\n".
encode(temp, Mode, Temp) ->
	<<Mode:2, (Temp * 2):6>>.
encode(date, Date) when is_list(Date) ->
	[D, M, Y] = string:tokens(Date ,"."),
	D1 = list_to_integer(D),
	M1 = list_to_integer(M) ,
	Y1 = list_to_integer(Y) - 2000,
	encode(date, {D1, M1, Y1});
encode(date, {D, M, Y}) ->
	<<Month_1:3, Month_2:1>> = <<M:4>>,
	<<Month_1:3, D:5, Month_2:1, 0:1, Y:6>>;
encode(time, Time) when is_list(Time) ->
	[Hour, Time1] = string:tokens(Time, ":"),
	encode(time,{list_to_integer(Hour), list_to_integer(Time1)});
encode(time, {Hour, Minute}) ->
	Time = trunc((Hour + (Minute / 60)) * 2),
	<<T:8>> = <<Time:8>>;
encode(rf_address,RF_address) when is_integer(RF_address) ->
	<<RF_address:3/little-unsigned-integer-unit:8>>;

encode(room_id, Room_id) when is_list(Room_id) ->
	encode(room_id, list_to_integer(Room_id));
encode(room_id, Room_id) ->
	<<R:8>> = <<Room_id:8>>.

bin_to_dez(Bin) ->
	list_to_integer(binary_to_list(Bin), 16).


%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

decode_date_test() ->
	?assertEqual("29.8.2011", decode(date, <<157,11>>)).
decode_date_time_test() ->
	?assertEqual("29.8.2011 00:00", decode(date_time, <<157,11>>, <<0>>)).

decode_time_test() ->
	?assertEqual("15:30", decode(time, <<31>>)),
	?assertEqual("02:00", decode(time, <<04>>)).

encode_command_test() ->
	?assertEqual("s:AARAAAAAAP4wAaiLix8=\r\n", encode(command, "01", 3210752, 10, 20, "11.9.2011", "15:30")).

encode_date_test() ->
	?assertEqual(<<157,11>>, encode(date, "29.8.2011")),
	?assertEqual(<<139,139>>, encode(date, "11.9.2011")).


encode_time_test() ->
	?assertEqual(<<31>>, encode(time, "15:30")),
	?assertEqual(<<04>>, encode(time, "02:00")).
encode_temp_test() ->
	?assertEqual(<<168>>, encode(temp, 10, 20)).

encode_rf_address_test() ->
	?assertEqual(<<11,35,145>>, encode(rf_address,9511691)).

decode_m_test() ->		  
	M = <<"M:00,01,VgIBAgpXb2huemltbWVyCyORAgELI5FLRVEwNTU4NjU1EFRoZXJtb3N0YXQgbGlua3MCAQlCUktFUTA1MjUxMDMRVGhlcm1vc3RhdCByZWNodHMCAQ==\r\n">>,
	decode(M).

decode_m_1_test() ->
	M = <<"M:00,01,VgICAgpXb2huemltbWVyCyORAQpOw6RoemltbWVyCG1IBAELI5FLRVEwNTU4NjU1EFRoZXJtb3N0YXQgbGlua3MCAQlCUktFUTA1MjUxMDMRVGhlcm1vc3RhdCByZWNodHMCAwePbUtFUTAwNjQ1MzkOV2FuZHRoZXJtb3N0YXQCAQhtSEtFUTA0MDIxODIMVGhlcm1vc3RhdCAxAQE=\r\n">>,
	decode(M).
decode_m_2_test() ->
	M = <<"M:00,01,VgICAgpXb2huemltbWVyCyORAQpOw6RoemltbWVyCG1IBQELI5FLRVEwNTU4NjU1EFRoZXJtb3N0YXQgbGlua3MCAQlCUktFUTA1MjUxMDMRVGhlcm1vc3RhdCByZWNodHMCAwePbUtFUTAwNjQ1MzkOV2FuZHRoZXJtb3N0YXQCAQhtSEtFUTA0MDIxODIMVGhlcm1vc3RhdCAxAQQFOLNKRVEwNDA0NjM3DkZlbnN0ZXJrb250YWt0AgE=\r\n">>,
	decode(M).

decode_l_test() ->
	M = <<"L:CwlCUgkSGAkqAOIACwsjkQkSGA4qAN4A\r\n">>,
	decode(M).

decode_l_1_test() ->
	M = <<"L:CwlCUgkSGAAYAAAACwsjkQkSGAAYAAAADAePbfYSGAQYAAAAyQsIbUhNEhkIIgCoAAYFOLNsEhJ=\r\n">>,
	decode(M).

decode_l_2_test() ->
	M = <<"L:CwlCUgkSGBYsAAAACwsjkQkSGBgsAAAADAePbfYSGAQsAAAA1g==\r\n">>,
	decode(M).

decode_c_message_test() ->
	M = <<"C:003508,0gA1CAEBFP9JRVEwMTA5MTI1KCg9CQcoAzAM/wBESFUIRSBFIEUgRSBFIEUgRSBFIEUgRSBFIERIVQhFIEUgRSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIERIUmxEzFUURSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIA==\r\n">>,
	decode(M).

decode_c_message_1_test() ->
	M = <<"C:094252,0glCUgECGP9LRVEwNTI1MTAzKiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:0b2391,0gsjkQECGP9LRVEwNTU4NjU1KiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\n">>,
	decode(M).

decode_c_message_2_test() ->	
	M =  <<"C:094252,0glCUgECGP9LRVEwNTI1MTAzKiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:0b2391,0gsjkQECGP9LRVEwNTU4NjU1KiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:078f6d,zgePbQMCEABLRVEwMDY0NTM5KiE9CURITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgBxgw\r\n">>,
	decode(M).

decode_c_message_3_test() ->
	M = <<"C:094252,0glCUgECGP9LRVEwNTI1MTAzKiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERUTG9UnVT3RSBFIEUgRSBFIEUgRSBFIEUgREhMbVadVvdFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:0b2391,0gsjkQECGP9LRVEwNTU4NjU1KiEsCQcYAzAM/wBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERUTG9UnVT3RSBFIEUgRSBFIEUgRSBFIEUgREhMbVadVvdFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:078f6d,zgePbQMCEABLRVEwMDY0NTM5KiE9CURITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgREhMbVicWPZFIEUgRSBFIEUgRSBFIEUgRSBESExtWJxY9kUgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgRFRMb1SdVPdFIEUgRSBFIEUgRSBFIEUgRSBESExtVp1W90UgRSBFIEUgRSBFIEUgRSBFIERITG1YnFj2RSBFIEUgRSBFIEUgRSBFIEUgBxgw\r\nC:086d48,0ghtSAEBGP9LRVEwNDAyMTgyKyE9CQcYAzAM/wBESFUIRSBFIEUgRSBFIEUgRSBFIEUgRSBFIERIVQhFIEUgRSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIERIVGxEzFUURSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:0538b3,EQU4swQCEw9KRVEwNDA0NjM3\r\n">>,
	decode(M).

decode_c_message_4_test() ->
	M = <<"C:0538b3,EQU4swQCEw9KRVEwNDA0NjM3\r\nC:086d48,0ghtSAEBGP9LRVEwNDAyMTgyKyE9CQcYAzAM/wBESFUIRSBFIEUgRSBFIEUgRSBFIEUgRSBFIERIVQhFIEUgRSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIERIVGxEzFUURSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:094252,0glCUgECGP9LRVEwNTI1MTAzKiEsCQcYAzAM/wBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:078f6d,zgePbQMCEABLRVEwMDY0NTM5KiE9CURUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgBxgw\r\nC:0b2391,0gsjkQECGP9LRVEwNTU4NjU1KiEsCQcYAzAM/wBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIERUTG5UnVT4RSBFIEUgRSBFIEUgRSBFIEUgRFRMblSdVPhFIEUgRSBFIEUgRSBFIEUgRSBEVExuVJ1U+EUgRSBFIEUgRSBFIEUgRSBFIA==\r\nC:08e1f2,0gjh8gEDGP9LRVEwNjQ3ODc5KyE9CQcYAzAM/wBESFUIRSBFIEUgRSBFIEUgRSBFIEUgRSBFIERIVQhFIEUgRSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIERIVGxEzFUURSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUg">>,
	decode(M).

decode_c_message_5_test() ->
	M = <<"C:0538b3,EQU4swQCEw9KRVEwNDA0NjM3\r\n">>,
	decode(M).
decode_c_message_6_test() ->
	M = <<"C:08e1f2,0gjh8gEDGP9LRVEwNjQ3ODc5KyE9CQcYAzAM/wBESFUIRSBFIEUgRSBFIEUgRSBFIEUgRSBFIERIVQhFIEUgRSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUgRSBESFRsRMxVFEUgRSBFIEUgRSBFIEUgRSBFIERIVGxEzFUURSBFIEUgRSBFIEUgRSBFIEUgREhUbETMVRRFIEUgRSBFIEUgRSBFIEUg">>,

	decode(M).
	
-endif.
