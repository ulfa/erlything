# Overview

This is a kind of sensor / actor plattform. 

## Feature

* hosting of things
* starting and stopping of things via config file
* configuration of messages which a thing understands
* dynamic handling of config files (things.config and messages.config)
* first version of the funrunner which is able to store function and run them if needed (more later)

# Installation and starting

To install the application on your machine, please do the following steps :

```bash
$ git clone https://github.com/ulfa/horst 
$ cd horst
$ make all
$ ./dev.sh
```

## Dependencies

Before you can install this appliction on your machine, you have to install an [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp) runtime. 
After installing the runtime, you have to install [rebar] (https://github.com/basho/rebar) on your machine. rebar is the build tool for erlang.

* [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp)
* [rebar] (https://github.com/basho/rebar) the build tool for erlang

## 3rd party libraries

* [lager](https://github.com/basho/lager) a logging framework
* [gpio](https://github.com/Feuerlabs/gpio) sysfs GPIO port driver and erlang module
* [wiringpi] (https://projects.drogon.net/raspberry-pi/wiringpi/) a GPIO access library written in C for the BCM2835
* [Adafruit_DHT] (https://github.com/adafruit/Adafruit-Raspberry-Pi-Python-Code/tree/master/Adafruit_DHT_Driver)
* [rc-switch] (https://code.google.com/p/rc-switch/) library to operate with 315/433 MHZ remote control devices

# Supported Sensors

* [DHT22] (http://www.amazon.de/Sensors-Temperature-SEN-10167-Feuchte-und-Temperatur-Sensor/dp/B005A9KJ4I) temperature-humidity sensor
* [hc_sr501] (http://www.elecfreaks.com/wiki/index.php?title=PIR_Motion_Sensor_Module:DYP-ME003) PIR motion sensor
* [TK0460] (http://www.aliexpress.com/snapshot/247141960.html) 433Mhz RF Transmitter And Receiver 

I will add more in the future...

## Configuration

### Things.config

What is a thing?

A thing can be an actor or a sensor. In my world, an actor is something which understands 
specific messages, which a described in the messages.config.
A sensor is something, which sends a specific message in the network. If there is an actor,
which understands the message, then something will happen. ...maybe

This configuration file contains the things, which you want to use in your env. 

```erlang
{thing, "Temperatur_Sensor",
	[
	{type, sensor},	 
	{driver, {dht22_driver, call_sensor}, []}, 
	{activ, false},
	{timer, 0},
	{database, []},
	{description, "Temp sensor in my office"}
	]}.
```
Type        | Value                       |Description
------------|-----------------------------|--------------------------------------------------------------
thing       | Name of the thing           | must be unique in a node
type        | sensor / actor              | type of thing (actor or sensor)
driver      | {Module, Function}, []      | the list contains driver specific paramter
activ       | true / false                | the thing will only be started if activ = true
timer       | int                         | if the driver needs timmer triggering, you must spec it here
database    | []                          | database parameter
descripition| String                      | a description

### Messages.config

This configuartion file contains the drivers and the messages which they understand

```erlang
{dht22_display_driver, [{<<"horst@raspberrypi">>,<<"dht22_driver">>, <<"0">>}]}.
```

The driver dht22_display_driver which is implemented in module dht22_display_driver.erl is
able to understand messages from node : horst@raspberrypi and generarted by module  dht22_driver.
The "0" stands for an unique id which is not implemented yet.
