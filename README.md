# Overview

erlyThing is a decentral, distributed message based thing plattform. 

## Features

* decentral system. 
* distributed system which communicate over messages
* starting and stopping of things during runtime
* configuration of messages which a thing understands
* dynamic handling of config files (things.config and messages.config)
* Integration of 3rd party applications is simple.(see cuberl) 
* Generic way of handling rules (see funrunner)
* very easy DSL (Prototype)
* Logging. Every node writes their own logs. You can have a look throw the logging ui,
* internal monitoring system:
	* webbased application explorer
	* webbased process explorer
	* list of all nodes in a network (alive or dead)
	* systen information of a node

## Applications

erlyThing consists of the following core applications:

* sue app for finding nodes and a interface for etop, sys_info, memory and others
* moni ui for sue based on web machine and erlydtl
* horst the runtime for the things
* leni ui for horst 

extra application

* cuberl is an integration app for the [Max! Cube](http://www.elv.de/max-cube-lan-gateway.html) product.

# Installation and starting

To install erlyThing on your machine, do the following steps :

Before you install erlyTHing on your machine, you have to install an [erlang R15](https://www.erlang-solutions.com/downloads/download-erlang-otp) runtime. 
After installing the runtime, you have to install [rebar](https://github.com/basho/rebar) on your machine. rebar is the build tool for erlang.

* [erlang R1503](https://www.erlang-solutions.com/downloads/download-erlang-otp) It also works with R16
* [rebar](https://github.com/basho/rebar) the build tool for erlang


```bash
$ git clone https://github.com/ulfa/erlything 
$ cd erlything
$ make
$ ./dev.sh
```

When erlyThing is up and running you can check it with : [moni](http://localhost:8000) and if you want to see your 
deployed things you can use : [leni](http://localhost:8080) In leni you can navigate to the System Logger and you will find 
the message that the system is started. You will also receive messages when a thing is stopped and when the thing is started.

# Installation on a raspberry pi

After you did the steps as described in the previous section you also have to do :

```bash
make rcswitch send
```


## Dependencies


## 3rd party libraries

* [lager](https://github.com/basho/lager) a logging framework
* [gpio](https://github.com/Feuerlabs/gpio) sysfs GPIO port driver and erlang module
* [erlcron](https://github.com/erlware/erlcron.git) Erlang cronish system
* [wiringpi](https://projects.drogon.net/raspberry-pi/wiringpi/) a GPIO access library written in C for the BCM2835
* [Adafruit_DHT](https://github.com/adafruit/Adafruit-Raspberry-Pi-Python-Code/tree/master/Adafruit_DHT_Driver)
* [rc-switch](https://code.google.com/p/rc-switch/) library to operate with 315/433 MHZ remote control devices
* [streamer](http://manpages.ubuntu.com/manpages/gutsy/man1/streamer.1.html) record audio and/or video 
* and others

# Supported Sensors

* [DHT22](http://www.amazon.de/Sensors-Temperature-SEN-10167-Feuchte-und-Temperatur-Sensor/dp/B005A9KJ4I) temperature-humidity sensor
* [hc_sr501](http://www.elecfreaks.com/wiki/index.php?title=PIR_Motion_Sensor_Module:DYP-ME003) PIR motion sensor
* [TK0460](http://www.aliexpress.com/snapshot/247141960.html) 433Mhz RF Transmitter
* [Max! Cube](http://www.elv.de/max-cube-lan-gateway.html) a heating system

I will add more in the future...

# Configuration

## Things.config

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
|Type        | Value                       |Description
|------------|-----------------------------|--------------------------------------------------------------
|thing       | Name of the thing           | must be unique in a node
|id			| The id of a thing			  | If the id is missing, the system will take "default". 
|ets	      | true/false | should we use an ets or the thing state (for more see the documentation)
|icon        | "/images/" and the name of the image.| The image must be placed in the apps/horst/priv/images/.  
|type        | sensor / actor              | type of thing (actor or sensor)
|driver      | {Module, Function}, []      | the list contains driver specific paramter
|activ       | true / false                | the thing will only be started if activ = true
|timer       | int                         | if the driver needs timmer triggering, you must spec it here
|database    | []                          | database parameter
|descripition| String                      | a description

### Messages.config

This configuartion file contains the drivers and the messages which they understand

```erlang
{{dht22_display_driver, "default"}, [{<<"horst@raspberrypi">>,<<"dht22_driver">>, <<"default">>}]}.
```

You can read the line as : 
The thing with its implementation dht22_display_driver and id "default" will listen to messages which comes from the node "horst@raspberrypi",
thing with an implementation module  dht22_driver and id (version) "default".

Note: listen means that the thing will receive the message and if he can handle the message, it will process the message. 

### Things which are implemented (in a very early version)

The following things (driver) are an example of how to implement a thing.
These things are used in my internal project and they work for me.

* mail_client driver sends a mail 
* usb_cam_driver makes pictures 
* seven_eleven_driver play the seven eleven melody 
* message_count_driver stores 20 messages and the overall count of messages
* cron_driver a cron thing. 
* dht22_driver the sensor thing which triggers the dht22 sensor
* dht22_display_driver stores 20 values of the dht22 
* transmitter_433_driver sends a signal via the transmitter to a switch
* boxcar_driver is an actor which sends messages to a [boxcar client](https://boxcar.io)
* cube_driver integrates the [Max! Cube](http://www.elv.de/max-cube-lan-gateway.html) 
* funge_driver is an application which informs you if you run in trouble with fungi
* os_driver takes data of your system like cpu temp and load and sends them to the os_display_driver
* funrunner is generic way to create rules for consuming and sending messages 
* sample_driver is an example 
* reloader_driver is for development. Everytime you make an update of modules the reloader will load them into the system.
* boxcar_driver sends messages to your boxcar account
* cuberl is an application which integrates the Max! Cube from eq-3
* fungi_driver is an application which warns you if one of your rooms is fungi  endangered.
* node_driver gets informed when a node is down or gets started
* mqttc_driver can send messages to http://www.mqttdashboard.com/


