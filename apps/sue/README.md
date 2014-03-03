# Overview

An application for connecting nodes together to get informations like the name, ip, state and so.
Also, you have interfaces to etop, sys_info and the memory usage the applications in a node.

After installing this application on a node, it broadcast a alive message and if there are other nodes, they will 
notice the new node. You can ask the application for other nodes and you get the following information:

```erlang
{<<"leni@raspberrypi">>,                %% name of the node
  [{ip,"192.168.178.45"},               %% ip adress of the node
   {state,<<"Alive">>},                 %% current state Alive or Dead
   {time,"2013-07-23 06:40:05"},        %% time of last update of state
   {reason,[]}]},                       %% reason for dead
 {<<"moni@raspberrypi">>,
  [{ip,"192.168.178.45"},
   {state,<<"Alive">>},
   {time,"2013-07-23 06:40:02"},
   {reason,[]}]},
```
I also created a graphical interface for this application. You can find it here : [moni] (https://github.com/ulfa/moni)

## Feature

* finding nodes with the same cookie and tracking the state of the nodes
* interface to etop of a node
* interface to sys_info of a node
* interface to memory of a node
* interface to the applications of a node

# Installation

To install the application on your machine, please do the following steps :

```bash
$ git clone https://github.com/ulfa/sue
$ cd sue
$ make all
$ ./dev.sh
```
** Note: ** 

For development i use the dev.sh script. In this script i start the app toolbar which i can't start on my servers.
On my test servers i use the prod.sh script which doesn't start the tools, only the needed applications. 


# Integration in your own app

To use this application with your own app it is so simple with erlang and rebar.
First you create a new dependency in your rebar.config 

```erlang
{sue, ".*", {git, "git@github.com:ulfa/sue.git", "HEAD"}},
```
Now you have to choices to start sue. 

* starting from your start script with -s sue
* starting from your application with application:start(sue)

## Dependencies

Before you can install this appliction on your machine, you have to install an [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp) runtime. 
After installing the runtime, you have to install [rebar] (https://github.com/basho/rebar) on your machine. rebar is the build tool for erlang.

* [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp)
* [rebar] (https://github.com/basho/rebar) the build tool for erlang

# Bugs