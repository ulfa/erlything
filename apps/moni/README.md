# Overview

This application is the graphical interface for sue. 
If you want to know more about sue, please look here : https://github.com/ulfa/sue

## Feature

* display all nodes in a network with the same cookie
* etop 
* appmon
* graphical memory display

# Installation and starting

To install the application on your machine, please do the following steps :

```bash
$ git clone https://github.com/ulfa/moni 
$ cd moni
$ make all
$ ./dev.sh
```
** Note: ** 

For development i use the dev.sh script. In this script i start the app toolbar which i can't start on the servers.
On my test servers i use the prod.sh script which doesn't start the tools, only the needed applications. 

Type in your browser http://localhost:8000

## Dependencies

Before you can install this appliction on your machine, you have to install an [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp) runtime. 
After installing the runtime, you have to install [rebar] (https://github.com/basho/rebar) on your machine. rebar is the build tool for erlang.

* [erlang R15] (https://www.erlang-solutions.com/downloads/download-erlang-otp)
* [rebar] (https://github.com/basho/rebar) the build tool for erlang

## 3rd party libraries

* [mochiweb] (https://github.com/mochi/mochiweb) a HTTP server
* [webmachine](https://github.com/basho/webmachine) REST toolkit
* [erlydtl](https://github.com/evanmiller/erlydtl) template engine
* [jsx](https://github.com/talentdeficit/jsx) json library
* [lager](https://github.com/basho/lager) a logging framework

# Bugs

* the release is not working. The application only runs with the dev.sh script.
