# taskwarrior

[![Hackage](https://img.shields.io/hackage/v/taskwarrior.svg)](https://hackage.haskell.org/package/taskwarrior)
[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fmaralorn%2Fhaskell-taskwarrior%2Fbadge%3Fref%3Dmaster)](https://actions-badge.atrox.dev/maralorn/haskell-taskwarrior/goto?ref=master)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/taskwarrior.svg)](http://packdeps.haskellers.com/reverse/taskwarrior) 

## About

[Taskwarrior](https://taskwarrior.org) is a feature rich command-line task management tool.

This Haskell library contains

* a data type to represent a taskwarrior task
* Aeson instances to deserialize and serialize a task according to the taskwarrior import/export specifications
* IO actions to load and manipulate tasks by calling the `task` command. (Since this is the encouraged way to library design by the taskwarrior developers.)

## Usage

Install `taskwarrior` from hackage. Have a look at `Taskwarrior.IO.getTasks` to get started.

## Contributions

Any form of issue reports, general feedback or suggestions and of course code contributions are highly welcome.
