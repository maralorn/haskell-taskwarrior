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

This example prints the description of (at the most) 5 pending tasks.
```haskell
import Taskwarrior.IO (getTasks)
import Taskwarrior.Task as Task

main :: IO ()
main = do
  tasks <- getTasks ["+PENDING", "limit:5"]
  print $ Task.description <$> tasks
```

## Contributions

Any form of issue reports, general feedback, feature requests or suggestions and of course code contributions are highly welcome.

Also I'd be curious to know what you use this library for.

This project uses brittany in default configuration as code formatter.
The tests on github will check for hlints, missing docs and unapplied formatting.

## Help & Contact

You can always open an issue on GitHub. You can also ask in `#haskell-taskwarrior` on freenode irc. If you don‘t have an irc client you can log in via [the webchat](https://webchat.freenode.net/#haskell-taskwarrior).
Shooting @maralorn a mail is also an option. But of course that won’t be public and therefore not help anyone else.
