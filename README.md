# taskwarrior

[![Hackage Deps](https://img.shields.io/hackage-deps/v/taskwarrior.svg)](http://packdeps.haskellers.com/reverse/taskwarrior)
[![CI](https://github.com/maralorn/haskell-taskwarrior/actions/workflows/haskell.yml/badge.svg)](https://github.com/maralorn/haskell-taskwarrior/actions)
[![Packaging status](https://repology.org/badge/vertical-allrepos/haskell:taskwarrior.svg?columns=3&header=)](https://repology.org/project/haskell:taskwarrior/versions)

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

This project uses fourmolu as code formatter.

## Help & Contact

You can always open an issue on GitHub. Contacting @maralorn:maralorn.de on
matrix is also an option. But of course that won’t be public and therefore not
help anyone else.
