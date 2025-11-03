# t4: terminal time tracking tool

**Haskell library and command line tools for simple time tracking.**

Storage uses very human-friendly yaml files in `~/.t4-data` that can be edited manually:

```
memowe@rakete:~$ ls ~/.t4-data
2025-10-15.yml  2025-10-25.yml  2025-10-24.yml
2025-10-23.yml  2025-10-27.yml
```

```yaml
$ cat ~/.t4-data/2025-10-27.yml 
- in:
    category: Writing t4 README
    tags:
    - t4
    - haskell
    - documentation
    time: 2025-10-27 10:24:57
- out:
    time: 2025-10-27 10:29:12
- in:
    category: Lunch break
    tags:
    - break
    - recreation
    - nom
    time: 2025-10-27 12:42:17
```

The project offers two command-line interfaces to edit these files for you.

## Preparations

This is a standard [GHC][ghc]/[cabal][cabal] project with a core [library](lib) and two executables [t4](exe-t4-commands)/[t5](exe-t5-interactive), so you can use the standard cabal commands to build dependencies, the project itself and run tests:

    cabal build --only-dependencies --enable-tests
    cabal build
    cabal test --test-show-details=direct

You can also build the core library's [Haddock API docs][haddock] by yourself (although they are not extensively commented):

    cabal haddock

## Command-based terminal interface `t4`

You can run the command-based tool (*terminal time tracking tool*) without installing using `cabal run t4 -- ARGUMENTS` and install it via `cabal install`.

```
$ t4 --help
t4 - terminal time tracking tool

Usage: t4 COMMAND

  Simple interface for clocking in and out

Available options:
  -h,--help                Show this help text

Available commands:
  in                       Clocking in
  out                      Clocking out
  status                   Show current status
  categories               List all categories
  tags                     List all tags
  report                   Report

$ t4 status
IN (2025-10-27 10:24:57) [Writing t4 README] #t4 #haskell #documentation
```

## Interactive terminal interface `t5`

You can run the interactive terminal interface (*terminal time tracking tool terminal user interface*) without installing using `cabal run t5` and install it via `cabal install`.

```
$ t5
IN (2025-10-27 10:24:57) [Writing t4 README] #t4 #haskell #documentation
Spent: 4mi 5s
[o]ut - [u]pdate - report [c]ategories - report [t]ags - [q]uit: o
Spent: 4mi 15s
OUT (2025-10-27 10:29:12)
```

*Hint*: the interactive TUI uses auto completion when entering categories or tags. Try it using <kbd>Tab</kbd>.

## Author and license

(c) 2025 Mirko Westermeier

Released under the MIT license. See [LICENSE](LICENSE) for details.

[ghc]: https://www.haskell.org/ghc/
[cabal]: https://www.haskell.org/cabal/
[haddock]: http://mirko.westermeier.de/t4/
