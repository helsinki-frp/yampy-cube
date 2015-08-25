# Building instructions

- Have [stack](https://github.com/commercialhaskell/stack)
- Have SDL 2.0.3 with library headers
  - If you don't have it, see [this instructions][build_sdl]
- Clone the repo:

  ```shell
  zudov@x200 ~/demo $ git clone "https://github.com/helsinki-frp/yampy-cube"
  Cloning into 'yampy-cube'...
  ...
  ```
- Build it with `stack build`:

  ```shell
  zudov@x200 ~/demo/ $ cd yampy-cube
  zudov@x200 ~/demo/yampy-cube $ stack build
  ...
  ```
- Run it:

  ```shell
  zudov@x200 ~/demo/yampy-cube $ stack exec yampy-cube
  ```

[build_sdl]: https://github.com/haskell-game/sdl2#building
