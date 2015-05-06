# Building instructions

- Have GHC-7.8 or newer
- Have cabal-install
- Have SDL 2.0.3 with library headers
  - If you don't have it, see [this instructions][build_sdl]
- Get git version of `sdl2` haskell bindings:

  ```bash
  zudov@x200 ~/demo $ git clone "https://github.com/haskell-game/sdl2"
  Cloning into 'sdl2'...
  ...
  zudov@x200 ~/demo $ cabal sandbox init
  Writing a default package environment file to
  /home/zudov/demo/cabal.sandbox.config
  Creating a new sandbox at /home/zudov/demo/.cabal-sandbox
  zudov@x200 ~/demo $ cabal sandbox add-source sdl2/
  zudov@x200 ~/demo $ cabal install sdl2
  Notice: installing into a sandbox located at /home/zudov/demo/.cabal-sandbox
  Configuring sdl2-2.0.0...
  Building sdl2-2.0.0...
  Installed sdl2-2.0.0
  ```

- Now you can finally build and run the demo:

  ```bash
  zudov@x200 ~/demo $ git clone "https://github.com/helsinki-frp/yampy-cube"
  Cloning into 'yampy-cube'...
  ...
  zudov@x200 ~/demo $ cd yampy-cube/
  zudov@x200 ~/demo/yampy-cube $ cabal sandbox init --sandbox ../.cabal-sandbox/
  Writing a default package environment file to
  /home/zudov/demo/yampy-cube/cabal.sandbox.config
  Using an existing sandbox located at /home/zudov/demo/.cabal-sandbox
  zudov@x200 ~/demo/yampy-cube $ cabal install
  Notice: installing into a sandbox located at /home/zudov/demo/.cabal-sandbox
  Configuring yampy-cube-0.1.0.0...
  Building yampy-cube-0.1.0.0...
  Installed yampy-cube-0.1.0.0
  zudov@x200 ~/demo/yampy-cube $ cabal run
  ```

[build_sdl]: https://github.com/haskell-game/sdl2#building
