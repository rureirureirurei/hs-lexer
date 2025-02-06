# Lexer ~~and Parser~~

### How to build and run this thing?
This project uses [**Stack**](https://docs.haskellstack.org/en/stable/) build tool.
In order to run the lexer and play arourd, `stack run` should do the job.
The script starts and waits for you to provide the string to lex. 

Example usage:
```
hs-lexer  (main * =) $ stack run
lnp-0.1.0.0: unregistering (local file changes: README.md app/Main.hs)
lnp> build (lib + exe) with ghc-9.8.4
Preprocessing library for lnp-0.1.0.0..
Building library for lnp-0.1.0.0..
Preprocessing executable 'lnp-exe' for lnp-0.1.0.0..
Building executable 'lnp-exe' for lnp-0.1.0.0..
[1 of 2] Compiling Main [Source file changed]
[3 of 3] Linking .stack-work/dist/aarch64-osx/ghc-9.8.4/build/lnp-exe/lnp-exe [Objects changed]
lnp> copy/register
Installing library in /Users/denys/uwr/fp/hs-lexer/.stack-work/install/aarch64-osx/41a62dbbefb4add655838497cb283700a67d7dc0443e8230457d497650b0d8b0/9.8.4/lib/aarch64-osx-ghc-9.8.4/lnp-0.1.0.0-JhE7nd0zcS6GBQ6tbAgYVJ
Installing executable lnp-exe in /Users/denys/uwr/fp/hs-lexer/.stack-work/install/aarch64-osx/41a62dbbefb4add655838497cb283700a67d7dc0443e8230457d497650b0d8b0/9.8.4/bin
Registering library for lnp-0.1.0.0..
a+b-10/20
[ID "a",OP "+",ID "b",OP "-",NUM "10",OP "/",NUM "20"]
hs-lexer  (main * =) $ 
```
* This will not only write the list of tokens, but also generate the `nfa.xdot` file containing the NFA!

There is also a simple testing facility that can be executed with `stack test`.