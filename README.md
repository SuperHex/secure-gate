# secure-gate

## Build

**Dependencies**

+ Stack
+ ZeroMQ-4.x

After installing dependencies listed above, it's easy to build the project with stack:

``` bash
cd /path/to/secure-gate
stack build
```

Stack may takes a while to install proper version GHC and packages. 

## Run the Program

After successfully build the project, you can run the program like this:

``` bash
cd /path/to/secure-gate
stack exec -- secure-gate-exe [server | client] ["some string" | -n integer]
# example:
#   (Alice) stack exec -- secure-gate-exe server -n 114
#   (Bob)   stack exec -- secure-gate-exe client -n 514
```

The default `Main.hs` will running ZeroMQ channels over "tcp://127.0.0.1:1145". You can
change this setting easily by overwriting the line in `Main.hs` (TODO: using config files).

