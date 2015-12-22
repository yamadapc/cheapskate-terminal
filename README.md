# cheapskate-terminal
Render markdown on the terminal. Ports `msee` from **Node.js** using the
`cheapskate` markdown parser. Exposes a library and a command-line tool for
doing so.

## Basic highlighting
![](/demo.gif)

## Installing
```
$ git clone git@github.com:yamadapc/cheapskate-terminal
$ stack install ./cheapskate-terminal
```

## Library Usage
The library exports:
```haskell
prettyPrint :: Doc -> IO ()
```

Example usage:
```haskell
import           Cheapskate
import           Cheapskate.Terminal
import qualified Data.Text.IO        as Text (readFile)

main = do
    tc <- Text.readFile "test-file"
    prettyPrint (markdown def tc)
```

## Haskell Source code highlighting
![](/demo2.gif)

## Pygments highlighting
![](/demo3.gif)

## Performance considerations
It's a complete implementation, but it's very reasonably inefficient; since when
printing to the terminal (even with multiple megabytes of data), most of the
time is spent printing - not parsing/manipulating - it's roughly as efficient as
the **Node.js** implementation; however, I did a bad job with this... Without
terminal wrapping it was about 10-20% faster, with terminal wrapping it's doing
stupid conversions and a lot of copying.

It'll be battle-tested soon, but it's enough to say that it serves it's
purpouse. For the actual usecase of pretty-printing markdown to the terminal
(not 20MB files for benchmarking), it's actually much faster than JavaScript
because it doesn't need to boot an interpreter or load a bunch of code.

If you can write a wrapping function that operates over a `Text.Builder` you'll
make it a whole lot faster (challenge accepted?). This actually took me quite a
bit to get working, ANSI escape sequences get in the way of wrapping text, since
they mess-up with your notion of length, unless you have a fancy abstraction or
an escape sequence parser.

I tried implementing this with `ansi-terminal-builder`, but eventually settled
for dropping windows support. There's a branch with the stash at that point.

## License
This code is published under the MIT License.
