# cheapskate-terminal
Render markdown on the terminal. Ports `msee` from **Node.js** using the
`cheapskate` markdown parser. Exposes a library and a command-line tool for
doing so.

It's a complete implementation except for terminal width wrapping. Will be
battle-tested soon.

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

## License
This code is published under the MIT License.
