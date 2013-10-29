import System.IO
import Control.Concurrent

main = do
  forkIO (hPutStr stdout "Hello, ")
  hPutStr stdout "World!"
