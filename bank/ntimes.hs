import System.IO

ntimes :: Int -> IO () -> IO ()
ntimes 0 _ = return ()
ntimes n act = do
  act
  ntimes (n-1) act

main = ntimes 10 (hPutStrLn stdout "Hello!")
