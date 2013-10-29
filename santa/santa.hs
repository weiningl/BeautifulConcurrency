import System.Random
import Control.Concurrent
import Control.Concurrent.STM

data Group = MkGroup Int (TVar (Int, Gate, Gate))
newGroup :: Int -> IO Group
newGroup n =
  atomically (do g1 <- newGate n
                 g2 <- newGate n
                 tv <- newTVar (n, g1, g2)
                 return $ MkGroup n tv)

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) =
  atomically (do (nLeft, g1, g2) <- readTVar tv
                 check (nLeft > 0)
                 writeTVar tv (nLeft - 1, g1, g2)
                 return (g1, g2)
             )

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (nLeft, g1, g2) <- readTVar tv
  check (nLeft == 0)
  ng1 <- newGate n
  ng2 <- newGate n
  writeTVar tv (n, ng1, ng2)
  return (g1, g2)

data Gate = MkGate Int (TVar Int)
newGate :: Int -> STM Gate
newGate n = do
  tv <- newTVar 0
  return $ MkGate n tv

passGate :: Gate -> IO ()
passGate (MkGate n tv) =
  atomically (do nLeft <- readTVar tv
                 check $ nLeft > 0
                 writeTVar tv (nLeft - 1))

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
  atomically (writeTVar tv n)
  atomically (do nLeft <- readTVar tv
                 check $ nLeft == 0)

elf1, reindeer1 :: Group -> Int -> IO ()
elf1 group id = helper1 group (meetInStudy id)
reindeer1 group id = helper1 group (deliverToys id)

helper1 :: Group -> IO () -> IO ()
helper1 group doTask = do
  (inGate, outGate) <- joinGroup group
  passGate inGate
  doTask
  passGate outGate

forever :: IO () -> IO ()
forever act = do act
                 forever act

elf :: Group -> Int -> IO ThreadId
elf group id = forkIO (forever (do elf1 group id
                                   randomDelay))

reindeer :: Group -> Int -> IO ThreadId
reindeer group id = forkIO (forever (do reindeer1 group id
                                        randomDelay))

randomDelay = do
  waitTime <- getStdRandom (randomR (1, 5000000))
  threadDelay waitTime

meetInStudy :: Int -> IO ()
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study.\n")

deliverToys :: Int -> IO ()
deliverToys id = putStr ("Reindeer" ++ show id ++ " delivering toys.\n")

santa :: Group -> Group -> IO ()
santa elfs reindeers = do
  putStrLn "--------"
  (task, (inGate, outGate)) <- atomically (
    (chooseGroup reindeers "deliver toys") `orElse`
    (chooseGroup elfs "meet in my study")
    )
  putStrLn $ "Ho! Ho! Ho! Let's " ++ task
  operateGate inGate
  operateGate outGate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate, Gate))
    chooseGroup gp task = do gates <- awaitGroup gp
                             return (task, gates)


main = do
  elfGroup <- newGroup 3
  sequence_ [elf elfGroup n | n <- [1..10]]
  reindeerGroup <- newGroup 9
  sequence_ [reindeer reindeerGroup n | n <- [1..9]]

  forever $ santa elfGroup reindeerGroup
