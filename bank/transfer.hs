import System.IO
import Control.Concurrent.STM
import Control.Concurrent

type Account = TVar Int

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amt = do
  bal <- readTVar acc
  check (amt <= 0 || amt <= bal)
  writeTVar acc (bal - amt)

limitedWithdraw2 :: Account -> Account -> Int -> STM ()
limitedWithdraw2 acc1 acc2 amt =
  orElse (limitedWithdraw acc1 amt) (limitedWithdraw acc2 amt)

delayDeposit :: Account -> Int -> IO ()
delayDeposit acc amt = do
  hPutStr stdout "Getting ready to deposit money ... hunting through pockets ... \n"
  threadDelay 3000000
  hPutStr stdout "Ok! Depositing now!\n"
  atomically (do bal <- readTVar acc
                 writeTVar acc (bal + amt))

showAccount :: String -> Account -> IO ()
showAccount name acc = do
  bal <- atomically (readTVar acc)
  hPutStr stdout (name ++ ": $" ++ (show bal) ++ "\n")

main = do
  acc1 <- atomically (newTVar 100)
  acc2 <- atomically (newTVar 100)
  showAccount "Left pocket" acc1
  showAccount "Right pocket" acc2
  forkIO (delayDeposit acc2 1)
  hPutStr stdout "Trying to withdraw money ... \n"
  atomically (limitedWithdraw2 acc1 acc2 101)
  hPutStr stdout "Successfully withdrawed!\n"
  showAccount "Left pocket" acc1
  showAccount "Right pocket" acc2
