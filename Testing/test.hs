import Control.Applicative
import Data.List
import Data.SelfML
import Numeric
import System.CPUTime
import System.Directory
import System.IO
import Text.Regex.Base
import Text.Regex.TDFA

main = do dcs <- getDirectoryContents "Testing/Testcases"
          (tests,t) <- timeIO $ mapM (test . ("Testing/Testcases/" ++)) (dcs \\ [".", ".."])
          case length $ filter (== False) tests of
            0 -> putStrLn $ "** " ++ green "all tests passed."        ++ " total: " ++ showFFloat (Just 3) t " ms."
            n -> putStrLn $ "** " ++ red   (show n ++ " failure(s).") ++ " total: " ++ showFFloat (Just 3) t " ms."

test fn =
  do putStr $ concat ["Testing ", fn, "... "]
     hFlush stdout
     i <- readFile fn
     (_,_,_,m) <- (fn =~~ "^(.*Testing/)Testcases/(.+)$") :: IO (String,String,String,[String])
     o <- readFile $ head m ++ "Outputs/output." ++ last m
     (ro,to) <- time $ prettyPrintSML <$> readSML i
     (rd,td) <- time $ prettyPrintSML <$> (readSML . prettyPrintSML =<< readSML i)
     case (ro,rd) of
       (Left err,_) ->
         do putStrLn $ red "parse error!"
            hPutStrLn stderr . red $ show err
            return False
       (_,Left err) ->
         do putStrLn $ red "mirror parse error!"
            hPutStrLn stderr . red $ show err
            return False
       (Right s, Right dbl)
         | s == dbl && s ++ "\n" == o ->
           do putStrLn $ green ("pass.") ++ " (" ++ showFFloat (Just 3) (to+td) " ms)"
              return True
         | s == dbl ->
           do putStrLn $ red "failed expected output test!"
              return False
         | s ++ "\n" == o ->
           do putStrLn $ red "failed mirror test!"
              return False
         | otherwise ->
           do putStrLn $ red "failed expected output test & mirror test!"
              return False
  
time c = timeIO (return c)

timeIO c =
  do t1 <- getCPUTime
     r  <- c
     return $! r -- ensure total evaluation
     t2 <- getCPUTime
     return (r, fromIntegral (t2-t1)/10^9)

attrib a s = "\ESC[" ++ a ++ "m" ++ s ++ "\ESC[0m"

red        = attrib "31"
green      = attrib "32"