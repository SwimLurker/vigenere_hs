module Main 
( main
) where

import System.Environment
import Data.List
import Data.Char
 
main ::  IO()
main = do
         args <- getArgs
         case args of (command:str:cipher:[]) ->
                          case command of "-e" -> putStrLn (encrypt str cipher)
                                          "-d" -> putStrLn (decrypt str cipher)
                                          _ -> printUsage    
                      _ -> printUsage
                      
printUsage :: IO()                        
printUsage = do 
              putStrLn "Usage: main -e <srouce string> <cipher> //Encrypt source string with the cipher by vigenere algorithm;"
              putStrLn "       main -d <encrypted string> <cipher> //Decrypt encrypted string with the cipher by vigenere algorithm;"

encrypt :: String -> String -> String
encrypt srcStr cipher = encryptInternal srcStr cipher cipher

decrypt :: String -> String -> String
decrypt encStr cipher = decryptInternal encStr cipher cipher

encryptInternal :: String -> String -> String -> String
encryptInternal [] _ _ = []
encryptInternal srcStr [] cipher = encryptInternal srcStr cipher cipher
encryptInternal (sc:ss) cipherRemain@(cc:cs) cipher
        |isLetter sc = (encryptChar ((toUpper sc),(toUpper cc))):(encryptInternal ss cs cipher)
        |otherwise = ' ':(encryptInternal ss cipherRemain cipher)
  
decryptInternal :: String -> String -> String -> String
decryptInternal [] _ _ = []
decryptInternal encStr [] cipher = encryptInternal encStr cipher cipher
decryptInternal (ec:es) cipherRemain@(cc:cs) cipher
        |isLetter ec = (decryptChar ((toUpper ec),(toUpper cc))):(decryptInternal es cs cipher)
        |otherwise = ' ':(decryptInternal es cipherRemain cipher)
              
encryptChar :: (Char, Char) -> Char
encryptChar (' ', _) = ' '
encryptChar (sc, cc) = chr ((((ord sc - ord 'A') + (ord cc - ord 'A')) `mod` 26) + ord 'A')
       
decryptChar :: (Char, Char) -> Char
decryptChar (' ', _) = ' '
decryptChar (ec, cc) = chr ((((ord ec + 26) - (ord cc)) `mod` 26) + (ord 'A'))