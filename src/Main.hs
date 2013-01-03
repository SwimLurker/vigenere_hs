module Main where



main::IO()
main = do 
       args <- getArgs
       if (length args) /= 3 then printUsage 
       else 
        case args[0] of "-e" -> show (encrypt args[1] args[2])
                        "-d" -> show (decrypt args[1] args[2])
                        _ -> printUsage
printUsage :: IO()                        
printUsage = show "Usage: main -e <srouce string> <cipher> //Encrypt source string with the cipher by vigenere algorithm;"
             show "       main -d <encrypted string> <cipher> //Decrypt encrypted string with the cipher by vigenere algorithm;"

encrypt :: String -> String -> String
encrypt srcStr cipher = encryptInternal srcStr cipher cipher

decrypt :: String -> String -> String
decrypt encStr cipher = decryptInternal encStr cipher cipher

encryptInternal :: String -> String -> String -> String
encryptInternal ([] _ _) = []
encryptInternal (srcStr [] cipher) = encryptInternal srcStr cipher cipher
encryptInternal (sc:ss cipherRemain@cc:cs cipher)
        |isLetter sc = encryptChar((toUpper sc),(toUpper cc)):(encryptInternal ss cs cipher)
        |otherwise = ' ':(encryptInternal ss cipherRemain cipher)
  
decryptInternal :: String -> String -> String -> String
decryptInternal ([] _ _) = []
decryptInternal (encStr [] cipher) = encryptInternal encStr cipher cipher
decryptInternal (ec:es cipherRemain@cc:cs cipher)
        |isLetter ec = decryptChar((toUpper ec),(toUpper cc)):(decryptInternal es cs cipher)
        |otherwise = ' ':(decryptInternal es cipherRemain cipher)
              
encryptChar :: (Char, Char) -> Char
encryptChar (' ', _) = ' '
encryptChar (sc, cc) = (((sc - 'A') + (cc - 'A')) mod 26) + 'A'
       
decryptChar :: (Char, Char) -> Char
decryptChar (' ', _) = ' '
decryptChar (ec, cc) = (((ec + 26) - cc) mod 26) + 'A'