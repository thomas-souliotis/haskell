module Task4 where

import Prelude as Pr
import Crypto.Hash
import Data.ByteString.Char8
import Data.Char
import Data.ByteString.Base58 (encodeBase58, decodeBase58, bitcoinAlphabet)
import Data.ByteString.Base16
import Data.Maybe

-- | 4.2.1

-- | the function that produces the hash
sha256 :: ByteString -> Digest SHA256
sha256 = hash

-- | the charlist from witch the valid mini private key should be consisted of
charlist :: String
charlist = "23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz?"

-- | the function that validates given a string that is supposed to be a mini private key
validator :: String -> String
validator inp =
  case Pr.length inp of 30 -> if check inp  then
                                              if checkHash inp then Pr.map toUpper $ show (sha256(pack inp))
                                                               else "Not valid-hash not correct"
                                            else "Not valid, input string containg non-valid chars"
                        _  -> "Not valid, wrong length of input"

-- | checks if the input string is consisted of chars from charlist
check :: String -> Bool
check  = Pr.foldr (\x -> (&&) (x `Pr.elem` charlist)) True

-- | checks if 2 first bits of hash are 00
checkHash :: String -> Bool
checkHash inp =
  let
    xs = show (sha256(pack (inp++['?'])))
    x = Pr.head xs
    x' = Pr.head (Pr.tail xs)
  in
    (x =='0') && (x'=='0')

-- | Correct example for 4.2.1
ex :: String
ex = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"

-- | Not correct- wrong number of input chars
ex1 :: String
ex1 = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrR"

-- | Not correct- containing incorrect char
ex2 :: String
ex2 = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrR1"

-- | Not correct- hash not correct
ex3 :: String
ex3 = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRm"

-- | 4.4.2

-- | walletEnc is the function that produces the WIF given the private key
input :: String -> String -> String
input x inp =
  case x of "0" -> "80" ++ inp
            "1" -> "80" ++ inp ++"01"
            _ -> error "Wrong input given."

output :: String -> String -> String
output x inp =
  case x of "0" -> Pr.drop 2 $ Pr.reverse $ Pr.drop 8 (Pr.reverse inp)
            "1" -> Pr.drop 2 $ Pr.reverse $ Pr.drop 10 (Pr.reverse inp)
            _ -> error "Wrong input given."

walletEnc :: String -> String -> String
walletEnc i inp =
  let
    extended = show $ sha256 $ toHex (input i inp)
    ext' =show $ sha256(toHex extended)
    checksum = Pr.take 8 ext'
    result = toBase58 $ toHex ((input i inp) ++ checksum)
  in
    Pr.init $ Pr.tail $ show result --cut the extra chars


-- | walletDec is the function that produces the private key given the WIF
walletDec :: String -> String -> String
walletDec i inp =
  let
    first  = fromHex $ fromBase58 inp
    first' = Pr.init $ Pr.tail  first --takes away the extra chars given by the above transformation
    second = output i first'
  in
     Pr.map toUpper second

-- | converts input string to hex_bytestring so as to perform right hash in walletEnc/Dec
toHex :: String -> ByteString
toHex x  = fst $ decode $ pack x

-- | reverse of toHex
fromHex :: ByteString -> String
fromHex x = show $  encode x

-- | converts input bytestring to the base58 bytestring
toBase58 :: ByteString -> ByteString
toBase58  =  encodeBase58 bitcoinAlphabet

-- | opposite of to Base58 but gets the input as string (the pack is performed in WalletDec here)
fromBase58 ::  String -> ByteString
fromBase58 x = fromMaybe (error "wrong input") (decodeBase58 bitcoinAlphabet (pack x))


-- | official test for 4.4.2 all cases
test :: Bool
test = walletDec "0" ( walletEnc "0" ex4 )== ex4

-- | test for both correct 4.2.1, 4.2.2
test2 :: Bool
test2 = walletDec "0" (walletEnc "0" (validator ex)) == validator ex

-- | A simple random private key
ex4 :: String
ex4 = "3A47FB70E4B779D1713F6C6835C9F46D868A4E8911241EC6ACE8134272B6FE79"

-- | Another random key
ex5 :: String
ex5 = "380EF9D452F4F81A70E1C776A81CBA7E48B60E6C87CABF612604EC815FA8A758"