

data Dict = Mt | Entry String String Dict
  deriving (Show, Eq)


type DB = [[Int]]



dict1 :: Dict
dict1 =
  Entry "a" "ab" (Entry "k" "ba" (Entry "b" "ka" (Entry "z" "ba" Mt)))

dict2 :: Dict
dict2 =
  Entry "a" "ab" (Entry "b" "ka" (Entry "u" "uu" Mt))


member :: String -> Dict -> Bool
member x Mt = False
member x (Entry y _ d) | x == y = True
member x (Entry _ _ d) = member x d








main :: IO ()
main = do

  putStrLn "member"
  print $ member "a" dict1

  print $ words "hey there"
  print $ unwords ["hey", "there"]
