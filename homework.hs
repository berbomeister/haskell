{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ()
import System.Directory ( renameFile ) 
--import qualified System.IO.Strict as SIO 
data Question = Question{
                        vupros :: String ,
                        ansYes :: Question,
                        ansNo :: Question}
                | Animal {animal :: String} deriving(Show ,Eq )


initQuestion' :: Question
initQuestion' = Question {vupros = "ima li hobot?", ansYes = Animal "slon", ansNo = Question {vupros = "golqmo li e?", ansYes = Question {vupros = "zemnovodno li e?", ansYes = Animal "aligator", ansNo = Animal "krava"}, ansNo = Question {vupros = "lae li?", ansYes = Animal "kuche", ansNo = Animal "kotka"}}}


writeQuestion :: Question -> String
writeQuestion q@Question{} = "$" ++ vupros q ++ "\n" ++ writeQuestion  (ansYes q) ++ writeQuestion (ansNo q)
writeQuestion q@Animal{} = "*" ++ animal q ++ "\n"


niz :: [String]
niz = lines $ writeQuestion initQuestion'


subtreeleft' :: [String] -> Int -> [String]
subtreeleft' (x:xs) num
  |num == 0 = []
  |head x == '$' = x : subtreeleft' xs (num + 1)
  |head x == '*' = x : subtreeleft' xs (num - 1)
  |otherwise = x : subtreeleft' xs num
subtreeleft :: [String] -> [String]
subtreeleft a = subtreeleft' (tail a) 1


subtreeright' :: [String] -> [String] -> [String]
subtreeright' x left
  |null left = x
  |otherwise = subtreeright' (tail x) (tail left)
subtreeright :: [String] -> [String]
subtreeright x = subtreeright' (tail x) $ subtreeleft x


readQuestion :: [String] -> Question
readQuestion [x] = Animal{animal = tail x}
readQuestion str@(x:_) 
  | head x == '$' = Question{vupros = tail x,ansYes = readQuestion $ subtreeleft str,ansNo = readQuestion $ subtreeright str}
  | head x == '*' = Animal{animal = tail x}

askQuestion :: String  -> IO Bool 
askQuestion question = do   putStrLn question
                            let loop = do   answer <- getLine
                                            case answer of
                                                "da" -> return True
                                                "ne" -> return False
                                                _ -> loop
                            loop



game :: Question -> IO Question
game question = do asking <- play question
                   answer <- askQuestion "Oshte edna igra?"
                   if answer then game asking else return asking

  

start :: IO ()
start = do  contents <- readFile filepath
            let tree = readQuestion $ lines contents
            newTree <- game tree
            writeFile filepath' $ writeQuestion newTree
            renameFile filepath' filepath


main :: IO ()
main = do start

play :: Question -> IO Question
play (Animal animal) = do answer <- askQuestion $ animal ++ " li e?"
                          if answer then do putStrLn "Pechelq." 
                                            return (Animal animal)
                                    else getSmart animal
play node@Question{} = do   answer <- askQuestion $ vupros node
                            next <- play $ if answer then ansYes node else ansNo node
                            return $ if answer then node{ansYes = next} else node{ansNo = next}


getSmart :: String  -> IO Question
getSmart oldAnimal = do putStrLn "Pechelish. Za koe jivotno si mislish?"
                        newAnimal <- getLine
                        putStrLn $ "S kakvo " ++ oldAnimal ++ " se razlichava ot " ++ newAnimal ++ "?"
                        question <- getLine 
                        answer <- askQuestion $ "kakuv e pravilniqt otgovor za " ++ newAnimal ++ "?"
                        let (yes,no) = if answer then (newAnimal,oldAnimal) else (oldAnimal, newAnimal)
                        return $ Question{vupros= question, ansYes = Animal yes, ansNo = Animal no}

filepath :: String
filepath = "oldquestiontree.txt"
filepath' :: String
filepath' = "newquestiontree.txt"
