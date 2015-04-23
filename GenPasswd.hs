import System.Random
import Data.List
import Control.Monad


chars = ['0'..'9'] ++
        ['a'..'z'] ++
        ['A'..'Z']

pick :: [a] -> IO a
pick xs = randomRIO (0,(length xs)-1) >>= return . (xs !!)

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not.null) . unfoldr (Just . splitAt n)

genPasswd :: Int -> IO String
genPasswd n = fmap (intercalate "-" . chunk 3) $ replicateM n (pick chars)

main :: IO ()
main = replicateM 5 (genPasswd 15) >>= mapM_ putStrLn
