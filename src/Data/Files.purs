module Data.FileSystem where
import Prelude
import Data.Path
import Data.Array
import Data.Maybe(Maybe(..))
import Control.MonadZero

allFiles:: Path->Array Path
allFiles file = file:concatMap allFiles (ls file)

onlyFiles:: Path->Array Path
onlyFiles file = filter (\x-> not isDirectory x) $ allFiles file

onlyDirs:: Path->Array Path
onlyDirs file = filter (\x-> isDirectory x) $ allFiles file



findComp :: (Maybe Int->Maybe Int->Boolean)->Path->Maybe Path
findComp op = foldl comp Nothing <<< onlyFiles
  where
    comp (Just x) y | op (size x)  (size y) = Just x
                    | otherwise           = Just y
    comp Nothing y = Just y

findSmallest = findComp (<)
findLargest = findComp  (>)

whereIs :: String->Path->Array Path
whereIs name rootDir = do
  dir <- onlyDirs rootDir
  file <- ls dir
  guard $ filename(file) == name
  pure dir 

