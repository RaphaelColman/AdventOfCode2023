module Common.MaybeUtils where
import Data.Foldable (find)
import Data.Maybe (isJust, catMaybes)
import Control.Monad (join)

loopMaybe :: (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
loopMaybe step shouldFinish state
  | shouldFinish state = pure state
  | otherwise = step state >>= loopMaybe step shouldFinish

firstJust :: [Maybe a] -> Maybe a
firstJust maybes = join (find isJust maybes)
