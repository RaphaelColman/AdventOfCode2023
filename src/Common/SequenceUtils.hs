module Common.SequenceUtils where

import           Data.Sequence as S

-- | Returns a subsequence of a given sequence.
-- Parameters: the sequence, the start index, the length of the subsequence.
subSequence :: S.Seq a -> Int -> Int -> S.Seq a
subSequence s start length = S.take length $ S.drop (start - 1) s
