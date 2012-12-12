module SequenceOperators 
	


where

import Converter
import Data.Digest.SHA2
import Data.List
--import Data.Map.Lazy as Map






instance Eq Gene where
	x == y = geneSeqEq x y


geneSeqEq :: Gene -> Gene -> Bool
geneSeqEq x y = (nucSequence x) == (nucSequence y)


--Use Monad for all of this? especially since could hold seen sequences and hashes


instance Num Gene where
	(+) = geneSeqAdd
	(-) = geneSeqDiff
	--Still need to handle correctly
	(*) = geneSeqMult -- Could do repeat expansion?
	abs = geneSeqAbs  -- Could use for getting rid of '-' chars?
	fromInteger = fromInteger
	signum = signum


--This is mainly for aligning sequence reads of length <50
{- Steps:
			1) Compute Map of hashes
			2) Probablistic decision of which direction to slide first
			3.a) Slide one direction
			3.b) Slide other direction
			3.c) Try to match in middle (test if this could be computationally less expensive to do first)
				-this can be tested for immediately by using isInfixOf
			4) Overlay sequences

-}
geneSeqAdd :: Gene -> Gene -> Gene
--geneSeqAdd a b = let m = generateMap a  --need a direction here (0=align on front, -1=align on back, >0 align in middle)
geneSeqAdd = geneSeqAdd


data AlignmentResult = AlignmentResult {  resultString :: String
										, amountSim :: Int
									   } deriving (Show, Eq)

frontOverlay::String->String->Int->AlignmentResult
frontOverlay s1 s2 i = 	if i<length s2
						then 	if isPrefixOf a s1
								then AlignmentResult {resultString=(b++s1), amountSim=(length a)}
								else frontOverlay s1 s2 (i+1)
						else AlignmentResult {resultString=(s1++s2), amountSim=0}
					where (b,a)=splitAt i s2
--Map - k=hash value, v=DNA seq

rearOverlay::String->String->Int->AlignmentResult
rearOverlay s1 s2 i = 	if i>0
						then	if isSuffixOf b s1
								then AlignmentResult {resultString=(s1++a), amountSim=(length b)}
								else rearOverlay s1 s2 (i-1)
						else AlignmentResult {resultString=(s1++s2), amountSim=0}
					where (b,a)=splitAt i s2

--generateMap :: Gene -> Integer -> Map String String
--generateMap a d = fromList $ makeTuples d $ nucSequence a


--makeTuples :: Integer -> [Char] -> [(String,String)]
--makeTuples d ss= if d==0
--					then let b = map reverse $ reverse $ init $ tails $ reverse ss--add so could align from front
--							in [(getHash x, x) | x <- b]
--					else let b = reverse $ init $ tails ss
--						 	in [(getHash x, x) | x <- b] --add so could align from back


{-
	Cases:
		1) Seq1 fits inside Seq2
		2) Seq1 fits on front of Seq2
		3) Seq1 fits on back of Seq2

-}


geneSeqDiff :: Gene -> Gene -> Gene
geneSeqDiff = geneSeqDiff


geneSeqMult :: Gene -> Gene -> Gene
geneSeqMult = geneSeqMult


geneSeqAbs :: Gene -> Gene
geneSeqAbs= geneSeqAbs

