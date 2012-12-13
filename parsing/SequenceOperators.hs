module SequenceOperators 
	


where


import Converter
import Data.Digest.SHA2
import Data.List
--import Data.Map.Lazy as Map

import Bio.Alignment.AAlign
import Bio.Alignment.AlignData
import Bio.Alignment.Matrices
import Bio.Sequence.SeqData
import Bio.Alignment.AlignData (Chr)



instance Eq Gene where
	x == y = geneSeqEq x y


geneSeqEq :: Gene -> Gene -> Bool
geneSeqEq x y = (nucSequence x) == (nucSequence y)


--Use Monad for all of this? especially since could hold seen sequences and hashes


instance Num Gene where
	(+) = geneSeqAdd
	(-) = geneSeqDiff
	--Still need to handle correctly
	(*) = geneSeqMultSeq -- Could do repeat expansion?THIS IS NOT LONGER THE CORRECT FUNCTION FOR THIS
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
geneSeqAdd g1 g2 = bestAlignment ar1 ar2 ar3 g1 g2
		where 	s1=nucSequence g1;
				s2=nucSequence g2;
				(s3,s4)=lengthOrder s1 s2;
				ar1=frontOverlay s3 s4 0;
				ar2=rearOverlay s3 s4 (length s4);
				ar3=innerOverlay s3 s4;	

bestAlignment :: AlignmentResult -> AlignmentResult -> AlignmentResult -> Gene -> Gene -> Gene
bestAlignment ar1 ar2 ar3 g1 g2 = buildNewGene ar g1 g2
		where ar=bestAlignmentResult ar1 ar2 ar3

buildNewGene :: AlignmentResult -> Gene -> Gene -> Gene
buildNewGene ar g1 g2 = Gene {geneInfo=((geneInfo g1)++" "++(geneInfo g2)++(show (overlayType ar))), nucSequence=(resultString ar)}

bestAlignmentResult :: AlignmentResult -> AlignmentResult -> AlignmentResult -> AlignmentResult
bestAlignmentResult ar1 ar2 ar3 = 	if (amountSim ar1) > (amountSim ar2) && (amountSim ar1) > (amountSim ar3)
									then ar1
									else 	if (amountSim ar2) > (amountSim ar3)
											then ar2
											else ar3

lengthOrder :: String -> String -> (String,String)
lengthOrder s1 s2 = if (length s1)<(length s2)
					then (s1,s2)
					else (s2,s1)

data OverlayType = FrontOverly | InnerOverlay | RearOverlay deriving (Show, Eq)
data AlignmentResult = AlignmentResult {  resultString :: String
										, amountSim :: Int
										, overlayType :: OverlayType
									   } deriving (Show, Eq)


frontOverlay::String->String->Int->AlignmentResult
frontOverlay s1 s2 i = 	if i<length s2
						then 	if isPrefixOf a s1
								then AlignmentResult {resultString=(b++s1), amountSim=(length a), overlayType=FrontOverly}
								else frontOverlay s1 s2 (i+1)
						else AlignmentResult {resultString=(s1++s2), amountSim=0, overlayType=FrontOverly}
					where (b,a)=splitAt i s2
--Map - k=hash value, v=DNA seq

rearOverlay::String->String->Int->AlignmentResult
rearOverlay s1 s2 i = 	if i>0
						then	if isSuffixOf b s1
								then AlignmentResult {resultString=(s1++a), amountSim=(length b), overlayType=RearOverlay}
								else rearOverlay s1 s2 (i-1)
						else AlignmentResult {resultString=(s1++s2), amountSim=0, overlayType=RearOverlay}
					where (b,a)=splitAt i s2


innerOverlay::String->String->AlignmentResult
innerOverlay s1 s2 = 	if isInfixOf s2 s1
						then AlignmentResult {resultString=s1, amountSim=(length(s2)), overlayType=InnerOverlay}
						else AlignmentResult {resultString=(s1++s2), amountSim=0, overlayType=InnerOverlay}



geneSeqDiff :: Gene -> Gene -> Gene
geneSeqDiff g1 g2 = 	if isInfixOf s2 s1
						then Gene {nucSequence=(removeInnerSeq s1 s2), geneInfo=((geneInfo g1)++" - "++(geneInfo g2))}
						else g1
	where 	s1=nucSequence g1;
			s2=nucSequence g2;

removeInnerSeq :: String -> String -> String
removeInnerSeq s1 s2 = ss1++ss2
	where 	p=getSeqSplit s1 (length s2) 0;
			s=[(y,x) | (y,x)<-p, x==s2];
			(b,a)=head s;
			(ss1,ss)=splitAt b s1;
			(ss0, ss2)=splitAt (b+length s2) s1;

getSeqSplit :: String -> Int -> Int -> [(Int, String)]
getSeqSplit s@(x:xs) l i=	if length s <l
					then []
					else let (b,a)=splitAt l s
							in [(i, b)] ++ getSeqSplit xs l (i+1)



geneSeqMultSeq :: Gene -> Gene -> Gene
geneSeqMultSeq=geneSeqMultSeq


geneSeqMult :: Gene -> Int -> Gene
geneSeqMult g i = Gene {geneInfo=(geneInfo g), nucSequence=s}
	where s = concat $ replicate i (nucSequence g)


geneSeqAbs :: Gene -> Gene
geneSeqAbs= geneSeqAbs

data GeneDifference = S 				--Same
					| D Char        	--Deletion
					| I Char 			--Insertion
					| R Char Char 		--Replacement
					deriving (Show, Eq)

data GeneDifferences = GeneDifferences {	gene1::Gene
										,	gene2::Gene
										,	differences::[GeneDifference]
									   } deriving (Show, Eq)


(-/) ::Gene -> Gene -> GeneDifferences
g1 -/ g2 = getGeneDifferences g1 g2

getGeneDifferences :: Gene -> Gene -> GeneDifferences
getGeneDifferences g1 g2 = constructGeneDifferences s1 s2 g1 g2
	where d=getGlobalAlignment (geneToSeq g1) (geneToSeq g2);
		  s=lines d;
		  (s1, s2)=strListToTuple s;


constructGeneDifferences :: String -> String -> Gene -> Gene -> GeneDifferences
constructGeneDifferences s1 s2 g1 g2=GeneDifferences { gene1=g1, gene2=g2, differences=a}
	where a=determineGeneDifferences s1 s2

determineGeneDifferences :: String -> String -> [GeneDifference]
determineGeneDifferences s1 s2 = zipWith differenceRelation s1 s2

differenceRelation :: Char -> Char -> GeneDifference
differenceRelation a b = 	if a==b
							then S
							else 	if a=='-'
									then I b
									else 	if b=='-'
											then D a
											else R a b

strListToTuple :: [String] -> (String, String)
strListToTuple s = (s!!0, s!!1)

geneToSeq :: Gene -> Sequence t
geneToSeq g = Seq (fromStr $ geneInfo g) (fromStr $ nucSequence g) (Nothing)

getGlobalAlignmentOfGenes :: Gene -> Gene -> String
getGlobalAlignmentOfGenes g1 g2 = getGlobalAlignment s1 s2
	where 	s1=geneToSeq g1;
			s2=geneToSeq g2;

--Ignoring dealing with "Quality" of sequence currently
getGlobalAlignment :: Sequence t -> Sequence t -> String
getGlobalAlignment s1 s2 = showalign e
	where (a,e)=global_align (blastn_default) (-10,-1) (castToNuc s1) (castToNuc s2);


test= let (a,e)=global_align (blastn_default) (-10,-1) (castToNuc (Seq (fromStr "header") (fromStr "AAAAATTTTT") (Nothing)))
													   (castToNuc (Seq (fromStr "header") (fromStr "AAAAATTTTG") (Nothing)))
			in putStrLn $ showalign e


geneFragmentsToGenes :: [String] -> Int-> [Gene]
geneFragmentsToGenes (x:[]) i= [Gene{geneInfo=("fragment "++(show i)), nucSequence=x}]
geneFragmentsToGenes (x:xs) i= [Gene{geneInfo=("fragment "++(show i)), nucSequence=x}] ++ geneFragmentsToGenes xs (i+1)


mergeFragments :: [Gene] -> Gene
mergeFragments g = foldl1 (+) g

getGCContent :: Gene -> Rational
getGCContent g = (toRational gc)/(toRational n)
	where 	a=nucSequence g;
			n=length a;
			gc=sum $ map isGC a;

isGC :: Char -> Int
isGC 'G' = 1
isGC 'C' = 1
isGC  _  = 0

------------------------------------------------------------------------------------------------------------------------------------------
--Testing data

seqFragments1=[	"AAACCAGCCGACTACAT","CTACATTTACGCCATTGAGGCCA","AGGCCACATGGATAGA"
			   ,"GATAGAGCGGAAAACTGGCTGTG","GCTGTGCGCCATGAAACAGACGG","AGACGGACACCAAGCC"
			   ,"CAAGCCCGCACACATAT","ACATATACACCAGCGCTCGAAAA","CGAAAAATAGAAACAAAAGTCGTGTAAGTG"]


seqFragments2=["AAAATTTT", "TTTTCCCC", "CCCCGGGG", "GGGGATAT", "ATATGCGC"]






