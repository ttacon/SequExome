module Converter
where

import ConverterTestData
import Data.List


data Gene = Gene 	{ geneInfo :: String
					, nucSequence :: String
					}

instance Show Gene where
  show g="Gene Name:\t"++(geneInfo g)++"\nGene sequence:\n"
          ++ geneShowFormat (nucSequence g)


geneShowFormat :: String -> String
geneShowFormat [] = []
geneShowFormat s =  if length s < 40
                    then insertSpaces s
                    else (insertSpaces a)++geneShowFormat b
                      where (a,b)= splitAt 40 s

insertSpaces :: String -> String
insertSpaces s = a++" "++b++" "++c++" "++d++"\n"
  where (a,e)=splitAt 10 s
        (b,f)=splitAt 10 e
        (c,d)=splitAt 10 f

data Protein = Protein {   proteinInfo :: String
                         , aaSequence :: [AA]
                       } deriving (Eq)

instance Show Protein where
  show p="Protein name:\t"++(proteinInfo p)++"\nProtein Sequence:\n"
          ++ proteinShowFormat (aaSequence p)

proteinShowFormat :: [AA] -> String
proteinShowFormat [] = []
proteinShowFormat s  =  if length s < 20
                        then insertSpacesForProteins s
                        else (insertSpacesForProteins a) ++ proteinShowFormat b
                          where (a,b) = splitAt 20 s

insertSpacesForProteins :: [AA] -> String
insertSpacesForProteins s = a++" "++b++" "++c++" "++d++"\n"
  where (a', e)=splitAt 5 s
        (b', f)=splitAt 5 e
        (c', d')=splitAt 5 f
        a=show a'
        b=show b'
        c=show c'
        d=show d'

data AA = Phe | Leu | Ile | Met
        | Val | Ser | Pro | Thr
        | Ala | Tyr | End | His
        | Gln | Asn | Lys | Asp
        | Glu | Cys | Trp | Arg
        | Gly
        deriving (Show, Eq)


toProtein :: Gene -> Protein
toProtein g = let gS = dnaToRNA $ getOrf g
              in let aa = convertToAA gS
                 in Protein {proteinInfo=(geneInfo g), aaSequence=aa}



convertToAA :: String -> [AA]
convertToAA nS@(n1:n2:n3:[]) = if 0 == invalidNucleotide n1 n2 n3
                               then [lookUpAA n1 n2 n3]
                               else error "invalid nucleotide encountered"
convertToAA nS@(n1:n2:n3:ns) = if 0 == invalidNucleotide n1 n2 n3
                               then [lookUpAA n1 n2 n3] ++ convertToAA ns
                               else error "invalid nucleotide encountered"


invalidNucleotide :: Char -> Char -> Char -> Int
invalidNucleotide n1 n2 n3 = if notNuc n1
	                         then 1
	                         else if notNuc n2
	                         	  then 2
	                         	  else if notNuc n3
	                         	  	   then 3
	                         	  	   else 0

notNuc :: Char -> Bool
notNuc 'A' = False
notNuc 'T' = False
notNuc 'G' = False
notNuc 'C' = False
notNuc 'U' = False
notNuc  _  = True

dnaToRNA :: String -> String
dnaToRNA [] = []
dnaToRNA ('A':xs)="A" ++ dnaToRNA xs
dnaToRNA ('T':xs)="U" ++ dnaToRNA xs
dnaToRNA ('G':xs)="G" ++ dnaToRNA xs
dnaToRNA ('C':xs)="C" ++ dnaToRNA xs
dnaToRNA ('U':xs)="U" ++ dnaToRNA xs --only here in the odd case that from a fasta file we get an RNA sequence

isStartCodon :: String -> Bool
isStartCodon ('A':'T':'G':xs) = True
isStartCodon _             = False

isStopCodon :: String -> Bool
isStopCodon ('T':'A':'G':xs) = True
isStopCodon ('T':'A':'A':xs) = True
isStopCodon ('T':'G':'A':xs) = True
isStopCodon _ = False

getOrf :: Gene -> String
getOrf g =  getOrfFromSequence a
  where a=nucSequence g

isDNA :: String -> Bool
isDNA [] = True
isDNA (x:[]) = isDNANuc x
isDNA (x:xs) =  if isDNANuc x
                then isDNA xs
                else False

isDNANuc :: Char -> Bool
isDNANuc 'A' = True
isDNANuc 'T' = True
isDNANuc 'C' = True
isDNANuc 'G' = True
isDNANuc  _  = False

getOrfFromSequence :: String -> String
getOrfFromSequence s = ss0 ++ (readRestOfOrf ss2)
  where   p=getSeqSplitForConverter s 3 0;
          d=[(y,x) | (y,x)<-p, isStartCodon x];
          (b,a)=head d;
          (ss1,ss)=splitAt b s;
          (ss0, ss2)=splitAt  3 ss;

getSeqSplitForConverter :: String -> Int -> Int -> [(Int, String)]
getSeqSplitForConverter s@(x:xs) l i= if length s <l
          then []
          else let (b,a)=splitAt l s
              in [(i, b)] ++ getSeqSplitForConverter xs l (i+1)

readRestOfOrf :: String -> String
readRestOfOrf s = if length s <3
                  then ""
                  else if isStopCodon s
                        then  if length s==3
                              then s
                              else take 3 s
                        else (take 3 s) ++ readRestOfOrf (drop 3 s)


-- change to use actual lookup function?
lookUpAA :: Char -> Char -> Char -> AA
lookUpAA 'U' 'U' 'U' = Phe
lookUpAA 'U' 'U' 'C' = Phe
lookUpAA 'U' 'U'  _  = Leu
lookUpAA 'C' 'U'  _  = Leu
lookUpAA 'A' 'U' 'G' = Met
lookUpAA 'A' 'U'  _  = Ile
lookUpAA 'G' 'U'  _  = Val
lookUpAA 'U' 'C'  _  = Ser
lookUpAA 'A' 'G' 'U' = Ser
lookUpAA 'A' 'G' 'C' = Ser
lookUpAA 'A' 'G'  _  = Arg
lookUpAA 'C' 'G'  _  = Arg
lookUpAA 'C' 'C'  _  = Pro
lookUpAA 'A' 'C'  _  = Thr
lookUpAA 'G' 'C'  _  = Ala
lookUpAA 'U' 'A' 'U' = Tyr
lookUpAA 'U' 'A' 'C' = Tyr
lookUpAA 'U' 'A'  _  = End
lookUpAA 'U' 'G' 'A' = End
lookUpAA 'C' 'A' 'U' = His
lookUpAA 'C' 'A' 'C' = His
lookUpAA 'C' 'A'  _  = Gln
lookUpAA 'A' 'A' 'U' = Asn
lookUpAA 'A' 'A' 'C' = Asn
lookUpAA 'A' 'A'  _  = Lys
lookUpAA 'G' 'A' 'U' = Asp
lookUpAA 'G' 'A' 'C' = Asp
lookUpAA 'G' 'A'  _  = Glu
lookUpAA 'U' 'G' 'U' = Cys
lookUpAA 'U' 'G' 'C' = Cys
lookUpAA 'U' 'G' 'G' = Trp
lookUpAA 'G' 'G'  _  = Gly
lookUpAA _ _ _ = error "unknown trinucleotide sequence"

validNucToAALength :: String -> Bool
validNucToAALength s = 0 == mod (length s) 3



complement :: String -> String
complement s = map getComplement s

--assuming we're going DNA -> RNA
getComplement :: Char -> Char
getComplement 'A' = 'U'
getComplement 'T' = 'A'
getComplement 'G' = 'C'
getComplement 'C' = 'G'








