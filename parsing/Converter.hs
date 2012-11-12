module Converter

where

import ConverterTestData


data Gene = Gene 	{ geneInfo :: String
					, nucSequence :: String
					} deriving (Show, Eq)

data Protein = Protein { proteinInfo :: String
                       , aaSequence :: [AA]
                       } deriving (Show, Eq)


data AA = Phe | Leu | Ile | Met
        | Val | Ser | Pro | Thr
        | Ala | Tyr | End | His
        | Gln | Asn | Lys | Asp
        | Glu | Cys | Trp | Arg
        | Gly
        deriving (Show, Eq)


toProtein :: Gene -> Protein
toProtein g = let gS = nucSequence g
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



complement :: String -> String
complement s = map getComplement s

--assuming we're going DNA -> RNA
getComplement :: Char -> Char
getComplement 'A' = 'U'
getComplement 'T' = 'A'
getComplement 'G' = 'C'
getComplement 'C' = 'G'








