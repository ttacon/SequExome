module FASTA 	
where

	{-
	GeneSequence
	SequenceInfo
	SequenceString
	-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos (newPos)
import Data.List
import qualified System.Time
import FastaSequenceTestData
import Converter
import Data.Either
import System.IO.Unsafe


-----------------------------------------------------------------------------------------------------------------------------

type FastaParser a = GenParser Char () a


getGenes :: String ->[Gene]
getGenes s = let a=parseFastaFileToGenes s in unsafePerformIO a

parseFastaFileToGenes a=do  b <- readFile a
                            --putStrLn b
                            s <- return $  parseFastaFile b
                            case s of
                                Left _  -> error ("parse failed => "++a)
                                Right g -> return g



--parseFastaFile :: String -> [Gene]
parseFastaFile a = parse parseGene "(unknown)" a

--testSeq :: [Char] -> Either ParseError [Gene]
testSeq a = parse parseGene "(unknown)" a


parseGene ::  FastaParser [Gene]
parseGene = try (do many newline
                    gI <- parseGeneInfo
                    gL <- many1 parseGeneLine
                    -- merge gL's into one?
                    moreGenes <- parseGene
                    return $ [Gene {geneInfo=gI,nucSequence=(concat gL)}]++moreGenes
            )
            <|> do  many newline
                    gI <- parseGeneInfo
                    gL <- many1 parseGeneLine
                    --newline
                    return $ [Gene {geneInfo=gI,nucSequence=(concat gL)}]


parseGeneInfo :: FastaParser String
parseGeneInfo = do  char '>'
                    i <- many1 (noneOf "\n")
                    newline
                    return i

parseGeneLine = do  i <- many1 (noneOf "\n>")
                    newline
                    return i

-----------------------------------------------------------------------------------------------------------------------------
--Writing to file

writeToFastaFile :: [Gene] -> String -> IO ()
writeToFastaFile gs fN = writeFile fN gs'
    where gs' = genesToFastaString gs

genesToFastaString :: [Gene] -> String
genesToFastaString (x:[]) = ">"++(geneInfo x)++"\n"++(wrappedSequence $ nucSequence x)++"\n"
genesToFastaString (x:xs) = ">"++(geneInfo x)++"\n"++(wrappedSequence $ nucSequence x)++"\n" ++ (genesToFastaString xs)

wrappedSequence :: String -> String
wrappedSequence s = if length s<70
                    then s
                    else (take 70 s)++"\n"++(wrappedSequence $ drop 70 s)

















