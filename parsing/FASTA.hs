module FASTA (
	
	parseGene

	) where

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




type FastaParser a = GenParser Char () a






parseFastaFile :: String -> [Gene]
parseFastaFile = parseFastaFile

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













