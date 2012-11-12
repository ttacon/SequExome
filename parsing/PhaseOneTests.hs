module PhaseOneTests

where


import FASTA (parseGene)
import Converter
import FastaSequenceTestData
import ConverterTestData
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos (newPos)



fastaGene a = parse parseGene "(unknown)" a
