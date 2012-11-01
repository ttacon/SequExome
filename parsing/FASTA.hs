module FASTA (
	Sequence
	SequenceInfo
	SequenceString

	) where

import Data.List
import qualified System.Time



parseFastaFile :: String -> [Sequence]
parseFastaFile = parseFastaFile