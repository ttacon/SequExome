--BioAlignTest.hs

import Bio.Alignment.AAlign
import Bio.Alignment.AlignData
import Bio.Alignment.Matrices
import Bio.Sequence.SeqData
import Bio.Alignment.AlignData (Chr)
import qualified Data.Map as M
import Data.Char (ord)

genMatrix :: [((Char,Char),Int)] -> M.Map (Chr,Chr) Int
genMatrix = M.fromList . map toW8
    where toW8 ((x,y),i) = ((fromIntegral $ ord x, fromIntegral $ ord y),i)



test= let (a,e)=global_align (nucI) (-100,-100000) (castToNuc (Seq (fromStr "header") (fromStr "AAAAATTTTT") (Nothing)))
													   (castToNuc (Seq (fromStr "header") (fromStr "AAAAATTTTG") (Nothing)))
			in putStrLn $ showalign e

test1= let (a,e)=global_align (nucI) (-100,-100000) (Seq (fromStr "header") (fromStr "AAAAATTTTT") (Nothing)) 
														  (Seq (fromStr "header") (fromStr "TTTT") (Nothing))
			in putStrLn $ showalign e

test2= let (a,e)= global_align (nucI) (-100,-100000) (Seq (fromStr "header") (fromStr "AAAAATTTTT") (Nothing)) 
														  	   (Seq (fromStr "header") (fromStr "AATT") (Nothing))
			in putStrLn $ showalign e

test3= let (a,e)= global_align (nucI) (-1000,-100000) (Seq (fromStr "header") (fromStr "AATT") (Nothing))
														  (Seq (fromStr "header") (fromStr "AAAAATTTTG") (Nothing))
			in putStrLn $ showalign e





nucI :: (Chr,Chr) -> Int
nucI m = M.findWithDefault (-11) m $ genMatrix [
			(('A','A'),1),(('A','T'),-100000000),(('A','C'),-100000000),(('A','G'),-100000000),
			(('T','A'),-100000000),(('T','T'),1),(('T','C'),-100000000),(('T','G'),-100000000),
			(('C','A'),-100000000),(('C','T'),-100000000),(('C','C'),1),(('C','G'),-100000000),
			(('G','A'),-100000000),(('G','T'),-100000000),(('G','C'),-100000000),(('G','G'),1)]
			