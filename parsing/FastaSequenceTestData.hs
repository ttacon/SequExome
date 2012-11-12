module FastaSequenceTestData

where


---------------------------------------------------------------------------------------------------------
-- Test sequences
---------------------------------------------------------------------------------------------------------

-- Testing one single gene
seq1=">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n"

-- Testing two genes
seq2=">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n"
      ++">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n"

-- Testing multiple genes with variable # of \n between genes
seq3=">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n"
      ++">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n\n\n"
      ++">gi|5524211|gb|AAD44166.1| cytochrome b [Elephas maximus maximus]\n"
      ++"LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV\n"
      ++"EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG\n"
      ++"LLILILLLLLLALLSPDMLGDPDNHMPADPLNTPLHIKPEWYFLFAYAILRSVPNKLGGVLALFLSIVIL\n"
      ++"GLMPFLHTSKHRSMMLRPLSQALFWTLTMDLLTLTWIGSQPVEYPYTIIGQMASILYFSIILAFLPIAGX\n"
      ++"IENY\n"

-- glued/p150 AA (Homo sapiens)
seq4=">tr|G5E9H4|G5E9H4_HUMAN Dynactin 1 (P150, glued homolog, Drosophila), isoform CRA_a OS=Homo sapiens GN=DCTN1 PE=4 SV=1\n"
     ++"MMRQAPTARKTTTRRPKPTRPASTGVAGASSSLGPSGSASAGELSSSEPSTPAQTPLAAP\n"
     ++"IIPTPVLTSPGAVPPLPSPSKEEEGLRAQVRDLEEKLETLRLKRAEDKAKLKELEKHKIQ\n"
     ++"LEQVQEWKSKMQEQQADLQRRLKEARKEAKEALEAKERYMEEMADTADAIEMATLDKEMA\n"
     ++"EERAESLQQEVEALKERVDELTTDLEILKAEIEEKGSDGAASSYQLKQLEEQNARLKDAL\n"
     ++"VRMRDLSSSEKQEHVKLQKLMEKKNQELEVVRQQRERLQEELSQAESTIDELKEQVDAAL\n"
     ++"GAEEMVEMLTDRNLNLEEKVRELRETVGDLEAMNEMNDELQENARETELELREQLDMAGA\n"
     ++"RVREAQKRVEAAQETVADYQQTIKKYRQLTAHLQDVNRELTNQQEASVERQQQPPPETFD\n"
     ++"FKIKFAETKAHAKAIEMELRQMEVAQANRHMSLLTAFMPDSFLRPGGDHDCVLVLLLMPR\n"
     ++"LICKAELIRKQAQEKFELSENCSERPGLRGAAGEQLSFAAGLVYSLSLLQATLHRYEHAL\n"
     ++"SQCSVDVYKKVGSLYPEMSAHERSLDFLIELLHKDQLDETVNVEPLTKAIKYYQHLYSIH\n"
     ++"LAEQPEDCTMQLADHIKFTQSALDCMSVEVGRLRAFLQGGQEATDIALLLRDLETSCSDI\n"
     ++"RQFCKKIRRRMPGTDAPGIPAALAFGPQVSDTLLDCRKHLTWVVAVLQEVAAAAAQLIAP\n"
     ++"LAENEGLLVAALEELAFKASEQIYGTPSSSPYECLRQSCNILISTMNKLATAMQEGEYDA\n"
     ++"ERPPSKPPPVELRAAALRAEITDAEGLGLKLEDRETVIKELKKSLKIKGEELSEANVRLS\n"
     ++"LLEKKLDSAAKDADERIEKVQTRLEETQALLRKKEKEFEETMDALQADIDQLEAEKAELK\n"
     ++"QRLNSQSKRTIEGLRGPPPSGIATLVSGIAGGAIPGQAPGSVPGPGLVKDSPLLLQQISA\n"
     ++"MRLHISQLQHENSILKGAQMKASLASLPPLHVAKLSHEGPGSELPAGALYRKTSQLLETL\n"
     ++"NQLSTHTHVVDITRTSPAAKSPSAQLMEQVAQLKSLSDTVEKLKDEVLKETVSQRPGATV\n"
     ++"PTDFATFPSSAFLRAKEEQQDDTVYMGKVTFSCAAGFGQRHRLVLTQEQLHQLHSRLIS\n"

-- glued/p150 Nuc (D. melanogaster)
seq5=">FBgn0001108 type=gene; loc=3L:13922487..13927756; ID=FBgn0001108; name=Gl; dbxref=FlyBase:FBan0009206,FlyBase:FBgn0001108,FlyBase_Annotation_IDs:CG9206,GB_protein:AAF49788,GB:AY118377,GB_protein:AAM48406,GB:BH900948,GB:BI216888,GB:CZ487831,GB:J02932,UniProt/Swiss-Prot:P13496,INTERPRO:IPR000938,OrthoDB5_Drosophila:EOG5WM4ND,OrthoDB5_Diptera:EOG57M1HQ,OrthoDB5_Insecta:EOG58W9QB,OrthoDB5_Arthropoda:EOG56WWQ9,OrthoDB5_Metazoa:EOG5ZW81K,EntrezGene:39536,INTERPRO:IPR022157,BDGP_clone:RE24170,InterologFinder:39536,BIOGRID:64877,DroID:FBgn0001108,DRSC:FBgn0001108,FLIGHT:FBgn0001108,FlyAtlas:CG9206-RA,FlyMine:FBgn0001108,GenomeRNAi:39536,modMine:FBgn0001108,REDfly:FBgn0001108; derived_computed_cyto=70C5-70C6%3B Limits computationally determined from genome sequence between @P{lacW}l(3)L5212L5212@%26@P{PZ}l(3)0587105871@ and @P{EP}EP3561EP3561@; gbunit=AE014296; MD5=aecab8682595df4f674e40b9df21144f; length=5270; release=r5.48; species=Dmel;\n"
     ++"CTAGTTCAACCGCACGGTCACATTTAAGACATTGCAAATATTTTAGAGGA\n"
     ++"ACTATTTCGTTGCAATTTTCAGCTGCCTGCAACGGAAATACAGTGCAAAT\n"
     ++"GGTAAGCGCAACGGCGGTGACAAAGAAAGGCACACCTATCCGATTGGAAA\n"
     ++"ACCAGGTAAATACCGAGAATAAGACCAGAACGGGAGTCGCGACAGACTGG\n"
     ++"AGCTACCTGTGCTGCCTGTATGGGTGACTAATGATGTCAACATTTTCTGT\n"
     ++"TTTCTCTCCTCAAGCCGACGGCCATTTTTGGCTAGCTAAGACGTGATTAT\n"
     ++"TGGCCCGAAAGCACAAGAAGTGCAGCGCAGTAAGTAGGCTGGCATTCCGT\n"
     ++"GTCCAGGGAACCCGATAACCACAGCTCCATTCCCAACGAACAGCAATCAT\n"
     ++"AAGTACATCAGTTATACCCACATCCTGGTCTCATACACGCACACAAACAC\n"
     ++"AGCGAGAGCGAGATGTCCGAGAAAAACCTGAAAGTGGGCGCCCGGGTCGA\n"
     ++"GCTGACCGGCAAGGATCTGCTTGGCACGGTTGCCTACGTGGGGATGACCA\n"
     ++"GCTTCGCCGTCGGCAAGTGGGTGGGCGTCGTGCTGGACGAGCCGAAGGGC\n"
     ++"AAAAACAGCGGCTCCATCAAGGGCCAGCAGTACTTCCAGTGCGATGAGAA\n"
     ++"CTGTGGCATGTTTGTGCGACCCACGCAGCTGCGTCTGCTGGAGGCTGCTC\n"
     ++"CTGGCAGCAGGCGCAGCATCGAGGATGTCAGCGGGGCTACGCCCACGGCT\n"
     ++"GCCCAACCCACAAAGGCGCGGCTGAGCAGCTCTCGCACCTCGCTCTCCTC\n"
     ++"CAGTCGCCAATCGCTGCTGGGTTCCCGCACCCAGTTGACCACTTCTCTGA\n"
     ++"GTGAACGCACTGCCTCCAGCAGCAGTATTGGCCCGAGGAAATCTTTGGCG\n"
     ++"CCGCAAAACAGCAAGGATAAGGAGTCCCCCAGCACTTCATTGGCAGAAGG\n"
     ++"AGCCCCAGCAGCAAGCGGTGGCAACGGTGCCGCTTCGCATGCCTCCTCCA\n"
     ++"AACGGGCTTCCTTCGTGGAGACGGGCTTCCTTGAAATTCTTAAGCCGCAG\n"
     ++"TTCACGCCTTCCCAGCCACTGCGATCGCCCTCTTTCACCATGCCCTCCAA\n"
     ++"CTCCGGTGCTGAAGACAAGGTCGCCCTGCTGGAGGCACAGAAAACGAGCG\n"
     ++"CCGAGCTGCAGGCTCAGCTGGCTGATCTCACCGAGAAGCTGGAAACTTTA\n"
     ++"AAGCAGCGCAGGAACGAGGATAAAGAAAGGTTGCGGGAGTTCGACAAGAT\n"
     ++"GAAGATTCAGTTTGAGCAGCTTCAAGAGTTTCGAACGAAAATCATGGGTG\n"
     ++"CTCAGGCTTCGCTTCAGAAGGAGTTACTGCGCGCCAAACAGGAGGCCAAG\n"
     ++"GATGCAATCGAGGCCAAGGAGCAGCATGCTCAGGAAATGGCAGATCTGGC\n"
     ++"AGACAATGTGGAGATGATCACGCTGGACAAGGAAATGGCCGAGGAGAAGG\n"
     ++"CCGACACGCTGCAGCTGGAGCTAGAGTCCTCCAAGGAGCGTATTGAAGAG\n"
     ++"TTGGAGGTAGATCTGGAGCTCTTACGCTCGGAGATGCAAAACAAGGCCGA\n"
     ++"ATCTGCCATCGGAAATATTTCTGGCGGCGGCGATTCGCCGGGCCTCTCTA\n"
     ++"CTTATGAATTCAAACAGCTGGAGCAACAGAACATTCGTTTGAAGGAAACA\n"
     ++"CTAGTGCGTCTGAGGGATCTATCTGCTCACGACAAGCACGACATCCAAAA\n"
     ++"GTTGAGCAAGGAACTGGAGATGAAGCGCTCTGAAGTCACCGAACTGGAGC\n"
     ++"GCACCAAGGAGAAGCTTAGTGCCAAGATTGATGAACTGGAGGCCATAGTC\n"
     ++"GCCGACTTGCAGGTAAGCAATTAAGGATTAGTTTTCAATTCAAGTGCTCA\n"
     ++"CTGCCCACCCTCAATTGGCTTACCCACATTCCGATGAAAAATTTCACGGC\n"
     ++"TTTTCCAGCACCTTATTGGCTTGCATTAGTTTGGCTTTCGTTATACTCTG\n"
     ++"CTTTTCTATATCAGATTAAACTGCCTATTAATAAGAAGATATCGGCTAAA\n"
     ++"AGCTTTATCAACTAGGCAATTTTAACTTATGAACTGTAGGTTTTCCTTGC\n"
     ++"AACGCTTTCTTTAATTGCCTAATTAAGTGCACACTTATTAAGCTGTGTTA\n"
     ++"ACATCTCTAGGCATTTTTAACCGATATCTTTAGTTTACATGTGTAATATG\n"
     ++"TTTTGCATAAGGTCAATCTAAAGATAAACTTTTTCATTGCATTTCTTTTT\n"
     ++"GCCGGCCTGCCTTTAAACCTAATAATTATCGCGGTTCAAGGAGGTAAATT\n"
     ++"GTGTAATTTCAACTTTTCCCCTGCCAAAATTTCAATCATATTCCTGTATA\n"
     ++"GTTTCGTTTTTAAATTAAATTTAAAAACGAAGAAACTAAATATATTTATT\n"
     ++"TTGTCCCAGGAACAAGTCGATGCTGCACTTGGTGCCGAGGAAATGGTGGA\n"
     ++"GCAGCTGGCTGAAAAGAAAATGGAATTGGAAGACAAAGTAAAACTGCTCG\n"
     ++"AGGAGGAAATTGCCCAATTGGAGGCCTTGGAGGAAGTGCACGAACAGCTG\n"
     ++"GTGGAGAGTAACCACGAACTGGAGCTTGATCTGCGCGAGGAATTGGATCT\n"
     ++"CGCCAATGGGGCCAAAAAGGAGGTGCTGCGAGAGCGGGATGCTGCCATTG\n"
     ++"AAACCATCTATGATCGCGACCAAACTATCGTTAAGTTTAGGGAACTGGTA\n"
     ++"CAGAAGCTAAACGACCAACTAACTGAGTTAAGGGATCGCAATTCTAGCAA\n"
     ++"CGAAAAGGAGTCGTTGCAGGATCCCAGTTTGAAAATGGTCACCGAAACCA\n"
     ++"TCGACTACAAACAAATGTTCGCCGAATCCAAGGCTTACACTCGCGCCATC\n"
     ++"GACGTTCAACTGCGCCAGATTGAGCTGAGCCAGGCCAATGAGCATGTCCA\n"
     ++"GATGCTTACCGCCTTCATGCCTGAGTCATTCATGAGTCGCGGTGGCGATC\n"
     ++"ACGACTCAATCCTTGTGATTCTGCTAATTTCACGAATTGTCTTTAAGTGC\n"
     ++"GACATTGTTGTTTCGCAAACGAGAGAGCGTTTCCCACCAGTGGATGCGAT\n"
     ++"TACCAGGGAGGCGGTGACCCAAGGCCATGCCGTCCAGCAGTATGCCTTCA\n"
     ++"AGTGTCGCCTGTTGCACTACGTCCACAGCCTGCAGTGTGCCCTTCACCAG\n"
     ++"ATCCTCTACGGACTCAACAGTTGTCAACCGGACACACTCCTGAGAGCCGG\n"
     ++"AAGTTCCCTGCCCGAAATGGTGGCTCAAGAAAAGATAGTGGACGGTATTA\n"
     ++"TCGAACTGCTGAAATCCAACCAGCTGGACGAGAACAGTACCACGGATAAT\n"
     ++"ATTGAGAAATGTGTGGCCTTCTTCAATGCCATGAACTCCGTACTTCTAGC\n"
     ++"CGGTGAACAGCTCCTCAACGAGATTCAGATGATCCGGGACTGCGTGGCCT\n"
     ++"CCTTGGGAGCAGCTTGTGAGAGCATTCTCAGCGACACGGCCATTGCAAAG\n"
     ++"GTGATCATTCAAGAGGCGGGCGCCACCAGCGATTCAGTGCTGCTGATCCA\n"
     ++"GTTCCTTAACGAGAACATGGAAAGCGTGCGGCAGCAAGTTAAGTTGATCA\n"
     ++"AGCGTCGCCTGCCAAGCGATCAGCACGTGATTAAGAGCGGTCTATCGCAG\n"
     ++"CACAAGGTGGAGGCGATGCGTGGTCTAGCCCAGAACATCAGTCGCATCAT\n"
     ++"GTCGGCGATGCACCAGGCCACCAAGCAGTCGCTCGCCGCCATTGTTTCCA\n"
     ++"CCATCGAGAGCGACAATGCAGCGGAGCACACTCTGCCCCAGGAGAAGTAC\n"
     ++"TGGGCCCTGTTGACCGCCTCCTGCGAGCGTATTTACGAACAGGATGATCG\n"
     ++"CGGACCGACACAGAACTTTAAGACCTTGCTGGCGCAAGCAAACTCCGATC\n"
     ++"TTCAGCTCATTGCCCAACATCTTCTGGACAAGGAGTACGACATCATTTCT\n"
     ++"GCAGCCAATAATGCCAGTAATCAGCAGAAATCGGGTGCCCACAGCACGCC\n"
     ++"CATTACTCAGAGGGCGCAGCTAATCAAGAAACAACTGGAGCAGAAGAACG\n"
     ++"TGCTGGCCGCCACGCTAGAGAATCGCGAGGCGGACGTCAAACAGCTGAAG\n"
     ++"GTTGCAGCCAAGATGAAGCAGAACGAATTGAGCGAGATGCAGATCCGAAA\n"
     ++"GGATCTAGCGGAGAAGAAGCTAAGCGTACTGCAAAACGAGTACGAGCACG\n"
     ++"CGGTCGACAAGTGGAAGCAGAAGTACGAGGAAACCTCCTTGCAGCTGCAG\n"
     ++"CTTAAGGAGAAGGAGTTTGAGGAGACGATGGACCACCTGCAAAGCGATAT\n"
     ++"CGATGCGCTGGAGAGCGAGAAGAGCGATCTACGCGACAAGCTGAAGCTGA\n"
     ++"ACTCGACTACAGGCAAGGTTCAGCCCGGCTCGGAATCCCACTCCCCGCAC\n"
     ++"AATATATCGCTATCAGGCAACACGTCCACTGCTCCGGGCATCAGCAATGT\n"
     ++"ATCCTACTCTGCTCCTGCCGGCACTGCTCCAGTGGTGGCCGAGGAAGTGG\n"
     ++"AGTTGCTGAAGAACGCCTTCAACCAGGAGCGCAACCAACGACTGCGTCTG\n"
     ++"CAGGCACAGGATATGCGTGCCAAGTTGTCCCAGTTTGAGCCCCTGCATGT\n"
     ++"GCCTCAGCCACAGGATCAGCGCATAACCGCTTTGGAATCCGAGCTGACCA\n"
     ++"GGATGAAGCACGCCTGGGTATTGTCGCTGCTGCAGGTGCGCTCGCAGGAT\n"
     ++"TCAGTGAATTCCGGTACACGTATCGACGCCGTGGCACTCCAAAGGCGCAA\n"
     ++"CCAGCCAGTTCCACTCAAGGGCGAGATCAGCTCGAAGGCTTCCCAGCTGG\n"
     ++"CCTCCGACATCCTGACGGAGTATCTGCAAAGGAAACCCCATCGTGCAACT\n"
     ++"CACGGACAGTTCGCCTCCTTTCCCACCGTCGATGTGAAGCGCGTGCTGCA\n"
     ++"GATCTAAAAAGGATCGTGTATCGTGGCAATGGAATCGGGGTCAGGGGCAA\n"
     ++"TCTGAATAGGATAGAATTTTATTTGTACTGCTAGCACAATTTCGGATCCC\n"
     ++"TCGCACAAGCAGCTTAGTCCAAAATCCAATAACACAGCTCCTCAAGACTC\n"
     ++"CCTGCTCCTGTGATGTGACCTAAACAAGTTAGCTAAACGAAGAGCAACTG\n"
     ++"AGAAATTATAATTCTACACTTAATTTATGTTTTTTGTATAAAGATTATAA\n"
     ++"ACCTATTGTAAAACTAACTTTGTGTTTAACTCCAACAAAGCCTGTTTCTG\n"
     ++"ATACTAAACTCAAGCGAACTAATACCGGTATTATCTATGAAGTCTATAGC\n"
     ++"AGGGGAGAACTATGTGAAAGACATAACTAAACCAGAGAAACAAATCTAAT\n"
     ++"TTGCATACGTGAGAATAAATAGTATATTATATATTAAAGGTAAATATATT\n"
     ++"TTGCTGATGTTGTATAACTC\n"