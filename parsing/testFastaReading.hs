
splitFasta :: String -> IO [String]
splitFasta s = do a <- readFile s
                  return $ lines a


