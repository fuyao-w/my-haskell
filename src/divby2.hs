

--divBy :: Integral a => a -> [a] -> Maybe [a]
--divBy _ [] = Just []
--divBy _ (0:_) = Nothing
--divBy numerator (denom :xs) =
--  case divBy numerator xs of
--    Nothing -> Nothing
--    Just results -> Just ((numerator `div` denom) : results)
--

divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator denominators =
  map worker denominators
  where worker 0 = Nothing
        worker x = Just (numerator `div` x)
        
        