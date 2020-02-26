doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

-- doubleSmallNumber x = if x > 100
--                      then 100
--                      else 2*x

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)