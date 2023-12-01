module Golf where

-- Hopscotch
skips :: [a] -> [[a]]
skips []=[]
skips p@(x:xs)=p:skips xs

-- Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  |x<y && y<z  = y:localMaxima xs
  | otherwise = localMaxima xs
localMaxima _=[]

-- Histogram
histogram :: [Integer] -> String
histogram lst =
  let 
    count = [length (filter (==n) lst) | n<-[0..9]]
    maxOfCount = maximum count
  in 
    (unlines [[if x>=i then '*' else ' '|x<-count]|i<-[maxOfCount,maxOfCount-1..1]])++"==========\n0123456789\n"