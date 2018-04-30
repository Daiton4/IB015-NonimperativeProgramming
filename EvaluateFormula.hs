eval :: String -> Int
eval x = read(head(evalArray(words(x))))::Int

evalArray :: [String] -> [String]
evalArray arr = sumRec(timRec(powRec arr))

powRec :: [String] -> [String]
powRec arr = if elem "^" arr then powRec(pow arr (findIndices (\x -> x=="^") arr)) else arr

pow :: [String] -> [Int] -> [String]
pow arr occ = (fst(splitAt ((last occ)-1) arr))++lines(show((read(arr!!((last occ)-1))::Int)^(read(arr!!((last occ)+1)))::Int))++(snd(splitAt ((last occ)+2) arr))

timRec :: [String] -> [String]
timRec arr = if ((elem "*" arr)||(elem "/" arr)||(elem "%" arr)) then timRec(tim arr (findIndices (\x -> elem x ["*","/","%"]) arr)) else arr

tim :: [String] -> [Int] -> [String]
tim arr occ = if arr!!(head occ) == "*" then (fst(splitAt ((head occ)-1) arr))++lines(show((read(arr!!((head occ)-1))::Int)*(read(arr!!((head occ)+1)))::Int))++(snd(splitAt ((head occ)+2) arr))
  else if arr!!(head occ) == "/" then (fst(splitAt ((head occ)-1) arr))++lines(show((read(arr!!((head occ)-1))::Int) `div` (read(arr!!((head occ)+1)))::Int))++(snd(splitAt ((head occ)+2) arr))
  else if arr!!(head occ) == "%" then (fst(splitAt ((head occ)-1) arr))++lines(show((read(arr!!((head occ)-1))::Int) `mod` (read(arr!!((head occ)+1)))::Int))++(snd(splitAt ((head occ)+2) arr))
  else []

sumRec :: [String] -> [String]
sumRec arr = if ((elem "+" arr)||(elem "-" arr)) then sumRec(summ arr (findIndices (\x -> elem x ["+","-"]) arr)) else arr

summ :: [String] -> [Int] -> [String]
summ arr occ = if arr!!(head occ) == "+" then (fst(splitAt ((head occ)-1) arr))++lines(show((read(arr!!((head occ)-1))::Int)+(read(arr!!((head occ)+1)))::Int))++(snd(splitAt ((head occ)+2) arr))
   else if arr!!(head occ) == "-" then (fst(splitAt ((head occ)-1) arr))++lines(show((read(arr!!((head occ)-1))::Int)-(read(arr!!((head occ)+1)))::Int))++(snd(splitAt ((head occ)+2) arr))
   else []
  
--Program evaluates aritmetic formulas in string form and returns int result
