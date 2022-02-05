
data CustomColor =
  CustomColor{
    red :: Int,
    green :: Int,
    blue :: Int
  } deriving (Eq, Show,Read)


data FuncRec  = FuncRec {name :: String, calc :: Int -> Int , namedCalc :: Int -> (String,Int)}

plus5func color x  = (color, x + 5)

purple = CustomColor 255 0 255

plus5 = mkFuncRec "plus5" (+ 5)
always0 = mkFuncRec "always0" (\_ -> 0)


mkFuncRec name calcFunc = FuncRec{name = name , calc= calcFunc,namedCalc = \x -> (name ,calcFunc x)}


