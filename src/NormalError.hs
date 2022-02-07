--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NormalError where


-- 前面是类型构造器    后面是值构造器  ，前面只能做声明，后面才可以在表达式中
data NormalError a = NormalError a deriving (Show)

-- 打印出NormalError 里的信息
-- 拼接 两个 NormalError 的内容
-- 修改 NormalError 里的信息

--replace  :: NormalError a -> NormalError b -> NormalError b
--replace (NormalError a)  (NormalError b) = NormalError b`



calc ::  NormalError a -> (a -> NormalError b) -> NormalError b
calc (NormalError a)  f = f a

instance Monad NormalError where
  (>>=) = calc


-- <*> :: f (a -> b) -> f a -> f b

appCalc :: NormalError (a -> b) -> NormalError a ->NormalError b
appCalc (NormalError func) (NormalError a) = NormalError (func a)


ppure a = NormalError a

instance Applicative NormalError where
    pure = ppure
    (<*>) = appCalc



emap :: (a -> b) -> NormalError a -> NormalError b
emap  f (NormalError a) =  NormalError( f a)

instance Functor NormalError where
  fmap = emap

class  Error f  where -- f 代表一个类型，
  cconcat :: (Show a)=> f a -> f a -> [Char] --关键点是给 a 一个约束

--econcat :: (Show a1, Show a2) => NormalError a1 -> NormalError a2 -> [Char]
--econcat  (NormalError a1 ) (NormalError a2 )  =  "[" ++ show a1 ++ "-" ++ show a2 ++"]"

mmconcat :: (Show a1 ,Show a2) => (Maybe a1) -> (Maybe a2) -> String
mmconcat (Just a1) (Just a2) = (show a1) ++ (show a2)
mmconcat (Just a1) Nothing = (show a1) ++ " nothin"
mmconcat Nothing (Just a1)  = " nothin " ++(show a1)
mmconcat _ _ = "nothing" ++ " nothin"

instance Error Maybe  where
    cconcat = mmconcat

