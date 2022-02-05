

data Play a = Play a deriving (Show)

pmap :: (a -> b) -> Play a -> Play b
pmap f (Play a)= Play (f a)

calc :: Play (a -> b) -> Play a ->Play b
calc (Play func) (Play a) = Play (func a)

ppure :: a -> Play a
ppure a = Play a
instance Functor Play where
  fmap = pmap

instance Applicative Play where
  (<*>) = calc
  pure = ppure

-- bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
mcalc ::  Play a -> (a -> Play b) -> Play b
mcalc (Play a)  func = func a

--mmcalc :: m a -> m b -> m b
--mmcalc (Play a) (Play b) = Play b

instance Monad Play where
  (>>=) = mcalc



-- (Play 1) >>= \a ->  ((\a -> a * 200 + 19) `fmap` (Play a))

-- (Play 1) >>= \a ->  ((\a -> a * 200) `fmap` (Play a))

-- (Play 1) >>= \a ->  Play (+200) <*> Play a