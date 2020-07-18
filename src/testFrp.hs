{-# LANGUAGE PackageImports, RecursiveDo #-}
import Control.Applicative
import Control.Monad
import FRP.Elerea.Simple

test1::IO()
test1 = do
  smp <- start ( stateful (3::Int) (+2))
  res <- replicateM 5 smp
  print res

test2::IO()
test2 = do
  (sig,snk) <- external (4::Int)
  smp <- start ( return sig)
  snk 2
  snk 5
  res <- replicateM 5 smp
  print res

test3::IO()
test3 = do
  (gen,snk) <- externalMulti
  smp <- start gen
  snk (2::Int)
  snk 5
  snk 7
  res <-replicateM 5 smp
  print res

test4::IO()
test4 = do
  smp <- start $ do
                    rec let fib'' = liftA2 (+) fib' fib
                        fib' <- delay (1::Int) fib''
                        fib <- delay 1 fib'
                    return fib
  res <- replicateM 7 smp
  print res

sigtest n gen = replicateM n =<< start gen
sigtest' n gen = start gen >>= replicateM n
sigtest'' n gen = do
  smp <- start gen
  replicateM n smp


countdown::String->Int->SignalGen( Signal (String, Maybe Int))
countdown name t = do
  let tick prev = do { t <- prev; guard(t>0); return(t-1)}
  timer <-stateful (Just t) tick
  return ((,) name <$> timer)

timerSource::[(String,Int,Int)]->SignalGen(Signal( [Signal(String, Maybe Int)]))
timerSource ts = do
  let gen t = mapM (uncurry countdown) newTimers
        where
          newTimers = [(n,v)|(n,v,st)<-ts, st == t]
  cnt <- stateful (0::Int) (+1)
  generator (gen <$> cnt)

signalCollection::Signal [Signal a] -> (a->Bool) -> SignalGen (Signal [a])
signalCollection source isAlive = mdo
  sig <- delay [] (map snd <$> collWithVals')
  coll <- memo ( liftA2 (++) source sig)
  let collWithVals = zip <$> (sequence =<< coll) <*> coll
  collWithVals' <- memo ( filter (isAlive . fst) <$> collWithVals)
  return $ map fst <$> collWithVals'


main::IO()
main = do
  test1
  test2
  test3
  test4
  res <- sigtest 15 $ stateful (2::Int) (+2)
  print res
  contdowntest <- sigtest 15 $ countdown "first" (5::Int)
  print contdowntest
