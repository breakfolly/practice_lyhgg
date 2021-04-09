import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

--applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
--applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    tell (toDiffList [show x])
