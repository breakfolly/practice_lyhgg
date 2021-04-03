applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)    
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

routine :: Maybe Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x: xs) <- Just "hello"
    return x


