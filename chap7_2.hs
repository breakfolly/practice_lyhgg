import qualified Data.Map as Map

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

mysteryDude = "Person { firstName =\"Michael\", lastName = \"Diamond\", age = 43}"

-- data Bool = False | True deriving (Ord)

data Day =  Monday | Tuesday | WednesDay | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String, String)]
phoneBook = 
    [("betty", "444-1234"),
    ("bonnie", "345-1234"),
    ("bonnie", "892-1234"),
    ("penny", "111-2344")
    ]

-- type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

--inPhoneBook :: String -> String -> [(String, String)] -> Bool
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]
type IntMap v = Map.Map Int

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
    Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList 
    [(100, (Taken, "aaaaa"))
    ,(101, (Free, "bbbbb"))
    ,(103, (Free, "ccccc"))
    ,(105, (Free, "ddddd"))
    ,(109, (Taken, "eeeee"))
    ,(110, (Taken, "fffff"))
     ]

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right 

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- just for fun, like javascript

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult


