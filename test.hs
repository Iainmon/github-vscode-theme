
myHead :: [a] -> a
myHead (first:_) = first

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) = (x:ys) : group zs
    where (ys, zs) = span ((==)x) xs

unstutter :: String -> String
unstutter = map myHead . group

maybeUnstutter :: Maybe String -> Maybe String
maybeUnstutter = fmap unstutter

threeTuple :: a -> b -> c -> (a,b,c)
threeTuple a b c = (a,b,c)

-- main = print $ maybeUnstutter $ Just "aaaaaaaaaamadeus"
main = print $ threeTuple 1 2 3

-- maybeHead :: [a] -> Maybe a
-- maybeHead [] = Nothing
-- maybeHead x = Just (head x)

-- data Color = Red | Yellow | Green
-- newtype TrafficLight = TrafficLight Color 
-- instance Eq TrafficLight where
--     TrafficLight Red == TrafficLight Red = True
--     _ == _ = False

-- canProceedThrough :: TrafficLight -> Bool
-- canProceedThrough t = t /= Red



newtype BankBalance = BankBalance Float
newtype InterestRate = InterestRate Float

bankFunction :: BankBalance -> InterestRate -> InterestRate -> BankBalance
bankFunction (BankBalance b) (InterestRate i) (InterestRate f) = BankBalance (b * (1 + i + f))

determineNewBalance :: BankBalance -> BankBalance
determineNewBalance bal = bankFunction bal intRate fedRate
  where 
    intRate = InterestRate 6.2
    fedRate = InterestRate 0.5










sumUp :: Num a => [a] -> a
sumUp = foldr (+) 0

fifteen = sumUp [1, 2, 3, 4, 5]

main = print $ 










data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    , hometown :: String
    , homestate :: String }
    deriving (Eq)

instance Show Person where
    show p1 = (firstName p1) ++ " " 
        ++ (lastName p1) ++ ", age: " 
        ++ (show (age p1)) ++ ", born in: " 
        ++ (hometown p1) ++ ", " 
        ++ (homestate p1)

data Student = Student
    { studentFirstName :: String
    , studentMiddleName :: String
    , studentLastName :: String
    , studentGradeLevel :: Int }

class HasName a where
    totalName :: a -> String

class Concatenation a where
    (<->) :: a -> a -> a

instance Concatenation Person where
    (<->) p1 p2 = p1

instance Concatenation Student where
    (<->) s1 s2 = (++) s1 s2

instance HasName Person where
    totalName p = firstName p ++ " " ++ lastName p

instance HasName Student where
    totalName s = studentFirstName s ++ " " ++ studentMiddleName s ++ studentLastName s


p1 = Person {firstName = "John", lastName = "Doe", age = 23, hometown = "El Paso", homestate = "Texas"}

main = print $ p1 <-> p1
