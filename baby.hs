doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2
-- eager type 은 ' 를 붙이는 것이 관례
doubleSmallNumber' x  = (if x > 100 then x else x * 2) + 1

-- 함수명은 대문자로 시작할 수 없다.
conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- 리스트의 모든 요소들을 읽어와 1로 변환
length' xs = sum [1 | _ <- xs]

removeNoneUppercase :: [Char] -> [Char] 
removeNoneUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r


circumference' :: Float -> Float
circumference' r = 2 * pi * r
