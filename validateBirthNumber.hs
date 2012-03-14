import List
import IO

type IDNumber = String
data Sex = Man | Woman deriving(Eq,Show)
type Postfix = String {- ID number postfix -}
type Year = Int
type Month = Int
type Day = Int
type Person = (Year,Month,Day,Sex,Postfix) {- Persons data extracted from ID number -}

persons :: [IDNumber]
persons = ["850228/7772","846224/7772","846224777","12345->123","850228->6666","123456->>123","880230-8889","9153028478"]

validateFormat :: [IDNumber] -> [Bool] 
validateFormat [] = []
validateFormat xs = map validateEachF xs
  
validateEachF :: IDNumber -> Bool
validateEachF x =
    ((areDigits (take 6 x)) && (areDigits (drop (length x - 3) x)) &&
    ((length x == 9 && areDigits x) || 
    (length x == 10 && (areDigits x || hasSep x)) ||
    (length x == 11 && (hasArrow x || (hasSep x && isDigit (x!!7)))) ||
    (length x == 12 && hasArrow x && isDigit (x!!8))))

hasSep :: IDNumber -> Bool
hasSep x =
    (elemIndices '/' x == [6]) ||
    (elemIndices '-' x == [6])

hasArrow  :: IDNumber -> Bool
hasArrow x =
    (elemIndices '-' x == [6]) && (elemIndices '>' x == [7])

areDigits = all isDigit

lineLength :: Int
lineLength = 10

idToPerson :: IDNumber -> String
idToPerson x = 
            if (validateEachF x)
            then st 
            else ""
            where
            y = 1900 + read (take 2 x) :: Year 
            m = read (drop 2 (take 4 x)) :: Month 
            m'= m - 50
            d = read (drop 4 (take 6 x)):: Day
            s = Man
            s'= Woman
            p = if isDigit (x!!(length x - 4))       
                then drop (length x-4) x
                else drop (length x-3) x
            validPostfix = read (take 6 x ++ p) `mod` 11
            st | (isValidDate(y,m',d,s',p) && validPostfix == 0 && m > 12) = createString (y,m',d,s',p)
               | (isValidDate(y,m,d,s,p) && validPostfix == 0 && m < 12) = createString (y,m,d,s,p)
               | ((not (isValidDate(y,m',d,s',p)) || validPostfix /= 0) && m > 12) = createErrString (y,m',d,s',p)
               | ((not (isValidDate(y,m,d,s,p)) || validPostfix /= 0) && m < 12) = createErrString (y,m,d,s,p)

isValidDate :: Person -> Bool
isValidDate (y,m,d,s,p) | m `elem` [1,3,5,7,8,10,12] && d <= 31 = True
                        | m `elem` [4,6,9,11] && d <= 30 = True
                        | m == 2 && d <= 29 && isLeapYear y = True
                        | m == 2 && d <= 28 && not (isLeapYear y) = True
                        | otherwise = False

isLeapYear :: Int -> Bool
isLeapYear y = ((y `mod` 100 /= 0) || (y `mod` 400 == 0)) && 
               (y `mod` 4 == 0)  
            
createString :: Person -> String
createString (y,m,d,s,p) = show y ++ "." ++ show m ++ "." ++ show d ++ take lineLength (repeat '.') ++ show s ++ "\n" 

createErrString :: Person -> String
createErrString (y,m,d,s,p) = show y ++ "." ++ show m ++ "." ++ show d ++ take lineLength (repeat '.') ++ 
                              show s ++ "-> validation error (postfix = " ++ p ++ ") \n"  


printPeople :: [IDNumber] -> IO ()
printPeople xs = putStr (concat (map idToPerson xs))
    

-- Zistit ci je validateFormat true ak jes
    -- Year 19 ++ take 2 x
    -- Month inRange(01..12) tak Sex = Man,ak inRange(51..62) tak Sex = Women
    -- Month ak je [1,3,5,7,8,10,12] -> Day inRange(1..31)
          -- ak je 2 -> leap year Day inRange(0..29) inak (0..28)
          -- else Day inRange (0..30)
    -- Postfix = areDigits lastFour x == True tak = lastfour 
          -- ak nie tak postfix = last Three
          -- 8 a 3
