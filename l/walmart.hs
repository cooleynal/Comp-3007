import Data.List (find)

data Beverage = Coffee | Tea | Nectar deriving (Show)

type IdNum = Int

-- This definition uses "record syntax". It's like
--
--   data Emp
--    = Emp IdNum (Maybe (String, String)) (Maybe IdNum) (Maybe Beverage)
--
--  except that it also defines accessors empId :: Emp -> IdNum etc.
data Emp = Emp
  { empId :: IdNum -- primary key
  , empName :: Maybe (String, String) -- "First-name Last-name"
  , empSupervisor :: Maybe IdNum -- supervisor key
  , empBeverage :: Maybe Beverage -- preferred beverage
  }

-- Malwart employee database
data Emps = Emps [Emp]

empLookup :: Int -> Emps -> Maybe Emp
empLookup idnum (Emps emps) =
  find (\emp -> idnum == empId emp) emps

-- coffeeOrder :: Int -> Emps -> Maybe (Beverage, String)

emps =
  Emps
    [ Emp
        0
        (Just ("Bingo", "Bongo"))
        (Just 1)
        (Just Tea)
    , Emp
        1
        (Just ("Prof", "Genki"))
        (Just 17)
        (Just Nectar)
    , Emp
        17
        (Just ("Face", "McShooty"))
        Nothing
        Nothing
    ]
