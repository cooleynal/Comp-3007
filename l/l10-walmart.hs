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
  { empId :: IdNum, -- primary key
    empName :: Maybe (String, String), -- "First-name Last-name"
    empSupervisor :: Maybe IdNum, -- supervisor key
    empBeverage :: Maybe Beverage -- preferred beverage
  }

data Emps = Emps [Emp]

empLookup :: Int -> Emps -> Maybe Emp
empLookup idnum (Emps emps) =
  find (\emp -> idnum == empId emp) emps

coffeeOrder :: Int -> Emps -> Maybe (Beverage, String)
coffeeOrder idNum emps =
  case empLookup idNum emps of
    Nothing -> Nothing
    Just emp ->
      case empSupervisor emp of
        Nothing -> Nothing
        Just bossId ->
          case empLookup bossId emps of
            Nothing -> Nothing
            Just bossEmp ->
              case empBeverage bossEmp of
                Nothing -> Nothing
                Just bev ->
                  case empName bossEmp of
                    Nothing -> Nothing
                    Just empName -> Just (bev, fst empName)

-- Note: "do" manages Nothing/Just book-keeping automatically
coffeeOrder' :: Int -> Emps -> Maybe (Beverage, String)
coffeeOrder' idNum emps =
  do
    emp <- empLookup idNum emps
    bossId <- empSupervisor emp
    bossEmp <- empLookup bossId emps
    bev <- empBeverage bossEmp
    empName <- empName bossEmp
    return (bev, fst empName)

emps =
  Emps
    [ Emp
        0
        (Just ("Bingo", "Bongo"))
        (Just 1)
        (Just Tea),
      Emp
        1
        (Just ("Prof", "Genki"))
        (Just 17)
        (Just Nectar),
      Emp
        17
        (Just ("Face", "McShooty"))
        Nothing
        Nothing
    ]


main :: IO ()
main = do
  let empId1 = 0
  let empId2 = 1
  let empId3 = 17

  -- Call coffeeOrder
  let order1 = coffeeOrder empId1 emps
  let order2 = coffeeOrder empId2 emps
  let order3 = coffeeOrder empId3 emps

  -- Print the results
  putStrLn $ "Coffee order for employee ID " ++ show empId1 ++ ": " ++ show order1
  putStrLn $ "Coffee order for employee ID " ++ show empId2 ++ ": " ++ show order2
  putStrLn $ "Coffee order for employee ID " ++ show empId3 ++ ": " ++ show order3

  -- You can also call coffeeOrder' if you wish
  putStrLn $ "Using coffeeOrder' for employee ID " ++ show empId1 ++ ": " ++ show (coffeeOrder' empId1 emps)
  putStrLn $ "Using coffeeOrder' for employee ID " ++ show empId2 ++ ": " ++ show (coffeeOrder' empId2 emps)
  putStrLn $ "Using coffeeOrder' for employee ID " ++ show empId3 ++ ": " ++ show (coffeeOrder' empId3 emps)
