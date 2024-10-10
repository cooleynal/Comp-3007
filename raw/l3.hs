data Colour = Blue | Red | Yellow
  deriving (Show) -- directs compiler to derive "show" method

data Price = Price Int
  deriving (Show)

data Car = Car Colour Price String
  deriving (Show)

data Fleet = Empty | AddCar Car Fleet
  deriving (Show)

car0 :: Car
car0 = Car Red (Price 60000) "Lincoln Juggernaut"

car1 :: Car
car1 = Car Yellow (Price 120000) "BMW Highsnoot"

car2 :: Car
car2 = Car Blue (Price 10000) "Fiat Roadkill"

fleet :: Fleet
fleet = AddCar car0 (AddCar car1 (AddCar car2 Empty))

-- Draw a tree for the example fleet.

-- Write down the rules for the corresponding inductive definitions.

-- Write a function unBlue on colours that changes blue to red.
-- data Colour = Blue | Red | Yellow
unBlue :: Colour -> Colour
-- unBlue Blue = Red
-- unBlue Red = Red
-- unBlue Yellow = Yellow
unBlue Blue = Red
unBlue x = x -- know that x is not Blue

-- Write a function size computing the number of cars in a fleet.
size :: Fleet -> Int
size Empty = 0
size (AddCar car fleet) =
  1 + size fleet

-- Write a function value that sums the prices of the cars in a fleet.
value :: Fleet -> Int
value Empty = 0
value (AddCar (Car colour (Price i) model) fleet) =
  i + value fleet

-- Write a function max2 that returns the largest of two prices
maxPrice2 :: Price -> Price -> Price
maxPrice2 (Price x) (Price y) =
  if x <= y 
    then Price y
    else Price x

-- Write a function maxPrice that returns the largest price in the fleet
maxPrice Empty = Price 0
maxPrice (AddCar (Car colour price model) fleet) =
  maxPrice2 price (maxPrice fleet)

-- Write a function hasModel which say if a fleet has a given model.
hasModel :: Fleet -> String -> Bool
hasModel Empty model = False
hasModel (AddCar car fleet) model = isModel car model || hasModel fleet model

isModel :: Car -> String -> Bool
isModel (Car colour price model0) model = model == model0

-- Write a function paint that applies a colour-changer (e.g. 2 above)
-- to all cars in a fleet.
paint :: Fleet -> (Colour -> Colour) -> Fleet
paint Empty cc = Empty
paint (AddCar (Car colour price model) fleet) cc = 
  AddCar (Car (cc colour) price model) (paint fleet cc)

