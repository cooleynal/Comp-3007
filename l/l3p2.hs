
data Colour = Blue | Red | Yellow
  deriving (Show)

type Model = String


-- data Model = String
--  deriving (Show)

data Price = Price Int
  deriving (Show)


data Car = Car Colour Price Model
  deriving (Show)


data Fleet = Empty | AddCar Car Fleet
  deriving (Show)


car0 :: Car
car0 = Car Red (Price 100) "car0"

car1 :: Car
car1 = Car Blue (Price 101) "car1"

car2 :: Car
car2 = Car Yellow (Price 102) "car2"

fleet :: Fleet
fleet = AddCar car0 (AddCar car1 (AddCar car2 Empty))



-- case car1 of Car colour price model -> Car (unBlue colour) price model

chModel:: String -> String
chModel x = "Cats"

unBlue :: Colour -> Colour
unBlue Blue = Red
unBlue x = x

chPrice :: Price -> Int -> Price
chPrice (Price _) = Price

-- Write a function size computing the number of cars in a fleet.
size :: Fleet -> Int
size Empty = 0
size (AddCar _ fleet) = 1 + (size fleet)


-- Write a function value that sums the prices of the cars in a fleet.
value :: Fleet -> Int
value Empty = 0
value (AddCar (Car _ (Price n) _ ) fleet) = n + (value fleet)

maxPrice2 :: Price -> Price -> Price
maxPrice2 (Price i) (Price j) = Price (max i j)

maxPrice :: Fleet -> Price
maxPrice Empty = Price 0
maxPrice (AddCar (Car _ price _) fleet) = maxPrice2 price (maxPrice fleet)

-- Write a function hasModel which say if a fleet has a given model.

hasModel :: Fleet -> String -> Bool -- takes in fleet and string of model
hasModel Empty target = False
hasModel (AddCar (Car _ _ model) fleet) target = (model == target)
  || (hasModel fleet target)



-- Write a function paint that applies a colour-changer (e.g. 2 above)
-- to all cars in a fleet.

-- paint fleet unBlue
paint :: Fleet -> (Colour -> Colour) -> Fleet
paint Empty cc = Empty
paint (AddCar (Car colour price model) fleet) cc =
  AddCar (Car (cc colour) price model) (paint fleet cc)

paint2 :: Fleet -> Fleet
paint2 Empty = Empty
paint2 (AddCar (Car colour price model) fleet) =
  AddCar (Car (unBlue colour) price model) (paint2 fleet)

paint3 :: Fleet -> Fleet
paint3 Empty = Empty
paint3 (AddCar (Car colour price model) fleet) =
  AddCar (Car (unBlue colour) (chPrice price 20) (chModel model)) (paint3 fleet)
