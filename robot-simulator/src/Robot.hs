module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing Integer Integer

bearing :: Robot -> Bearing
bearing (Robot bearing _ _) = bearing 

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ x y) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction (fst coordinates) (snd coordinates)

turnLeft :: Bearing -> Bearing
turnLeft bearing = case bearing of  North -> West
                                    West  -> South
                                    South -> East
                                    East  -> North

turnRight :: Bearing -> Bearing
turnRight = turnLeft . turnLeft . turnLeft

move :: Robot -> String -> Robot
move robot instructions = foldl moveOne robot instructions
                            where moveOne r i = case (r,i) of (Robot b x y, 'L')     -> Robot (turnLeft b) x y
                                                              (Robot b x y, 'R')     -> Robot (turnRight b) x y
                                                              (Robot North x y, 'A') -> Robot North x (y + 1)
                                                              (Robot South x y, 'A') -> Robot South x (y - 1)
                                                              (Robot East x y, 'A')  -> Robot East (x + 1) y
                                                              (Robot West x y, 'A')  -> Robot West (x - 1) y
