data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle h w) = 0.5 * h * w
area (Rectangle h w) = h * w
area (Circle r) = pi * r * r

Eq Shape where           
  (==) (Triangle h w) (Triangle h' w') = h == h' && w == w'
  (==) (Rectangle h w) (Rectangle h' w') = h == h' && w == w'
  (==) (Circle r) (Circle r') = r == r'
  (==) _ _ = False

Ord Shape where
    compare s s' = compare (area s) (area s')

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]