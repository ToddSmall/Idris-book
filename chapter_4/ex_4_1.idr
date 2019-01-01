data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
               
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                           LT => Node (insert x left) val right
                                           EQ => orig
                                           GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

%name Expr expr, expr1

evaluate : Expr -> Int
evaluate (Val val) = val
evaluate (Add left right) = evaluate left + evaluate right
evaluate (Sub left right) = evaluate left - evaluate right
evaluate (Mult left right) = evaluate left * evaluate right

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) (Just y) = Just (max x y)

||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double
             
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle x y)) = Just(area tri)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
