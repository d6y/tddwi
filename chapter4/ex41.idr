data Direction = North | East | South | West

turn_clockwise : Direction -> Direction
turn_clockwise North = East
turn_clockwise East  = South
turn_clockwise South = West
turn_clockwise West  = North

||| Shapes!
data Shape = ||| A triangle, with its base length and height
             Triangle  Double Double
           | ||| A rectangle, defined by width and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle    Double

data Shape2 : Type where
     Tri2 : Double -> Double -> Shape2
     Rec2 : Double -> Double -> Shape2
     Cir2 : Double -> Shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle l h) = l * h
area (Circle r) = pi * r * r

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture


rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
                (Combine (Translate 35 5 circle)
                (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic1 pic2) = picture_area pic1 + picture_area pic2
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic


{-
picture_area test_picture
328.53981633974485 : Double
-}

safe_divide : Double -> Double -> Maybe Double
safe_divide x y = if y == 0 then Nothing else Just (x / y)


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right)
    = case compare x y of
      LT => Node (insert x left) y right
      EQ => orig
      GT => Node left y (insert x right)

{- Ex.1
listOfTree [1,10,2,3]
-}
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

{- Ex.2
treeTolistL(listToTree [1,10,3,2])
-}
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList(left) ++ [x] ++ treeToList(right)

{- Ex. 3 & 4
evaluate (Mul (Num 2) (Add (Num 20) (Num 1)))
42 : Int
-}

data Expr : Type where
  Num : Int -> Expr
  Add : Expr -> Expr -> Expr
  Sub : Expr -> Expr -> Expr
  Mul : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Num x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

{- Ex. 5
-}

find_shapes : Picture -> List Shape
find_shapes (Primitive shape) = [shape]
find_shapes (Combine pic1 pic2) = find_shapes pic1 ++ find_shapes pic2
find_shapes (Rotate x pic) = find_shapes pic
find_shapes (Translate x y pic) = find_shapes pic

triangle_area : Shape -> Maybe Double
triangle_area shape@(Triangle x y) = Just (area shape)
triangle_area _ = Nothing

biggestTriangle : Picture -> Maybe Double
biggestTriangle pic = head' (sort all_triangles)
  where
    all_shapes : List Shape
    all_shapes = find_shapes pic

    all_triangles : List Double
    all_triangles = mapMaybe triangle_area all_shapes


