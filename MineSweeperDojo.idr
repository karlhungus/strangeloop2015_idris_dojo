module Main
import Data.Vect

data Cell = Mine | Nothing

data Board = MkBoard Nat Nat (List (Nat, Nat))

atPosition : Nat -> Nat -> (Nat, Nat) -> Bool
atPosition x y (p, q) = if p == x && y == q then True else False

renderCell : Nat -> Nat -> (List (Nat, Nat)) -> String
renderCell x y [] = "o"
renderCell x y (k::ks) = if (atPosition x y k)
                            then "x"
                            else (renderCell x y ks)

renderRow : Nat -> Nat -> (List (Nat, Nat)) -> String
renderRow x Z k = (renderCell x Z k)
renderRow x y k = (renderRow x (y - 1) k) ++ (renderCell x y k)

boardToString : Board -> String
boardToString (MkBoard x Z k) = (renderRow x Z k)
boardToString (MkBoard x y k) = (renderRow x y k) ++ (renderRow x y - 1 k)

emptyBoard : Board
emptyBoard = MkBoard 10 5 []

simpleBoard : Board
simpleBoard = MkBoard 4 3 [(0,0)]

main : IO ()
main = putStrLn (boardToString emptyBoard)

-- create a an empty board


