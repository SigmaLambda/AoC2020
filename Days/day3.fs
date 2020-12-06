module Day3

let input =
    System.IO.File.ReadAllLines ".//input/day3.txt"

let GetB b = fun string -> int

let expandMap map = fun Array2D -> Array2D 