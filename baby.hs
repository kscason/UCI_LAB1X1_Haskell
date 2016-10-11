doubleU2 x y = doubleMe x + doubleMe y
doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleList xs = [doubleMe x | x <- xs]
doubleListMap xs = map doubleMe xs
inc = \x -> x+1
incSection = (+ 1)
insert (x:xs) y = if x < y then x : (insert xs y) else y : x : xs 
