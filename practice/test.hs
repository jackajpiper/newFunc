doubleMe x = x * 2

doubleUs x  y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeUppercase st = [c | c <- st, not (elem c ['A'.. 'Z'])]
