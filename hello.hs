module Hello
    where

        -- Opdracht 1a
        -- Berekenen van de faculteit met pattern matching
        faca::Int->Int
        faca 0 = 1
        faca x = x * faca(x-1)

        -- Opdracht 1b
        -- Berekenen van de faculteit met guards
        facb:: Int->Int 
        facb x
            | x <= 0    = 1
            | otherwise = x * facb(x-1)

        -- Opdracht 2a
        -- Berekent nulpunten van een tweedegraads functie
        nulpuntena::Double->Double->Double->[Double]
        nulpuntena a b c = 
            if (b^2 - 4*a*c) > 0
                then (-b + sqrt(b^2 - 4*a*c))/2*a : (-b - sqrt(b^2 - 4*a*c))/2*a : []
                else if (b^2 - 4*a*c) == 0
                    then -b/2*a : []
                    else error "Geen snijpunten met deze nulpunten" : []

        -- Opdracht 2b
        -- Berekent nulpunten van een tweedegraads functie en maakt gebruik van het 'where' keyword en guards
        nulpuntenb::Double->Double->Double->[Double]
        nulpuntenb a b c
            | a == 0 && b == 0 && c == 0 = 0 : []
            | d == 0 = -b/2*a : []
            | d > 0 = -b+d/2*a : []
            | otherwise = error "Geen snijpunten op de punten" : []
            where d = b^2 - 4*a*c

        -- Opdracht 2c
        -- Er wordt gegooid met 3 dobbelstenen en de functie geeft een lijst met tuples terug, elke tuple is een worp
        -- Alleen de worpen, waarvan de som van de ogen een veelvoud van 5 is, worden in de lijst opgenomen.
        dobbelsteena    = [(x, y, z) | x <- [1..6], y <- [1..6], z <- [1..6], (x+y+z) `mod` 5 == 0 ]
        
        -- Opdracht 2d
        -- Herschrijf de functie uit 2c zodanig dat de lijst met tuples een meervoud is van 'n'
        dobbelsteenb n  = [(x, y, z) | x <- [1..6], y <- [1..6], z <- [1..6], (x+y+z) `mod` n == 0 ]

        -- Test faculteit functie
        fac 0 = 1
        fac n = n * fac(n-1)