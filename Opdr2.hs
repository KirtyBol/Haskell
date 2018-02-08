module Opdr2
    where
        import Data.Char
    
    -- Opdracht 1a
        -- Berekenen van de grootste gemene deler van 2 gegeven natuurlijke getallen 'x' en 'y'
        euclid::Integer->Integer->Integer
        euclid x 0 = x
        euclid x y = euclid y (x`mod`y)
    
    -- Opdracht 1b
        -- Benodigde berekening voor het genereren van publieke en prive sleutel
        -- Een gegeven algoritme om de congruentie op te lossen
        egcd :: Integer->Integer->(Integer,Integer,Integer)
        egcd 0 b = (b, 0, 1)
        egcd a b =
            let (g, s, t) = egcd (b `mod` a) a
            in (g, t - (b `div` a) * s, s)
    
        -- Mijn egcd, de verkregen egcd herschreven om negatief om te zetten naar positief door de modulus erbij op te tellen
        -- a = e, b = m'
        mijnegcd a b
            | x < 0 =  
                let (g, s, t) = egcd (b `mod` a) a
                in (g, t - (b `div` a) * s + m', s)
            | otherwise = egcd a b
            where x = middle(egcd a b)
    
        middle (_,b,_) = b
    
        prime :: Integer -> Bool
        prime n = (n > 1) && all (\ x -> rem n x /= 0) [2..n-1]
    
    -- Opdracht 2: Sleutel generatie
        -- Kies twee priemgetallen [x|x<-[100..500],prime x]
        p = 313
        q = 491
    
        -- modulus m = 153683
        m = p * q
    
        -- m' eulers = 152880
        m' = (p-1) * (q-1)
    
        -- e < m' & ggd = 1 : [x|x<-[1..m'],gcd x m' ==1]
        -- e is de prive sleutel
        e = 139493
    
        -- d = 126317
        -- d is de publieke sleutel
        d = middle(mijnegcd e m')

    -- Opdracht 3a: rsa encryptie
        -- Een functie met een tuple van sleutel en modulus
        -- a = e, b = m'
        first (a,_) = a
        second (_,b) = b

        -- Een getal als parameter en dit getal verslueteld  
        rsaencrypt::(Integer, Integer)->Integer->Integer
        rsaencrypt (e, m) x = (x^e) `mod` m
    
    -- Opdracht 3b: rsa decryptie
        -- Een functie die een tuple van sluetel en modulus
        -- Een getals als parameter en dit getal ontsleutelt
        rsadecrypt::(Integer, Integer)->Integer->Integer
        rsadecrypt (e, m) x = (x^d) `mod` m

    -- Opdracht 4
        -- Versleutelen van een letter mbv functies uit opdracht 3
        -- Deze werkt alleen niet...
        --textencrypt::(Integer, Integer)->Char->Integer
        --textencrypt (e, m) a = ord a + e + m

        -- Ontsleutelen van een letter mbv functies uit opdracht 3
        -- En deze dus ook niet...
        --textdecrypt::(Integer, Integer)->Integer->Char
        --textdecrypt (e, m) a = chr (a - e - m)

    -- Opdracht 5
        -- Alice gebruikt de public key van Bob om te encrypten
        -- Bob kan het bericht dan encrypten met de public key van Alice
        -- Alice en Bob moeten met hun eigen public key decrypten
