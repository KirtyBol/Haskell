module Opdr3
    where
        import Data.List

        -- Opdracht 1a: partieel parametiseren
        differentieer::(Double->Double)->Double->Double->Double
        differentieer f p x = (f (x + p) - f x) / p

        -- Opdracht 1b: partieel parametiseren
        integreer::(Double->Double)->Double->Double->Double->Double
        integreer f a b p = foldr (\l r -> (dx * (f (a + (l * dx)))) + r) 0 [0..p-1]
            where dx = (b-a)/p  

        -- Opdracht 2: Werken met bibliotheken
        dubbelen::(Ord a) => [a]->[a]
        dubbelen s = sort(nub((s) \\ (nub s)))

        -- Opdracht 3: Hogere orde functies

        -- Bouwt lijst met lijsten van iedere worp
        s = [1..6] 
        dices = [[a,b,c,d,e] | a<-s, b<-s, c<-s, d<-s, e<-s]

        -- Geeft het aantal voorkomens van 'c' in een lijst
        count::Integer->[Integer]->Integer
        count c [] = 0
        count c (x:xs)
            | c == x = 1 + (count c xs)
            | otherwise = count c xs

        -- Deze functie converteert een lijst in een aantal tuples met voorkomens
        convert list = ([a, b, c, d, e, f], list) 
            where
                a = count 1 list
                b = count 2 list
                c = count 3 list
                d = count 4 list
                e = count 5 list
                f = count 6 list

        -- Spelen van poker 
        same::Integer->[[Integer]]->[[Integer]]
        same x list = filter (elem x) $map fst (map convert list)

        -- Poker: 5 stenen met gelijke ogen
        poker = (fromIntegral totalPoker) / (fromIntegral (length dices))
            where
                totalPoker = length pokerList 
                pokerList = filter (elem 5) (map fst (map convert dices))

        -- Four of a kind: 4 stenen met gelijke ogen
        fourOfaKind = (fromIntegral totalFourOfaKind) / (fromIntegral (length dices))
            where
                totalFourOfaKind = length fourOfaKindList 
                fourOfaKindList = filter (elem 4) (map fst (map convert dices))

        -- Three of a kind: 3 stenen met gelijke ogen
        threeOfaKind = (fromIntegral totalThreeOfaKind) / (fromIntegral (length dices))
            where
                totalThreeOfaKind = length threeOfaKindList
                threeOfaKindList = filter (elem 3) (map fst (map convert dices))

        -- Full house: 3 stenen en 2 stenen gelijk (22332)
        fullHouse = (fromIntegral totalFullHouse) / (fromIntegral (length dices))
            where 
                totalFullHouse = length fullHouseList
                fullHouseList = filter (elem 2) (map fst (map convert threeList))
                threeList = same 3 dices

        -- Two pair: twee paar met gelijke ogen (35453)
        twoPairs = (fromIntegral (totalTwoPairs)) / (fromIntegral (length dices))
            where 
                totalTwoPairs = length (twoPairsList)
                twoPairsList = filter (elem 2) (map fst (map convert pairList))
                pairList = same 2 dices

        -- One pair: een paar met gelijke ogen (12344)
        onePair = twoSame - (twoPairs + fullHouse)
            where
                twoSame = (fromIntegral totalTwoSame) / (fromIntegral (length dices))
                    where
                        totalTwoSame = length twoSameList
                            where 
                                twoSameList = same 2 dices

        -- Straight: 12345 of 23456
        -- uit de Straight kom ik gewoon niet en niemand kan mij er echt een duidelijke uitleg over geven
        -- als ik hier eventueel een uitleg over zou kunnen krijgen, hoor ik dat graag

        -- Bust: overgebleven worpen
        bust = 1 - (poker + fourOfaKind + threeOfaKind + fullHouse + twoPairs + onePair)

