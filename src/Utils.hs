module Utils where

degreesToRadians:: Double -> Double
degreesToRadians d = d * pi / 180

-- Finds the roots of a function defined as ax^2 + bx + c = 0
roots :: Double -> Double -> Double -> [Double]
roots a b c
    | descriminant == 0 = [0.5 * (-b)]
    | descriminant > 0 = [0.5 * (-b + sqrt descriminant), 0.5 * (-b - sqrt descriminant)]
    | otherwise = []
    where descriminant = b * b - 4 * a * c
