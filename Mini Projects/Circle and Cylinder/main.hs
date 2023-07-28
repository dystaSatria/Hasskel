circleArea :: Double -> Double
circleArea r = do
    let piValue = 22 / 7
    piValue * r * r

cylinderVolume :: Double -> Double -> Double
cylinderVolume r h = circleArea r * h
