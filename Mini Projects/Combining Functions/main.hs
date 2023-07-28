-- Function to calculate the area of a triangle
luasSegitiga :: Double -> Double -> Double
luasSegitiga alas tinggiSisi = 0.5 * alas * tinggiSisi


-- Function to calculate the volume of a triangular prism
volumePrismaSegitiga :: Double -> Double -> Double -> Double
volumePrismaSegitiga alas tinggiSisi tinggi = luasSegitiga  alas tinggiSisi * tinggi

main :: IO ()
main = print (volumePrismaSegitiga 3 4 10)
