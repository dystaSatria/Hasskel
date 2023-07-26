main = do
    let nilaiSiswa = 75
    let grade = if nilaiSiswa >= 80 then "A"
                else if nilaiSiswa >= 60 then "B"
                else if nilaiSiswa >= 40 then "C"
                else if nilaiSiswa >= 20 then "D"
                else "E"
    putStrLn grade
