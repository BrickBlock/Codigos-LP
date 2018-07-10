type Pos = (Int,Int)  -- posição (coluna, linha)

type Sol = [Pos] -- uma solução é uma lista de posições

-- testar se duas rainhas se atacam
cheque :: Pos -> Pos -> Bool
cheque (x,y) (x',y') = x==x' ||   -- coluna (teste desnecessário)
                       y==y' ||   -- linha
                       x-y == x'-y' || -- diagonais
                       x+y == x'+y'
                      
-- testar segurança de uma nova posição 
-- (não ataca qualquer rainha já colocada)
segura :: Sol -> Pos -> Bool
segura s p = and [not (cheque p p') | p'<-s]

-- procurar todas as soluções n rainhas
-- caso base: uma solução vazia
-- caso recursivo: acrescentar uma rainha a cada solução menor 
-- e testar segurança
rainhas :: Int -> [Sol]
rainhas 0       = [[]] 
rainhas n | n>0 = [(n,y):s | s<-rainhas (n-1), y<-[1..8], segura s (n,y)]

-- imprimir uma solução 
printSol :: Sol -> IO ()
printSol s = sequence_ (map putStrLn m)
  where m = [concat [symbol x y | x<-[1..8]] | y<-[1..8]]
        symbol x y | (x,y)`elem`s = "x" -- unicode "BLACK CHESS QUEEN"
                   | otherwise    = ". "     -- vazio


-- procurar e imprimir todas as soluções
main :: IO ()
main = sequence_ [ do { putStrLn ("Solução "++show i)
                      ; printSol s
                      ; putChar '\n' }
                 | (i,s)<-zip [1..] (rainhas 8)]