
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C    --- Loop E C: Executa E vezes o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = (s,n):xs
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------


ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s)                  = procuraVar s x
ebigStep (Num n,s)                  = n
ebigStep (Soma e1 e2,s)             = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)              = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)             = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep(Div e1 e2,s)               = div (ebigStep (e1,s)) (ebigStep (e2,s))


bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)                   = True
bbigStep (FALSE,s)                  = False
bbigStep (Not b,s)
   | bbigStep (b,s)                 = False
   | otherwise                      = True
bbigStep (And b1 b2,s )
   | bbigStep (b1, s)               = bbigStep (b2, s)
   | otherwise                      = False
bbigStep (Or b1 b2,s )
   | bbigStep (b1, s)               = True
   | otherwise                      = bbigStep (b2, s)
bbigStep (Leq e1 e2,s)              = ebigStep (e1, s) <= ebigStep (e2, s)
bbigStep (Igual e1 e2,s)            = ebigStep (e1, s) == ebigStep (e2, s)


cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
   | bbigStep (b,s)                 = cbigStep (c1, s)
   | otherwise                      = cbigStep (c2, s)
cbigStep (Unless b c1 c2, s)                                               --- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
   | bbigStep (b, s)                = cbigStep (c2, s)
   | otherwise                      = cbigStep (c1, s)
cbigStep (Seq c1 c2,s)              = let (_, s2) = cbigStep (c1, s) in cbigStep (c2, s2)
cbigStep (Atrib (Var x) e,s)        = (Skip, mudaVar s x (ebigStep (e, s)))
cbigStep (While b c,s)
   | bbigStep (b, s)                = cbigStep (Seq c (While b c), s)
   | otherwise                      = (Skip, s)
cbigStep (DoWhile c b,s)            = cbigStep (Seq c (While b c), s)      --- Repete C enquanto  B seja verdadeiro
cbigStep (Loop e c,s)                                                      --- Repete E vezes o comando C
   | bbigStep (Leq e (Num 0), s)     = (Skip, s)
   | otherwise                      = cbigStep (Seq c (Loop (Sub e (Num 1)) c), s)
cbigStep (Swap (Var x) (Var y),s)   =                                      --- Recebe duas variáveis e troca o conteúdo delas
   let
      novoX = mudaVar s x (procuraVar s x)
      novoXY = mudaVar novoX y (procuraVar s y)
   in (Skip, novoXY)
cbigStep (DAtrrib (Var x) (Var y) e1 e2,s) = cbigStep (Seq (Atrib (Var x) e1) (Atrib (Var y) e2), s)   --- Dupla atribuição: recebe duas variáveis x e y e duas expressões "e1" e "e2". Faz x:=e1 e y:=e2.

--------------------------------------
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------
---
--- Exemplos de programas para teste
---

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

--- Exemplos de expressões booleanas:

teste1 :: B
teste1 = Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3))

teste2 :: B
teste2 = Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3))

-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z"))

fatorial :: C
fatorial = Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1)))))

-- **************************************** Exemplos - Henrique **************************************** --

-- Começando com um clássico: o querido Fibonnaci.
sigmaFib :: Memoria
sigmaFib = [("x", 0), ("y", 0), ("fib", 20), ("fib_res", 0)]

fibonacci :: C
fibonacci = If (Leq (Var "fib") (Num 1)) 
               
               Skip
               
               (Seq
                  (DAtrrib (Var "x") (Var "y") (Num 0) (Num 1))

                  (DoWhile 
                     (Seq 
                        (Atrib (Var "fib_res") (Soma (Var "x") (Var "y")))
                        (Seq
                           (DAtrrib (Var "x") (Var "y") (Var "y") (Var "fib_res"))
                           (Atrib (Var "fib") (Sub (Var "fib") (Num 1)))
                        )
                     )
                     (Leq (Num 2) (Var "fib"))
                  )
               )

-- Aqui temos, um mais simples. Fazemos a soma dos n primeiros números pares dos números naturais. Isso inclui o zero como par.

sigmaSomaN :: Memoria
sigmaSomaN = [("n", 5), ("num", 0), ("soma", 0)]

somaN :: C
somaN = Seq
            (DAtrrib (Var "soma") (Var "num") (Num 0) (Num 0))
            (Loop (Var "n")
               (Seq
                  (Atrib (Var "soma") (Soma (Var "soma") (Var "num")))     -- Calcula a soma
                  (Atrib (Var "num") (Soma (Var "num") (Num 2)))           -- Calcula o próximo número para ser somado
               )
            )

-- Este programa calcula o resto da divisão inteira usando subtrações sucessivas

sigmaMod :: Memoria
sigmaMod = [("val", 10), ("divisor", 2), ("mod", 0)]

modCalculator = Seq
         (Atrib (Var "mod") (Var "val"))

         (If (Igual (Var "divisor") (Num 0)) 
            Skip 
            
            (
               Loop (Var "val")
                  (
                     If (Leq (Var "divisor") (Var "mod"))
                        (Atrib (Var "mod") (Sub (Var "mod") (Var "divisor")))

                        Skip
                  )
            )
         )