module Tarefa3_2022li1g020_Spec where

import LI12223
import Tarefa3_2022li1g020
import Test.HUnit

testsT31 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: (Jogo (Jogador (3,-3)) (Mapa 4 ((Rio -1,(Tronco,Tronco,Tronco,Nenhum)),
(Estrada 1,(Carro,Nenhum,Nenhum,Nenhum)),(Relva,(Nenhum,Arvore,Nenhum,Arvore))))) ~=? animaJogo (Jogo (Jogador (3,-3)) (Mapa 4 ((Rio -1,(Nenhum,Tronco,Tronco,Tronco)),
(Estrada 1,(Nenhum,Nenhum,Nenhum,Carro)),(Relva,(Nenhum,Arvore,Nenhum,Arvore))))) (Parado)]

testsT32 :: Test 
testsT32 = TestLabel "Testes Tarefa 3" $ test ["Teste 2" ~: (Jogo (Jogador (1,-1)) (Mapa 3 ((Rio -1,(Tronco,Tronco,Nenhum)),(Relva,(Nenhum,Arvore,Arvore)))))  ~=? animaJogo 
(Jogo (Jogador (1,-2)) (Mapa 3 ((Rio -1,(Tronco,Nenhum,Tronco)),(Relva,(Nenhum,Arvore,Arvore))))) (Move Cima)]

testsT33 :: Test
testsT33 = TestLabel "Testes Tarefa 3" $ test ["Teste 3" ~: (Jogo (Jogador (1,-1)) (Mapa 3 ((Relva,(Nenhum,Nenhum,Arvore)),(Rio (-1),(Tronco,Tronco,Nenhum)),
(Rio 2,(Nenhum,Tronco,Tronco)),(Relva,(Arvore,Nenhum,Nenhum))))) ~=? animaJogo (Jogo (Jogador (1,-2)) (Mapa 3 ((Relva,(Nenhum,Nenhum,Arvore)),(Rio (-1),(Nenhum,Tronco,Tronco)),
(Rio 2,(Tronco,Tronco,Nenhum)),(Relva,(Arvore,Nenhum,Nenhum))))) (Move Esquerda)]

testsT34 :: Test
testsT34 = TestLabel "Testes Tarefa 3" $ test ["Teste 4" ~: (Jogo (Jogador (2,-1)) (Mapa 3 ((Relva,(Nenhum,Nenhum,Arvore)),(Estrada 1,(Nenhum,Nenhum,Carro)))))  ~=? animaJogo 
(Jogo (Jogador (1,-1)) (Mapa 3 ((Relva,(Nenhum,Nenhum,Arvore)),(Estrada 1,(Nenhum,Carro,Nenhum))))) (Move Direita)]

testsT35 :: Test
testsT35 = TestLabel "Testes Tarefa 3" $ test ["Teste 5" ~: (Jogo (Jogador (1,-2)) (Mapa 3 ((Rio 1,(Nenhum,Tronco,Tronco)),(Relva,(Nenhum,Arvore,Arvore)))))
 ~=? animaJogo (Jogo (Jogador (1,-1)) (Mapa 3 ((Rio 1,(Tronco,Tronco,Nenhum)),(Relva,(Nenhum,Arvore,Arvore)))) (Move Baixo))

testsT3 = runTestTT (TestList [testsT31,
                             testsT32, 
                             testsT33,
                             testsT34,
                             testsT35                            
                            ])

