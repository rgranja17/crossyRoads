module Tarefa1_2022li1g020_Spec where

import LI12223
import Tarefa1_2022li1g020
import Test.HUnit

testsT1_1 :: Test
testsT1_1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: True ~=? mapaValido 
(Mapa 5 [(Rio 3,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),
(Estrada 4,[Carro,Carro,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum]),
(Relva,[Nenhum,Nenhum,Arvore,Arvore,Arvore])])]

testsT1_2 :: Test
testsT1_2 = TestLabel "Testes Tarefa 1" $ test ["Teste 2" ~: True ~=? mapaValido (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),
(Rio (-1),[Tronco,Nenhum,Tronco])])]   

testsT1_3 :: Test
testsT1_3 = TestLabel "Testes Tarefa 1" $ test ["Teste 3" ~: True ~=? mapaValido (Mapa 9 [(Estrada -3,[Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum])])]

testsT1_4 :: Test
testsT1_4 = TestLabel "Testes Tarefa 1" $ test ["Teste 4" ~: True ~=? mapaValido (Mapa 2 [(Estrada 3,[Carro,Nenhum]),(Relva,[Arvore,Nenhum]),(Rio 2,[Troco,Nenhum]),(Rio(-1),[Tronco,Nenhum]),
(Relva,[Nenhum,Arvore]),(Estrada 2,[Nenhum,Carro]),(Estrada (-2),[Carro,Nenhum])])]

testsT1_5 :: Test
testsT1_5 = TestLabel "Testes Tarefa 1" $ test ["Teste 5" ~: False ~=? mapaValido  (Mapa 3 [(Rio 3,[Tronco,Nenhum,Tronco]),
(Rio (-2),[Nenhum,Nenhum,Tronco]), (Rio 1,[Carro,Carro,Nenhum])])]

testsT1_6 :: Test
testsT1_6 = TestLabel "Testes Tarefa 1" $ test ["Teste 6" ~: False ~=? mapaValido  (Mapa 4 [(Rio 1,[Tronco,Tronco,Tronco,Nenhum]),(Rio 2,[Nenhum,Tronco,Tronco,Tronco]), (Rio 5,[Tronco,Nenhum,Nenhum,Tronco]),
(Relva,[Arvore,Arvore,Arvore,Nenhum])])]

testsT1_7 :: Test
testsT1_7 = TestLabel "Testes Tarefa 1" $ test ["Teste 7" ~: False ~=? mapaValido (Mapa 7 [(Rio 3,[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])])]

testsT1_8 :: Test
testsT1_8 = TestLabel "Testes Tarefa 1" $ test ["Teste 8" ~: False ~=? mapaValido (Mapa 5 [(Estrada -2,[Carro,Carro,Carro,Carro,Nenhum])])]

testsT1_9 :: Test
testsT1_9 = TestLabel "Testes Tarefa 1" $ test ["Teste 9" ~: False ~=? mapaValido (Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Rio (-1),[Tronco,Nenhum]), 
(Relva,[Arvore,Arvore]),
(Relva,[Arvore,Nenhum]),(Rio 5,[Nenhum,Tronco])])]

testsT1_10 :: Test
testsT1_10 = TestLabel "Testes Tarefa 1" $ test ["Teste 10" ~: False ~=? mapaValido (Mapa 4 [(Rio 3,[Nenhum,Tronco,Tronco]),(Rio (-1),[Tronco,Nenhum,Tronco]), 
(Estrada 1,[Carro,Nenhum,Nenhum])])] 

testsT1_11 :: Test
testsT1_11 = TestLabel "Testes Tarefa 1" $ test ["Teste 11" ~: False ~=? mapaValido (Mapa 2 [(Rio 2,[Nenhum,Tronco]), (Rio (-3),[Tronco,Nenhum]),
(Rio 2,[Tronco,Nenhum]),(Rio -4,[Tronco,Nenhum]),(Rio 1,[Nenhum,Tronco]),]

testsT1 = runTestTT (TestList [testsT1_1,
                             testsT1_2, 
                             testsT1_3,
                             testsT1_4,
                             testsT1_5,
                             testsT1_6,
                             testsT1_7, 
                             testsT1_8, 
                             testsT1_9, 
                             testsT1_10,
                             testsT1_11                              
                            ])