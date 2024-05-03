module Tarefa2_2022li1g020_Spec where

import LI12223
import Tarefa2_2022li1g020
import Test.HUnit

testsT21 :: Test
testsT21 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: Mapa 3 [(Estrada 0,[Carro,Nenhum,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-2),[Nenhum,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco])] ~=? estendeMapa (Mapa 3 [(Rio 3,[Tronco,Nenhum,Tronco]), (Rio (-2),[Nenhum,Nenhum,Tronco]), (Rio 1,[Tronco,Tronco,Nenhum]), (Rio (-2),[Tronco,Nenhum,Tronco])]) 2]

testsT22 :: Test
testsT22 = TestLabel "Testes Tarefa 2" $ test ["Teste 2" ~: Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco])] ~=? estendeMapa (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]), (Rio (-1),[Tronco,Nenhum,Tronco])]) 3]

testsT23 :: Test
testsT23 = TestLabel "Testes Tarefa 2" $ test ["Teste 3" ~: Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Rio 2,[Nenhum,Tronco]),(Rio (-3),[Tronco,Nenhum]),(Rio 2,[Tronco,Nenhum]),(Rio (-4),[Tronco,Nenhum]),(Rio 1,[Nenhum,Tronco])] ~=? estendeMapa (Mapa 2 [(Rio 2,[Nenhum,Tronco]), (Rio (-3),[Tronco,Nenhum]), (Rio 2,[Tronco,Nenhum]), (Rio (-4),[Tronco,Nenhum]), (Rio 1,[Nenhum,Tronco])]) 56]

testsT24 :: Test
testsT24 = TestLabel "Testes Tarefa 2" $ test ["Teste 4" ~: Mapa 9 [(Rio 0,[Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco]),(Estrada (-3),[Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum])] ~=? estendeMapa (Mapa 9 [(Estrada (-3),[Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum])]) 79]

testsT25 :: Test
testsT25 = TestLabel "Testes Tarefa 2" $ test ["Teste 5" ~: Mapa 4 [(Relva,[Nenhum,Arvore,Arvore,Arvore]),(Rio 1,[Tronco,Tronco,Tronco,Nenhum]),(Rio (-2),[Nenhum,Tronco,Tronco,Tronco]),(Rio 5,[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Arvore,Nenhum])] ~=? estendeMapa (Mapa 4 [(Rio 1,[Tronco,Tronco,Tronco,Nenhum]),(Rio (-2),[Nenhum,Tronco,Tronco,Tronco]), (Rio 5,[Tronco,Nenhum,Nenhum,Tronco]), (Relva,[Arvore,Arvore,Arvore,Nenhum])]) 99]

testsT26 :: Test
testsT26 = TestLabel "Testes Tarefa 2" $ test ["Teste 6" ~: Mapa 3 [(Estrada 0,[Nenhum,Carro,Carro]),(Estrada 2,[Nenhum,Nenhum]),(Rio (-5),[Tronco,Nenhum]),(Rio 4,[Tronco,Tronco]),(Relva,[Arvore,Nenhum])] ~=? estendeMapa (Mapa 3 [(Estrada 2,[Nenhum,Nenhum]), (Rio (-5),[Tronco,Nenhum]), (Rio 4,[Tronco,Tronco]), (Relva,[Arvore,Nenhum])]) 47] 

testsT27 :: Test
testsT27 = TestLabel "Testes Tarefa 2" $ test ["Teste 7" ~: Mapa 4 [(Relva,[Nenhum,Arvore,Arvore,Arvore]),(Rio (-1),[Tronco,Tronco,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Arvore])] ~=? estendeMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Tronco,Nenhum]), (Estrada 1,[Carro,Nenhum,Nenhum,Nenhum]), (Relva,[Nenhum,Arvore,Nenhum,Arvore])]) 12]

testsT2 = runTestTT (TestList [testsT21,
                             testsT22, 
                             testsT23,
                             testsT24,
                             testsT25,
                             testsT26,
                             testsT27                           
                            ])