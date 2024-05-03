module Tarefa4_2022li1g020_Spec where

import LI12223
import Tarefa4_2022li1g020
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: False ~=? jogoTerminou (Jogo (Jogador (2,-3)) (Mapa 10 [(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])]))]

testsT41 :: Test
testsT41 = TestLabel "Testes Tarefa 4" $ test ["Teste 2"  ~: True ~=? jogoTerminou (Jogo (Jogador (-1,-3)) (Mapa 10 [(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])]))]

testsT42 ::  Test
testsT42 = TestLabel "Testes Tarefa 4" $ test ["Teste 3" ~: True ~=? jogoTerminou (Jogo (Jogador (0,-3)) (Mapa 10 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Nenhum])]))]

testsT43 :: Test
testsT43 = TestLabel "Testes Tarefa 4" $ test ["Teste 4" ~: True ~=? jogoTerminou (Jogo (Jogador (0,-1)) (Mapa 10 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Carro])]))]

testesT44 :: Test
testsT44 = TestLabel "Testes Tarefa 4" $ test ["Teste 5" ~: True ~=? jogoTerminou (Jogo (Jogador (1,3)) (Mapa 10 [(Estrada 2,[Carro,Nenhum,Carro])]))]

todos = runTestTT (TestList [testsT4,
                             testsT42, 
                             testsT43,
                             testsT44,                              
                            ])
