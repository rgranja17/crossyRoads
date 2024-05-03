{- |
Module      : Tarefa2_2022li1g020
Description : Geração contínua de um mapa
Copyright   : André Filipe Pereira Ribeiro  <a104436@alunos.uminho.pt>
              Rodrigo Miguel Granja Ferreira <a104531@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g020 where

import LI12223
import Tarefa1_2022li1g020
import System.Random

{- |
= @Função principal@

A função estendeMapa deve gerar e adicionar uma nova linha válida ao topo de um dado mapa. Para isso é necessário utilizar funções auxiliares.

=== Propriedades:
estendeMapa m 0 = m 

-}
estendeMapa :: Mapa -> Int -> Mapa 
estendeMapa (Mapa l t) i = if mapaValido (estendeMapa1 (Mapa l t) i)
                           then (estendeMapa1 (Mapa l t) i)
                           else (estendeMapa1 (Mapa l t) (i+1))
{-|
a estendeMapa1 recebe o mapa atual e o inteiro (i) como argumentos. 
Ela usa a função "proximosTerrenosValidos" para gerar uma lista de opções de terrenos válidos para o próximo elemento do mapa. 
Ela seleciona um desses terrenos aleatoriamente usando "mod n (length terrenosOpcoes)" e adiciona-o ao mapa. 
Ela também usa a função "criaobs" para gerar uma lista de obstáculos válidos para o terreno selecionado e adiciona-os ao mapa.
-}

estendeMapa1 :: Mapa -> Int -> Mapa
estendeMapa1 (Mapa l (((h,t)):x)) n = Mapa l ((s,m):(h,t):x)
                                        where aleatorios = numeroRandom n l 
                                              terrenosOpcoes = proximosTerrenosValidos (Mapa l ((h,t):x))
                                              s = terrenosOpcoes !! mod n (length terrenosOpcoes)
                                              m = criaobs ((Mapa l [(s,[])])) n 
{-|
A função "criaobs" é responsável por gerar uma lista de obstáculos válidos para o terreno selecionado. Ela usa a função "proximosObstaculosValidos" para gerar uma lista de opções de obstáculos válidos. 
Ela seleciona um obstáculo aleatoriamente usando "mod (aleatorios !! 0) (length obstaculosOpcoes)" e adiciona-o à lista de obstáculos. 
-}

criaobs :: Mapa -> Int -> [Obstaculo]
criaobs (Mapa l [(terreno,obstaculo)]) n | length obstaculo < l = criaobs (Mapa l [(terreno,obstaculo ++ [o])]) n
                                         | otherwise = obstaculo
                                    where obstaculosOpcoes = proximosObstaculosValidos l (terreno,obstaculo)
                                          o = obstaculosOpcoes !! mod (aleatorios !! 0) (length obstaculosOpcoes)       
                                          aleatorios = numeroRandom n l 
{-|
A função __numeroRandom__ a partir de deois números inteiros vai gerar números grandes aleatórios que vão contribuir na expansão do mapa que será
dado e que constitui o jogo.
-}

numeroRandom :: Int -> Int -> [Int]
numeroRandom s n = take n (randoms (mkStdGen s))

{-| 
= __Funções Auxiliares__

== Função Auxiliar 1

A função __proximosTerrenosValidos__ deve gerar a lista de terrenos passíveis de serem usados numa nova linha no topo do mapa dado. 
Assumimos para esta funçao que a velocidade é 0.
Utilizamos uma função auxiliar por acumuladores, pois temos de ter em conta o aspeto de mais do que 4 rios, nem 5 estradas
ou relvas. 

=== Exemplos de utilização:

>>> proximosTerrenosValidos (Mapa 2 [(Rio 0,[Nenhum,Arvore,Carro])])
[Rio 0,Estrada 0,Relva]

>>> proximosTerrenosValidos (Mapa _ [(Rio _, _),(Rio _, _),(Rio _, _),(Rio _, _),_]) 
[Estrada 0, Relva]
-}

proximosTerrenosValidos :: Mapa -> [Terreno]

proximosTerrenosValidos (Mapa _ []) = [Relva, Rio 0, Estrada 0]

proximosTerrenosValidos (Mapa _ [(Estrada v1, obs1),(Estrada v2,obs2),(Estrada v3,obs3),(Estrada v4,obs4),(Estrada v5,obs5)]) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa _ [(terreno,obstaculo),(Estrada v1,obs1),(Estrada v2,obs2),(Estrada v3,obs3),(Estrada v4,obs4),(Estrada v5,obs5)]) = [Rio 0,Relva]

proximosTerrenosValidos (Mapa _ [(Rio v1,obs1),(Rio v2,obs2),(Rio v3,obs3),(Rio v4,obs4)]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ [(terreno,obstaculo),(Rio v1,obs1),(Rio v2,obs2),(Rio v3,obs3),(Rio v4,obs4)]) = [Estrada 0,Relva]

proximosTerrenosValidos (Mapa _ [(Relva,obs1),(Relva,obs2),(Relva,obs3),(Relva,obs4),(Relva,obs5)]) = [Rio 0,Estrada 0]
proximosTerrenosValidos (Mapa _ [(terreno,obstaculo),(Relva,obs1),(Relva,obs2),(Relva,obs3),(Relva,obs4),(Relva,obs5)]) = [Rio 0,Estrada 0]

proximosTerrenosValidos (Mapa _ _) = [Relva,Rio 0,Estrada 0]

{-|
== __Função Auxiliar 2__

A função __proximosObstaculosValidos__ deve gerar a lista de obstaculos passiveis de serem usados para continuar uma dada linha do mapa.

Nesta função, deve ter em atenção não só os obstáculos permitidos no tipo de terreno indicado como as regras a respeito 
do comprimento dos obstaculos.

=== Exemplos de utilização:

>>> proximosObstaculosValidos (Mapa 2 [(Estrada 3,[Carro,Nenhum,Arvore,Nenhum])])
False

>>> proximosObstaculosValidos (Mapa 3 [(Rio 2,[Carro,Nenhum,Arvore,Nenhum])])
False

=== Propriedades:
prop> proximosObstaculosValidos a [] = []
-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]

proximosObstaculosValidos l (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos l (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos l (Relva, []) = [Nenhum, Arvore]

proximosObstaculosValidos l (terreno, (h:t)) | observaTerreno (terreno) == 1 && length (h:t) < l = contaTroncos (h:t) 0
                                             | observaTerreno (terreno) == 2 && length (h:t) < l = contaCarros (h:t) 0
                                             | observaTerreno (terreno) == 3 && length (h:t) < l = [Arvore,Nenhum]
                                             | length (h:t) >= l = []

{-|
A função "observaTerreno" recebe um terreno e devolve um inteiro, que é utilizado para substituir o nome do terreno presente.
-}
observaTerreno :: Terreno -> Int 
observaTerreno (Rio _) = 1 
observaTerreno (Estrada _) = 2
observaTerreno (Relva ) = 3

{-|
As funções __contaTroncos__ e __contaCarros__ recebem uma lista de obstaculos e um número inteiro até atingit o limite máximo possível (5 e 3 respetivmente).
O número dado vai aumentando se encontrar esse obstaculo.
-}
contaTroncos :: [Obstaculo] -> Int -> [Obstaculo]
contaTroncos (h:t) x | h == Tronco = contaTroncos t (x+1)
                     | otherwise = contaTroncos t 0 
contaTroncos [] x | x>=5 = [Nenhum]
                  | x < 5 = [Tronco,Nenhum]


contaCarros :: [Obstaculo] -> Int -> [Obstaculo]
contaCarros (h:t) x | h == Carro = contaCarros t (x+1)
                    | otherwise = contaCarros t 0
contaCarros [] x | x >= 3 = [Nenhum]
                 | x < 3 = [Carro,Nenhum]


