{- |
Module      : Tarefa3_2022li1g020
Description : Movimentação do personagem e obstáculos
Copyright   : André Filipe Pereira Ribeiro  <a104436@alunos.uminho.pt>
              Rodrigo Miguel Granja Ferreira <a104531@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g020 where

import LI12223
{-|
A função @animaJogo@ 'movimenta' os obstáculos (em função da velocidade) do terreno em que se encontram
e o personagem de acordo com a jogada dada.
-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (a,b)) (Mapa larg ((terreno,obstaculo):t))) movimento = Jogo (Jogador (moveJogador larg (a,b) movimento)) (Mapa larg (movimentoObstaculos ((terreno,obstaculo):t)))

{-|
= @Funções 1@

A função __moveObstaculos__ tem em conta de que numa estrada ou rio com velocidade v, os obstáculos devem mover-se |v| unidades 
na direcção determinada.
No rio, a velocidade do mesmo tem de ser a mesma que a do tronco (obstaculo).
No outro caso, a velocidade da estrada terá que ser a mesma que a do carro.

== Propriedades:
prop> moveObstaculos [] = []
-}
moveObstaculos :: [(Terreno, [Obstaculo])] -> [Int]
moveObstaculos [] = []
moveObstaculos ((Rio v, obs):t) = if v>0 then v : moveObstaculos t else (-1)*v : moveObstaculos t 
moveObstaculos ((Estrada v, obs):t) = if v>0 then v : moveObstaculos t else (-1)*v : moveObstaculos t 

{-|
= @Função 2@

A função __moveJogador__ dita as coordenadas do jogador quando é feita uma jogada pelo jogador. Tendo em conta uma função definida seguidamente.

== Exemplos de utilização:

>>> moveJogador (3,-2) Move Cima
(3,-3)
-}  
moveJogador :: Largura -> Coordenadas -> Jogada -> Coordenadas
moveJogador larg (a,b) (Move x) | x == Cima = if maybeMove larg (a,b) (Move Cima) == True then (a,b+1) else (a,b)
                                | x == Baixo = if maybeMove larg (a,b) (Move Baixo) == True then (a,b-1) else (a,b)
                                | x == Direita = if maybeMove larg (a,b) (Move Direita) == True then (a+1,b) else (a,b)
                                | x == Esquerda = if maybeMove larg (a,b) (Move Esquerda) == True then (a-1,b) else (a,b)
moveJogador larg (a,b) Parado = (a,b)
{-|
= @Função 3@
A função __movimentoNoTronco__ terá que ter a mesma velocidade do rio e do tronco. Este terá de ir para o fim da lista até atingir 
as coordenadas (0,0).

== Exemplos de utilização:

>>> movimentoNoTronco (Jogador (2,-2)) (Mapa 10 [(Rio 2, [Nenhum])]) 
Jogador (2,-2)
-}
movimentoNoTronco :: Jogador -> Mapa -> Jogador 
movimentoNoTronco (Jogador (a,b)) (Mapa larg ((Rio v,obs):t)) | b == 0 && obs !! a == Tronco = Jogador (a+v,b)   
                                                              | otherwise = Jogador (a,b)

{-|
= @Função 4@
A função __maybeMove__ tendo a largura do mapa quando o personagem se encontra nas extremidades do mapa, não sofre qualquer efeito na sua posição.
Esta função é importante para o movimento do Jogador.

== Exemplos de utilização:

>>> maybeMove 10 (2,-2) (Move Baixo)
True
-}
maybeMove :: Largura -> Coordenadas -> Jogada -> Bool
maybeMove larg (a,b) (Move x) | a == larg && x == Direita = False
                              | b == larg && x == Baixo = False
                              | a == larg && b /= larg && x == Direita = False
                              | a == 0 && b /= larg && x == Esquerda = False
                              | b == 0 && x == Cima = False
                              | a /= larg && b == 0 && x == Cima = False
                              | a /= larg && b == larg && x == Baixo = False
                              | otherwise = True

{-|
= @Função 5@
A função __movimentosObstaculos__ ao deslocar os obstáculos de uma linha, temos de ter em conta que ao desaparecerem por um dos lados do mapa
devem reaparecer no lado oposto.

== Exemplos de utilização:

>>> movimentoObstaculos 10 [Nenhum,Tronco,Nenhum]
[Nenhum,Nenhum,Tronco]

>>> movimentoObstaculos -10 [Nenhum,Tronco,Tronco]
[Tronco,Tronco,Nenhum]

== Propriedades:

prop> movimentoObstaculos [] = [] 
-}
movimentoObstaculos :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
movimentoObstaculos [] = [] 
movimentoObstaculos ((Relva,obstaculo):t) = (Relva,obstaculo) : movimentoObstaculos t
movimentoObstaculos ((Rio v,obstaculo):t) | v > 0 = (Rio v,movimentoDireita v obstaculo) : movimentoObstaculos t
                                          | otherwise = (Rio v, movimentoEsquerda v obstaculo) : movimentoObstaculos t
movimentoObstaculos ((Estrada v,obstaculo):t)   | v > 0 = (Estrada v,movimentoDireita v obstaculo) : movimentoObstaculos t
                                                | otherwise = (Estrada v,movimentoEsquerda v obstaculo) : movimentoObstaculos t                                            

movimentoEsquerda :: Int -> [Obstaculo] -> [Obstaculo]
movimentoEsquerda 0 obstaculo = obstaculo
movimentoEsquerda a obstaculo = movimentoEsquerda (a+1) (tail obstaculo ++ [head obstaculo])

movimentoDireita :: Int -> [Obstaculo] -> [Obstaculo]
movimentoDireita 0 obstaculo = obstaculo
movimentoDireita a obstaculo = movimentoDireita (a-1) (last obstaculo : init obstaculo)


                                



