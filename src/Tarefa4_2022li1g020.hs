{- |
Module      : Tarefa4_2022li1g020
Description : Determinar se o jogo terminou
Copyright   : André Filipe Pereira Ribeiro  <a104436@alunos.uminho.pt>
              Rodrigo Miguel Granja Ferreira <a104531@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g020 where

import LI12223
{-| 
= Função Principal

A função __jogoTerminou__ que serve para ver se o jogador perdeu, onde o resultado /True/ significa que sim. Para isso usamos
funções auxiliares para ver se o jogador se encontra fora do mapa, na água, ou por baixo de um carro, isto é, se encontre na 
mesma posição de um carro.

=== Exemplos de utilização:

>>> jogoTerminou (Jogo (Jogador (2,-3)) (Mapa 10 [(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])]))
False

=== Propriedades:

>>> jogoTerminou (Jogo (Jogador (x,y)) (Mapa larg [])) = False
-}
jogoTerminou :: Jogo -> Bool  
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) = morreste (Jogador (x,y)) t
                                             || foramapa l (Jogador (x,y)) t
                                             || atropelado (Jogador (x,y)) (Mapa l t)

{-|
O jogo termina se o jogador morreu, se o jogador saiu do mapa ou se o jogador foi atropelado por um veículo. 
Essas condições são verificadas pelas funções __morreste__, __foramapa__ e __atropelado__, respectivamente.

A função `morreste` recebe um jogador Jogador e a lista de linhas do mapa t, e retorna um valor booleano indicando se o jogador morreu ou não. 
Ela faz isso verificando se o jogador está na água (se estiver, ele morre) ou se ele está em uma linha de estrada e há um obstáculo (carro) na mesma posição do jogador.
-}

morreste :: Jogador -> [(Terreno, [Obstaculo])] -> Bool
morreste (Jogador (x, y)) t = if morreste1 (Jogador (x,y)) h then True else False
                              where h = (!!) t (abs y)

morreste1 :: Jogador -> (Terreno, [Obstaculo]) -> Bool
morreste1 (Jogador (x, y)) (Rio z, obs) = if elem x xs then True else False
                              where xs = xNenhum4 Nenhum obs  
morreste1 (Jogador (x, y)) (Estrada z, obs) = if elem x ss then True else False
                              where ss = xNenhum4 Carro obs
morreste1 (Jogador (x, y)) (Relva, _) = False

{-|
A função foramapa recebe a largura do mapa l, o jogador Jogador e a lista de linhas do mapa t, e retorna um valor booleano indicando se o jogador saiu do mapa ou não.

A função atropelado recebe o jogador Jogador e o mapa Mapa, e retorna um valor booleano indicando se o jogador foi atropelado ou não. 
Ela faz isso verificando se o jogador está em uma linha de estrada e há um veículo se movendo na mesma direção e passando pelo jogador.

As funções xNenhum4 e xNenhum4aux são auxiliares na verificação de se o jogador morreu ou foi atropelado. A função xNenhum4 recebe um tipo de obstáculo Obstaculo e uma lista de obstáculos [Obstaculo], e retorna uma lista de inteiros indicando as posições na lista de obstáculos em que o obstáculo especificado aparece. 
A função xNenhum4aux é uma função recursiva auxiliar de xNenhum4, que percorre a lista de obstáculos ver
-}
xNenhum4 :: Obstaculo -> [Obstaculo] -> [Int]
xNenhum4 o l = xNenhum4aux o l 0

xNenhum4aux :: Obstaculo -> [Obstaculo] -> Int -> [Int]
xNenhum4aux _ [] _ = []
xNenhum4aux o (h:t) i
    | o == h = i : xNenhum4aux o t (i+1)
    | otherwise = xNenhum4aux o t (i+1)

foramapa :: Largura -> Jogador -> [(Terreno, [Obstaculo])] -> Bool  
foramapa l (Jogador (x, y)) t | y <= 0 = if x < 0 || y>0 || x >= l || (abs y) >= a then True else False
                                      where a = length t

atropelado :: Jogador -> Mapa -> Bool  
atropelado (Jogador (x,y)) (Mapa l t) = atropeladoaux (Jogador (x,y)) linha  
                                        where linha = (!!) t (abs y)

atropeladoaux :: Jogador -> (Terreno, [Obstaculo]) -> Bool
atropeladoaux (Jogador (x, y)) (Estrada v, obs) = testa x v i
                       where i = xNenhum4 Carro obs

atropeladoaux (Jogador (x,y)) (_, obs) = False  

testa :: Int -> Int -> [Int] -> Bool
testa x v [] = False
testa x v (c:i) = if v > 0 && c <= x && (c+v) >= x then True else
                  if v < 0 && c >= x && (c+v) <= x then True else testa x v i
{-|
As funções move4 e movelinha4 são responsáveis por mover os obstáculos (veículos) do mapa. A função move4 recebe um jogador Jogador e uma jogada Jogada e retorna um novo jogador resultante da aplicação da jogada ao jogador. 
A função movelinha4 recebe um mapa Mapa e retorna um novo mapa com os obstáculos das linhas do mapa movidos de acordo com as respectivas velocidades.
-}


move4 :: Jogador -> Jogada -> Jogador
move4 (Jogador (x, y)) Parado = (Jogador (x, y))
move4 (Jogador (x, y)) (Move Cima) = if y == 0 then (Jogador (x, y)) else (Jogador (x, y+1))
move4 (Jogador (x, y)) (Move Baixo) = (Jogador (x, y-1))
move4 (Jogador (x, y)) (Move Direita) = (Jogador (x+1, y))
move4 (Jogador (x, y)) (Move Esquerda) = (Jogador (x-1, y))


movelinha4 :: Mapa -> Mapa
movelinha4 (Mapa l ((terr, obs) :t)) = Mapa l (movelinhaaux4 ((terr, obs) : t))

movelinhaaux4 :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
movelinhaaux4 [(Rio x, obs)] = if x > 0 then [(Rio x, dl)] else [(Rio x, el)]
                          where dl = deslocaDireita x obs
                                el = deslocaEsq (abs x) obs
movelinhaaux4 ((Rio x, obs) : t) = if x > 0 then ((Rio x, dl) : recurs) else ((Rio x, el) :recurs)
                           where dl = deslocaDireita x obs
                                 recurs = movelinhaaux4 t
                                 el = deslocaEsq (abs x) obs
movelinhaaux4 [(Estrada y, obs)] = if y > 0 then [(Estrada y, dl)] else [(Estrada y, el)]
                          where dl = deslocaDireita y obs
                                el = deslocaEsq (abs y) obs
movelinhaaux4 ((Estrada y, obs) : t) = if y > 0 then ((Estrada y, dl) : recurs) else ((Estrada y, el) :recurs)
                           where dl = deslocaDireita y obs
                                 recurs = movelinhaaux4 t
                                 el = deslocaEsq (abs y) obs
movelinhaaux4 [h] = [h]
movelinhaaux4 (h : t) = h : recurs
                    where recurs = movelinhaaux4 t

deslocaEsq :: Int -> [a] -> [a]
deslocaEsq n l = let a = mod n (length l)
                 in drop a l ++ take a l

deslocaDireita :: Int -> [a] -> [a]
deslocaDireita n l = drop ((length l) - a) l ++ take ((length l) - a) l
                     where a = mod n (length l)