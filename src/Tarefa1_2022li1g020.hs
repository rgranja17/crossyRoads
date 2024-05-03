{- |
Module      : Tarefa1_2022li1g020
Description : Validação de um mapa
Copyright   : André Filipe Pereira Ribeiro  <a104436@alunos.uminho.pt>
              Rodrigo Miguel Granja Ferreira <a104531@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g020 where

import LI12223
{- |
= __Função Principal__

A função __mapaValido__ verifica se um certo mapa não viola certas restrições, que são invocadas através do uso de funções auxiliares.

=== Exemplos de utilização:

>>>mapaValido (Mapa 2 ((Rio,(Nenhum,Tronco)),(Relva,(Nenhum,Nenhum))))

True

=== Propriedades:

mapaValido (Mapa larg []) = []
-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa larg []) = True
mapaValido (Mapa larg ((terreno,(x:y)):t)) = mapaValido1 (Mapa larg ((terreno,(x:y)):t)) && mapaValido2 (Mapa larg ((terreno,(x:y)):t)) && mapaValido3 (Mapa larg ((terreno,(x:y)):t)) && mapaValido4 (Mapa larg ((terreno,(x:y)):t)) && mapaValido5 (Mapa larg ((terreno,(x:y)):t)) && mapaValido6 (Mapa larg ((terreno,(x:y)):t)) && mapaValido7 (Mapa larg ((terreno,(x:y)):t))&& mapaValido7 (Mapa larg ((terreno,(x:y)):t)) && mapaValido8 larg ((terreno,(x:y)):t)
                                       
{- |
= __Funções Auxiliares__

== Função Auxiliar 1

A função auxiliar __mapaValido1__ verifica se não existem obstáculos em terrenos impróprios, como por exemplo, troncos em estradas ou relvas,  ́arvores em rios ou estradas e carros em relvas e rios.

=== Exemplos de utilização:

>>> mapaValido1 (Mapa 2 (Relva,(Nenhum,Carro)))
False   

>>> mapaValido1 (Mapa 2 (Rio,(Tronco,Nenhum)))
True

=== Propriedades:


mapaValido1 (Mapa 3 []) = True
-}
mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa larg []) = True
mapaValido1 (Mapa larg ((Relva,(x:y)):t)) = if elem Tronco (x:y) || elem Carro (x:y) then False else mapaValido1 (Mapa larg t)
mapaValido1 (Mapa larg ((Rio v,(x:y)):t)) = if elem Arvore (x:y) || elem Carro (x:y) then False else mapaValido1 (Mapa larg t)
mapaValido1 (Mapa larg ((Estrada v,(x:y)):t)) = if elem Arvore (x:y) || elem Tronco (x:y) then False else mapaValido1 (Mapa larg t)

{- |
== Função Auxiliar 2

A função auxiliar __mapaValido2__ verifica se Rios contíguos têm direções opostas, ou seja, rios que apareçam seguidos não podem apresentar a mesma direção. Neste caso utilizamos outra
função (/mapaAuxRios/) para quando dois terrenos seguidos sejam iguais, esta descarte se não forem rios e se forem execute.se de forma a que a multiplicação das suas velocidades seja menor que zero.
Ao dizer isto, consideramos uma velocidade __negativa__ e outra __positiva__, o que faz com que os rios apresentem direções contrárias.

=== Exemplos de utilização:

>>> mapaValido2 (Mapa 2 (Relva,(Nenhum,Carro),(Rio 3,(Nenhum,Tronco))))
True 

>>> mapaValido2 (Mapa 2 (Rio 2,(Tronco,Nenhum)),(Rio 2,(Tronco,Nenhum)))
False

=== Propriedades:

mapaValido2 (Mapa 3 []) = True
-}
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa l (h:[])) = True
mapaValido2 (Mapa l ((Rio x,l1): (Rio y,l2) : t )) = ((x 

mapaValido5 (Mapa largura []) = True
-}


mapaValido5 :: Mapa -> Bool> 0 && y < 0) || (x < 0 && y>0)) && mapaValido2 (Mapa l ((Rio y,l2):t))
mapaValido2 (Mapa l ((_,_):t)) = mapaValido2 (Mapa l t)
{- |
== Função Auxiliar 3

A função auxiliar __mapaValido3__ verifica se quando estamos num rio, troncos têm, no máximo, 5 unidades de comprimento. Para isto acontecer usamos uma função auxiliar (/mapaAuxTronco) que
verifica, quando o rio tem mais do que 5 unidades de comprimento, se existe um tronco maior do que 5, ou seja, que mais de 5 obstáculos seguidos sejam "Tronco".

=== Exemplos de utilização:

>>> mapaValido3 (Mapa 3 (Rio 3,(Tronco,Tronco,Nenhum)))
True

>>> mapaValido3 (Mapa 7 (Rio 5,(Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum)))
False

=== Propriedades:

mapaValido3 (Mapa largura (Rio 5,[]) = True
-}
mapaValido3 :: Mapa -> Bool 
mapaValido3 (Mapa k ((Rio v, (x:xs)) : [])) = auxiliar3aux (x:xs) 0
mapaValido3 (Mapa k ((Rio v, (x:xs)) : t)) = auxiliar3aux (x:xs) 0 && mapaValido3 (Mapa k t)
mapaValido3 (Mapa k (_ : t)) = mapaValido3 (Mapa k t)
mapaValido3 (Mapa k []) = True 


auxiliar3aux :: [Obstaculo] -> Int -> Bool
auxiliar3aux [] y = y <= 5 
auxiliar3aux (x:xs) y = if y > 5 then False else if x == Tronco then auxiliar3aux xs (y+1)
                        else auxiliar3aux xs 0             
                            
{- |
== Função Auxiliar 4

A função auxiliar __mapaValido4__ funciona quase da mesma forma que a função anterior, só que neste caso serve para verificar se os carros não tem mais de 3 unidades de comprimento.
Para isso usamos uma função auxiliar (/mapaAuxCarros/) que, quando a estrada tiver mais de 3 unidades de comprimento, vê se existem mais de 3 obstáculos seguido com o nome "Carro".

=== Exemplos de utilização:

>>> mapaValido4 (Mapa 3 (Estrada 3,(Carro,Carro,Nenhum)))
True

>>> mapaValido4 (Mapa 5 (Rio 6,(Carro,Carro,Carro,Carro,Nenhum,Nenhum)))
False

=== Propriedades:

mapaValido4 (Mapa largura (Estrada 3,[]) = True
-}
mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa l ((Estrada v, (x:y)) : [])) = mapaValido4aux (x:y) 0
mapaValido4 (Mapa l ((Estrada v, (x:y)) : t)) = mapaValido4aux (x:y) 0 && mapaValido4 (Mapa l t)
mapaValido4 (Mapa l (_:t)) = mapaValido4 (Mapa l t)
mapaValido4 (Mapa l []) = True

mapaValido4aux :: [Obstaculo] -> Int -> Bool
mapaValido4aux [] y = y <= 3 
mapaValido4aux (c:cs) y = if y>3 then False else if c == Carro then mapaValido4aux cs (y+1)
                                                             else mapaValido4aux cs 0

                     
{- |
== Função Auxiliar 5

A função auxiliar __mapaValido5__ observamos se existe pelo menos em cada linha, um obstáculo "Nenhum". Isto serve para o jogador poder atravessar essa mesma linha enquanto decorre o jogo.

=== Exemplos de utilização:

>>> mapaValido5 (Mapa 3 (Estrada -1,(Carro,Carro,Nenhum)))
True

>>> mapaValido5 (Mapa 4 (Estrada 3,(Carro,Carro,Carro,Carro,)))
False

=== Propriedades:

mapaValido5 (Mapa largura []) = True
-}


mapaValido5 :: Mapa -> Bool
mapaValido5 (Mapa larg []) = True
mapaValido5 (Mapa larg ((ter,(x:y)):t)) | elem Nenhum (x:y) == False = False
                                        | otherwise = mapaValido5 (Mapa larg t)

{- |
== Função Auxiliar 6

A função auxiliar __mapaValido6__ faz com que o comprimento da lista de obstáculos corresponda exatamente à largura do mapa. 
=== Exemplos de utilização:

>>> mapaValido6 (Mapa 3 (Estrada 3,(Carro,Carro,Nenhum)))
True

>>> mapaValido6 (Mapa 5 (Rio -2,(Tronco,Nenhum,Nenhum)))
False

=== Propriedades:

mapaValido6 (Mapa largura []) = True
-}
mapaValido6 :: Mapa -> Bool
mapaValido6 (Mapa larg []) = True
mapaValido6 (Mapa larg ((ter,(x:y)):t)) | larg /= length (x:y) = False
                                        | otherwise = mapaValido6 (Mapa larg t)
                                        
{- |
== Função Auxiliar 7

A função auxiliar __mapaValido7__ existe para verificar se não existem de forma seguida no mapa, mais do que 4 rios, nem 5 estradas ou relvas.

=== Exemplos de utilização:

>>> mapaValido7 (Mapa 3 (Estrada 3,(Carro,Carro,Nenhum),(Estrada 3,(Carro,Carro,Nenhum),(Rio 6,(Nenhum,Nenhum,Tronco)))
True

>>> mapaValido7 (Mapa 2 (Rio 6,(´Tronco,Nenhum),(Rio -3,(Tronco,Nenhum),(Rio 4,(Tronco,Nenhum),(Rio -2,(Tronco,Nenhum),(Rio 3,(Tronco,Nenhum)))
False

=== Propriedades:

mapaValido7 (Mapa largura []) = True
-}

mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa larg []) = True
mapaValido7 (Mapa larg ((Rio v,(x:y)):t)) 
                                            | length ((Rio v, (x:y)):t) > 4 = False
                                            | otherwise = True                             
mapaValido7 (Mapa larg ((Estrada v, (x:y)):t)) 
                                            | length ((Estrada v, (x:y)):t) > 5 = False
                                            | otherwise = True
mapaValido7 (Mapa larg ((Relva,(x:y)):t)) 
                                            | length ((Relva,(x:y)):t) > 5 = False
                                            | otherwise = True


mapaValido8 :: Largura -> [(Terreno,[Obstaculo])] -> Bool
mapaValido8 k t = mapaValido8' t 
            where mapaValido8' [] = True
                  mapaValido8' ((_,obs):ts) = contaObs obs <= 4 && mapaValido8' ts 
                  contaObs obs = length [o | o <- obs, o /= Nenhum]




                                       
