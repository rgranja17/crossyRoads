{- |
Module      : Tarefa5_2022li1g020
Description : Processamento do deslize do mapa
Copyright   : André Filipe Pereira Ribeiro  <a104436@alunos.uminho.pt>
              Rodrigo Miguel Granja Ferreira <a104531@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g020 where

import LI12223
import Tarefa2_2022li1g020
import Tarefa1_2022li1g020

{- |
= __Função Principal__

A função __deslizaJogo__ faz com que a última linha do mapa desapareça e apareça uma nova linha na parte superior do mapa, de forma de continuar o jogo. Desta maneira, o mapa mantêm o tamanho original.
A nova linha adicionada é criada de uma forma aleatória, através do número inteiro apresentado, que passará pela função __estendeMapa__ anteriormente definida.
Temos também de ter atenção à adição de uma unidade no eixo dos __yy__, para o jogador não ser afetado por esta alteração e se manter no mesmo sítio.
-}

{- |
= __Funções Auxiliares__

== Função Auxiliar 1

A função auxiliar __remove__ retira a última linha do mapa anteriormente apresentado, com a ajuda da função predifinida no Prelude  __init__, que retira o último item de uma lista, sendo a lista o mapa e o item a uĺtima linha.
-}
deslizaJogo i (Jogo (Jogador (a,b)) (Mapa larg l)) = if mapaValido (remove (estendeMapa (Mapa larg l) i))
                                                     then (Jogo (Jogador (a,b-1)) (remove (estendeMapa (Mapa larg l) i)))
                                                     else (Jogo (Jogador (a,b-1)) (remove (estendeMapa(Mapa larg l) (i+1))))
                                                                                              
remove :: Mapa -> Mapa
remove (Mapa larg l) = Mapa larg (init l)

