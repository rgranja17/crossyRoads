import LI12223
import Tarefa1_2022li1g020
import Tarefa2_2022li1g020
import Tarefa3_2022li1g020
import Tarefa4_2022li1g020
import Tarefa5_2022li1g020
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import Data.List 
import Graphics.Gloss.Interface.IO.Game
import System.Random

type Estado = (Jogo,Jogada)

data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | Perdeu

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Images)


-- | The width of the window, in pixels
windowWidth :: Int
windowWidth = 900

-- | The height of the window, in pixels
windowHeight :: Int
windowHeight = 900

-- | The initial state of the game
initialState :: [Picture]-> World 
initialState images = (Opcoes Jogar,(Jogo (Jogador (4,-9)) (Mapa 8 [(Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum,Carro,Nenhum,Carro,Nenhum]),
                                                (Estrada 1, [Nenhum,Nenhum,Nenhum,Carro,Nenhum, Nenhum, Nenhum, Nenhum]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Rio (-1), [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum,Nenhum,Nenhum,Arvore,Nenhum, Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore])])),Parado,images)    
{-|

Ela recebe uma largura, uma lista de tuplas com um tipo de terreno e uma lista de obstáculos, uma posição x, uma posição y, um índice de obstáculo 
e uma lista de imagens.
A função "escolhelinha" usa uma estrutura condicional para verificar se a posição x é igual ao tamanho da lista de tuplas menos 1. 
Se for verdade, significa que é a última linha a ser desenhada, então a função chama a função "linhagrafica" passando
 a largura, a última tupla da lista de tuplas, o índice de obstáculo e a lista de imagens,para desenhar a última linha na tela.

Se a condição não for verdadeira, significa que ainda há mais linhas a serem desenhadas. 
A função "escolhelinha" chama a si própria passando a largura, a lista de tuplas, a próxima posição x (x+1), a próxima posição y (y-100) 
e o índice de obstáculo (0), para continuar o loop e desenhar as próximas linhas.
-}
escolhelinha :: Largura -> [(Terreno, [Obstaculo])] -> Int -> Float -> Int -> [Picture] -> [Picture]
escolhelinha l t x y o images
   | x == ((length t)-1) = (translate 0 y (linhagrafica l ((!!) t ((length t)-1)) 0 images)) : []
   | otherwise = (translate 0 y (linhagrafica l ((!!) t x) 0 images)) : escolhelinha l t (x+1) (y-100) 0 images

linhagrafica :: Largura -> (Terreno, [Obstaculo]) -> Int -> [Picture] -> Picture
linhagrafica l (Estrada v, obs) o images = Pictures [linhagrafica1 (Estrada v, obs) images, Pictures (linhagrafica2 l (Estrada v, obs) 0 images)]
linhagrafica l (Rio v, obs) o images = Pictures [linhagrafica1 (Rio v, obs) images, Pictures (linhagrafica2 l (Rio v, obs) 0 images)]
linhagrafica l (Relva, obs) o images = Pictures [linhagrafica1 (Relva, obs) images, Pictures (linhagrafica2 l (Relva, obs) 0 images)]

{-|

Desenho dos terrenos a linhagrafica1
-}

linhagrafica1 :: (Terreno, [Obstaculo])-> [Picture] -> Picture
linhagrafica1 (Estrada v, obs)  images = translate 0 100 $ estrada
                                  where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
linhagrafica1 (Relva, obs) images = translate 0 100 $ relva
                                  where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
linhagrafica1 (Rio v, obs) images = translate 0 100 $ rio
                                  where [estrada,rio,relva,carro,arvore,personagem,tronco] = images

{-|
A condição " (l-1) == o " verifica se o índice do obstáculo atual é igual ao último elemento da lista de obstáculos. 
Se for verdade, significa que é o último obstáculo a ser desenhado, então o loop é interrompido e a função "linhagrafica2" retorna.

PS : indice de obstaculo : O índice de obstáculo é um número inteiro que vai indicar a posição de um obstáculo específico na lista de obstáculos. 
Ele é usado para acessar o obstáculo correto na lista de obstáculos e para posicioná-lo corretamente na tela.
Esta função tem haver com o desenho dos obstaculos
-}
linhagrafica2 :: Largura -> (Terreno, [Obstaculo]) -> Int -> [Picture] -> [Picture]
linhagrafica2 l (Estrada v, obs) o images
          | (l-1) == o = if (!!) obs o == Carro then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Carro images )) : []
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : []
          | otherwise = if (!!) obs o == Carro then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Carro images )) : linhagrafica2 l (Estrada v, obs) (o+1) images
                                               else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : linhagrafica2 l (Estrada v, obs) (o+1) images
               where l1 = fromIntegral l
                     o1 = fromIntegral o

linhagrafica2 l (Relva, obs) o images
          | (l-1) == o = if (!!) obs o == Arvore then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Arvore images)) : []
                                                 else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Nenhum images)) : []
          | otherwise = if (!!) obs o == Arvore then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Arvore images)) : linhagrafica2 l (Relva, obs) (o+1) images
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo 0 Nenhum images)) : linhagrafica2 l (Relva, obs) (o+1) images
                 where l1 = fromIntegral l
                       o1 = fromIntegral o

linhagrafica2 l (Rio v, obs) o images
          | (l-1) == o = if (!!) obs o == Tronco then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Tronco images)) : []
                                                 else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : []
          | otherwise = if (!!) obs o == Tronco then (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Tronco images)) : linhagrafica2 l (Rio v, obs) (o+1) images
                                                else (translate ((o1*(900/l1)+((900/l1)/2))-450) 100 (desenhaobstaculo v Nenhum images)) : linhagrafica2 l (Rio v, obs) (o+1) images
                 where l1 = fromIntegral l
                       o1 = fromIntegral o

desenharmapa :: Mapa -> [Picture] -> Picture
desenharmapa (Mapa l t) images = Pictures (escolhelinha l t 0 300 0 images)
                          

desenhaEstado :: World -> Picture
desenhaEstado (ModoJogo, (Jogo jogador mapa),j,images) = Pictures [desenharmapa mapa images, desenhaJogador jogador images]
                                                where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
desenhaEstado (Opcoes Jogar,(Jogo jogador mapa),j,images) = Pictures [translate (-200) (-100) $ color red $ drawOption "Jogar", translate (-200) (-300) $ color white $ drawOption "Sair"]
desenhaEstado (Opcoes Sair,(Jogo jogador mapa),j,images) = Pictures [translate (-200) (-100) $ color white $ drawOption "Jogar", translate (-200) (-300) $ color red $ drawOption "Sair"]
desenhaEstado (Perdeu,(Jogo jogador mapa),j,images) = Pictures [color red $ drawOption "Perdeste!"]


drawOption :: String -> Picture 
drawOption texto = Text texto

desenhaJogador :: Jogador -> [Picture] -> Picture
desenhaJogador (Jogador (x,y)) images = translate xx1 yy1 $ personagem
                     where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
                           x1 = fromIntegral x
                           y1 = fromIntegral y
                           xx1 = (x1 * 100)-400
                           yy1 = (y1* 100)+400
                           

desenhaobstaculo:: Velocidade -> Obstaculo -> [Picture] -> Picture
desenhaobstaculo v Nenhum images = Blank 
                
desenhaobstaculo v Tronco images= tronco
                where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
              
desenhaobstaculo v Carro images = carro
                where [estrada,rio,relva,carro,arvore,personagem,tronco] = images
desenhaobstaculo v Arvore images = arvore 
                where [estrada,rio,relva,carro,arvore,personagem,tronco] = images

update :: Float -> World -> World 
update n (ModoJogo,(Jogo (Jogador (a,b)) mapa), j,images) = if (jogoTerminou (Jogo (Jogador (a,b)) mapa)) == True 
                                                     then (Perdeu,(Jogo (Jogador (a,b)) mapa),j,images)  -- return the current state if the game is over
                                                     else (ModoJogo,((animaJogo (deslizaJogo (a*b) (Jogo (Jogador (a,b)) mapa))) j), j,images) -- update the game state with the jogada
update _ w = w

reageEvento :: Event -> World -> World
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo,j,images) =
    (ModoJogo, jogo,j,images)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo,j,images) =
   (Opcoes Sair, jogo,j,images)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo,j,images) =
   (Opcoes Sair, jogo,j,images)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,j,images) =
   (Opcoes Jogar, jogo,j,images)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,j,images) =
   (Opcoes Jogar, jogo,j,images)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,j,images) =
   error "Fim de Jogo"
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu,jogo,j,images) =
   (Opcoes Jogar,(Jogo (Jogador (4,-9)) (Mapa 8 [(Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Estrada 1, [Nenhum, Nenhum, Nenhum, Nenhum,Carro,Nenhum,Carro,Nenhum]),
                                                (Estrada 1, [Nenhum,Nenhum,Nenhum,Carro,Nenhum, Nenhum, Nenhum, Nenhum]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Rio (-1), [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore]),
                                                (Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum,Nenhum,Nenhum,Arvore,Nenhum, Nenhum, Nenhum, Nenhum]),
                                                (Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum,Arvore,Nenhum,Arvore])])),Parado,images)       

reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo,(Jogo(Jogador(a,b)) (Mapa larg l)),j,images) = (ModoJogo,(Jogo(Jogador (moveJogador larg (a,b) j1)) (Mapa larg l)),j,images)
                                                                      where j1 = (Move Cima)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo,(Jogo(Jogador(a,b)) (Mapa larg l)),j,images)= (ModoJogo,(Jogo(Jogador (moveJogador larg (a,b) j1)) (Mapa larg l)),j,images)
                                                                      where j1 = (Move Baixo)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo,(Jogo(Jogador(a,b)) (Mapa larg l)),j,images) = (ModoJogo,(Jogo(Jogador (moveJogador larg (a,b) j1)) (Mapa larg l)),j,images)
                                                                      where j1 = (Move Esquerda)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo,(Jogo(Jogador(a,b)) (Mapa larg l)),j,images) = (ModoJogo,(Jogo(Jogador (moveJogador larg (a,b) j1)) (Mapa larg l)),j,images)
                                                                      where j1 = (Move Direita)
reageEvento _ s = s  -- ignora qualquer outro evento

janela :: Display
janela =  FullScreen
      
fr :: Int
fr = 1

main :: IO ()
main = do
          personagem1 <- loadBMP "mario.bmp"
          relva1 <- loadBMP "relva.bmp"
          rio1 <- loadBMP "rio.bmp"
          estrada1 <- loadBMP "estrada.bmp"
          carro1 <- loadBMP "carro.bmp"
          arvore1 <- loadBMP "arvore.bmp"
          tronco1 <- loadBMP "tronco.bmp"
          let images = [estrada,rio,relva,carro,arvore,personagem,tronco]
              personagem = (scale 0.2 0.2 personagem1)
              relva = (scale 1 1 relva1)
              rio = (scale 1 1 rio1)
              estrada = (scale 1 1 estrada1)
              carro = (scale 0.2 0.2 carro1)
              arvore = (scale 0.1 0.1 arvore1)
              tronco = (scale 0.1 0.1 tronco1)
          play janela
               black
               fr
               (initialState images)
               desenhaEstado
               reageEvento
               update


