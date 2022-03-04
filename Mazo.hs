module Mazo
(
    Color(..),
    Carta(..),
    getValor,
    getValorPts,
    getColor,
    crearMazo,
    mezclarCartas,
) where

import System.Random

-- Definimos el tipo de dato Color
data Color = Negro | Azul | Amarillo | Rojo | Verde  deriving (Show, Enum, Eq)

-- Definimos el tipo de dato Carta
data Carta = Carta { valor:: Int, color:: Color } deriving Eq

-- Redefinimos el Show para Carta
instance Show Carta where
  show (Carta valor color) = "[" ++ getValor valor ++ ", " ++ show color ++ "]"

-- Obtenemos el valor de la carta parseado
getValor :: Int -> String 
getValor valor 
  | valor == 0 = show "0"
  | valor == 1 = show "1"
  | valor == 2 = show "2"
  | valor == 3 = show "3"
  | valor == 4 = show "4"
  | valor == 5 =show "5"
  | valor == 6 = show "6"
  | valor == 7 = show "7"
  | valor  == 8 = show "8"
  | valor  == 9 = show "9"
  | valor  == 10 = show "+2"
  | valor  == 11 = show "Cambiar Sentido"
  | valor  == 12 = show "Saltear Jugador"
  | valor  == 13 = show "Comodin"
  | valor  == 14 = show "+4"
  | otherwise = show "indeterminado"

getValorPts :: Carta -> Int
getValorPts carta 
  | valor carta == 1 = 1
  | valor carta == 2 = 1
  | valor carta == 3 = 3
  | valor carta == 4 = 4
  | valor carta == 5 = 5
  | valor carta == 6 = 6
  | valor carta == 7 = 7
  | valor carta == 8 = 8
  | valor carta == 9 = 9
  | valor carta == 10 = 20
  | valor carta == 11 = 50
  | valor carta == 12 = 50
  | valor carta == 13 = 20
  | valor carta == 14 = 100
  | otherwise = 0

-- Obtenemos el Color de la carta
getColor :: Carta -> Color
getColor carta
  | valor carta ==13 = Negro                
  | valor carta == 14 = Negro
  | otherwise = color carta

cartasComunes = [Carta v p | p <- [Azul ..], v <- [0..12]] 
cartasEspeciales = [Carta v p | p <- [Negro], v <- [13,14]] 

crearMazo :: [Carta]
crearMazo =  cartasComunes  ++ cartasComunes  ++ cartasEspeciales ++ cartasEspeciales

mezclarCartas :: [Carta] -> [Carta] -> IO [Carta]
mezclarCartas mezclado [] = return mezclado
mezclarCartas mezclado sinMezclar = do
  indiceCartaRandom <- randomRIO (0, length sinMezclar - 1)
  let cartaRandom = sinMezclar !! indiceCartaRandom
      sinMezclarAntes = take indiceCartaRandom sinMezclar
      sinMezclarDesppues = drop (indiceCartaRandom + 1) sinMezclar

  mezclarCartas (cartaRandom:mezclado) (sinMezclarAntes ++ sinMezclarDesppues)

