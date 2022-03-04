module Jugadores
(
    Jugador(..),
    crearJugadores,
    crearJugador,
    servirJugadores,
    manosVacias,
) where

import Mazo
import Data.List (sortOn)
import Data.Ord (comparing)

data Jugador = Jugador {id::Int ,nombre::String, cartas::[Carta]}

data Partida = Partida { jugadores::[Jugador], mazo::[Carta], turno::Int, pendientes::Int, block::Bool, ganador::Bool}

data CM = CM {jcartas::[Carta], jmazo::[Carta]}

instance Show Jugador where
    show (Jugador id nombre cartas) = nombre ++ " - Cartas:" ++ show cartas

crearJugador :: Int -> IO Jugador
crearJugador x = do
    putStr "Por favor, ingrese el nombre del jugador: "
    name <- getLine
    let nombre = name
        jugador = Jugador x nombre []
    return jugador

crearJugadores :: Int -> IO Jugador -> [IO Jugador]
crearJugadores 0 jugador = []
crearJugadores n jugador = jugador : crearJugadores (n-1) (crearJugador (n-1))

obtenerJugador :: [Jugador] -> Int -> Jugador
obtenerJugador [] k = error "empty list"
obtenerJugador ((Jugador i x n):xs) k = if i==k then Jugador i x n else obtenerJugador xs k

servirJugador :: [Carta] -> Jugador -> Jugador
servirJugador mano (Jugador i x n) = Jugador i x (take 7 mano)


servirJugadores :: [Jugador] -> [Carta] -> [Jugador]
servirJugadores [] mazo = []
servirJugadores ((Jugador i x n):xs) mazo = servirJugador mazo (Jugador i x n):servirJugadores xs (drop 7 mazo)


manosVacias :: [Jugador] -> Bool
manosVacias jugadores = map manoVacia jugadores == replicate (length jugadores) False

manoVacia :: Jugador -> Bool
manoVacia x
    | null (cartas x) = False
    | otherwise = True


calcularPuntajes :: [Jugador] -> [(Int, Jugador)]
calcularPuntajes jugadores = sortOn fst (map calularPuntaje jugadores)

calularPuntaje :: Jugador -> (Int, Jugador)
calularPuntaje jugador = (sum (map getValorPts (cartas jugador)), jugador)

obtenerCartasJugador :: Jugador -> [Carta]
obtenerCartasJugador (Jugador i x n) = n

obtenerCartaJugable :: [Carta] -> Carta -> [Carta]
obtenerCartaJugable [] x = []
obtenerCartaJugable (x:xs) y = if cartaCompatible x y then [x] else obtenerCartaJugable xs y

puedeJugar :: Jugador -> Carta -> Bool
puedeJugar j c = length (obtenerCartaJugable (obtenerCartasJugador j) c) > 1

cartaCompatible :: Carta -> Carta -> Bool
cartaCompatible (Carta x n) (Carta y m) = x==y || n==m || m==Negro

esBlock :: Carta -> Bool
esBlock (Carta x n) = x==12 || x==14

borrarCarta :: [Carta] -> Carta -> [Carta]
borrarCarta [] c = []
borrarCarta ((Carta x n):xs) (Carta y m) = if x==y && n==m then xs else Carta x n:borrarCarta xs (Carta y m)

descartarCarta :: [Jugador] -> Int -> Carta -> [Jugador]
descartarCarta [] id c = []
descartarCarta ((Jugador i x n):xs) id c = if i==id then [Jugador i x (borrarCarta n c)] else Jugador i x n:descartarCarta xs id c

calcularPendientes :: Carta -> Int
calcularPendientes (Carta x n)
  | x==10 = 2
  | x==14 = 4
  | otherwise = 0

calcularSiguienteTurno :: [Jugador] -> Int -> Int
calcularSiguienteTurno jugadores x = if length jugadores==x then 0 else x+1

jugarTurnoValido :: Partida -> Partida
jugarTurnoValido (Partida xs (m:mazo) turno p b ganador) = Partida (descartarCarta xs turno (head (obtenerCartaJugable (obtenerCartasJugador (obtenerJugador xs turno)) m))) ((head (obtenerCartaJugable (obtenerCartasJugador (obtenerJugador xs turno)) m)):m:mazo) (calcularSiguienteTurno xs turno)

--jugarTurno :: Partida -> Partida
--jugarTurno (Partida xs mazo turno x True ganador) = Partida xs mazo (calcularSiguienteTurno xs turno) x True ganador
--jugarTurno (Partida xs (m:mazo) turno 0 False ganador) = if puedeJugar (obtenerJugador xs turno) m 



