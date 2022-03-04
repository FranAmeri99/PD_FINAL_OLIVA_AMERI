module Jugadores
(
    Jugador(..),
    crearJugadores,
    crearJugador,
    servirJugadores,
    manosVacias,
    --calcularPuntajes,
) where

import Mazo
import Data.List (sortOn)
import Data.Ord (comparing)

{- Defininmos un nuevo tipo de datos Jugador al cual le redefinimos el Show-}
data Jugador = Jugador {id::Int ,nombre::String, cartas::[Carta]}

data Partida = Partida { jugadores::[Jugador], mazo::[Carta], turno::Int, pendientes::Int, block::Bool, ganador::Bool}

data CM = CM {jcartas::[Carta], jmazo::[Carta]}

instance Show Jugador where
    show (Jugador id nombre cartas) = nombre ++ " - Cartas:" ++ show cartas

{- Nos permite crear un Jugador con un nombre, una mano vacia y un monton vacio -}
crearJugador :: Int -> IO Jugador
crearJugador x = do
    putStr "Por favor, ingrese el nombre del jugador: "
    name <- getLine
    let nombre = name
        jugador = Jugador x nombre []
    return jugador

{- Nos permite crear n jugadores, se llama recursivamente decrementando el contador
   hasta que este llegue a 0, entonces encierra a los jugadores en una lista y la devuelve -}
crearJugadores :: Int -> IO Jugador -> [IO Jugador]
crearJugadores 0 jugador = []
crearJugadores n jugador = jugador : crearJugadores (n-1) (crearJugador (n-1))

obtenerJugador :: [Jugador] -> Int -> Jugador
obtenerJugador [] k = error "empty list"
obtenerJugador ((Jugador i x n):xs) k = if i==k then Jugador i x n else obtenerJugador xs k

{- Entrega a cada jugador 3 cartas del mazo, estas 3 se corresponden con su posicion
    es decir si es el jugador  1 decibe las  3 primeras cartas, cuadno es el jugador 2
    se dropean las 3 primeras cartas y se entregan las 3 nuevas primeras -}
servirJugador :: [Carta] -> Jugador -> Jugador
servirJugador mano (Jugador i x n) = Jugador i x (take 7 mano)

{- Currificamos la funcion servirJugador para poder zipearla a lo largo de las tuplas jugador, numero.
   Lo cual tiene como resultado que se le entreguen las 3 cartas que corresponden a cada jugador -}
servirJugadores :: [Jugador] -> [Carta] -> [Jugador]
servirJugadores [] mazo = []
servirJugadores ((Jugador i x n):xs) mazo = servirJugador mazo (Jugador i x n):servirJugadores xs (drop 7 mazo)

{- Nos permite saber si todos los jugadorres se quedaron sin cartas en la mano, devuelve True
   en ese caso, y falso en cualquier otro caso -}
manosVacias :: [Jugador] -> Bool
manosVacias jugadores = map manoVacia jugadores == replicate (length jugadores) False

{- Nos permite saber si un jugador se quedo sin cartas en la mano, cuando no tiene cartas en la mano devuelve False -}
manoVacia :: Jugador -> Bool
manoVacia x
    | null (cartas x) = False
    | otherwise = True
{- Nos permite reemplazar un jugador por uno actualizado, esto lo hace cuando esta posicion 0, en todos
   los otros casos simplemte deja intacto a los jugadores, pero cuando llega a la posicion 0 significa que
   encontro el jugador a cambiar y lo descarta y coloca el nuevo jugador 
reemplazarJugador :: Jugador -> [Jugador] -> Int -> [Jugador]
reemplazarJugador jugador jugadores posicion
    | null jugadores = []
    | posicion == 0 = jugador : reemplazarJugador jugador (tail jugadores) (posicion-1)
    | otherwise = head jugadores : reemplazarJugador jugador (tail jugadores) (posicion-1)
    -}

{- Calculamos los puntajes de todos los jugadores y los ordenamos de menor a mayor -}
calcularPuntajes :: [Jugador] -> [(Int, Jugador)]
calcularPuntajes jugadores = sortOn fst (map calularPuntaje jugadores)

{-Sumamos el valor de todas las cargas de cada monton de jugador -}
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



