-- -----------------------------------------------------------------------------
-- Programación Declarativa 2022/23
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Trabajo: Juego de Puzzle Full House                    
-- -----------------------------------------------------------------------------
-- Apellidos: Chen Su
-- Nombre: Jialuo
-- UVUS: jiache9

-- Apellidos: Villanueva Duque
-- Nombre: Hugo
-- UVUS: hugvilduq
-- -----------------------------------------------------------------------------
-- Requisitos mínimos:

-- 2 funciones básicas de prelude y Data.List
-- 2 funciones recursivas
-- 2 funciones por patrones 
-- 2 usos de guardas 
-- 2 usos de case of 
-- 2 usos de listas por comprensión 
-- 2 usos de orden superior 
-- Uso declaraciones de tipos para todas las funciones definidas, etc.
-- Creación de un módulo: Tomamos este archivo como módulo
-- Creación de dos tipos de datos algebraicos y usos de éstos.
-- Uso de al menos dos de tipos de datos abstractos o librerías vistos en la asignatura (por ejemplo, pilas, colas, map, matrix, array).
-- -----------------------------------------------------------------------------
-- La idea consiste en, a partir de un laberinto recorrer todos los espacios CON UN '.'
-- continuamente, y sólo pudiendo recorrer una vez cada cuadricula.
-- En resumen, no es posible dar pasos hacia atrás y los movimientos permitidos son:
-- arriba, abajo , izquierda, derecha.
-- -----------------------------------------------------------------------------
-- Entrada del problema:

--                #######
--                #     #
--                #    ##
--                ##    #
--                #   # #
--                #     #
--                #######

-- # = Representa pared
-- o = Representa el espacio recorrido
-- . = Representa el espacio posible para recorrer
-- $ = Representa la posicion del jugador



{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- ============================================================================
--Modulo
-- ============================================================================
module FullHouse_funciones
    (posicion,
     vecinos,
     moves,
     vecinos2mov,
     movableTo,
     pos2mov,
     pzl2moves,
     siguientePosicion,
     replaceMaze,
     mueve,
     startPos,
     buildTree,
     isValidMove,
     matriz,
     conversorFormato,
     formatoIsWin,
     isWin
    ) where

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================  
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Array
import PilaTA
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import System.Directory
import System.IO (stdin, hSetEcho, hSetBuffering)
import System.IO( BufferMode( NoBuffering ) )
import Data.Colour.CIE.Illuminant


-- -----------------------------------------------------------------------------
-- Para que salga la condicion de victoria, no debe quedar '.'
-- En caso contratio -> Derrota
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------

-- EJERCICIO 1 - Creamos variables con ejemplos para entradas del problema. 
-- Para comprobar si las funciones estan correctas.

puzzle0 = ["#######",
           "#o....#",
           "#######"]

puzzle1 = ["#######",
           "#.....#",
           "#....##",
           "##....#",
           "#...#.#",
           "#.....#",
           "#######"]

puzzle2= [ "#####",
           "#..##",
           "#...#",
           "#####"]

puzzle3 = ["#######",
           "#oooo.#",
           "#######"]

-- EJERCICIO 2 - Dos funciones por orden superior: 
--  1) obtener un elemento desde lista de listas dado su coordenada -> 2xfoldl
--  2) obtener los vecinos de las posiciones permitidas (arr, ab, izq, der) 1Xfoldr

--posicion puzle (f,c)
-- posicion puzzle0 (0,0)
-- '#'
-- posicion2 puzzle0 (1,5)
-- posicion puzzle0 (1,5)
-- 'S'
-- posicion puzzle1 (4,3)
-- '.'
-- posicion puzzle1 (4,4)
-- '#'

posicion :: [[Char]] -> (Int, Int) -> Char
posicion pzl (i,j) = head (foldl (\acc (x,y) -> if j==y then acc++[x] else acc) "" (zip (head ls) [0,1..]))
    where ls = foldl (\acc (x,y) -> if i==y then acc++[x] else acc) [] (zip pzl [0,1..])

--posicion de una manera mas simple
posicion2 :: [[Char]] -> (Int, Int) -> Char
posicion2 pzl (f,c) = pzl !! f !! c

-- vecinos puzzle0 (1,1)
-- "### "
-- vecinos puzzle1 (4,1)
-- "# # "
-- vecinos puzzle1 (4,5)
-- "  ##"

vecinos :: [[Char]] -> (Int, Int) -> [Char]
vecinos pzl (i,j) = foldr (\c acc -> (posicion pzl c):acc) [] coords
    where coords = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
                -- (ARRIBA, ABAJO,IZQUIERDA, DERECHA)

-- EJERCICIO 3 - Una funcion por case of y lista de comprension. 
-- 1) Comprobar si la casilla es un movimiento valido. (case of)
-- 2) Dada una lista de vecinos, devolver los movimientos "moves" validos. (lista de comprension)


-- vecinos2mov ".#O." 
-- ["AR","DE"]
-- vecinos2mov "##*#" 
-- [] 

-- data Move = AR | AB | IZ | DE
-- deriving (Show, Eq)

-- moves :: [Move]
-- moves = [AR, AB, IZ, DE]

moves :: [String]
moves = ["AR", "AB", "IZ", "DE"]
--
-- -----------------------------------------------------------------------------
vecinos2mov :: String -> [String]
vecinos2mov cs = [ moves!!(i) | (c,i)<-zip cs [0,1..], movableTo c]

movableTo :: Char -> Bool
movableTo cell = case cell of
                'o' ->  False
                '#' ->  False
                '.' ->  True
                _    -> error "Invalid cell type"

movableTo2 :: Char -> Bool
movableTo2 'o' = False
movableTo2 '#' = False
movableTo2 '.' = True
movableTo2 _   = error "Invalid cell type"

-- EJERCICIO 4 - Una funcion por guardas
-- 1) Dado un laberinto y una posicion devolver una lista de movimientos validos que se pueda realizar
-- pos2mov puzzle0 (1,5)
-- ["IZ"] 
-- pos2mov puzzle1 (4,5)
-- ["AR","AB"]

pos2mov :: [[Char]] -> (Int, Int) -> [String]
pos2mov pzl (x,y)
   | posicion pzl (x,y) == '#' = []
   | otherwise = vecinos2mov ls_vecinos
   where ls_vecinos = vecinos pzl (x,y)


-- EJERCICIO 5 - Funcion con lista por comprension
-- Convierte un laberinto a una lista de movimientos posibles por cada casilla

-- pzl2moves puzzle0
-- > [[["DE"],["DE"],["IZ","DE"],["IZ","DE"],["IZ"]]]
-- pzl2moves puzzle1
-- [[["AB","DE"],["AB","IZ","DE"],["AB","IZ","DE"],["AB","IZ","DE"],["IZ"]],[["AR","DE"],["AR","AB","IZ","DE"],["AR","AB","IZ","DE"],["AR","AB","IZ"],[]],[[],["AR","AB","DE"],["AR","AB","IZ","DE"],["AR","IZ","DE"],["AB","IZ"]],[["AB","DE"],["AR","AB","IZ","DE"],["AR","AB","IZ"],[],["AR","AB"]],[["AR","DE"],["AR","IZ","DE"],["AR","IZ","DE"],["IZ","DE"],["AR","IZ"]]]

pzl2moves :: [[Char]] -> [[[String]]]
pzl2moves css = [ [ pos2mov css (i,j) | (c,j)<- zip cs [0..length cs], j>0 && j<tamanyo_j]| (cs,i) <- zip css [0,1..], i>0 && i<tamanyo_i]
      where tamanyo_i = length css-1
            tamanyo_j = length(head css)-1

-- EJERCICIO 6 - Una funcion por case of: 
-- 1) una funcion donde devuelve la siguiente posicion dado un movimiento y una posicion

-- siguientePosicion "AR" (1,1)
-- (0,1)
-- siguientePosicion "AB" (1,1)
-- (2,1)
-- siguientePosicion "IZ" (1,1)
-- (1,0)
-- siguientePosicion "DE" (1,1)
-- (1,2)


siguientePosicion2 :: (Int, Int) -> [Char] -> (Int, Int)
siguientePosicion2 (x, y) "AR" = (x-1, y)
siguientePosicion2 (x, y) "AB" = (x+1, y)
siguientePosicion2 (x, y) "IZ" = (x, y-1)
siguientePosicion2 (x, y) "DE" = (x, y+1)
siguientePosicion2 _ _ = error "Invalid movement direction"

siguientePosicion :: [Char] -> (Int, Int) -> (Int, Int)
siguientePosicion mov (x,y) = case mov of
                                "AR" -> (x-1, y)
                                "AB" -> (x+1, y)
                                "IZ" -> (x, y-1)
                                "DE" -> (x, y+1)
                                _    -> error "Invalid movement direction"


-- Funcion auxiliar para marcar 'o' (o desmarcar '.') qué casillas han sido recorridas :
-- Reemplaza una casilla (i,j) del laberinto pzl por el caracter c

-- replaceMaze (1,3) 'o' puzzle0
-- ["#######","#..o..#","#######"]

replaceMaze :: (Int,Int)-> Char -> [String] -> [String]
replaceMaze (i,j) c pzl = replace i nuevaFila pzl
    where nuevaFila = replace j c (pzl !! i)
          replace i c s = [if j == i then c else r | (j, r) <- zip [0..] s]


-- EJERCICIO 7 - Funcion con foldr y lista de comprension
-- 1) Devuelve la lista de movimientos que se puede realizar y la nueva posicion.

-- mueve puzzle0 "DE" (1,1)
-- (["#######", "#o.....#", "#######"],["DE"],(1,1))

-- mueve puzzle0 "DE" (1,2)
-- (["#######", "#oo....#", "#######"],["DE"],(1,3))
mueve ::  [String] -> String -> (Int, Int) -> ([String],[String], (Int, Int))
mueve pzl mov (x,y) = (replaceMaze (x,y) 'o' pzl, pos2mov pzl (x,y), siguientePosicion mov (x,y))


-- startPos ["...",".S"]
-- (1,1)  
startPos :: [[Char]] -> (Int, Int)
startPos pzl = head [(head (elemIndices row pzl), head (elemIndices 'S' row)) | row <- pzl, elem 'S' row]


-- EJERCICIO 8 - Funcion por tipo abstracto y arbol

-- buildTree puzzle2 (1,2) []
-- N (1,2) [N (2,2) [N (2,1) [H (1,1)],H (2,3)],N (1,1) [N (2,1) [N (2,2) [H (2,3)]]]]

-- Crea un árbol que representa todos los posibles caminos que se pueden tomar 
-- desde la posición de inicio hasta quedarse sin celdas

-- puzzle2= [ "#####",
--            "#..##",
--            "#...#",
--            "#####"]

data Arbol = N Pos [Arbol] | H Pos deriving (Show)
type Pos = (Int, Int)

buildTree :: [String] -> (Int, Int) -> [(Int, Int)] -> Arbol
buildTree maze pos visited 
    |validMoves == [] = H pos
    |otherwise = N pos (map (\p -> buildTree maze p (pos : visited)) validMoves)
    where validMoves = filter (isValidMove maze (pos : visited)) (posAdyacentes pos)
          posAdyacentes (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- isValidMove puzzle2 [(1,1)] (1,1)
-- False
-- Comprueba si una posicion es visitable, recibiendo el historial de posiciones recibidas
isValidMove :: [String] -> [Pos] -> Pos -> Bool
isValidMove maze visited (x,y) = (maze !! x !! y) == '.' && not (elem (x,y) visited)

-- EJERCICIO 9 - Funcion matriz y sinonimo de tipos (1 array, 1 sinonimo de tipo, 1 guarda)
-- Representar el puzzle segun el numero de movimiento que se pueda realizar.
-- Si la posicion es #, 0 posiciones posibles
-- Si la posicion es o, -1 posiciones posibles. Esto ocurre cuando hemos elegido un camino (podriamos cambiar a otro valor, a 0 igual?)
-- Cualquier otro caso, contar los caminos posibles. 

type Matriz = Array (Int,Int) Int

-- matriz puzzle0
-- array ((1,1),(3,7)) [((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),((2,1),0),((2,2),-1),((2,3),1),((2,4),2),((2,5),2),
-- ((2,6),1),((2,7),0),((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((3,6),0),((3,7),0)]


-- matriz puzzle2
-- array ((1,1),(4,5)) [((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((2,1),0),((2,2),2),((2,3),2),((2,4),0),((2,5),0),((3,1),0),((3,2),2),
-- ((3,3),3),((3,4),1),((3,5),0),((4,1),0),((4,2),0),((4,3),0),((4,4),0),((4,5),0)]

matriz :: [[Char]] -> Array (Int, Int) Int
matriz pzl = array ((1,1),(m,n)) [ ((i,j), condicion (i-1) (j-1)) | i<-[1..m], j<-[1..n]]
      where m = length pzl
            n = length (head pzl)
            condicion x y
                  | posicion pzl (x,y) == '#' = 0
                  | posicion pzl (x,y) == 'o' = -1
                  | otherwise = length (pos2mov pzl (x,y))
-- EJERCICIO 10 - Funcion por Pila
-- Dado un puzzle, determinar si es victoria (True) o derrota (False)

-- Convierte el puzzle, en formato Pila, para que podamos operar sobre ella
-- conversorFormato puzzle0
-- "#######"|"#o....#"|"#######"|-
conversorFormato :: Foldable t => t a -> Pila a
conversorFormato ls = foldr (apila) vacia ls

-- Separamos nuevamente para que sean individuales los valores
-- formatoIsWin (conversorFormato puzzle0)
-- '#'|'#'|'#'|'#'|'#'|'#'|'#'|'#'|'o'|'.'|'.'|'.'|'.'|'#'|'#'|'#'|'#'|'#'|'#'|'#'|'#'|-
formatoIsWin :: Foldable t => Pila (t a) -> Pila a
formatoIsWin p 
    | esVacia p = vacia 
    | otherwise = foldr apila (formatoIsWin dp) cp
    where 
        cp = cima p
        dp = desapila p


-- Comprueba si el puzzle dado, es condicion de victoria o no. Reusando las funciones previas para poder tener un output de tipo Bool
-- isWin puzzle0
-- isWin ["#######", "#ooooo..#", "#######"]
isWin :: (Foldable t1, Foldable t2) => t1 (t2 Char) -> Bool
isWin pzl = (res 0 p1) <= 1
    where 
        cf = conversorFormato pzl 
        p1 = formatoIsWin cf 
        res contador p 
            | esVacia p = contador 
            | cp == '.' = res (contador+1) dp
            | otherwise = res contador dp 
            where cp = cima p 
                  dp = desapila p 
        

-- Este es un método más simple de realizarlo, pero es para poder trabajar con Pila.
-- isWin pzl = sum ([length $ filter (== '.') row | row <- pzl ]) == 1

    

