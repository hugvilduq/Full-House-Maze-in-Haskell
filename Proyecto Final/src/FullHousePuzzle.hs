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
import FullHouse_funciones

-- -----------------------------------------------------------------------------

-- EJERCICIO 11 - ENTRADA Y SALIDA PARA JUGAR AL PUZZLE
-- Hacemos un minijuego de Full House Puzzle, con E/S para que sea interactivo para el jugador.

main :: IO ()
main= do
    level_select

level_select :: IO ()
level_select = do
    let level_list=
            [
               ["#######",
                "#....##",
                "#.....#",
                "#######"],

               ["#######",
                "#.....#",
                "#....##",
                "##....#",
                "#...#.#",
                "#.....#",
                "#######"],

               ["#######",
                "#....##",
                "#.....#",
                "#...#.#",
                "#.....#",
                "##....#",
                "#######"]
            ]
    mapM_ printLevel level_list
    putStr "Escribe el número del nivel que desea jugar. Niveles disponibles: " 
    putStrLn (show (length level_list))
    c <- getChar
    if isDigit c then do
        let maze = level_list !! (digitToInt c - 1)
        positionMaze maze
    else do
        putStrLn "\nEntrada incorrecta (se esperaba un dígito)"
        level_select
    where printLevel level = do
            putStrLn "\n"
            mapM_ putStrLn level



positionMaze :: [String] -> IO()
positionMaze maze = do
    putStrLn (unlines maze)
    putStrLn "Indica en qué FILA quieres empezar"
    xLine <- getLine
    let x = read xLine::Int
    if all isDigit xLine && (xLine /= "" )then do
        putStrLn "Indica en qué COLUMNA quieres empezar"
        putStrLn "Tu jugador es el símbolo $"
        yLine <- getLine
        if all isDigit yLine && (yLine /= "" ) then do
            let y = read yLine::Int
            let newMaze = replaceMaze (x,y) 'S' maze
            playMaze newMaze (startPos newMaze)
        else do
            putStrLn "Por favor, introduce un número válido para la columna"
            positionMaze maze
    else do
        putStrLn "Por favor, introduce un número válido para la fila"
        positionMaze maze


playMaze :: [String] -> (Int, Int) -> IO ()
playMaze maze pos = do
    if pos2mov maze pos == [] then do --No hay movimientos disponibles
        if isWin maze then do
            let currentMaze = replaceMaze pos 'o' maze
            putStrLn (unlines currentMaze)
            putStrLn "Enhorabuena! Has completado el puzzle"
            _ <- getLine
            level_select
        else do
            putStrLn (unlines maze)
            putStrLn "Prueba de nuevo. Te has quedado atrapado."
            _ <- getLine
            level_select


    else do
        let currentMaze = replaceMaze pos '$' maze
        putStrLn (unlines currentMaze)
        putStrLn "¿Hacia dónde moverse? "
        print (pos2mov maze pos)
        mov <- getLine
        if elem mov (pos2mov maze pos) then do
            let (newMaze, _, _) = mueve maze mov pos
            playMaze newMaze (siguientePosicion mov pos)
        else do
            putStrLn "Movimiento Inválido"
            playMaze maze pos


