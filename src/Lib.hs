module Lib () where
import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String, 
    poderBasico :: String, 
    superPoder :: String,
    poderActivo :: Bool,
    cantidadDeVida :: Int
    } deriving Show


personajeEspina :: Personaje
personajeEspina = UnPersonaje {
    nombre = "Espina",
    poderBasico= "Bola de espinas",
    superPoder ="Granada de espinas",
    poderActivo = True,
    cantidadDeVida = 4800
}

personajePamela :: Personaje
personajePamela = UnPersonaje {
    nombre = "Pamela",
    poderBasico= "Lluvia de tuercas",
    superPoder ="Torre curativa",
    poderActivo = False,
    cantidadDeVida = 9600
}

nombres :: Personaje -> String
nombres personaje = nombre personaje

restandoVida :: Int -> Int -> Int
restandoVida vida danio = max (vida - danio) 0

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio contrincante danio = contrincante {cantidadDeVida  = restandoVida (cantidadDeVida contrincante) danio}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = hacerDanio contrincante 1000

lluviaDeTuercas :: Personaje -> String -> Personaje 
lluviaDeTuercas contrincante mismoEquipo 
    | mismoEquipo == "mismo equipo" = contrincante {cantidadDeVida = (cantidadDeVida contrincante) + 800}
    | mismoEquipo == "distinto equipo" = contrincante {cantidadDeVida = (cantidadDeVida contrincante) - div (cantidadDeVida contrincante) 2}


enLasUltimas :: Personaje -> String
enLasUltimas personaje
    | cantidadDeVida personaje < 800 = "El personaje esta en las ultimas"
    | otherwise = "El personaje no esta en las ultimas"
