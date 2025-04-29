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

restandoVida :: Int -> Int
restandoVida vida 
    | vida - 1000 > 0 = vida - 1000
    | vida - 1000 < 0 = 0

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = personaje {cantidadDeVida = restandoVida (cantidadDeVida personaje) } 

lluviaDeTuercas :: Personaje -> String -> Personaje 
lluviaDeTuercas personaje mismoEquipo 
    | mismoEquipo == "mismo equipo" = personaje {cantidadDeVida = (cantidadDeVida personaje) + 800}
    | mismoEquipo == "distinto equipo" = personaje {cantidadDeVida = (cantidadDeVida personaje) - div (cantidadDeVida personaje) 2}


enLasUltimas :: Personaje -> String
enLasUltimas personaje
    | cantidadDeVida personaje < 800 = "El personaje esta en las ultimas"
    | otherwise = "El personaje no esta en las ultimas"
