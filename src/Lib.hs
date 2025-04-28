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

cantDeVida :: Int -> Int
cantidadDeVida vida = (-1000) vida

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa (nombre, poder, superPoder, poderActivo, cantidadDeVida) = UnPersonaje (cantidadDeVida = cantDeVida cantidadDeVida)




