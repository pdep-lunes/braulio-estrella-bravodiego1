module Lib () where
import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String, 
    poderBasico :: String, 
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadDeVida :: Int
    } deriving Show


espina :: Personaje
espina = UnPersonaje {
    nombre = "Espina",
    poderBasico= "Bola de espinas",
    superPoder ="Granada de espinas",
    superPoderActivo = True,
    cantidadDeVida = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
    nombre = "Pamela",
    poderBasico= "Lluvia de tuercas",
    superPoder ="Torre curativa",
    superPoderActivo = True,
    cantidadDeVida = 9600
}

restandoVida :: Int -> Int -> Int
restandoVida vida danio = max (vida - danio) 0

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio contrincante danio = contrincante {cantidadDeVida  = restandoVida (cantidadDeVida contrincante) danio}

sumandoVida :: Int -> Int -> Int
sumandoVida vida agregado = (+vida) agregado

vidaAequipo :: Personaje -> Int -> Personaje
vidaAequipo compaDeEquipo agregado = compaDeEquipo {cantidadDeVida = sumandoVida (cantidadDeVida compaDeEquipo) agregado}

-- TRATAR DE JUNTAR hacerDanio Y vidaAequipo PARA NO REPETIR LOGICA

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = hacerDanio contrincante 1000

lluviaDeTuercas :: Personaje -> Bool -> Personaje 
lluviaDeTuercas jugador mismoEquipo 
    | mismoEquipo = vidaAequipo jugador 800
    | otherwise = hacerDanio jugador (div (cantidadDeVida jugador) 2) 

granadaDeEspinas :: Personaje -> Int -> Personaje
granadaDeEspinas contrincante radio
    | radio > 3 && cantidadDeVida contrincante < 800 =
      hacerDanio (contrincante {
          nombre = nombre contrincante ++ " ...Espina estuvo aqui",
          superPoderActivo = False
      }) (cantidadDeVida contrincante)
    | radio > 3 = contrincante {nombre = nombre contrincante ++ " ...Espina estuvo aqui"}
    | otherwise = bolaEspinosa contrincante

torreCurativa :: Personaje -> Personaje
torreCurativa compaDeEquipo = vidaAequipo compaDeEquipo (cantidadDeVida compaDeEquipo)


ataquePoderEspecialEspina :: Personaje -> Int -> Personaje
ataquePoderEspecialEspina contrincante radio
    | superPoderActivo contrincante = (bolaEspinosa.granadaDeEspinas contrincante) radio
    | otherwise = bolaEspinosa contrincante 

-- ataquePoderEspecialPamela :: Personaje -> Personaje
-- ataquePoderEspecialPamela jugador =

enLasUltimas :: Personaje -> String
enLasUltimas personaje
    | cantidadDeVida personaje < 800 = "El personaje esta en las ultimas"
    | otherwise = "El personaje no esta en las ultimas"


