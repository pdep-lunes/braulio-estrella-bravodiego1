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
    superPoderActivo = False,
    cantidadDeVida = 9600
}

type Daño = Int
type Vida = Int
type Agregado = Int
type Radio = Int
type Modificacion = Int
type TipoDeModificacion = String
type Equipo = String

-- Funciones secundarias

modificandoVida :: Vida -> Modificacion -> TipoDeModificacion -> Vida
modificandoVida vida modificacion tipo
    | tipo == "sumar vida" = (+vida) modificacion
    | tipo == "restar vida" = max (vida - modificacion) 0
    | otherwise = vida

hacerDanio :: Personaje -> Daño -> Personaje
hacerDanio contrincante danio = contrincante {cantidadDeVida  = modificandoVida (cantidadDeVida contrincante) danio "restar vida"}

vidaAequipo :: Personaje -> Agregado -> Personaje
vidaAequipo aliado agregado = aliado {cantidadDeVida = modificandoVida (cantidadDeVida aliado) agregado "sumar vida"} 

esMayorA3 :: Int -> Bool
esMayorA3 valor = valor >= 3

cambioNombre :: String -> String
cambioNombre nombre = nombre ++ " ...Espina estuvo aqui"

vidaMenorA :: Personaje -> Int -> Bool
vidaMenorA personaje valor = cantidadDeVida personaje < valor 

tieneElSuperActivo :: Personaje -> String
tieneElSuperActivo personaje 
    | superPoderActivo personaje = "Si"
    | otherwise = "No"

poderEspecialEspina :: Personaje -> Personaje
poderEspecialEspina contrincante = (bolaEspinosa.granadaDeEspinas contrincante) 5 

poderEspecialPamela :: Personaje -> Personaje
poderEspecialPamela jugador = ((lluviaDeTuercas "mismo equipo").torreCurativa) jugador

tienePocaVidaOno :: Personaje -> String
tienePocaVidaOno personaje
    | cantidadDeVida personaje < 800 = "El personaje esta en las ultimas"
    | otherwise = "El personaje se encuentra bien"

-- Funciones principales: 

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = hacerDanio contrincante 1000

lluviaDeTuercas :: Equipo -> Personaje -> Personaje 
lluviaDeTuercas equipo jugador
    | equipo == "mismo equipo" = vidaAequipo jugador 800
    | equipo == "distinto equipo" = hacerDanio jugador ((div (cantidadDeVida jugador)) 2) 
    | otherwise = jugador

granadaDeEspinas :: Personaje -> Radio -> Personaje
granadaDeEspinas contrincante radio
    | esMayorA3 radio && vidaMenorA contrincante 800 = contrincante {
          nombre = cambioNombre (nombre contrincante),
          superPoderActivo = False,
          cantidadDeVida = 0
      } 
    | esMayorA3 radio = contrincante {nombre = nombre contrincante ++ " ...Espina estuvo aqui"}
    | otherwise = bolaEspinosa contrincante

torreCurativa :: Personaje -> Personaje
torreCurativa aliado = vidaAequipo aliado (cantidadDeVida aliado)

ataquePoderEspecial :: Personaje -> Personaje -> Personaje
ataquePoderEspecial personaje contrincante
    | (tieneElSuperActivo personaje == "Si" && nombre personaje == "Espina") = poderEspecialEspina contrincante
    | (tieneElSuperActivo personaje == "Si" && nombre personaje == "Pamela") = poderEspecialPamela contrincante
    | otherwise = contrincante 

enLasUltimas :: Personaje -> String
enLasUltimas personaje = tienePocaVidaOno personaje
    


