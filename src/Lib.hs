module Lib () where
import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String, 
    poderBasico :: String, 
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadDeVida :: Int
    } deriving (Show, Eq)


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
type Nombre = String
type Equipos = [Personaje]

team :: Equipos
team = [espina,pamela]

team2 :: Equipos
team2 = []

modificandoVida :: TipoDeModificacion -> Vida -> Modificacion -> Vida
modificandoVida "sumar vida" vida modificacion = (+vida) modificacion
modificandoVida "restar vida" vida modificacion = max (vida - modificacion) 0
modificandoVida _ vida _ = vida

hacerDanio :: Personaje -> Daño -> Personaje
hacerDanio contrincante danio = contrincante {cantidadDeVida  = modificandoVida "restar vida" (cantidadDeVida contrincante) danio}

vidaAequipo :: Personaje -> Agregado -> Personaje
vidaAequipo aliado agregado = aliado {cantidadDeVida = modificandoVida "sumar vida" (cantidadDeVida aliado) agregado} 

esDelMismoEquipo :: Personaje -> Equipos -> Bool
esDelMismoEquipo jugador equipo = (elem jugador) equipo

esMayorA3 :: Int -> Bool
esMayorA3 valor = valor >= 3

cambioNombre :: Nombre -> Nombre
cambioNombre nombre = nombre ++ " ...Espina estuvo aqui"

vidaMenorA :: Personaje -> Int -> Bool
vidaMenorA personaje valor = (<valor) (cantidadDeVida personaje)  

tienePocaVida :: Personaje -> String
tienePocaVida personaje
    | vidaMenorA personaje 800 = "El personaje esta en las ultimas"
    | otherwise = "El personaje se encuentra bien"

-- Funciones principales: 

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = hacerDanio contrincante 1000

lluviaDeTuercas :: Personaje -> Equipos -> Personaje
lluviaDeTuercas jugador equipo 
    | esDelMismoEquipo jugador equipo = vidaAequipo jugador 800
    | (not.esDelMismoEquipo jugador) equipo = hacerDanio jugador ((div (cantidadDeVida jugador)) 2) 
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

poderEspecialEspina :: Personaje -> Personaje
poderEspecialEspina contrincante = (bolaEspinosa.granadaDeEspinas contrincante) 5 

poderEspecialPamela :: Personaje -> Equipos -> Personaje
poderEspecialPamela jugador equipo = (torreCurativa.lluviaDeTuercas jugador) equipo

ataquePoderEspecial :: Personaje -> Personaje -> Personaje
ataquePoderEspecial personaje contrincante
    | (superPoderActivo personaje && nombre personaje == "Espina") = poderEspecialEspina contrincante
    | (superPoderActivo personaje && nombre personaje == "Pamela") = poderEspecialPamela contrincante team
    | otherwise = contrincante 

enLasUltimas :: Personaje -> String
enLasUltimas personaje = tienePocaVida personaje
    


