module Library where
import PdePreludat

-- Defino mis alias
type Ataque = Personaje-> Personaje
type Defensa = Personaje-> Personaje
type Elementos = [Elemento]
type Nombre = String
type Salud = Number
type Anio = Number
type Transformacion = Personaje -> Personaje
type Danio = Number
type Enemigos = [Personaje]
type Esbirros = [Elemento]

-- Defino mis tipos
data Elemento = UnElemento { 
    tipo :: String,
    ataque :: Ataque,
    defensa :: Defensa
} deriving Show

data Personaje = UnPersonaje { 
    nombre :: Nombre,
    salud :: Salud,
    elementos :: Elementos,
    anioPresente :: Anio 
} deriving Show

-- Inicializo mis sujetos de pruebas
elementoGenerico = UnElemento "Generico" (causarDanio 10) noHacerNada
elementoMalvado = UnElemento "Malvado" (causarDanio 10000) noHacerNada

protagonista = UnPersonaje "Protagonista" 100 [] 2023

villanoGenerico = UnPersonaje "Villano Generico" 1000 [elementoGenerico] 2023
villanoMalefico = UnPersonaje "Villano Malefico" 10000 [elementoMalvado, elementoGenerico] 2023

villanos = [villanoGenerico, villanoMalefico]

-- Defino algunas transformaciones basicas
noHacerNada :: Transformacion
noHacerNada personaje = personaje

mandarAlAnio :: Anio -> Transformacion
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Transformacion
meditar personaje = personaje {salud = salud personaje * 1.5}

calcularDanio :: Danio -> Salud -> Salud
calcularDanio danio saludPersonaje 
    | 0 > (saludPersonaje - danio) = 0
    | otherwise = saludPersonaje - danio
causarDanio :: Danio -> Transformacion
causarDanio danio personaje = personaje {salud = calcularDanio danio (salud personaje)}

-- Defino funciones para obtener información extra de los personajes
esMalvadoElemento :: Elemento -> Bool
esMalvadoElemento = (== "Malvado").tipo -- esMalvadoElemento elemento = (tipo elemento) == "Malvado"
esMalvado :: Personaje -> Bool
esMalvado = any (esMalvadoElemento).elementos -- esMalvado personaje = any (esMalvadoElemento) (elementos personaje)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = (salud personaje) - salud ((ataque elemento) personaje)

loMataConUno :: Personaje -> Personaje -> Bool
loMataConUno personaje enemigo = any ((== salud personaje).(danioQueProduce personaje)) (elementos enemigo)
enemigosMortales :: Personaje -> Enemigos -> Enemigos
enemigosMortales personaje enemigos = filter (loMataConUno personaje) enemigos

-- Defino personajes y elementos
concentracion :: Number -> Elemento
concentracion cantidad = UnElemento "Magia" noHacerNada (foldl1 (.) (replicate cantidad meditar)) 
                                                        -- Compongo la lista, por lo que aplico meditar la cantidad de veces 
                                                        -- que cantidad de elementos tenga la lista

esbirrosMalvados :: Number -> Esbirros
esbirrosMalvados cantidad = replicate cantidad (UnElemento "Maldad" (causarDanio 1) noHacerNada)

katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada
jack = UnPersonaje "Jack" 300 [(concentracion 3), katanaMagica] 200

generarNuevo :: Anio -> Salud -> Transformacion
generarNuevo anio saludActual aku = aku {salud = saludActual, anioPresente = anio}
portalAlFuturo :: Anio -> Salud -> Elemento
portalAlFuturo anio saludActual = UnElemento "Magia" (mandarAlAnio (anio + 2800)) (generarNuevo (anio + 2800) saludActual)
aku :: Number -> Number -> Personaje
aku anio salud = UnPersonaje "Aku" salud ((esbirrosMalvados (100*anio))++[(concentracion 4), (portalAlFuturo anio salud)]) anio

-- Defino luchar
defensorAtacado :: Elementos -> Personaje -> Personaje
defensorAtacado elementos defensor = foldr ($) defensor $ (map (ataque) elementos)

atacanteAtacado :: Elementos -> Personaje -> Personaje
atacanteAtacado elementos atacante = foldr ($) atacante $ (map (defensa) elementos)

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar personaje1 personaje2 
    | (== 0) (salud (atacanteAtacado (elementos personaje2) personaje1)) = (personaje2, personaje1)
    | otherwise = luchar (defensorAtacado (elementos personaje1) personaje2) (atacanteAtacado (elementos personaje2) personaje1)

-- Inferir tipo de la función
-- Eq t => (t -> a1 -> (a2, a2)) -> (Number -> t) -> t -> [a1] -> [a2]
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))