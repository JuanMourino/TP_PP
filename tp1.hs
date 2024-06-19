import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> b) -> (b -> Dirección -> b) -> (b -> b) -> Personaje -> b
foldPersonaje cBase cMueve cMuere per = case per of
                                        Personaje pos nombre -> cBase pos nombre
                                        Mueve personaje dir -> cMueve (f personaje) dir 
                                        Muere personaje -> cMuere (f personaje)
                                        where f = foldPersonaje cBase cMueve cMuere

foldObjeto :: (Posición -> String -> b) -> (b -> Personaje -> b) -> (b -> b) -> Objeto -> b
foldObjeto cBase cTomado cDestruido obj = case obj of
                                          Objeto pos nombre -> cBase pos nombre
                                          Tomado objeto personaje -> cTomado (f objeto) personaje 
                                          EsDestruido objeto -> cDestruido (f objeto)
                                          where f = foldObjeto cBase cTomado cDestruido

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (const) (siguiente_posición) (id)

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) (const) (id) 

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en u = map (objeto_de) (filter es_un_objeto u)

personajes_en :: Universo -> [Personaje]
personajes_en u = map (personaje_de) (filter es_un_personaje u)

{-Demostración:
qvq forall u :: Universo. forall o :: Objeto. elem o (objetos_en u) ==> elem (Right o) u

Para esto vamos a usar:

filter p [] = [] {F0}
filter p (x:xs) = if (p x) then x : (filter p xs) else (filter p xs) {F1}

map f [] = [] {M0}
map f (x:xs) = (f x) : (map f xs) {M1} 

elem e [] = False {E0}
elem e (x:xs) = e == x || elem e xs {E1}

La definición de objetos_en que está más arriba, como {O0},
Las definiciones de cada caso de es_un_objeto como {EO0} y {EO1} respectivamente
La definición de objeto_de como {OD0}

Uso inducción sobre listas sobre el predicado P(xs) tq
P(ys) = forall o :: Objeto. elem o (objetos_en ys) ==> elem (Right o) ys

-caso base: P([])
elem o (objetos_en [])
= elem o (map (objeto_de) (filter es_un_objeto [])) por O0
= elem o (map []) por F0
= elem o [] por M0
==> elem o []

-caso inductivo: qvq P(ys) ==> P(y:ys)
                      H.I.     T.I.
elem o (objeto_en (y:ys))
= elem o (map (objeto_de) (filter es_un_objeto (y:ys))) por O0
= elem o (map(objeto_de) if (es_un_objeto y) then y:(filter es_un_objeto ys) else (filter es_un_objeto ys)) por F1

Usamos extensión de Either:
-caso y = Left y':
elem o (map (objeto_de) (if (es_un_objeto (Left y')) then (Left y'):(filter es_un_objeto ys) else (filter es_un_objeto ys))))
= elem o (map (objeto_de) (if False then (objeto_de Left y'):(filter es_un_objeto ys) else (filter es_un_objeto ys)))) por EO0
= elem o (map (objeto_de (filter es_un_objeto ys))) por def de if
= elem o (objetos_en ys) por O0
==> elem (Right o) ys por HI
= False || (elem (Right o) ys)
= ((Right o) == (Left y')) || (elem (Right o) ys)
= elem (Right o) (Left y' : ys) por E1
= elem (Right o) (y:ys)

-caso y = Right y'
elem o (map (objeto_de) (if (es_un_objeto (Right y') then (objeto_de (Right y')):(filter es_un_objeto ys)) else (filter es_un_objeto ys))))
= elem o (map (objeto_de) (if True then (objeto_de (Right y')):(filter es_un_objeto ys) else (filter es_un_objeto ys))) por EO1
= elem o (map (objeto_de) (Right y'):(filter es_un_objeto ys) por def de if
= elem o ((objeto_de Left y') : (map (objeto_de) (filter es_un_objeto ys))) por M1
= elem o (y' : (map (objeto_de) (filter es_un_objeto ys))) por OD0
= elem o (y' : objetos_en ys) por O0
= (o == y') || (elem o (objetos_en ys)) por E1
==> (o == y') || (elem (Right o) ys) por HI
= ((Right o) == (Right y')) || (elem (Right o) ys)
= ((Right o) == y) || (elem (Right o) ys)
= elem (Right o) (y:ys) por E1
-}

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = filter (en_posesión_de n) (objetos_en u)

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano per u = foldr1 (\obj mas_cercano -> if (distancia (Left per) (Right obj)) < (distancia (Left per) (Right mas_cercano))
                                                            then obj else mas_cercano) (objetos_libres_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = (está_el_personaje "Thanos" u) && (gemas_de_thanos u) == 6
--Usamos la función está_el_personaje para comprobar que está thanos y además si está vivo, porque la consigna pide que esté el personaje de nombre Thanos

gemas_de_thanos :: Universo -> Int
gemas_de_thanos u = length (filter (es_una_gema) (objetos_en_posesión_de "Thanos" u))

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = (not (tiene_thanos_todas_las_gemas u))  && (estaThor u || wanda_Vision u)

estaThor :: Universo -> Bool
estaThor u =  (está_el_personaje "Thor" u)  && (está_el_objeto "StormBreaker" u) && (en_posesión_de "Thor" (objeto_de_nombre "StormBreaker" u))

wanda_Vision :: Universo -> Bool
wanda_Vision u = (está_el_personaje "Wanda" u) && (está_el_personaje "Visión" u) && (está_el_objeto "Gema de la Mente" u) && (en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u))

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]
steve = Personaje (0, 0) "Steve"

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

phil_cuatro_direcciones = Mueve (Mueve (Mueve (Mueve phil Este) Oeste) Sur) Norte

thor = Personaje (1, 1) "Thor"
thor_movido = Mueve thor Este
thor_muerto = Muere thor
thor_movido_postmortem = Mueve (Muere thor_movido) Sur

thanos = Personaje (-5, 4) "Thanos"
thanos_muerto = Muere thanos

wanda = Personaje (3, -1) "Wanda"
wanda_muerta = Muere wanda

vision = Personaje (3, -2) "Visión"
vision_muerto = Muere vision

stormBreaker = Objeto (0, 0) "StormBreaker"
stormBreaker_destruido = EsDestruido stormBreaker
stormBreaker_con_thor = Tomado stormBreaker thor
stormBreaker_destruido_con_thor = EsDestruido stormBreaker_con_thor
stormBreaker_tomado_destruido = Tomado stormBreaker_destruido_con_thor thanos

gemaMente = Objeto (-10, 1) "Gema de la Mente"
gemaMente_vision = Tomado gemaMente vision
gemaMente_thanos = Tomado gemaMente thanos
gemaMente_destruida = EsDestruido gemaMente_thanos

gemaHaskell = Objeto (0,0) "Gema de Haskell"
gemaHaskell_thanos = Tomado gemaHaskell thanos
gemaHaskell_vision = Tomado gemaHaskell vision

gemaRecursion = Objeto (0, 1) "Gema de Recursion"
gemaRecursion_thanos = Tomado gemaRecursion thanos
gemaRecursion_vision = Tomado gemaRecursion vision

gemaProlog = Objeto (1, 0) "Gema de Prolog"
gemaProlog_thanos = Tomado gemaProlog thanos
gemaProlog_vision = Tomado gemaProlog vision

gemaJava = Objeto (1, 1) "Gema de Java"
gemaJava_thanos = Tomado gemaJava thanos
gemaJava_vision = Tomado gemaJava vision

gemaPython = Objeto (-1, 2) "Gema de Python"
gemaPython_thanos = Tomado gemaPython thanos
gemaPython_vision = Tomado gemaPython vision

gemas_thanos = [gemaHaskell_thanos, gemaProlog_thanos, gemaPython_thanos, gemaJava_thanos, gemaRecursion_thanos]
gemas_vision = [gemaHaskell_vision, gemaProlog_vision, gemaPython_vision, gemaJava_vision, gemaRecursion_vision, gemaMente_vision]

universo_gema_destruida = universo_con [thor, thanos, wanda] (gemas_thanos ++ [gemaMente_destruida])

universo_thanos_con_gemas = universo_con [thor, thanos, wanda, phil, vision] (gemas_thanos ++ [stormBreaker_con_thor] ++ [gemaMente_thanos])
universo_ganamos_thor = universo_con [thor, wanda, thanos, phil, vision] ([stormBreaker_con_thor]++gemas_thanos)
universo_ganamos_wandaVision = universo_con [thor, wanda, thanos, phil, vision] ([gemaMente_vision] ++ gemas_thanos)
universo_ganamos_con_todo = universo_con [thor, wanda, thanos, phil, vision] ([gemaMente_vision, stormBreaker_con_thor] ++ gemas_thanos)
universo_perdemos_por_poco = universo_con [thor, wanda, vision_muerto, thanos] ([gemaMente_vision, stormBreaker_destruido_con_thor])
universo_todo_mal = universo_con [thor_muerto, wanda_muerta, vision, thanos, phil] [stormBreaker_con_thor, gemaMente_vision]
universo_sin_heroes = universo_con [phil, thanos] gemas_thanos
universo_gemas_vision = universo_con [thanos, vision, wanda, phil, steve] gemas_vision

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                              -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     
    ~=? 1
  ,
  foldPersonaje(\p s -> 0) (\r d -> r+1) (\r -> r+1) (Mueve (phil) Este)   
    ~=? 1         
  ,
  foldPersonaje(\p s -> 0) (\r d -> r+1) (\r -> r+1) (Mueve (Muere (steve)) Este) 
    ~=? 2 
  ,
  foldPersonaje(\p s -> 0) (\r d -> r+1) (\r -> r+1) (Mueve (Mueve (Muere (steve)) Este) Sur)
   ~=? 3
  ,
  foldPersonaje (\p s -> []) (\r d -> d : r) (id) thor_movido
    ~=? [Este]
  ,
  foldPersonaje (\p s -> []) (\r d -> d : r) (id) thor_movido_postmortem
    ~=? [Sur, Este]
  ,
  foldPersonaje (\p s -> []) (\r d -> d : r) (id) phil_cuatro_direcciones
    ~=? [Norte, Sur, Oeste, Este]
  ,
  foldObjeto(\p s -> 0) (\r p -> r+1) (\r -> r+1) stormBreaker
   ~=? 0
  ,
  foldObjeto(\p s -> 0) (\r p -> r+1) (\r -> r+1) (EsDestruido (EsDestruido (stormBreaker)))
   ~=? 2
  ,
  foldObjeto(\p s -> 0) (\r p -> r+1) (\r -> r+1) (Tomado (Tomado (EsDestruido (gemaMente)) phil) vision)
   ~=? 3
  ,
  foldObjeto(\p s -> 0) (\r p -> r+1) (\r -> r+1) (Tomado (Tomado gemaHaskell vision )vision)
   ~=? 2
  ,
  foldObjeto (\p s -> []) (\r p -> p : r) (id) (Tomado gemaMente_thanos vision)
    ~=? [vision, thanos]
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       
    ~=? (0,0)                   
  ,
  posición_personaje phil_cuatro_direcciones --Se mueve en todas las direcciones y vuelve al lugar inicial
    ~=? (0,0)
  ,
  posición_personaje thor
    ~=? (1, 1)
  ,
  posición_personaje thor_movido
    ~=? (2, 1)
  ,
  posición_personaje thor_muerto --Un personaje que se movio y despues muere
    ~=? (1, 1)
  ,
  posición_personaje thor_movido_postmortem --Un personaje que se mueve despues de muerto
    ~=? (2, 0)
  ,
  nombre_objeto gemaHaskell
    ~=? "Gema de Haskell"
  ,
  nombre_objeto stormBreaker
    ~=? "StormBreaker"
  ,
  nombre_objeto mjölnir
    ~=? "Mjölnir"
  ,
  nombre_objeto gemaProlog
    ~=? "Gema de Prolog"
  ,
  nombre_objeto gemaPython
     ~=? "Gema de Python"
  ,
  nombre_objeto stormBreaker_destruido --Da el nombre de un objeto destruido
    ~=? "StormBreaker"
  ,
  nombre_objeto gemaMente_thanos --Da el nombre de un objeto tomado por algun personaje
    ~=? "Gema de la Mente"
  ,
  nombre_objeto gemaMente_vision
    ~=? "Gema de la Mente"
  ,
  nombre_objeto stormBreaker_destruido_con_thor --Da el nombre de un objeto destruido y tomado
    ~=? "StormBreaker"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []      
    ~=? []            
  ,
  personajes_en []
    ~=? []
  ,
  objetos_en (universo_con [] [stormBreaker])
    ~=? [stormBreaker]
  ,
  objetos_en [Left thanos, Right stormBreaker,Left thor]
    ~=? [stormBreaker]
  ,
  objetos_en [Left thanos, Right gemaMente,Left thor, Right stormBreaker_con_thor]
    ~=? [gemaMente, stormBreaker_con_thor] --No importa si hay objetos entre los personajes
  ,
  objetos_en (universo_con [thor] [gemaMente, stormBreaker_destruido_con_thor])
    ~=? [gemaMente, stormBreaker_destruido_con_thor]
  ,
  personajes_en (universo_con [] [stormBreaker])
   ~=? []
  ,
  personajes_en (universo_con [phil, thor, thanos] [])
    ~=? [phil, thor, thanos]
  ,
  personajes_en universo_perdemos_por_poco
    ~=? [thor, wanda, vision_muerto, thanos] --Se asegura de incluir personajes muertos
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       
    ~=? []                             
  ,
  objetos_en_posesión_de "Thor" universo_ganamos_thor
    ~=? [stormBreaker_con_thor]
  ,
  objetos_en_posesión_de "Thanos" universo_thanos_con_gemas
    ~=? (gemas_thanos ++ [gemaMente_thanos])
  ,
  objetos_en_posesión_de "Thor" universo_perdemos_por_poco
   ~=? [] --Donde hay un objeto en su posesion pero destruido
  ,
  objetos_en_posesión_de "Visión" universo_perdemos_por_poco
    ~=? [gemaMente_vision] --Donde el personaje esta muerto
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       
    ~=? mjölnir                                       
  ,
  objeto_libre_mas_cercano phil (universo_con [thor, phil] [mjölnir, stormBreaker])
    ~=? stormBreaker
  ,
  elem (objeto_libre_mas_cercano steve (universo_con [steve, phil, thanos] [gemaRecursion, gemaJava, gemaProlog, gemaMente, gemaPython])) [gemaRecursion, gemaProlog]
    ~=? True --Hay mas de una opcion, solo se asegura que este entre las opciones
  ,
  objeto_libre_mas_cercano steve (universo_con [thanos, steve, thor] (gemas_thanos ++ [mjölnir]))
    ~=? mjölnir --Donde hay otros mas cercanos, pero no estan libres
  ,
  objeto_libre_mas_cercano steve (universo_con [thanos, thor, steve] [stormBreaker_destruido, mjölnir, gemaMente]) 
    ~=? mjölnir --Donde hay uno mas cercano pero destruido 
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas []
    ~=? False
  ,
  tiene_thanos_todas_las_gemas universo_sin_thanos       
    ~=? False                                            
  ,
  tiene_thanos_todas_las_gemas universo_thanos_con_gemas
    ~=? True
  ,
  tiene_thanos_todas_las_gemas universo_gema_destruida
    ~=? False --Tiene todas las gemas, pero una esta destruida
  ,
  tiene_thanos_todas_las_gemas (universo_con [thor, thanos] [stormBreaker])
    ~=? False --No estan las gemas en el universo
  ,
  tiene_thanos_todas_las_gemas universo_gemas_vision
    ~=? False --Otro personaje tiene todas las gemas
  ,
  tiene_thanos_todas_las_gemas (universo_con [thor, thanos_muerto] (gemas_thanos++[gemaMente_thanos]))
    ~=? False --Thanos tiene todas las gemas pero está muerto
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos []
    ~=? False
  ,
  podemos_ganarle_a_thanos universo_sin_thanos         
    ~=? False
  ,
  podemos_ganarle_a_thanos universo_ganamos_con_todo
    ~=? True
  ,
  podemos_ganarle_a_thanos universo_ganamos_thor
    ~=? True --Caso donde ganamos por thor y no por wanda y vision
  ,
  podemos_ganarle_a_thanos universo_ganamos_wandaVision
    ~=? True --Ganamos con Wanda y vision y no con Thor
  ,
  podemos_ganarle_a_thanos universo_perdemos_por_poco
    ~=? False --Vision tiene la gema, pero esta muerto, Stormbreaker esta destruido en posesion de thor
  ,
  podemos_ganarle_a_thanos universo_sin_heroes
    ~=? False --Aunque Phil es mi heroe
  ,
  podemos_ganarle_a_thanos universo_thanos_con_gemas
    ~=? False --Thanos tiene todas las gemas y thor tiene stormbreaker
  ,
  podemos_ganarle_a_thanos universo_todo_mal
    ~=? False --No se cumple ninguna condicion donde se le puede ganar
  ,
  podemos_ganarle_a_thanos universo_gemas_vision
    ~=? True --Vision tiene todas las gemas
  ]