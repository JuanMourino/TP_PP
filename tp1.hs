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
--El caso base es const porque no interesa el nombre y en Personaje esta primero la posición
--Por def de const: const p s = (\p -> _ -> p) p s = p
--Uso flip siguiente_posicion porque en la recursion esta primero la dirección y despues el llamado recursivo

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) (const) (id) 

{-Ejercicio 3-}
{-Idea: Como el universo es una lista podemos usar foldr y usar las funciones es_un_objeto y es_un_personaje respectivamente
Despues simplemente usar (:) como funcion del foldr para agregarlos a todos en una lista, caso base []
Podemos usar objeto_de y personaje_de para que la lista devuelta sea de tipo Objeto o Personaje y no de tipo Either
No importa el orden para la consigna YEY-}

objetos_en :: Universo -> [Objeto]
objetos_en = foldr(\x acc -> if (es_un_objeto x)
                              then (objeto_de x) : acc
                              else acc) []

personajes_en :: Universo -> [Personaje]
personajes_en = foldr(\x acc -> if (es_un_personaje x)
                                then (personaje_de x) : acc
                                else acc) []

{-Demostración:
qvq forall u :: Universo. forall o :: Objeto. elem o (objetos_en u) ==> elem (Right o) u

Para esto vamos a usar:

foldr f z [] = z {F0}
foldr f z (x:xs) = f x (foldr f z xs) {F1}

elem e [] = False {E0}
elem e (x:xs) = e == x || elem e xs {E1}

La definición de objetos_en que está más arriba, como {O0},
las definiciones de cada caso de es_un_objeto como {EO0} y {EO1} respectivamente
la definición de objeto_de como {OD0}

Y las reglas:
1) (\x -> Y) Z = Y remplazando x por Z
2) (\x -> F x) = F

Uso inducción sobre listas sobre el predicado P(xs) tq
P(ys) = forall o :: Objeto. elem o (objetos_en ys) ==> elem (Right o) ys

-caso base: P([])
elem o (objetos_en [])
= elem o (foldr (\x acc -> if (es_un_objeto x) then (objeto_de x):acc else acc) [] []) por O0
= elem o [] por F0
==> elem o []

-caso inductivo: qvq P(ys) ==> P(y:ys)
                      H.I.     T.I.
elem o (objeto_en (y:ys))
= elem o (foldr (\x acc -> if (es_un_objeto x) then (objeto_de x):acc else acc) [] (y:ys)) por O0
= elem o ((\x acc -> if (es_un_objeto x) then (objeto_de x):acc else acc) y foldr (\x acc -> if (es_un_objeto x) then (objeto_de x):acc else acc) [] ys) por F1
= elem o ((\x acc -> if (es_un_objeto x) then (objeto_de x):acc else acc) y (objetos_en ys)) por O0
= elem o (if (es_un_objeto y) then (objeto_de y):(objetos_en ys) else (objetos_en ys)) por la regla 2

Usamos extensión de Either:
-caso y = Left y':
elem o (if (es_un_objeto (Left y')) then (objeto_de Left y'):(objetos_en ys) else (objetos_en ys)))
= elem o (if False then (objeto_de Left y'):(objetos_en ys) else (objetos_en ys))) por EO0
= elem o (objetos_en ys) por def de if
==> elem (Right o) ys por HI
= False || (elem (Right o) ys)
= ((Right o) == (Left y')) || (elem (Right o) ys)
= elem (Right o) (Left y' : ys)
= elem (Right o) (y:ys)

-caso y = Right y'
elem o (if (es_un_objeto (Right y') then (objeto_de (Right y')):(objetos_en ys)) else (objetos_en ys)))
= elem o (if True then (objeto_de (Right y')):(objetos_en ys) else (objetos_en ys)) por EO0
= elem o (objeto_de (Right y')):(objetos_en ys) por def de if
= elem o (y' : (objetos_en ys)) por OD0
= (o == y') || (elem o (objetos_en ys)) por E1
==> (o == y') || (elem (Right o) ys) por HI
= ((Right o) == (Right y')) || (elem (Right o) ys)
= ((Right o) == y) || (elem (Right o) ys)
= elem (Right o) (y:ys) por E1
-}

{-Ejercicio 4-}
{-Idea: va revisando el universo, si vemos un objeto lo analizamos con foldObjeto (si esta destruido o en posesion de otra persona), 
filtramos todas sus demas apariciones del universo, si la persona que lo posee es la que se nos paso como parametro, lo agregamos a la 
lista que devuelve la funcion
En todos los casos seguimos con la recursion sobre el universo (afectado o no por un filtro)-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = foldr(\x res-> if en_posesión_de n x then x:res else res) [] (objetos_en u)  

{-Ejercicio 5-}
{-Idea: Usar una funcion auxiliar de distancia para los objetos (YA EXISTE), posiblemente una para obtener la posicion(usando foldObjeto)
Usar una funcion auxiliar para hallar la posicion del personaje en cuestion
Como asumimos que hay al menos un objeto podemos usar foldr1 yey
Problema, si el primer elemento del universo es un personaje, lo toma como caso base, no yey
Posible solucion al problema: al inicio filtrar el universo para que solo haya objetos sin quitar los personajes que los construyen
tal vez yey?-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano per u = foldr1 (\obj mas_cercano -> if (distancia (Left per) (Right obj)) < (distancia (Left per) (Right mas_cercano))
                                                            then obj else mas_cercano) (objetos_libres_en u)

{-Ejercicio 6-}
{-Idea: Recorremos el universo con una auxiliar que devuelve un Int, cuando vemos un objeto, usamos la funcion es_una_gema, si lo es,
vemos si la ultima persona que la tomo es Thanos (con foldObjeto), si tambien se cumple, sumamos 1 a lo que retorna
Si al final esta funcion retorna 6 gemas, entonces tiene_thanos_todas_las_gemas es True, sino es False-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = (está_el_personaje "Thanos" u) && (gemas_de_thanos u) == 6

gemas_de_thanos :: Universo -> Int
gemas_de_thanos u = length (filter (es_una_gema) (objetos_en_posesión_de "Thanos" u))

{-Ejercicio 7-}
{-Idea: Primero ver que not tiene_thanos_todas_las_gemas, despues usamos 2 auxiliares:
Una para ver si esta el personaje Thor y ademas el objeto Stormbreaker PREGUNTAR SI DEBE ESTA EN POSESION DE THOR
Otra para ver si estan los Personajes Wanda y Vision y esta el objeto "Gema de la Mente" y en posesion de Vision
usando estas 3 funciones, la funcion seria
(not tiene_thanos_todas_las_gemas) && (auxThor || auxWanda)
Podemos usar objeto_de_nombre para que nos devuelva Stormbreaker y Gema de la mente yey
Importante ver que Wanda, Vision y Thor esten vivos en sus respectivos casos-}

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

gemaRecursion = Objeto (0, 1) "Gema de Recursion"
gemaRecursion_thanos = Tomado gemaRecursion thanos

gemaProlog = Objeto (1, 0) "Gema de Prolog"
gemaProlog_thanos = Tomado gemaProlog thanos

gemaJava = Objeto (1, 1) "Gema de Java"
gemaJava_thanos = Tomado gemaJava thanos

gemaPython = Objeto (-1, 2) "Gema de Python"
gemaPython_thanos = Tomado gemaPython thanos

gemas_thanos = [gemaHaskell_thanos, gemaProlog_thanos, gemaPython_thanos, gemaJava_thanos, gemaRecursion_thanos]

universo_gema_destruida = universo_con [thor, thanos, wanda] (gemas_thanos ++ [gemaMente_destruida])

universo_thanos_con_gemas = universo_con [thor, thanos, wanda, phil, vision] (gemas_thanos ++ [stormBreaker_con_thor] ++ [gemaMente_thanos])
universo_ganamos_thor = universo_con [thor, wanda, thanos, phil, vision] ([stormBreaker_con_thor]++gemas_thanos)
universo_ganamos_wandaVision = universo_con [thor, wanda, thanos, phil, vision] ([gemaMente_vision] ++ gemas_thanos)
universo_ganamos_con_todo = universo_con [thor, wanda, thanos, phil, vision] ([gemaMente_vision, stormBreaker_con_thor] ++ gemas_thanos)
universo_perdemos_por_poco = universo_con [thor, wanda, vision_muerto, thanos] ([gemaMente_vision, stormBreaker_destruido_con_thor])
universo_todo_mal = universo_con [thor_muerto, wanda_muerta, vision, thanos, phil] [stormBreaker_con_thor, gemaMente_vision]
universo_sin_heroes = universo_con [phil, thanos] gemas_thanos

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
  foldObjeto(\p s -> 0) (\r d -> r+1) (\r -> r+1) stormBreaker
   ~=? 0
  ,
  foldObjeto(\p s -> 0) (\r d -> r+1) (\r -> r+1) (EsDestruido (EsDestruido (stormBreaker)))
   ~=? 2
  ,
  foldObjeto(\p s -> 0) (\r d -> r+1) (\r -> r+1) (Tomado (Tomado (EsDestruido (gemaMente)) phil) vision)
   ~=? 3
  ,
  foldObjeto(\p s -> 0) (\r d -> r+1) (\r -> r+1) (Tomado (Tomado gemaHaskell vision )vision)
   ~=? 2
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       
    ~=? (0,0)                   
  ,
  posición_personaje phil_cuatro_direcciones
    ~=? (0,0)
  ,
  posición_personaje thor
    ~=? (1, 1)
  ,
  posición_personaje thor_movido
    ~=? (2, 1)
  ,
  posición_personaje thor_muerto
    ~=? (1, 1)
  ,
  posición_personaje thor_movido_postmortem
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
  --A mi parecer, no tiene mucha utilidad poner muchos test en "nombre_objeto". Es preferible meter mas testeos en fold objeto, por ejemplo.

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
  tiene_thanos_todas_las_gemas universo_sin_thanos       
    ~=? False                                            
  ,
  tiene_thanos_todas_las_gemas universo_thanos_con_gemas
    ~=? True
  ,
  tiene_thanos_todas_las_gemas universo_gema_destruida
    ~=? False
  ,
  tiene_thanos_todas_las_gemas (universo_con [thor, thanos] [stormBreaker])
    ~=? False
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         
    ~=? False                                          
  ,
  podemos_ganarle_a_thanos universo_ganamos_con_todo
    ~=? True
  ,
  podemos_ganarle_a_thanos universo_ganamos_thor
    ~=? True
  ,
  podemos_ganarle_a_thanos universo_ganamos_wandaVision
    ~=? True
  ,
  podemos_ganarle_a_thanos universo_perdemos_por_poco
    ~=? False
  ,
  podemos_ganarle_a_thanos universo_sin_heroes
    ~=? False --Aunque Phil es mi heroe
  ,
  podemos_ganarle_a_thanos universo_thanos_con_gemas
    ~=? False
  ,
  podemos_ganarle_a_thanos universo_todo_mal
    ~=? False
  ]