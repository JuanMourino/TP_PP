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
nombre_objeto = foldObjeto (flip const) (const) (id) --flip porque quiero que me devuelva el argumento de la derecha

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
-caso Left y:
elem o (if (es_un_objeto (Left y)) then (objeto_de Left y):(objetos_en ys) else (objetos_en ys)))
= elem o (if False then (objeto_de Left y):(objetos_en ys) else (objetos_en ys))) por EO0
= elem o (objetos_en ys) por def de if
= elem o (objetos_en ys) por OD0
==> elem (Right o) ys por HI
==> (Right o) == (Left y) || elem (Right o) ys
= elem (Right o) (Left y : ys)

-caso Right y
elem o (if (es_un_objeto (Right y) then (objeto_de (Right y)):(objetos_en ys)) else (objetos_en ys)))
= elem o (if True then (objeto_de (Right y)):(objetos_en ys) else (objetos_en ys)) por EO0
= elem o (objeto_de (Right y)):(objetos_en ys) por def de if
= elem o (y : (objetos_en ys)) por OD0
= o == y || elem o (objetos_en ys) por E1
==> o == y || elem (Right o) ys por HI
= (Right o) == (Right y) || elem (Right o) ys
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
objeto_libre_mas_cercano per u = foldr1 (\obj mas_cercano -> if (distancia per obj) < (distancia per mas_cercano)
                                                            then obj else acc) (objetos_en u)

{-Ejercicio 6
{-Idea: Recorremos el universo con una auxiliar que devuelve un Int, cuando vemos un objeto, usamos la funcion es_una_gema, si lo es,
vemos si la ultima persona que la tomo es Thanos (con foldObjeto), si tambien se cumple, sumamos 1 a lo que retorna
Si al final esta funcion retorna 6 gemas, entonces tiene_thanos_todas_las_gemas es True, sino es False-}

tiene_thanos_todas_las_gemas :: ?
tiene_thanos_todas_las_gemas = ?

{-Ejercicio 7-}
{-Idea: Primero ver que not tiene_thanos_todas_las_gemas, despues usamos 2 auxiliares:
Una para ver si esta el personaje Thor y ademas el objeto Stormbreaker PREGUNTAR SI DEBE ESTA EN POSESION DE THOR
Otra para ver si estan los Personajes Wanda y Vision y esta el objeto "Gema de la Mente" y en posesion de Vision
usando estas 3 funciones, la funcion seria
(not tiene_thanos_todas_las_gemas) && (auxThor || auxWanda)
Podemos usar objeto_de_nombre para que nos devuelva Stormbreaker y Gema de la mente yey
Importante ver que Wanda, Vision y Thor esten vivos en sus respectivos casos-}

podemos_ganarle_a_thanos :: ?
podemos_ganarle_a_thanos = ?

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

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ]
  -}