-- Author: Till Hildebrandt, Antonio Galeazzi

module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0
fib     :: Integer -> Integer
fib x
  | x == 0    = 0
  | x == 1    = 1
  | x > 1     = fib(x-1) + fib(x-2)
  | otherwise     = error "undefined for negative input"


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 x
  | x >= 0 = fib2' 0 0 1
  | otherwise = error "undefined for negative input"
  where
  fib2' i x0 x1
    | x == i = x0
    | otherwise  = fib2' (i+1) x1 (x0+x1)


-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.

c       :: Integer -> Integer
c n 
  | n > 0 = c' 0 n
  | otherwise = error "undefined for values <= 0"
  where
  c' i n0
    | n0         == 1 = i
    | n0 `mod` 2 == 0 = c' (i+1) (n0 `div` 2)
    | n0 `mod` 2 == 1 = c' (i+1) (n0*3+1)
    | otherwise       = error "can't be reached"


-- Definieren Sie ein endrekurive Variante von c

c1      :: Integer -> Integer
c1 = undefined


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub
  | lb == ub = c lb
  | lb < ub = cmax (lb + 1) ub `max` c lb
  | otherwise = error "empty interval"


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub
  | lb == ub = f lb
  | lb < ub = imax f (lb + 1) ub `max` f lb
  | otherwise = error "empty interval"


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub
  | lb == ub  = (0, f lb)
  | lb < ub   = imax2' 0 f lb ub
  | otherwise = error "empty interval"
  where
  imax2' max f lb ub
    | lb == ub  = (lb, max)
    | lb < ub   = imax2' (max `max` fb lb) f (lb+1) ub
    | otherwise = error "empty interval"


-- ----------------------------------------
