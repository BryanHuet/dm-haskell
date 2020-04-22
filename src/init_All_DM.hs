import Data.List
data Formule = Var String
      | Non Formule
      | Imp Formule Formule
      | Et Formule Formule
      | Ou Formule Formule
      | Equi Formule Formule
    deriving (Eq, Show)

f1 = (Ou (Et (Var "c") (Var "d")) (Et (Var "a") (Var "b")))
f2 = (Imp (Var "d") (Ou (Var "c") (Non (Var "b"))))
f3 = (Equi (Var "d") (Et (Var "c") (Var "d")))
f4 = (Imp (Non (Var "d")) (Ou (Var "c") (Var "b")))
f5 = (Non (Non (Var "a")))
f6 = (Imp (Non (Ou (Var "a") (Var "d"))) (Ou (Var "c") (Var "b")))


-- Q1
visuFormule :: Formule -> String
visuFormule (Var p) = p
visuFormule (Non f) = "~" ++ visuFormule f
visuFormule (Et g d) = "(" ++ (visuFormule g) ++ " & " ++ (visuFormule d) ++ ")"
visuFormule (Ou g d) =  "(" ++ (visuFormule g) ++ " v " ++ (visuFormule d) ++ ")"
visuFormule (Imp g d) = "(" ++ (visuFormule g) ++ " => " ++ (visuFormule d) ++ ")"
visuFormule (Equi g d) ="(" ++ (visuFormule g) ++ " <=> " ++ (visuFormule d) ++ ")"



-- Q3
elimine :: Formule -> Formule
elimine (Var p) = (Var p)
elimine (Non f) = (Non (elimine f))
elimine (Et g d) = (Et (elimine g) (elimine d))
elimine (Ou g d) =(Ou (elimine g) (elimine d))
elimine (Imp g d) = (Ou (Non (elimine g)) (elimine d))
elimine (Equi g d) = (Et (elimine (Imp (elimine g) (elimine d))) (elimine (Imp (elimine d) (elimine g))))



f2b = (Imp (Non (Var "d")) (Ou (Var "c") (Var "b")))

-- Q6
ameneNon, disNon :: Formule -> Formule

ameneNon (Var p) = (Var p)
ameneNon (Non f) = disNon f
ameneNon (Et g d) = (Et (ameneNon g) (ameneNon d))
ameneNon (Ou g d) = (Ou (ameneNon g) (ameneNon d))

disNon (Var p) = (Non (Var p))
disNon (Non f) = (f)
disNon (Et g d) = (Ou (disNon g) (disNon d))
disNon (Ou g d) = (Et (disNon g) (disNon d))



-- Q7
normalise :: Formule -> Formule
normalise (Et g d) = concEt (normalise g) (normalise d)
normalise (Ou g d) = developper (normalise g) (normalise d)
normalise f = f

concEt :: Formule -> Formule -> Formule
concEt (Et g d) f = (Et g (concEt d f))
concEt g f = (Et g f)

developper :: Formule -> Formule -> Formule
developper (Et g d) x = (Et (Ou x g) (Ou x d))
developper x (Et g d) = (Et (Ou x g) (Ou x d))
developper x y = (Ou x y)

-- Q8
formeClausale :: Formule -> Formule
formeClausale f = normalise (ameneNon (elimine f))



-- FIN PARTIE 1
--


type Clause = [Formule]
type FormuleBis = [Clause]


-- Q9

t = (Et (Et (Ou (Var "B") (Var "C")) (Var "D")) (Var "E"))

etToListe :: Formule -> FormuleBis
etToListe (Et g d) = (ouToListe g) : (etToListe d)
etToListe f = [ouToListe f]

ouToListe :: Formule -> Clause
ouToListe (Ou g d) =   (ouToListe g )++(ouToListe d)
ouToListe f = [f]

-- fonction introduite pour une meilleure lisibilite
--
formeClausaleBis :: Formule -> FormuleBis
formeClausaleBis f = etToListe f



-- Q10 et Q11

neg :: Formule -> Formule
neg (Non f) = f
neg f = (Non f)


sontLiees :: Clause -> Clause -> Bool
sontLiees [] _ = False
sontLiees (x:xs) ys = ((neg x) `elem` ys) || (sontLiees xs ys)


-- Q12
resolvante :: Clause -> Clause -> Clause
resolvante [] ys = []
resolvante (x:xs) (y:ys)
      | ((neg x) `elem` (y:ys))== True =  xs ++ (delete (neg x) (y:ys))
      | ((neg y) `elem` (x:xs))== True =  ys ++ (delete (neg y) (x:xs))
      | otherwise = [x] ++ [y] ++ resolvante xs ys


-- FIN PARTIE 2
--



deduire :: Formule -> Clause
deduire x = resoudre (head sorite) (tail sorite)
  where sorite = (formeClausaleBis (formeClausale x))

resoudre :: Clause -> FormuleBis -> Clause
resoudre xs [] = xs
resoudre xs (ys:yss)
  | sontLiees xs ys = resoudre (resolvante xs ys) yss
  | otherwise = resoudre xs (yss ++ [ys])



-- exemple #1
--
exemple1 = (Et (Imp (Var "A") (Var "B"))
    (Et (Imp (Var "B") (Var "C"))
    (Et (Imp (Var "C") (Non (Var "D")))
    (Et (Imp (Non (Var "D")) (Non (Var "E")))
    (Imp (Non (Var "E")) (Var "F"))))))


-- exemple #2
--
exemple2 = (Et (Imp (Non (Var "A")) (Var "B"))
  (Et (Imp (Var "C") (Non (Var "D")))
  (Et (Imp (Var "E") (Non (Var "F")))
  (Et (Imp (Non (Var "D")) (Non (Var "B")))
  (Imp (Var "A") (Var "F"))))))

-- autre formulation de exemple #2
--
a = "etre un exercice qui me fait ronchonner"
b = "etre un exercice que je comprends"
c = "etre parmi ces sorites"
d = "etre dispose regulierement, comme les exercices auxquels je suis habitue"
e = "etre un exercice facile"
f = "etre un exercice qui me donne mal a la tete"

testBis = (Et (Imp (Non (Var a)) (Var b))
  (Et (Imp (Var c) (Non (Var d)))
  (Et (Imp (Var e) (Non (Var f)))
  (Et (Imp (Non (Var d)) (Non (Var b)))
  (Imp (Var a) (Var f))))))


-- exemple 3 - les bebes
--
bebe = Et (Imp (Var "bebe") (Non (Var "logique")))
  (Et (Imp (Var "tuer crocodile") (Non (Var "meprise")))
  (Imp (Non (Var "logique")) (Var "meprise")))

-- QUESTION BONUS
--

logicien = (Et (Imp (Var "saint") (Var ("logicien")))
  (Et (Imp (Var "malade") (Non(Var "saint")))
  (Et (Imp (Var "enfant") (Non(Var "logicien")))
  (Imp (Non (Var "malade")) (Var "saint")))))
