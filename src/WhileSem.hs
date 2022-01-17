import ReadWrite
import Domain
import Control.DeepSeq
import System.IO

eval :: E -> State -> Domain
eval (Evar x) s = s x

-- Cell Values

eval (Ehd e) s = return . head $ eval e s
eval (Etl e) s = tail $ eval e s
eval (Econs e1 e2) s = (eval e1 s) ++ (eval e2 s)

-- Int Values

eval Ezero s = return (IntValue 0)
eval (Esucc n) s = (IntValue . (+ 1) . getInt) <$> eval n s
eval (Epred n) s = (IntValue . (\x -> x - 1) . getInt) <$> eval n s
eval (Eif b n1 n2) s = do
    t <- eval b s
    let t' = getBool t
    if (t' == True) then eval n1 s
                    else eval n2 s

-- Bool Values 

eval Etrue s = return (BoolValue True)
eval Efalse s = return (BoolValue False)
eval (Elt n1 n2) s = do
    n1' <- eval n1 s
    n2' <- eval n2 s
    return . BoolValue $ n1' < n2'
eval (Eeq n1 n2) s = do
    n1' <- eval n1 s
    n2' <- eval n2 s
    return . BoolValue $ n1' == n2'
eval (Enot b) s = (BoolValue . not . getBool) <$> eval b s

sem :: C -> State -> State
sem Cskip s = s
sem (Cassign x e) s = update s x (eval e s)
sem (Cseq c1 c2) s = sem c2 (sem c1 s)
sem (Cfor n c) s = expon i (sem c) s
    where i = getInt . head $ eval n s

sem (Cif b c1 c2) s 
    | getBool . head $ eval b s = sem c1 s
    | otherwise = sem c2 s

sem (Cwhile b c) s = fix bigF s
    where bigF f s  | getBool . head $ eval b s = f (sem c s)
                    | otherwise = s

expon 0 f = id
expon n f = f . expon (n-1) f

update s x n y | x == y    = n
               | otherwise = s y

fix f = f (fix f)

s0 x = error ("not initialized variable " ++ x)

--

parse = do
    input <- getLine
    let c :: C
        c = read input
    print c

main = do 
    input <- getContents
    let c :: C
        c = read input
    print (sem c s0 "result")

