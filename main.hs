import PA1Helper
import System.Environment (getArgs)
import Debug.Trace

-- input: lexp output: lexp
-- return the lexp after renaming
ram::  Lexp->Lexp
ram f@(Lambda a b) = (Lambda (a ++ "1") ( brdc (Apply (Lambda a b)  (Atom (a ++ "1"))) ))

-- input: lexp output: lexp
-- eta reduction:
-- input in the form of λx. (E M) M is atom
-- return E if M is x and there isn't a x in E, else return the original lexp
erdc:: Lexp -> Lexp
erdc lexp@(Lambda a b@(Apply c d@(Atom e))) =
 if a == e && a `notElem` (free c) then
    c
 else lexp
erdc lexp = lexp

-- input: lexp output: lexp
-- beta reduction:
-- input in the form of λx. E M
-- subsitute all x in E with M
brdc::Lexp -> Lexp
brdc lexp@(Apply (Lambda a b@(Atom d)) c) =
  if a /= d then
    b
  else c
brdc lexp@(Apply (Lambda a b@(Lambda d e)) c) =
  if d /= a then
    if d `notElem` (free c) then
      (Lambda d (brdc (Apply (Lambda a e)  c)))
    else   brdc (Apply  (Lambda a (ram b))  c)
  else b
brdc lexp@(Apply (Lambda a b@(Apply d e)) c) = (Apply (brdc (Apply (Lambda a d) c))   (brdc (Apply (Lambda a e) c)) )

-- input: lexp output: lexp
-- lexp in lambda form
-- do eta reduction if type fit, else put it back
-- to reducer to check and do futher reduction
lam::Lexp -> Lexp
lam lexp@(Lambda a d@(Apply b c)) = erdc lexp
lam lexp@(Lambda a b) = (Lambda a  (reducer b) )

-- input: lexp output: lexp
-- lexp in apply form (e1 e2)
-- do beta reduction if type fit, else return apply form of
-- the result of e1 and e2 reduction
app::Lexp -> Lexp
app lexp@(Apply (Lambda a b) c) = brdc lexp
app lexp@(Apply a b) = (Apply (reducer a) (reducer b))


-- input: lexp output: list of string
-- return the list of free variable in the lexp in a list format
free:: Lexp -> [String]
free lexp@(Atom a) = [a]
free lexp@(Apply a b) = (free a) ++ (free b)
free lexp@(Lambda a b) = filter (\c -> c /= a)  (free b)

--input: lexp output: lexp
--check if the reduction actually changes the lexp. If not, stop the reduction
-- from creating infinite loops
dummy:: Lexp-> Lexp -> Lexp
dummy a b =
  if a == b then
    b
  else
    reducer b

-- input: lexp output: lexp
-- check for input expression type
-- and do the corresponding redution or return
reducer :: Lexp -> Lexp
reducer v@(Atom a) = v
reducer lexp@(Lambda _ _) = dummy lexp (lam lexp)
reducer lexp@(Apply _ _) = dummy lexp  (app lexp)



-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram inFile outFile reducer
