{-
CS381 HW3
Xiaoyi Yang, Jiaxu Li
Due 5-16-2017
-}
import Data.Maybe
import Data.List
--1

--a)
type Prog = [Cmd]
data Cmd = LD Int 
		| ADD 
		| MULT 
		| DUP 
		| INC 
		| SWAP 
		| POP Int 
		deriving Show

type Rank = Int
type CmdRank = (Int, Int)

rankC:: Cmd -> CmdRank
rankC (LD x) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP x) = (x, 0)

rankP::Prog -> Maybe Rank
rankP [] = Just 0
rankP xs = rank xs 0

rank:: Prog->Rank->Maybe Rank
rank [] x = Just x
rank (x:xs) y = if(y - i < 0)
					then Nothing
					else rank xs (j - i + y)
					where (i, j) = rankC x

--Test way: show test = rankP[LD 1, LD 1, LD 1], [LD 2, LD 2, ADD] 

--b)
--sem for semCmd
{-
new type of function sem is Maybe. In the case of "Nothing" in semStatTC, it can be remove because the static said 
there is no error.
Function below: test: show test = semStatTC [LD 1, LD 1, LD 1], [LD 2, LD 2, ADD]
-}
--makes Stack works
type Stack = [Int]

sem::Prog->Stack->Stack
sem [] xs = xs
sem (x:xs) ys = sem xs (semCmd x ys)

semCmd::Cmd->Stack->Stack
semCmd (LD x) xs = x:xs					--put x into first of xs
semCmd (ADD) (x1:x2:xs) = (x1+x2:xs)	--add first two element
semCmd (MULT) (x1:x2:xs) = (x1*x2:xs)	--mult first two element
semCmd (DUP) (x:xs) = (x:x:xs)			--copy first element
semCmd (INC) (x:xs) = ((x+1):xs)		--first element add 1
semCmd (SWAP) (x1:x2:xs) = (x2:x1:xs)	--switch first two element
semCmd (POP x) xs = drop x xs			--pop x from xs


semStatTC::Prog->Maybe Stack
semStatTC xs = case rankP xs of
						Nothing -> Nothing
						Just x -> Just (sem xs [])


--2
--a)
data Shape = X
		| TD Shape Shape
		| LR Shape Shape
		deriving Show

type BBox = (Int, Int)

{-
where doesn't work? why?
It lets me use let. Even let doesn't work??? -- Update: just syntax error, solved.
-}

bbox::Shape->BBox
bbox X = (1, 1)
bbox (TD shape1 shape2) = 
						  let (x1, y1) = bbox shape1
						      (x2, y2) = bbox shape2
						  in (max x1 x2, y1+y2)
bbox (LR shape1 shape2) = 
						  let (x1, y1) = bbox shape1
						      (x2, y2) = bbox shape2
						  in (x1+x2, max y1 y2)

--b)
{-
function rect checks the value of x1 and x2/y1 and y2. return (?, ?) if they are same. or return nothing.
-}
rect:: Shape -> Maybe BBox
rect X = Just (1, 1)               {-dont using Just bbox X-}
rect (TD shape1 shape2) | x1==x2  =Just (x1, y1+y2)
						| otherwise  =Nothing
                       where Just (x1,y1) = rect shape1
                             Just (x2,y2) = rect shape2
rect (LR shape1 shape2) | y1==y2  =Just (x1+x2, y1)
						| otherwise  =Nothing
                       where Just (x1,y1) = rect shape1
                             Just (x2,y2) = rect shape2
						  
						  
{- one thing I dont get it because sometime syntax isn't right, but when I change the lines/spaces it is right.
						  let Just (x1, y1) = rect shape1
							  Just (x2, y2) = rect shape2
						  in (Just (x1, y1), Just (x2, y2)) -> if (x1==x2) then (Just (x1, y1+y2)) otherwise Nothing-}


--3

{- (a) Consider the functions f and g, which are given by the following two function definitions.
f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]
 -}
 
{-(1)
f::[x'] -> x' -> [x']
g::[x'] -> y' -> [y']
I didn't find that x and y already used in f and g.
-}

{-(2)
f: x takes null, so x must be list ([x]). Because it return [y] or x, both of them are list, so y should be element.
g: condition is not (null x), means x should be list. But whatever x is, its return [] or [y], not related to x in this
   function.
-}

{-(3)
g is more general. For given functions, function f return list or list, but both of them should have the same type.
in other words, same type list. But function g doesn't care it.
-}

{-(4)
function g doesn't care x's type. x is only a check condition here. Whatever x is, it will return y's type list or null which
doesn't care x's type.
but for function f, it cares x and y's type. return value should have same type.
-}

--b)
--function h: add an element of a point(second element), to given list's first place. 
{-
h :: [b] -> [(a, b)] -> [b]
h xs [(x, y)] = y:xs
-}
--c)
--function k: takes two function, function1 the result of function2->function1.
{-
k :: (a -> b) -> ((a -> b) -> a) -> b
k X Y = X (Y X)
-}
--d)
--No, we can't define a->b, because it is to board. You don't know which types that a and b have. It is too difficult
--to difine a function like this.


				