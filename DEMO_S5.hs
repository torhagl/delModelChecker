module DEMO_S5_extended where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

infixr 5 |=
(|=) :: (Ord state) => (EpistM state, state) -> Form state -> Bool
(model, state) |= form = isTrueAt model state form

newtype Agent = Ag Int deriving (Eq,Ord)

a,b,c,d,e :: Agent
a = Ag 0; b = Ag 1; c = Ag 2; d = Ag 3; e = Ag 4

instance Show Agent where
  show (Ag 0) = "a"; show (Ag 1) = "b";
  show (Ag 2) = "c"; show (Ag 3) = "d";
  show (Ag 4) = "e";
  show (Ag n) = 'a': show n

data Prp = P Int | Q Int | R Int | S Int | Prop Agent Int deriving (Eq,Ord)
instance Show Prp where
  show (P 0) = "p"; show (P i) = "p" ++ show i
  show (Q 0) = "q"; show (Q i) = "q" ++ show i
  show (R 0) = "r"; show (R i) = "r" ++ show i
  show (S 0) = "s"; show (S i) = "s" ++ show i
  show (Prop i y) = show i ++ show y

p,q,r,s :: Form a
p = Prp $ P 0; q = Prp $ Q 0
r = Prp $ R 0; s = Prp $ S 0

data Form a = Top
            | Info a
            | Prp Prp
            | Ng (Form a)
            | Conj [Form a]
            | Disj [Form a]
            | Kn Agent (Form a)
            | CK [Agent] (Form a)
            | Pub (Form a) (Form a)
          deriving (Eq,Ord,Show)

data EpistM state = Mo
             [state]
             [Agent]
             (Map.Map state [Prp])
             (Map.Map Agent (Erel state))
             [state]  deriving (Eq,Show,Ord)

-- Returns the indistinguishability relation for an agent
rel :: Agent -> EpistM a -> Erel a
rel ag (Mo _ _ _ rels _) = Map.findWithDefault (error err) ag rels
  where err = "Can't find agent: " ++ show ag

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) =
  powerList xs ++ map (x:) (powerList xs)

sortL :: Ord a => [[a]] -> [[a]]
sortL  = sortBy
  (\ xs ys -> if length xs < length ys
                then LT
              else if length xs > length ys
                then GT
              else compare xs ys)

-- Implication operator defined as (neg a or b)
impl :: Form a -> Form a -> Form a
impl form1 form2 = Disj [Ng form1, form2]

isTrueAt :: Ord state =>
            EpistM state -> state -> Form state -> Bool
isTrueAt _ _ Top = True
isTrueAt _ w (Info x) = w == x -- Info apparently just checks if the current state is equal to its argument
isTrueAt (Mo _ _ val _ _) w (Prp p) =
  let props = Map.findWithDefault (error "no props") w val
  in elem p props
isTrueAt m w (Ng f) = not (isTrueAt m w f)
isTrueAt m w (Conj fs) = all (isTrueAt m w) fs
isTrueAt m w (Disj fs) = any (isTrueAt m w) fs
isTrueAt model w (Kn ag f) = let
    r = rel ag model -- r = the equivalence relation of ag in m
    b = bl r w   -- b == the list of states the agent considers indistinguishable from w
  in all (flip (isTrueAt model) f) b
-- Update model by phi, then check if psi is valid in same state of updated model
isTrueAt model w (CK agents form) = let
  r = removeSubsets $ transitiveClosureErel $ unionOfErels agents model
  b = bl r w
  in all (flip (isTrueAt model) form) b
isTrueAt model state (Pub announcement form) =
  let updatedModel@(Mo states' _ _ _ _) = upd_pa model announcement
  in if state `elem` states'
    then isTrueAt updatedModel state form
    else True -- If the 'present' state does not exist, announcement was false, and formula is satisfied

--Bulk the equivalence classes of the relevant agents together, in preparation of transitive closure.
unionOfErels :: (Ord a) => [Agent] -> EpistM a -> Erel a
unionOfErels [] _ = []
unionOfErels (x:xs) model = rel x model ++ unionOfErels xs model

transitiveClosureErel :: (Ord a) => Erel a -> Erel a
transitiveClosureErel [] = []
transitiveClosureErel (x:xs)
  | length added > length x = Set.toList $ Set.fromList $ transitiveClosureErel (added:xs) -- If appendIfContains is longer than x itself, then run it again with the added list, as you might uncover more transitive relations when you have added everything together.
  | otherwise =  x : transitiveClosureErel xs -- if no elements have been added, run transitive closure on the remaining lists and add x to that.
  where added = appendIfContains x xs

removeSubsets :: (Ord a) => [[a]] -> [[a]]
removeSubsets [] = []
removeSubsets (x:xs)
    | any (\y -> Set.isSubsetOf (Set.fromList x) (Set.fromList y)) xs = removeSubsets xs --x has a superset later in the list, and can therefore be removed.
    | otherwise = x : removeSubsets (removeSubset x xs) -- x does not have a superset later in the list, remove subsets of x later in the list and continue.

removeSubset :: (Ord a) => [a] -> [[a]] -> [[a]]
removeSubset _ [] = []
removeSubset superlist (h:tl)
  | Set.isSubsetOf (Set.fromList h) (Set.fromList superlist) = removeSubset superlist tl -- if head is a subset of the superlist, remove head.
  | otherwise = h : removeSubset superlist tl -- head is not a subset of the superlist, append head and continue checking.

-- Partition the elements of checkList, such that 'fits' are the elements of 'checkList' that shares an element with 'list'
-- Concatenate the list with 'fits' to add the lists together. Then Set.fromList, toList to remove duplicate elements.
appendIfContains :: (Ord a) => [a] -> [[a]] -> [a]
appendIfContains _ [] = []
appendIfContains list checkList = Set.toList $ Set.fromList $ concat (list : fits)
  where (fits,_) = partition (\y -> any (\x -> x `elem` list) y) checkList


isTrue :: Ord a => EpistM a -> Form a -> Bool
isTrue m@(Mo _ _ _ _ points) f =
  all (\w -> isTrueAt m w f) points

-- Public announcement model update, generates new model from model and announced formula
upd_pa :: Ord state =>
          EpistM state -> Form state -> EpistM state
upd_pa m@(Mo states agents val rels actual) f =
  (Mo states' agents val' rels' actual')
   where
   states' = [ s | s <- states, isTrueAt m s f ]
   val'    = Map.filterWithKey (\k _ -> k `elem` states') val--[ (s, ps) | (s,ps) <- val, s `elem` states' ]
   rels'   = Map.map (restrict states') rels--[(ag,restrict states' r) | (ag,r) <- rels ]
   actual' = [ s | s <- actual, s `elem` states' ]

-- Series of public announcements
upds_pa ::  Ord state =>
            EpistM state -> [Form state] -> EpistM state
upds_pa = foldl upd_pa

sub :: Eq a => [(Prp,Form a)] -> Prp -> Form a
sub subst p =
  if p `elem` map fst subst
    then apply subst p
    else Prp p

type Erel a = [[a]]

{- Gets the list of propositions that are true in the given state from a
   valuation function -}
apply :: (Ord a,Eq a) => [(a,b)] -> a -> b
apply t x = Data.Maybe.fromMaybe (error "apply") (lookup x t)

bl :: Eq a => Erel a -> a -> [a]
bl r x = head (filter (elem x) r)

restrict :: Ord a => [a] -> Erel a -> Erel a
restrict domain =
  Set.toList . Set.fromList . filter (/= []) . map (filter (`elem` domain))
