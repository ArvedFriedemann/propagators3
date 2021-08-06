module Parsing.TermStrucFunctions where

import "this" Terms.Terms
import "this" Parsing.TermStruc
import "this" Class.MonadProp
import "this" PropagatorTypes
import "this" Util
import "base" Data.Functor
import qualified "containers" Data.Set as S
import "transformers" Control.Monad.Trans.Writer


fromVarsAsCells :: (MonadProp m v) =>
  TermStruc (TSP v) -> m (TSP v)
fromVarsAsCells SBOT = newTSP' bot
fromVarsAsCells STOP = newTSP
fromVarsAsCells (SCON c) = newTSP' (cset c)
fromVarsAsCells (SVAR v) = pure v
fromVarsAsCells (SAPPL a b) = do
    ca <- fromVarsAsCells a
    cb <- fromVarsAsCells b
    newTSP' (aplset (ca, cb))

--gives a flattened term in the current state, keeping unassigned variables the same
--assumes state to be propagated
fromTSP :: (MonadProp m v) =>
  TSP v -> m (TermStruc (TSP v))
fromTSP = fromTSPSize (-1)

fromTSPSize :: (MonadProp m v) =>
  Int -> TSP v -> m (TermStruc (TSP v))
fromTSPSize 0 p = return $ SVAR p
fromTSPSize i p = readState p >>= \ts@TS{..} ->
  if isTop ts
    then return STOP
  else if isBot ts
    then return SBOT
  else if not $ S.null constants
    then return (SCON (S.findMin constants))
  else if not $ S.null applications
    then let (x,y) = S.findMin applications
          in SAPPL <$> fromTSPSize (i - 1) x <*> fromTSPSize (i - 1) y
  else return $ SVAR $ S.findMin variables


------------------------------
--Reverse Parsing
------------------------------
lassocOp :: (Eq a) => TermStruc a -> TermStruc a -> [TermStruc a]
lassocOp op = lassocOpF (==op)
lassocOpF :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> [TermStruc a]
lassocOpF opf t = execWriter (lassocOp' opf t)
lassocOp' :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> Writer [TermStruc a] ()
lassocOp' opf (SAPPL (SAPPL x op') y)
  | opf op' = lassocOp' opf x >> tell [y]
lassocOp' _ t = tell [t]

rassocOp :: (Eq a) => TermStruc a -> TermStruc a -> [TermStruc a]
rassocOp op = rassocOpF (==op)
rassocOpF :: (Eq a) => (TermStruc a -> Bool) -> TermStruc a -> [TermStruc a]
rassocOpF opf (SAPPL x (SAPPL op' y))
  | opf op' = x : (rassocOpF opf y)
rassocOpF _ t = [t]

removeLrecBrackets :: (Eq a) => TermStruc a -> TermStruc a -> TermStruc a -> TermStruc a
removeLrecBrackets on off = removeLrecBracketsF (==on) (==off)
removeLrecBracketsF :: (Eq a) => (TermStruc a -> Bool) -> (TermStruc a -> Bool) -> TermStruc a -> TermStruc a
removeLrecBracketsF onf offf (SAPPL (SAPPL on' t) off')
  | onf on' && offf off' = removeLrecBracketsF onf offf t
removeLrecBracketsF onf offf (SAPPL a b) = SAPPL
                              (removeLrecBracketsF onf offf a)
                              (removeLrecBracketsF onf offf b)
removeLrecBracketsF _ _ t = t

removeRrecBrackets :: (Eq a) => TermStruc a -> TermStruc a -> TermStruc a -> TermStruc a
removeRrecBrackets on off = removeRrecBracketsF (==on) (==off)
removeRrecBracketsF :: (Eq a) => (TermStruc a -> Bool) -> (TermStruc a -> Bool) -> TermStruc a -> TermStruc a
removeRrecBracketsF onf offf (SAPPL on' (SAPPL t off'))
  | onf on' && offf off' = removeRrecBracketsF onf offf t
removeRrecBracketsF onf offf (SAPPL a b) = SAPPL
                              (removeRrecBracketsF onf offf a)
                              (removeRrecBracketsF onf offf b)
removeRrecBracketsF _ _ t = t
