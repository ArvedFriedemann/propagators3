{-# LANGUAGE NoImplicitPrelude #-}
module Parsing.TermStruc where

import "base" Prelude hiding ( read )
import "base" GHC.Exts
import "base" Data.Functor
import "base" Debug.Trace

import "transformers" Control.Monad.Trans.Writer

import "this" Terms.Terms


data TermStruc a
    = STOP
    | SBOT
    | SCON TermConst
    | SVAR a
    | SAPPL (TermStruc a) (TermStruc a)
  deriving (Eq, Ord, Functor)
  {-}
instance HasTop (TermStruc a) where
    top = STOP
    isTop STOP = True
    isTop _ = False
instance HasBot (TermStruc a) where
    bot = SBOT
    isBot SBOT = True
    isBot _ = False
-}

instance Show a => Show (TermStruc a) where
    showsPrec _ STOP = showString "top"
    showsPrec _ SBOT = showString "bot"
    showsPrec _ (SCON (CUST s)) = showString s
    showsPrec n (SCON c) = showsPrec n c
    --TODO: Should not be used when variables are not strings
    showsPrec n (SVAR v) = (showsPrec n v)
    --showsPrec n (SVAR v) = (showString "@").(showsPrec n v).(showString "@")
    showsPrec n (SAPPL s c@(SAPPL _ _)) = (showsPrec n s).(showString "(").(showsPrec n c).(showString ")")
    --showsPrec n (SAPPL s@(SAPPL _ _) c) = (showString "(").(showsPrec n s).(showString ")").(showsPrec n c)
    showsPrec n (SAPPL s c) = (showsPrec n s).(showString " ").(showsPrec n c)

instance Semigroup (TermStruc a) where
    STOP <> a = a
    a <> STOP = a
    a <> b = SAPPL a b
instance Monoid (TermStruc a) where
    mempty = STOP

--TODO: use a reader!
instance IsList (TermStruc a) where
    type Item (TermStruc a) = TermStruc a
    fromList [] = STOP
    fromList lst = foldl1 SAPPL lst
    toList  = pure

instance IsString (TermStruc a) where
    fromString = SCON . CUST

var :: a -> TermStruc a
var = SVAR

tsVars :: TermStruc a -> [a]
tsVars = execWriter . tsVars'
tsVars' :: TermStruc a -> Writer [a] ()
tsVars' (SVAR s) = tell [s]
tsVars' (SAPPL a b) = tsVars' a >> tsVars' b
tsVars' _ = return ()

exchangeVars :: (a -> TermStruc b) -> TermStruc a -> TermStruc b
exchangeVars fkt (SVAR v) = fkt v
exchangeVars fkt (SAPPL a b) = SAPPL (exchangeVars fkt a) (exchangeVars fkt b)
exchangeVars _ STOP = STOP
exchangeVars _ SBOT = SBOT
exchangeVars _ (SCON c) = SCON c

apls :: TermStruc a -> TermStruc a -> TermStruc a
apls x STOP = x
apls STOP x = x
apls x y = SAPPL x y

stdlst :: [TermStruc a] -> TermStruc a
stdlst = foldl apls STOP
