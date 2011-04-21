{-# LANGUAGE NoMonomorphismRestriction #-}
module System.GetConf.Test where

--import System.GetConf
import System.GetConf.FromData

import Language.Haskell.TH
import Data.Maybe
import System.Console.GetOpt

data Foo = Foo 
  { foo  :: Int
  , quux :: Maybe (Maybe Double)
  , boo  :: Bool
  , narf :: String
  } 

bar = $(litE . StringL . show =<< getFields ''Foo)

bar2 = $(mkOpts ''Foo) :: [OptDescr (Foo -> Foo)]


instance Show (ArgDescr a) where
  show (NoArg _ ) = "NoArg"
  show (ReqArg _ desc ) = "ReqArg: " ++ desc
  show (OptArg _ desc ) = "OptArg: " ++ desc

instance Show (OptDescr a) where 
  show (Option shorts longs desc doc) = "Option | " ++ shorts ++ " | " ++ show longs ++ " | " ++ show desc ++ " | " ++ doc