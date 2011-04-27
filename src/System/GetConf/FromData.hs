{-# LANGUAGE TemplateHaskell, ViewPatterns, DeriveFunctor #-}
module System.GetConf.FromData where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Control.Monad
import System.Console.GetOpt
import Language.Haskell.TH.Quote

import Control.Arrow(first, second)


import System.IO.Unsafe

data OptArg a = NilArg | Arg a deriving (Eq,Show,Functor)

toMaybe :: OptArg a -> Maybe a
toMaybe NilArg = Nothing
toMaybe (Arg x) = Just x

resolveType :: Type -> Q Type
resolveType t@(ConT name) = do
  (TyConI ty) <- reify name
  case ty of 
    TySynD _ [] base@(ConT _) -> resolveType base
    TySynD _ [] base -> return base 
    TySynD _ xs _   -> error "Only grounded types can be resolved atm."
    x               -> return t
resolveType t = return t 

-- TODO: This is a hot mess. Simplify!
-- (ArgDescr, default value)
--argByType :: Name -> Type -> Q (Exp, Exp,Exp)
argByType fName fType =
  uncurry (liftM2 (,)) $ case fType of
    ConT tp | tp == ''String -> (opt [| ReqArg $update "String" |], undef)
            | tp == ''Bool   -> (opt [| NoArg  $ $update True   |], [| False |] )
    TupleT 0                 -> (opt [| OptArg  (const id) "DEPRECATED" |], [| () |] )
    AppT (ConT mb) x | mb == ''OptArg ->  
                                (opt [| OptArg 
                                        ($update . maybe NilArg (Arg . $(readByType x))) 
                                        ($(liftString . show $ ppr x) ++ 
                                      " (optional)") |]
                                  , undef )
                     | mb == ''Maybe -> 
                        case x of 
                          AppT (ConT mb) y | mb == ''OptArg -> 
                              (opt [| OptArg 
                                    ($update . maybe (Just NilArg) (Just . Arg . $(readByType y))) 
                                    ($(liftString . show $ ppr x) ++ 
                                     " (optional)") |]
                              , [| Nothing |] )
                          y -> (opt [| ReqArg (\a -> $update . Just $ $(readByType x) a)  
                                         $(liftString . show $ ppr x)|]
                                  , [| Nothing |] )
    x ->                        (opt [| ReqArg (\a -> $update $ $(readByType x) a) 
                                       $(liftString . show $ ppr x) |]
                                 , undef)
  where 
    update = do 
      x <- newName "x"
      record <-newName "record"
      lamE [varP x, varP record] $ recUpdE (varE record) [return (fName, VarE x) ]
    undef = [| error ("undef: " ++ $(liftString $ show fName ++ show fType)) |]
    opt arg = [| ($(liftString . nameBase $ fName) ,  $arg) |]
    readByType (ConT t) | t == ''String = [| id |]
    readByType _ = [| read |]
    

mapArg f (NoArg a)      = NoArg $ f a
mapArg f (ReqArg g doc) = ReqArg (f . g) doc
mapArg f (OptArg g doc) = OptArg (f . g) doc

toOpt fieldName arg = (Option "" [ fieldName ] (mapArg (\x -> (fieldName , x) ) arg) "") 

toOpts = map $ uncurry toOpt

mapE f m = ListE `fmap` mapM f m

-- TODO: update Documentation
-- | Create option description based on field type
-- It works like you would expect:
-- A String is returned verbatim
-- A Bool creates an (argumentless) switch
-- All other values are read according to their Read instance but
-- Maybe values need not have an argument
-- Note: You can't *read* a Bool or Maybe value, just 
-- use a String and convert it by hand later (or use a newtype)
--
-- Takes the *name* of a record Datatype. 
-- Splices list of option descriptions where each option
-- returns a pair containing the name of the field where it originated from
-- and a function that updates the record according to the read arguments
-- Use like this:
--
-- data Foo = Foo 
--   { foo  :: String
--   , bar  :: Bool
--   , quux :: Maybe Int
--   }  deriving Show
--
-- options = $mkOpts ''Foo 
--
-- main = do
--   (fields, changes) <- unzip `fmap` getConf exampleOpt getConfDefaults
--   print $ foldr ($) (Foo "" False Nothing) changes

data OptConstr a = OptConstr
  { options :: [(String, ArgDescr (a -> a))]
  , hollow :: a               
  }

mkOpts :: Name -> Q Exp
mkOpts name = do 
  this <- reify name 
  (tyName, fields, rCons) <- case this of 
    TyConI (DataD _ (conT -> tyName') _ [RecC rCons' (map unStrictInfo -> fields') ] _ ) 
      -> return (tyName', fields', rCons')
    _ -> fail $ show name ++ " is not a record type with a single constructor"
--  opts <- flip sigE ([t| [ (String, ArgDescr ($tyName -> $tyName)) ] |]) $ mapE (uncurry mkOpt) fields
  (opts,  hollowCons) <- fmap unzip . mapM (uncurry argByType) $ fields
  undef <- [| undefined |]
  let hollow = RecConE rCons $ zip (map fst fields) hollowCons
  flip sigE [t| OptConstr $tyName |] $ 
    [| OptConstr 
      { options = $(return $ ListE opts)
      , hollow  = $(return hollow) 
      }|]
    where unStrictInfo (x,_,y) = (x,y)
          
trace message val = unsafePerformIO $ putStrLn message >> return val