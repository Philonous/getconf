{-# LANGUAGE TemplateHaskell, ViewPatterns, DeriveFunctor #-}
module System.GetConf.FromData where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List
import Control.Monad
import System.Console.GetOpt
import System.GetConf
import System.Exit
import Language.Haskell.TH.Quote

import Control.Arrow(first, second)



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
argByType :: Name -> Type -> Q (Exp, (Exp,Bool))
argByType fName fType =
  (\(arg',(skel',mand)) -> do 
    arg <- arg'
    skel <- skel'
    return (arg,(skel,mand)))
    $ case fType of
    ConT tp | tp == ''String -> (opt [| ReqArg $update "String" |], undef)
            | tp == ''Bool   -> (opt [| NoArg  $ $update True   |], ([| False |], False) )
    TupleT 0                 -> (opt [| OptArg  (const id) "DEPRECATED" |], ([| () |], False) )
    AppT (ConT mb) x | mb == ''OptArg ->  
                                (opt [| OptArg 
                                        ($update . maybe NilArg (Arg . $(readByType x))) 
                                        ($(liftString . show $ ppr x) ++ 
                                      " (optional)") |]
                                  , undef)
                     | mb == ''Maybe -> 
                        case x of 
                          AppT (ConT mb) y | mb == ''OptArg -> 
                              (opt [| OptArg 
                                    ($update . maybe (Just NilArg) (Just . Arg . $(readByType y))) 
                                    ($(liftString . show $ ppr x) ++ 
                                     " (optional)") |]
                              ,( [| Nothing |]  , False))
                          y -> (opt [| ReqArg (\a -> $update . Just $ $(readByType x) a)  
                                         $(liftString . show $ ppr x)|]
                                  , ([| Nothing |]  , False))
    x ->                        (opt [| ReqArg (\a -> $update $ $(readByType x) a) 
                                       $(liftString . show $ ppr x) |]
                                 , undef)
  where 
    update = do 
      x <- newName "x"
      record <-newName "record"
      lamE [varP x, varP record] $ recUpdE (varE record) [return (fName, VarE x) ]
    undef = ([| error ("undef: " ++ $(liftString $ show fName ++ show fType)) |], True)
    opt arg = [| ($(liftString . nameBase $ fName) ,  $arg) |]
    readByType (ConT t) | t == ''String = [| id |]
    readByType _ = [| read |]
    
toOpt fieldName arg = (Option "" [ fieldName ] (mapArg (\x -> (fieldName , x) ) arg) "") 

toOpts = map $ uncurry toOpt

mapE f m = ListE `fmap` mapM f m

-- TODO: update Documentation
-- | Create option description based on field type
-- It works like you would expect:
-- A String is returned verbatim
-- A Bool creates an (argumentless) switch
-- All other values are read according to their Read instance but
-- Maybe values need not be given (You get Nothing)
-- OptArg need not have a parameter (you get NilArg instead)
-- Maybe (OptArg a) makes both the parameter and the argument optional
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
--   { myString  :: String
--   , myBool  :: Bool
--   , optionalInt :: Maybe Int
--   , totallyOptional :: Maybe (OptArg Double)
--   , deprecated ()
--   }  deriving Show
--
-- main = do
--   changes <- 
--   print $ foldr ($) (Foo "" False Nothing) changes

data OptConstr a = OptConstr
  { options :: [(String, ArgDescr (a -> a))]
  , skelleton :: a               
  , mandatories :: [String]
  }

mkOpts :: Name -> Q Exp
mkOpts name = do 
  this <- reify name 
  (tyName, fields, rCons) <- case this of 
    TyConI (DataD _ (conT -> tyName') _ [RecC rCons' (map unStrictInfo -> fields') ] _ ) 
      -> return (tyName', fields', rCons')
    _ -> fail $ show name ++ " is not a record type with a single constructor"
--  opts <- flip sigE ([t| [ (String, ArgDescr ($tyName -> $tyName)) ] |]) $ mapE (uncurry mkOpt) fields
  (opts, unzip -> (skelletonCons, mandatory)) <- fmap unzip . mapM (uncurry argByType) $ fields
  let skelleton = RecConE rCons $ zip (map fst fields) skelletonCons
  let man = map (stringE . nameBase) . map snd . filter fst $ zip mandatory $ map fst fields
  flip sigE [t| OptConstr $tyName |] $ 
    [| OptConstr 
      { options = $(return $ ListE opts)
      , skelleton  = $(return skelleton) 
      , mandatories = $(listE man)
      }|]
    where unStrictInfo (x,_,y) = (x,y)
          
validateOpts :: OptConstr a -> [(String, b)] -> Bool
validateOpts constr (map fst -> opts) = all (`elem` opts) (mandatories constr)

optToData constr confOptions = do
  opts <- getConf (toOpts $ options constr) confOptions
  let missing = (mandatories constr) \\ (map fst opts)
  unless (null missing) $ do 
    putStrLn "Mandatory parameters not give: "
    mapM_ putStrLn missing
    exitFailure
  let changes = map snd opts
  return $ foldr ($) (skelleton constr) changes
