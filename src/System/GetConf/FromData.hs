{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module System.GetConf.FromData where

import Language.Haskell.TH
import Data.Maybe
import System.Console.GetOpt
import Language.Haskell.TH.Quote

-- Get fields of record datatype
getFields :: Name -> Q [(Name, Type)]
getFields name = do 
  this <- reify name
  fields <- case this of 
        TyConI (DataD _ _ _ [RecC _ fields' ] _ ) -> return fields'
        _ -> fail $ show name ++ " is not a record type with a single constructor"
  return $ map unStrictInfo fields
    where unStrictInfo (x,_,y) = (x,y)

mkOpt fName fType = do
  -- (unqualified) name as literal String
  let fString = litE . StringL . nameBase $ fName
  -- construct a record update. That is basically one half of a fclabel
  let update = do 
          x <- newName "x"
          record <-newName "record"
          lamE [varP x, varP record] $ recUpdE (varE record) [return (fName, VarE x) ]
  let opt arg = [| (Option "" [ $fString ] (fmap (\x -> ($fString , x) ) $arg) "") 
                 |]
  case fType of 
    -- TODO: return *unqualified* type name as argument restriction
    ConT tp | tp == ''String -> opt [| ReqArg $update "String" |]
            | tp == ''Bool   -> opt [| NoArg  $ $update True     |]         
    AppT (ConT mb ) x | mb == ''Maybe ->  
                                opt [| OptArg 
                                       ($update . maybe Nothing read) 
                                       ($(litE . StringL . show $ ppr x) ++ 
                                        " (optional)") |]
    x ->                        opt [|ReqArg (\x -> $update $ read x) 
                                      $(litE . StringL . show $ ppr x) |]

mapE f m = ListE `fmap` mapM f m

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

mkOpts :: Name -> Q Exp
mkOpts name = do 
  TyConI (DataD _ (conT -> tyName) _ _ _ ) <- reify name
  flip sigE ([t| [OptDescr (String, $tyName -> $tyName) ] |]) $ getFields name >>= mapE (uncurry mkOpt)