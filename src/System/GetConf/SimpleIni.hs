-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE NoMonomorphismRestriction, PackageImports, FlexibleContexts #-}

module System.GetConf.SimpleIni where


import Text.Parsec
import Text.Parsec.Error
import "mtl" Control.Monad.State.Strict
import Data.Maybe
import Data.Char
import Control.Arrow

-- Treat eof as newline-eof.
newline' = (void newline <|> lookAhead eof) >> return '\n'

whitespaces = many $ char ' ' <|> tab

sectionHeading :: (Stream s m Char) => ParsecT s u m String
sectionHeading = do
  char '['
  spaces
  sectionName <- manyTill anyChar (char ']')
  whitespaces
  newline'
  return sectionName

comment :: (Stream s m Char) => ParsecT s u m String
comment = oneOf ";#" >> manyTill anyChar newline'

definition :: (Stream s m Char) => ParsecT s u m (String, String)
definition = do
  varname <- manyTill anyChar (try whitespaces >> (char '=' <|> lookAhead newline'))
  value <- manyTill anyChar (void newline' <|> void comment)
  return (varname, value)

iniParser  :: (MonadState String m, Stream s m Char) =>
     ParsecT s u m [((String, String), String)]
iniParser =  fmap catMaybes . flip manyTill eof $ whitespaces >>
            (   ((comment >> return Nothing) <?> "comment" )
            <|> ((sectionHeading >>= put >> return Nothing) <?> "sectionHeading")
            <|> (do
                  section <- get
                  (var, val) <- definition
                  return $ Just ((section,var),val)
                  <?> "definition" )
            )

parseIni filename ini =  runState (runParserT iniParser () filename ini ) ""

parseIniFile :: FilePath -> IO (Either String [((String, String), String)])
parseIniFile filename = (left (show) . fst . parseIni filename) `fmap`  readFile filename

testParse = parseIniFile "System/GetConf/simpleini.ini"
