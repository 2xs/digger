-- |
-- Module      :  digger (Main)
-- Copyright   :  Université Lille 1, Veïs Oudjail, 2016-2017
-- License     :  CeCILL
--
-- Maintainer  :  samuel.hym@univ-lille1.fr

-- Stability   :  experimental
-- Portability :  portable
--
-- Description :  Convert Coq code written in a "C-style" (imperative
--                style based on a monad, with full application of
--                functions) into the corresponding C code or to an
--                intermediate representation (deep) output as Coq
--                source code.
--                Start from the Coq code extracted as JSON by the
--                internal extraction facility.

-- This software is a computer program whose purpose is to run a minimal,
-- hypervisor relying on proven properties such as memory isolation.

-- This software is governed by the CeCILL license under French law and
-- abiding by the rules of distribution of free software.  You can  use,
-- modify and/ or redistribute the software under the terms of the CeCILL
-- license as circulated by CEA, CNRS and INRIA at the following URL
-- "http://www.cecill.info".

-- As a counterpart to the access to the source code and  rights to copy,
-- modify and redistribute granted by the license, users are provided only
-- with a limited warranty  and the software's author,  the holder of the
-- economic rights,  and the successive licensors  have only  limited
-- liability.

-- In this respect, the user's attention is drawn to the risks associated
-- with loading,  using,  modifying and/or developing or reproducing the
-- software by the user in light of its specific status of free software,
-- that may mean  that it is complicated to manipulate,  and  that  also
-- therefore means  that it is reserved for developers  and  experienced
-- professionals having in-depth computer knowledge. Users are therefore
-- encouraged to load and test the software's suitability as regards their
-- requirements in conditions enabling the security of their systems and/or
-- data to be ensured and,  more generally, to use and operate it in the
-- same conditions as regards security.

-- The fact that you are presently reading this means that you have had
-- knowledge of the CeCILL license and that you accept its terms.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad                (when, (<=<))
import           Data.Aeson                   (eitherDecode')
import qualified Data.ByteString.Lazy         as BS
import           Data.Default
import           Data.List                    (elemIndex, intercalate)
import qualified Data.Map                     as Map
import           Data.Monoid                  ((<>))
import           Data.Text.Lazy               (Text, pack, unpack)
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.IO            as TIO
import           Data.Version                 (showVersion)
import qualified Language.C                   as C
import           Options.Applicative          hiding (empty)
import           System.Environment           (getProgName)
import           System.Exit                  (exitFailure)
import           System.IO                    (IOMode (WriteMode), hPutStr,
                                               hPutStrLn, stderr, stdout,
                                               withFile)
import qualified Text.PrettyPrint             as PP
import qualified Text.PrettyPrint.Leijen.Text as WL
import           Text.Read                    (readMaybe)

import           Language.Coq.Deep
import           Language.Coq.ExtractedAST

import           Paths_digger (version)


-- | Cleanup options to activate before conversion
data Cleaning = Cleaning { modulesToDrop  :: [ModuleUsed]
                         , nameRewrites   :: [(String,String)]
                         , dotReplacement :: String
                         }
    deriving (Show,Eq)

instance Default Cleaning where
    def = Cleaning { modulesToDrop  = ["Datatypes"]
                   , nameRewrites   = [ ("Coq_true",  "true")
                                      , ("Coq_false", "false")
                                      , ("Coq_tt",    "tt") ]
                   , dotReplacement = "_"
                   }

-- | Parse a command-line argument containing a pair of strings
-- separated with a colon
colonSeparatedPair :: ReadM (String,String)
colonSeparatedPair = eitherReader go
    where go cs = case ':' `elemIndex` cs of
                    Just i  -> Right (take i cs, drop (i+1) cs)
                    Nothing -> Left "Expected a colon-separated argument \"before:after\""

-- | Parse command-line options for the 'Cleaning' step
optsCleaning :: Parser Cleaning
optsCleaning = Cleaning <$> optModulesToDrop <*> optRewrites <*> optDotReplacement
    where optModulesToDrop = some (strOption $ short 'm' <> long "drop-module" <> metavar "<module>"
                                            <> help ( "module to drop from fully-qualified identifiers in source "
                                                   ++ "(default, if no '-m' is given: "
                                                   ++ intercalate ", " (modulesToDrop def) ++ "); "
                                                   ++ "this option can be used multiple times" ))
                             <|> pure (modulesToDrop def)
          optRewrites = some (option colonSeparatedPair $
                                     short 'r' <> long "rename" <> metavar "<before>:<after>"
                                     <> help ( "identifiers to rename (performed after dropping modules from "
                                            ++ "fully-qualified names) "
                                            ++ "(default, if no '-r' is given: "
                                            ++ intercalate ", " (map (\(a,b) -> a ++ ":" ++ b) $ nameRewrites def) ++ ")" ))
                        <|> pure (nameRewrites def)
          optDotReplacement = strOption $ short 's' <> long "separator" <> metavar "<char>"
                                       <> value (dotReplacement def) <> showDefaultWith id
                                       <> help "separator to use in qualified names instead of dots"

-- | Parse a symbol on command-line
optSymbol :: Char -> String -> String -> String -> Parser String
optSymbol s l v h = strOption $ short s <> long l <> metavar "<symb>"
                           <> value v <> showDefaultWith id <> help h

-- | Parse command-line options for natural numbers
optsNat :: Parser CoqNat
optsNat = CN <$> optNatO
             <*> optNatS
             <*> optAdd
    where optNatO = optSymbol 'O' "natO"    (natO  def)  "how Coq's O (zero, type nat) was extracted"
          optNatS = optSymbol 'S' "natS"    (natS  def)  "how Coq's S (successor, type nat) was extracted"
          optAdd  = optSymbol 'A' "nat-add" (natAdd def) "how Coq's addition of natural number was extracted"

-- | Parse command-line options for the 'ConversionParams'
--
-- TODO: find a way to expose the parameters for unary and binary
-- operators, or it would not make much sense anyway?
optsCP :: Parser ConversionParams
optsCP = CP <$> optMonad
            <*> optBind
            <*> optRet
            <*> optUnit
            <*> optTt
            <*> optTrue
            <*> optFalse
            <*> optsNat
            <*> optConsts

            <*> optIgnores

            <*> optPrefixConst
            <*> optPrefixFun
            <*> optPrefixFTyp
            <*> optPrefixVTyp

            <*> optRecBound
            <*> pure (unaryOps def)
            <*> pure (binaryOps def)

    where optMonad      = optSymbol 'M' "monad" (monad def) "support monad"
          optBind       = optSymbol 'B' "bind"  (bind  def) "bind function of the monad"
          optRet        = optSymbol 'R' "ret"   (ret   def) "return function of the monad"
          optUnit       = optSymbol 'U' "unit"  (unit  def) "how Coq's unit (type) was extracted"
          optTt         = optSymbol 't' "tt"    (tt    def) "how Coq's tt (unit value) was extracted"
          optTrue       = optSymbol 'T' "true"  (true  def) "how Coq's true was extracted"
          optFalse      = optSymbol 'F' "false" (false def) "how Coq's false was extracted"
          optRecBound   = optSymbol 'v' "bound-var" (recBoundName def)
                                                            "name of the C variable used to bound recursive calls"

          optConsts     = some (strOption $ short 'c' <> long "const" <> metavar "<const>"
                                         <> help ( "constructor to accept and re-export as is "
                                                ++ "(default, if no '-c' is given: "
                                                ++ intercalate ", " (consts def) ++ ")" ))
                          <|> pure (consts def)

          optIgnores    = many $ strOption $ long "ignore" <> metavar "<symb>"
                                          <> help "symbol to ignore (do not try to convert it)"

          optPrefixConst = optPref "prefix-const" (prefixConst def) "prefix for exported constructor names"
          optPrefixFun   = optPref "prefix-fun"   (prefixFun def)   "prefix for exported function names"
          optPrefixFTyp  = optPref "prefix-ftyp"  (prefixFTyp def)  "prefix for exported function types"
          optPrefixVTyp  = optPref "prefix-vtyp"  (prefixVTyp def)  "prefix for exported value types"

          optPref l v h = option textR $ long l <> metavar "<pref>"
                                      <> value v <> showDefaultWith unpack <> help h
          textR :: ReadM Text
          textR = eitherReader (Right . pack)

-- | All command-line options
data CLiOptions = CLO { cleaning         :: Cleaning
                      , conversionParams :: ConversionParams
                      , deepOnly         :: Bool
                      -- | Whether we should output a .h or a .c
                      , headerOnly       :: Bool
                      , inputFile        :: FilePath
                      , outputFile       :: Maybe FilePath
                      -- | List of (prefix,Json file)
                      , dependencies     :: [(String,FilePath)]
                      , headerFile       :: Maybe FilePath
                        -- | #include or Require Import
                      , includes         :: [String]
                      , includesQuote    :: [String]
                        -- | Initial content of RecursiveBoundMap
                      , initRecBounds    :: [(FunName, RecursiveBoundId)]
                      }
    deriving (Show,Eq)

versionHelper :: String -> Parser (a -> a)
versionHelper progname = abortOption (InfoMsg (progname ++ " " ++ showVersion version)) $ mconcat
  [ long "version"
  , short 'v'
  , help "Show the version of the program"
  , hidden ]

-- | Command-line options
-- Take the name of the program as argument
options :: String -> ParserInfo CLiOptions
options pn = info (helper <*> versionHelper pn <*> cliopts) (fullDesc <> progDesc longDesc <> header shortDesc)
    where longDesc = unwords
              [ "Convert Coq code written in a \"C-style\" (imperative style"
              , "based on a monad, with full application of functions) into"
              , "the corresponding C code or to an intermediate representation (deep)"
              , "output as Coq source code."
              , "Start from the Coq code extracted as JSON by the internal extraction facility." ]
          shortDesc = pn ++ " " ++ showVersion version
                         ++ " - convert \"C-style\" Coq code into C code or an intermediate "
                         ++ "representation in Coq"

          cliopts = CLO <$> optsCleaning
                        <*> optsCP
                        <*> deepOnlyOpt
                        <*> headerOnlyOpt
                        <*> inputOpt
                        <*> outputOpt
                        <*> dependenciesOpt
                        <*> headerOpt
                        <*> includesOpt
                        <*> includesQuoteOpt
                        <*> recBoundsOpt

          deepOnlyOpt = switch $ long "deep"
                              <> help "stop conversion at the Deep intermediate language"

          headerOnlyOpt = switch $ long "header"
                                <> help "produce a .h file instead of a .c file"

          inputOpt = strArgument $ metavar "<file.json>"
                       <> help "input file to convert (produced by Coq JSON extractor)"

          outputOpt = optional $ strOption $ short 'o' <> metavar "<file>"
                       <> help "output result in <file> (default: stdout)"

          dependenciesOpt = many $ option colonSeparatedPair $
                               short 'd' <> long "dependency" <> metavar "<namespace>:<file.json>"
                            <> help ( "add to the initial symbol table and to the initial set of "
                                   ++ "recursive functions the content of the given module "
                                   ++ "(produced by Coq JSON extractor) into the given namespace" )

          headerOpt = optional $ strOption $ short 'H' <> metavar "<file>"
                       <> help "add the content of this file as header of the output"

          includesOpt = many $ strOption $ short 'i' <> long "include" <> metavar "<module>"
                        <> help "output a #include <...> (or \"Require Import\") for the <module>"
          includesQuoteOpt = many $ strOption $ short 'q' <> long "include-quote" <> metavar "<module>"
                           <> help "output a #include \"...\" (or \"Require Import\") for the <module>"

          recBoundsOpt = many $ option (eitherReader colonNat) $
                            short 'b' <> long "recursive-bound" <> metavar "<fun>:<n>"
                            <> help ( "assume <fun> is a recursive function and that its <n>-th argument "
                                   ++ "(0-indexed) is the bound on the number of recursive calls" )

          colonNat :: String -> Either String (FunName, RecursiveBoundId)
          colonNat cs = case ':' `elemIndex` cs of
                          Nothing -> Right (cs, 0)
                          Just i  -> case readMaybe (drop (i+1) cs) of
                                       Just n | n >= 0 -> Right (take i cs, n)
                                       _               -> Left msg
              where msg = "Expected a colon-separated argument \"function:n\" (with n positive)"
                        ++ " or just a function name"

cleanAST :: Cleaning -> Module -> Module
cleanAST Cleaning{..} = cleanModule modulesToDrop rewrts dotReplacement
    where rewrts = Map.fromList nameRewrites

renderRequires :: [String] -> [String] -> Text
renderRequires xs ys = T.unlines $ map renderRequire $ xs ++ ys
    where renderRequire i = T.concat ["Require Import ", T.pack i, "."]

renderIncludes :: [String] -> [String] -> Text
renderIncludes is qs = T.unlines $ map renderInclude is ++ map renderQuote qs
    where renderInclude i = T.concat ["#include <",  T.pack i, ">" ]
          renderQuote   i = T.concat ["#include \"", T.pack i, "\""]

readModule :: FilePath -> IO (Either String Module)
readModule = fmap eitherDecode' . BS.readFile

traverse2 :: (Applicative f, Applicative g, Traversable t) => (a -> f (g b)) -> t a -> f (g (t b))
traverse2 f = fmap sequenceA . traverse f

traverse2x2 :: (Applicative f, Applicative g, Traversable t, Traversable u) =>
               (a -> f (g b)) -> t (u a) -> f (g (t (u b)))
traverse2x2 f = traverse2 (traverse2 f)

zipEither :: Either a b -> Either a c -> Either [a] (b,c)
zipEither (Right x) (Right y) = Right (x,y)
zipEither (Left  x) (Right _) = Left [x]
zipEither (Right _) (Left  y) = Left [y]
zipEither (Left  x) (Left  y) = Left [x,y]

infoDependency :: ConversionParams -> Cleaning -> (String, Module) -> (SymbolTable, RecursiveBoundMap)
infoDependency cp cleanOpts (prefix, modul) = (go syms, go recs)
    where modul' = cleanAST cleanOpts modul
          syms   = extractSymbolTable cp $ declarationsMod modul'
          recs   = detectBounds (nats cp) modul'
          go     = Map.mapKeys (prefix ++)

main :: IO ()
main = do progname <- getProgName
          CLO{..}  <- execParser (options progname)
          input    <- readModule inputFile
          deps     <- traverse2x2 readModule dependencies

          case zipEither input deps of
            Right (input', deps') -> do
              let (symss, recss) = unzip $ map (infoDependency conversionParams cleaning) deps'
                  recs           = Map.unions (Map.fromList initRecBounds : recss)
                  syms           = Map.unions symss
                  deepMod        = fromCoq conversionParams recs syms $ cleanAST cleaning input'

              maybe ($ stdout) (`withFile` WriteMode) outputFile $ \out -> do
                let putText    = TIO.hPutStrLn out
                    putWL      = WL.displayIO out . WL.renderPretty 0.8 110
                    putPP      = hPutStr out . PP.render

                -- Header
                let renderIncs = if deepOnly then renderRequires else renderIncludes
                mapM_ (putText <=< TIO.readFile) headerFile
                putText $ renderIncs includes includesQuote

                -- Body
                if deepOnly
                  then putWL $ prettyModule conversionParams deepMod
                  else do let (errs, src) = convert conversionParams deepMod
                              convert     = if headerOnly then toCHeader else toCSource
                          mapM_ (hPutStrLn stderr) errs
                          putPP (C.pretty src)
                          when (not $ null errs) exitFailure

            Left errs -> mapM_ (hPutStrLn stderr) errs >> exitFailure
