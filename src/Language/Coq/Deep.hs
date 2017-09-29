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

module Language.Coq.Deep (
          -- * Deep intermediate representation
          -- $deep
          Id
        , VId
        , FId
        , Const
        , VTyp
        , ValTC
        , Fun (..)
        , Exp (..)
        , DeepModule (..)

        -- * Parameters for the conversions
        , ConversionParams (..)

        -- * Conversion from Coq.ExtractedAST syntax to Deep
        -- $coq2deep
        , SymbolTable
        , extractSymbolTable
        , fromCoq

        -- * Pretty-print a Deep module into Coq syntax
        -- $deep2coq
        , prettyModule

        -- * Conversion from Deep intermediate representation to C
        -- $deep2c
        , toCSource
        , toCHeader
    ) where

import           Control.Arrow                (second, (&&&))
import           Data.Default
import           Data.Either                  (partitionEithers)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust, maybeToList)
import qualified Data.Set                     as Set
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as Text
import           Language.C                   hiding (Name)
import           Language.C.Data.Ident        (Ident (..))
import           Text.PrettyPrint.Leijen.Text (Doc, dquotes, empty, enclose,
                                               encloseSep, hang, hcat, lbracket,
                                               line, lparen, nest, parens,
                                               rbracket, rparen, semi, sep,
                                               space, text, tupled, vcat, vsep,
                                               (<+>), (</>), (<>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import           Language.Coq.ExtractedAST

-- $deep
-- Define the Deep intermediate language.
-- This intermediate language only captures the part of the Gallina
-- language that can be converted into C because it is structured just
-- like C code (using a monad to represent sequence).

-- | Identifiers
type Id = String
-- | Variable identifiers
type VId = Id
-- | Function identifiers
type FId = Id

-- | Constants
type Const = String

-- | Types for base values (i.e. not functions)
-- As far as we are concerned, these will be only manipulated as
-- strings in the converter
type VTyp = String

-- | Typing contexts for values
-- In particular, a typing context describes the types of the argument
-- of a function
type ValTC = [(VId, VTyp)]

-- | Actual term (content) of a function
--
-- Built with:
--
-- * the name of the function itself (it will be bound in the body for
--   the recursive case)
-- * a list of functions it can call (and their recursive bounds,
--   given by their names, when applicable)
-- * a list of arguments (and their types)
-- * a return type
--
-- and, if the function is recursive:
--
-- * the body of the function on the base case
-- * the body of the function on the recursive case
--
-- otherwise, the body of the function.
--
-- Compared with Deep syntax, this does not give the bound of
-- recursive functions since it will be defined only in calling
-- functions
--
-- TODO: Should 'calls' be replaced with something containing also the
-- global variables?
data Fun = FunRec { self   :: FId
                  , calls  :: [(FId, Maybe RecursiveBound)]
                  , args   :: ValTC
                  , resTyp :: VTyp
                  , bodyO  :: Exp
                  , bodyS  :: Exp }
         | Fun    { self   :: FId
                  , calls  :: [(FId, Maybe RecursiveBound)]
                  , args   :: ValTC
                  , resTyp :: VTyp
                  , body   :: Exp }
    deriving (Show,Eq)

-- | Syntax of expressions
-- Bind and Return are the standard monadic operators
-- BindN is Bind ignoring the value produced by the first expression
-- Call applies (fully) a function to its arguments
-- IfThenElse, Var and Const are surpriseless
data Exp = Bind       VId (Maybe VTyp) Exp Exp
         | BindN      Exp Exp
         | Return     Exp
         | Call       FId [Exp]
         | IfThenElse Exp Exp Exp
         | Var        VId            -- ^ TODO: Should this be called differently?
         | Const      VId            -- ^ TODO: Should this be specialised to Bool?
    deriving (Show,Eq)

-- | A full module in the Deep world
-- Its symbol table and recursive-bounds map contain also information
-- about its dependencies, not only about the module itself
data DeepModule = DeepModule { nameDeepMod            :: Name
                             , symbolTableDeepMod     :: SymbolTable
                             , recursiveBoundsDeepMod :: RecursiveBoundMap
                             , functionsDeepMod       :: [Either ErrorMsg (Name,Fun)]
                             }


-- | Parameters for the conversion
--
-- Provide various parameters:
--
-- * the constants to recognize in the Coq source (in particular what
--   is the name of the support monad used to represent C sequence)
-- * the prefixes to use when outputting Deep intermediate language in
--   Gallina (so that source definition "func" becomes "prefix_func")
-- * a few parameters to translate Deep into C: how the recursive
--   bound should be named in C code and which Coq functions should be
--   mapped to native C operators

data ConversionParams = CP
        { monad        :: Name                  -- ^ Name (type) of the support monad
        , bind         :: Name                  -- ^ Bind of the support monad
        , ret          :: Name                  -- ^ Return of the support monad
        , unit         :: Name                  -- ^ How Coq’s unit (type) is extracted
        , tt           :: Name                  -- ^ How Coq’s tt (value of type unit) is extracted
        , true         :: Name                  -- ^ How Coq’s true is extracted
        , false        :: Name                  -- ^ How Coq’s false is extracted
        , nats         :: CoqNat                -- ^ How Coq’s nat are extracted
        , consts       :: [Name]                -- ^ Constructors that should be preserved

        , ignores      :: [Name]                -- ^ Declarations that should be ignored in Coq source
                                                --   (for instance because they cannot be translated)

        , prefixConst  :: Text                  -- ^ Prefix of Deep versions of constructors
        , prefixFun    :: Text                  -- ^ Prefix added to function names in their Deep versions
        , prefixFTyp   :: Text                  -- ^ Prefix for function types definition
        , prefixVTyp   :: Text                  -- ^ Prefix for value types

        , recBoundName :: Name                  -- ^ Name of the recursive-bound variable in generated C
        , unaryOps     :: Map.Map FId CUnaryOp  -- ^ Map from Coq functions to C unary operators
        , binaryOps    :: Map.Map FId CBinaryOp -- ^ Map from Coq functions to C binary operators
        }
    deriving (Show,Eq)

instance Default ConversionParams where
    def = CP { monad        = "Monad"
             , bind         = "bind"
             , ret          = "ret"
             , unit         = "unit"
             , tt           = "tt"
             , true         = "true"
             , false        = "false"
             , nats         = def
             , consts       = ["true","false","tt"]

             , ignores      = []

             , prefixConst  = "deepConst_"
             , prefixFun    = "deepFun_"
             , prefixFTyp   = "deepFTyp_"
             , prefixVTyp   = "deepVTyp_"

             , recBoundName = "rec_bound"
             , unaryOps     = Map.fromList [("negb",CNegOp)]
             , binaryOps    = Map.fromList [("andb",CLndOp)
                                           ,("orb", CLorOp)]
             }


-- $coq2deep
-- Define the conversion from the extracted Coq AST into the Deep
-- intermediate representation.

-- | Symbol table
--
-- Maps to every known symbol:
--
-- * the type of the value it returns if it is a function,
-- * its type otherwise.
--
-- Warning: this assumes that the VId and FId types are indeed synonyms
type SymbolTable = Map.Map Id VTyp

-- | Extract the "symbol type"
--
-- * if it is a function, the type of the value it returns
-- * if it is a global variable, its type
extractSymbolType :: ConversionParams -> Type -> Either ErrorMsg VTyp
extractSymbolType  _ (Glob ty []) = pure ty
extractSymbolType cp typ          = go typ
    where go (Arrow _ ty)                          = go ty
          go (Glob m [Glob ty []]) | m == monad cp = pure ty
          go ty                                    = Left $ impossibleType ty

          impossibleType ty = "Impossible to extract symbol type of: " ++ show ty

-- | Extract the symbol table from a list of declarations
extractSymbolTable :: ConversionParams -> [Decl] -> SymbolTable
extractSymbolTable cp = Map.fromList . go . concatMap open
    where open :: Decl -> [(Id, Type)]
          open t@Term{}       = [(nameTerm t, typeTerm t)]
          open (Fixgroup fis) = map (nameItem &&& typeItem) fis

          go :: [(Id, Type)] -> [(Id, VTyp)]
          go xs = [ (n,t) | (n, Right t) <- map (second (extractSymbolType cp)) xs]

-- | Infer the result type of an expression
-- Use a very simple strategy, since the set of expressions is really
-- limited
-- Return the modified expression (in case some included type
-- annotations were modified) and the result type, when possible
inferExpTypes :: SymbolTable -> Exp -> (Maybe VTyp, Exp)
inferExpTypes st (Bind v _ e1 e2)   = let (ty1, e1') = inferExpTypes st e1
                                          st'        = maybe st (\t -> Map.insert v t st) ty1
                                      in  Bind v ty1 e1' <$> inferExpTypes st' e2
inferExpTypes st (BindN    e1 e2)   = let (_, e1') = inferExpTypes st e1
                                      in  BindN e1' <$> inferExpTypes st e2
inferExpTypes st (Return e)         = Return <$> inferExpTypes st e
inferExpTypes st (Call f es)        = (Map.lookup f st, Call f (map (snd . inferExpTypes st) es))
inferExpTypes st (IfThenElse i t e) = let (_,   i') = inferExpTypes st i
                                          (tyt, t') = inferExpTypes st t
                                          (tye, e') = inferExpTypes st e
                                          ty        = if isJust tyt then tyt else tye
                                      in  (ty, IfThenElse i' t' e')
inferExpTypes st e@(Var v)          = (Map.lookup v st, e)
inferExpTypes st e@(Const v)        = (Map.lookup v st, e)

inferTypes :: SymbolTable -> Fun -> Fun
inferTypes st fun@Fun{}    = fun{body  = snd $ inferExpTypes st (body fun)}
inferTypes st fun@FunRec{} = fun{bodyO = snd $ inferExpTypes st (bodyO fun)
                                ,bodyS = snd $ inferExpTypes st (bodyS fun)}

-- | Convert the type of a function
-- The types that can be converted are of the form
--     x -> y -> z -> Monad t
-- where x, y, z and t are global names, not variables.
-- This function only converts the original type, the actual argument
-- names are required to build a FTyp

convertFunType :: ConversionParams -> Type -> Either ErrorMsg ([VTyp], VTyp)
convertFunType cp = go
    where go (Arrow (Glob n []) ty2)         = go ty2 >>= \(tys, ty) -> pure (n:tys, ty)
          go (Glob m' [Glob t []]) | m == m' = pure ([], t)
          go ty                              = Left $ impossibleType ty

          m = monad cp

          impossibleType ty = "Impossible to convert type: " ++ show ty

-- | Convert the body of a function
-- Return either the resulting Deep expression or some (Coq AST)
-- sub-expression that could not be converted
--
-- Handle the following possible cases:
-- - monadic bind
-- - monadic bind where the second action ignores the value generated
--   by the first
-- - monadic return
-- - application of a function to some arguments (since we are not
--   compiling a functional language, that function can only be a
--   globally existing function)
-- - using a global or local name
-- - if then else, which is represented in Coq by a pattern-matching
--   on boolean, so we unfold them to recover the two branches and
--   check the two constructors
-- - constructors for inline booleans

convertFunBody :: ConversionParams -> Expr -> Either ErrorMsg Exp
convertFunBody cp = go True
    where -- | Translate an expression
          -- Take as first argument whether the expression is monadic
          -- or not
          go :: Bool -> Expr -> Either ErrorMsg Exp
          go True  (Apply (Global b) [e1, Lambda ["_"] e2]) | b == bind cp = BindN          <$> go True e1 <*> go True e2
          go True  (Apply (Global b) [e1, Lambda [v] e2])   | b == bind cp = Bind v Nothing <$> go True e1 <*> go True e2
          go True  (Apply (Global r) [e])                   | r == ret  cp = Return <$> go False e
          go _     (Apply (Global f) as)                                   = Call f <$> traverse (go False) as
          go True  (Global v)                                              = Right (Call v [])
          go False (Global v)                                              = Right (Var v)
          go False (Rel v)                                                 = Right (Var v)
          go True  (Case cond [C (ConstructorP c1 []) e1
                              ,C (ConstructorP c2 []) e2])  | checkIFT     = IfThenElse <$> go False cond
                                                                                        <*> go True thenE
                                                                                        <*> go True elseE
                      where checkIFT = c1 == true  cp && c2 == false cp
                                    || c1 == false cp && c2 == true  cp
                            (thenE,elseE) = if c1 == true cp then (e1,e2) else (e2,e1)
          go False (ConstructorE c []) | c `elem` consts cp                = Right (Const c)

          go monadic e                                                     = Left $ impossibleExpr monadic e

          impossibleExpr m ex = "Impossible to convert expression \"" ++ show ex ++ "\" " ++
                                "in " ++ prefix m ++ "monadic context"
          prefix True         = ""
          prefix False        = "non-"

-- | Complete the list of calls in a function declaration
-- Add the calls to all the functions (including the non-recursive
-- ones) to the list of calls
completeCalls :: Fun -> Fun
completeCalls fun = fun{calls = calls fun ++ map (\f -> (f, Nothing)) newUnique}
    where extract (Bind _ _ e1 e2)   = concatMap extract [e1,e2]
          extract (BindN    e1 e2)   = concatMap extract [e1,e2]
          extract (Return e)         = extract e
          extract (Call f as)        = f : concatMap extract as
          extract (IfThenElse i t e) = concatMap extract [i,t,e]
          extract _                  = []

          allCalled Fun{}    = extract (body fun)
          allCalled FunRec{} = concatMap extract [bodyO fun, bodyS fun]

          allCalled' = Set.fromList . allCalled

          already   = Set.fromList (self fun : map fst (calls fun))
          newCalled = Set.filter (`Set.notMember` already) (allCalled' fun)
          newUnique = Set.toList newCalled

-- | Convert a declaration (a function or a fixgroup)
--
-- FIXME? here there are some cases of inconsistency between the
-- RecursiveBoundMap and the actual number of arguments encountered
-- that are _not_ detected (and would result in exceptions)
convertDecl :: ConversionParams -> RecursiveBoundMap -> SymbolTable -> Decl -> Either ErrorMsg [(Name, Fun)]
convertDecl cp recs syms = go
    where -- | Base case of transformation of one function
          base nam typ ex = do (tyArgs, tyRes)         <- convertFunType cp typ
                               (recCalls, ex')         <- extractRecCalls (nats cp) nam recs ex
                               (args', bodyO', bodyS') <- unLambda nam ex'
                               bodyO''                 <- convertFunBody cp bodyO'
                               bodyS''                 <- maybe (Right Nothing) (fmap Just . convertFunBody cp) bodyS'
                               let args''    = zip args' tyArgs
                                   args'''   = case Map.lookup nam recs of
                                                 Just pos -> case splitAt pos args'' of
                                                               (args1, _:args2) -> args1 ++ args2
                                                 Nothing  -> args''
                                   recCalls' = map (second Just) recCalls
                                   whole     = case bodyS'' of
                                                 Just bodyS''' -> FunRec nam recCalls' args''' tyRes bodyO'' bodyS'''
                                                 Nothing       -> Fun    nam recCalls' args''' tyRes bodyO''
                               pure (nam, inferTypes syms $ completeCalls whole)

          ignores' = Set.fromList (ignores cp)

          base' nam typ ex | Set.member nam ignores' = Right []
                           | otherwise               = case base nam typ ex of
                                                         Left err -> Left $ "Error in " ++ nam ++ ": " ++ err
                                                         Right r  -> Right [r]

          unLambda _
            (Lambda args'
              (Case (Rel _) [C (ConstructorP o []) bodyO'
                            ,C (ConstructorP s [_]) bodyS']))
                |    o == natO (nats cp)
                  && s == natS (nats cp)     = pure (args', bodyO', Just bodyS')
          unLambda   _ (Lambda args' body')  = pure (args', body', Nothing)
          unLambda nam e                     = Left $ "Lambda expected in " ++ nam
                                                   ++ " in expression: " ++ show e

          go (Term nam typ ex) = base' nam typ ex
          go (Fixgroup funs)   = concat <$> traverse (\(FixItem nam typ ex) -> base' nam typ ex) funs

-- | Convert a "ExtractedAST" Coq 'Module' to Deep language
-- Takes as arguments the 'RecursiveBoundMap' and 'SymbolTable'
-- corresponding to the modules the converted one requires
fromCoq :: ConversionParams -> RecursiveBoundMap -> SymbolTable -> Module -> DeepModule
fromCoq cp recs syms modul = DeepModule{ nameDeepMod            = nameMod modul
                                       , symbolTableDeepMod     = syms'
                                       , recursiveBoundsDeepMod = recs'
                                       , functionsDeepMod       = funs
                                       }
    where recs' = Map.union recs $ detectBounds (nats cp) modul
          syms' = Map.union syms $ extractSymbolTable cp decls
          decls = declarationsMod modul
          funs  = concatMap (sequence . convertDecl cp recs' syms') decls


-- $deep2coq
-- Define the conversion of the Deep intermediate language into an
-- explicit representation of Deep in Coq.

-- | Pretty-print Id
prettyId :: Id -> Doc
prettyId = text . Text.pack

quoteId :: Id -> Doc
quoteId = dquotes . prettyId

-- | Pretty-print a recursive bound constant (Coq nat number)
prettyBoundConst :: RecursiveBoundConst -> Doc
prettyBoundConst = text . Text.pack . show

-- | Pretty-print a recursive bound
prettyBound :: RecursiveBound -> Doc
prettyBound (b, vs) = go vs''
    where vs'              = map prettyId vs
          vs'' | b == 0    = vs'
               | otherwise = vs' ++ [prettyBoundConst b]
          go []            = text "0"
          go [x]           = x
          go xs            = encloseSep lparen rparen (text " + ") xs

-- | Pretty-print a constructor
prettyConst :: ConversionParams -> Id -> Doc
prettyConst cp x = text (prefixConst cp) <> prettyId x

-- | Pretty-print a VTyp
prettyVTyp :: ConversionParams -> VTyp -> Doc
prettyVTyp cp typ = text (prefixVTyp cp) <> prettyId typ

-- | Pretty-print a Coq list
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket semi

-- | Pretty-print a sub-expression
sub :: ConversionParams -> Exp -> Doc
sub cp = nest 2 . parens . prettyExp cp

-- | Pretty-print an expression
prettyExp :: ConversionParams -> Exp -> Doc
prettyExp cp (Bind v _ e1 e2)      = nest 2 ( sep [ text "BindS" </> quoteId v
                                                  , sub cp e1 ] )
                                     PP.<$> sub cp e2
prettyExp cp (BindN e1 e2)         = nest 2 ( sep [ text "BindN"
                                                  , sub cp e1 ] )
                                     PP.<$> sub cp e2
-- | Special cases of return
prettyExp  _ (Return (Var x))      = text "Return RR" </> parens (text "Var" </> quoteId x)
prettyExp cp (Return (Const c))    = text "Return RR" </> prettyConst cp c
-- | Strangest of all special cases: if it is not a Var or a Const, we
-- handle it as if it were a boolean expression, to bypass current
-- limitations in Return
prettyExp cp (Return x)            = prettyExp cp (IfThenElse x (Return (Const (true  cp)))
                                                                (Return (Const (false cp))))
-- The following should be the actual normal case for return
-- prettyExp cp (Return x)            = text "Return RR" </> sub cp x
prettyExp cp (Call f as)           = sep [ text "Apply"
                                         , parens (text "FVar" </> quoteId f)
                                         , parens (text "PS" </> list (map (prettyExp cp) as)) ]
prettyExp cp (IfThenElse e1 e2 e3) = sep ["IfThenElse", sub cp e1, sub cp e2, sub cp e3]
prettyExp  _ (Var x)               = text "Return LL" </> parens (text "Var" </> quoteId x)
prettyExp cp (Const c)             = text "Return LL" </> prettyConst cp c

-- | Pretty-print an argument list
prettyArgs :: ConversionParams -> ValTC -> Doc
prettyArgs cp = list . map prettyArg
    where prettyArg (var, typ) = tupled [quoteId var, prettyVTyp cp typ]

-- | Pretty-print a Fun
prettyFun :: ConversionParams -> Fun -> Maybe Doc -> Doc
prettyFun cp fun bnd =
    nest 2 $ vsep [ sep [ text "FC"
                        , list (map prettyCalled (calls fun))
                        , prettyArgs cp (args fun) ]
                  , prettyBody (bodyO' fun)
                  , prettyBody (bodyS' fun)
                  , quoteId (self fun) <+> fromMaybe empty bnd ]
    where prettyCalled (fun', bnd') = tupled [ quoteId fun'
                                             , hcat [ text (prefixFun cp)
                                                    , prettyId fun'
                                                    , maybe empty (const space) bnd'
                                                    , maybe empty prettyBound bnd' ] ]

          prettyBody = hang 2 . enclose (text "( ") (text " )") . prettyExp cp
          bodyO' (FunRec _ _ _ _ b _) = b
          bodyO' (Fun    _ _ _ _ b)   = b
          bodyS' (FunRec _ _ _ _ _ b) = b
          bodyS' (Fun    n _ as _ _)  = Call n (map (Var . fst) as)

-- | Pretty-print a FTyp
prettyFTyp :: ConversionParams -> Fun -> Doc
prettyFTyp cp fun = nest 2 $ sep [ text "FT"
                                 , prettyArgs cp (args fun)
                                 , prettyVTyp cp (resTyp fun) ]

-- | Pretty-print a definition
prettyDef :: ConversionParams -> RecursiveBoundMap -> (Name, Fun) -> Doc
prettyDef cp recs (nam, fun) = hang 2 deffun PP.<$> hang 2 deftyp
    where isRec           = isJust $ Map.lookup nam recs
          bnd | isRec     = Nothing
              | otherwise = Just $ prettyId $ natO $ nats cp
          typ | isRec     = text "nat -> Fun"
              | otherwise = text "Fun"

          deffun = text "Definition" </> text (prefixFun cp) <>
                   prettyId nam </> text ":" </> typ </> text ":=" <> line <>
                   prettyFun cp fun bnd <> text "." <> line
          deftyp = text "Definition" </> text (prefixFTyp cp) <>
                   prettyId nam </> text ": FTyp" </> text ":=" <> line <>
                   prettyFTyp cp fun <> text "." <> line

-- | Pretty-print a 'Doc' as a Coq comment
comment :: Doc -> Doc
comment = hang 3 . enclose (text "(* ") (text " *)")

-- | Pretty-print an error
-- When a function fails to convert, pretty-print the error message
prettyErr :: ErrorMsg -> Doc
prettyErr err = comment (text (Text.pack err)) <> line

-- | Pretty-print a definition whose convertion could have failed
prettyDefErr :: ConversionParams -> RecursiveBoundMap -> Either ErrorMsg (Name, Fun) -> Doc
prettyDefErr cp recs = either prettyErr (prettyDef cp recs)

-- | Pretty-print a module
prettyModule :: ConversionParams -> DeepModule -> Doc
prettyModule cp modul = vcat $ map (prettyDefErr cp recs) funs
    where recs = recursiveBoundsDeepMod modul
          funs = functionsDeepMod modul


-- $deep2c
-- Define the conversion from the Deep intermediate representation
-- into C code, using the "Language.C" library to represent the result
-- of that conversion.

-- | Define an identifier without any node information
-- To build an AST, the information would not be much use
ident :: String -> Ident
ident n = let Ident n' i _ = internalIdent n
          in  Ident n' i undefNode

-- | Return an unsigned int constant
unsignedConst :: Integer -> CExpr
unsignedConst n = CConst (CIntConst (CInteger n DecRepr flagUnsigned) undefNode)
    where flagUnsigned = setFlag FlagUnsigned noFlags

-- | Convert a Deep type into a C type declaration
cVTyp :: ConversionParams -> VTyp -> CDeclSpec
cVTyp cp = cMaybeVTyp cp . Just

-- | Convert a possibly-absent Deep type into a C type declaration
-- When absent, generate an "auto" type instead of a type
cMaybeVTyp :: ConversionParams -> Maybe VTyp -> CDeclSpec
cMaybeVTyp cp (Just typ) | typ == unit cp = CTypeSpec $ CVoidType undefNode
                         | otherwise      = CTypeSpec $ CTypeDef (ident typ) undefNode
cMaybeVTyp _   Nothing                    = CStorageSpec $ CAuto undefNode

-- | Convert function arguments into C
cFunArgs :: ConversionParams -> Fun -> Either [Ident] ([CDecl],Bool)
cFunArgs cp fun = Right (argsDefs, False)
    where argDef (i,t) = CDecl [cVTyp cp t]
                               [(Just (CDeclr (Just (ident i)) [] Nothing [] undefNode),Nothing,Nothing)]
                               undefNode
          argsDefs = let defs = map argDef (args fun)
                     in  case fun of
                           Fun{}    -> defs
                           FunRec{} -> boundarg : defs
          boundarg = CDecl [CTypeSpec (CUnsigType undefNode),CTypeSpec (CIntType undefNode)]
                           [(Just (CDeclr (Just (ident (recBoundName cp))) [] Nothing [] undefNode)
                            ,Nothing,Nothing)]
                           undefNode

-- | Convert a function prototype into C
toCPrototype :: ConversionParams -> Fun -> CExtDecl
toCPrototype cp fun = CDeclExt (CDecl [CStorageSpec (CExtern undefNode)
                                      ,cVTyp cp (resTyp fun)]
                                      [(Just (CDeclr (Just (ident (self fun)))
                                                     [CFunDeclr (cFunArgs cp fun) [] undefNode]
                                                     Nothing [] undefNode)
                                       ,Nothing,Nothing)]
                                      undefNode)

-- | Convert a Deep expression into a C expression
-- Take as arguments the conversion parameters and the enclosing
-- function
-- It might result in no expression at all, when the expression is Coq
-- unit (since no expression should be of type void)
--
-- TODO: Should we accept IfThenElse in expressions (we might turn
-- them into ternary expressions)?
cExpression :: ConversionParams -> Fun -> Exp -> Either ErrorMsg (Maybe CExpr)
cExpression cp fun (Return e)             = cExpression cp fun e
cExpression cp   _ (Const v) | v == tt cp = pure Nothing
                             | otherwise  = pure $ Just $ CVar (ident v) undefNode
cExpression  _   _ (Var v)                = pure $ Just $ CVar (ident v) undefNode
-- | Call of a unary operator
cExpression cp fun e@(Call f [a]) | Map.member f (unaryOps cp) = do
                                a' <- cExpression cp fun a
                                case a' of
                                  Nothing  -> Left $ "Impossible to convert unary op " ++ show e
                                  Just a'' -> pure $ Just $ CUnary (fromJust $ Map.lookup f $ unaryOps cp) a'' undefNode
-- | Call of a binary operator
cExpression cp fun e@(Call f as@[_,_]) | Map.member f (binaryOps cp) = do
                                as' <- traverse (cExpression cp fun) as
                                case as' of
                                  [Just a1, Just a2] -> pure $ Just $ CBinary (fromJust $ Map.lookup f $ binaryOps cp) a1 a2 undefNode
                                  _                  -> Left $ "Impossible to convert binary op " ++ show e
-- | Call, general case
-- This must handle in particular whether we are performing a
-- recursive call, or a call to another recursive function
cExpression cp fun (Call f as) = do as' <- traverse (cExpression cp fun) as
                                    let as'' = catMaybes (recBound:as')
                                    pure $ Just $ CCall (CVar (ident f) undefNode) as'' undefNode
    where recBound | f == self fun = -- recBoundName - 1
                                     Just $ CBinary CSubOp
                                                    (CVar (ident (recBoundName cp)) undefNode)
                                                    (unsignedConst 1)
                                                    undefNode
                   | otherwise     = case lookup f (calls fun) of
                                       Just (Just v) -> Just $ boundExp v
                                       _             -> Nothing
          boundExp (c, vs) = let vs'  = map (flip CVar undefNode . ident) vs
                                 vs'' = if c == 0 then vs' else vs' ++ [unsignedConst c]
                             in  if null vs''
                                    then unsignedConst 0
                                    else foldr1 (\e1 e2 -> CBinary CAddOp e1 e2 undefNode) vs''
cExpression   _   _ e           = Left $ "Impossible to convert expression \"" ++ show e ++ "\" to C"

-- | Convert an expression into statements
-- Take as arguments the conversion parameters, the enclosing
-- function and whether the result should be returned or not
-- It will result in 0, 1 or 2 statements
-- It will result in two statements when it should return
-- and the expression is of type void
cExprStmt :: ConversionParams -> Fun -> Bool -> Exp -> Either ErrorMsg [CStat]
cExprStmt cp fun tailRet e = do e' <- cExpression cp fun e
                                pure $ case tailRet of
                                  True | resTyp fun == unit cp
                                         && isJust e'           -> [ CExpr   e'      undefNode
                                                                   , CReturn Nothing undefNode ]
                                       | otherwise              -> [ CReturn e'      undefNode ]
                                  _    | isJust e'              -> [ CExpr   e'      undefNode ]
                                       | otherwise              -> []

-- | Convert a Deep expression into a block of statements
-- Take as arguments the conversion parameters and the enclosing function
cBlock :: ConversionParams -> Fun -> Exp -> Either ErrorMsg CStat
cBlock cp fun = goBlock True
    where goStmt :: Bool -> Exp -> Either ErrorMsg [CStat]
          -- | The Return case is ready for the Return-less Deep
          -- language
          goStmt tailRet (Return e)         = goStmt tailRet e
          goStmt tailRet (IfThenElse i t e) = do i' <- cExpression cp fun i
                                                 case i' of
                                                   Just i'' -> do
                                                         t' <- goBlock tailRet t
                                                         e' <- goBlock tailRet e
                                                         pure [CIf i'' t' (Just e') undefNode]
                                                   Nothing  -> Left $ "Impossible to convert " ++ show i
          goStmt tailRet e@(Var _)    = cExprStmt cp fun tailRet e
          goStmt tailRet e@(Const _)  = cExprStmt cp fun tailRet e
          goStmt tailRet e@(Call _ _) = cExprStmt cp fun tailRet e

          -- | Flatten a sequence of binds into a list of triples
          -- ('tailRet', 'VId' if named 'Bind', 'exp')
          -- Note that 'tailRet' is always 'False' when 'VId' is not
          -- 'Nothing'
          flatten :: Bool -> Exp -> [(Bool, Maybe (VId, Maybe VTyp), Exp)]
          flatten tailRet (Bind v t e1 e2) = (False, Just (v, t),  e1) : flatten tailRet e2
          flatten tailRet (BindN    e1 e2) = (False, Nothing,      e1) : flatten tailRet e2
          flatten tailRet e                = [(tailRet, Nothing, e)]

          goBlock :: Bool -> Exp -> Either ErrorMsg CStat
          goBlock tailRet e = block <$> traverse entry (flatten tailRet e)
              where block stmts = CCompound [] (concat stmts) undefNode

                    entry (tailRet', Nothing, e') = map CBlockStmt <$> goStmt tailRet' e'
                    entry (       _, Just vt, e') = maybeToList . fmap (CBlockDecl . decl vt)
                                                                   <$> cExpression cp fun e'

                    decl (v,t) e' = CDecl [cMaybeVTyp cp t]
                                          [(Just (CDeclr (Just (ident v)) [] Nothing [] undefNode)
                                           ,Just (CInitExpr e' undefNode)
                                           ,Nothing)]
                                          undefNode

-- | Convert a function body into C
cFunBody :: ConversionParams -> Fun -> Either ErrorMsg CStat
cFunBody cp fun@(Fun    _ _ _ _ b)     = cBlock cp fun b
cFunBody cp fun@(FunRec _ _ _ _ bO bS) = do bO' <- cBlock cp fun bO
                                            bS' <- cBlock cp fun bS
                                            pure $ CCompound []
                                                             [CBlockStmt $
                                                                CIf (CBinary CEqOp bnd zero undefNode)
                                                                    bO' (Just bS') undefNode]
                                                             undefNode
    where zero = unsignedConst 0
          bnd  = CVar (ident (recBoundName cp)) undefNode

-- | Convert a function definition into C
toCDefinition :: ConversionParams -> Fun -> Either ErrorMsg CFunDef
toCDefinition cp fun = do b <- cFunBody cp fun
                          pure $ CFunDef [cVTyp cp (resTyp fun)]
                                         (CDeclr (Just (ident (self fun)))
                                                 [CFunDeclr (cFunArgs cp fun) [] undefNode]
                                                 Nothing [] undefNode)
                                         [] b undefNode

-- | Convert a module from its Deep intermediate representation into C
-- source code
toCSource :: ConversionParams -> DeepModule -> ([ErrorMsg], CTranslUnit)
toCSource cp modul = second buildUnit $ partitionEithers $ map go funs
    where funs = functionsDeepMod modul
          go :: Either ErrorMsg (Name,Fun) -> Either ErrorMsg CExtDecl
          go funErr = do (_,fun) <- funErr
                         fun'    <- toCDefinition cp fun
                         pure (CFDefExt fun')
          buildUnit defs = CTranslUnit defs undefNode

-- | Convert a module from its Deep intermediate representation into a
-- C header file
toCHeader :: ConversionParams -> DeepModule -> ([ErrorMsg], CTranslUnit)
toCHeader cp modul = second (buildUnit . map (toCPrototype cp . snd)) $ partitionEithers funs
    where funs           = functionsDeepMod modul
          buildUnit defs = CTranslUnit defs undefNode
