-- |
-- Module      :  Extractor (Main)
-- Copyright   :  Veïs Oudjail 2016
-- License     :  CeCILL
--
-- Maintainer  :  veis.oudjail@gmail.com
--                samuel.hym+bugs@rustyne.lautre.net

-- Stability   :  experimental
-- Portability :  multi plateform
--
-- Description : Compiler Coq (AST JSON) -> subset C
--

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
module Main where

import Control.Applicative
import Control.Monad       (zipWithM, join)
import Data.Aeson
import Data.Bool           (bool)
import Data.Char           (toUpper)
import Data.List           (intercalate, union, find)
import Data.List.Split     (splitOn)
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import Options.Applicative (strArgument,strOption,short,long,metavar,optional,help)
import System.Environment  (getProgName)
import Text.Regex.Posix    ((=~))

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Options.Applicative        as OA

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

----------- Module ASTCoq -----------------

-- Ce type peut-être interprété comme une énumération.
-- Chacun de ses éléments représente une entité de l'arbre syntaxique Coq.
data WhatT = DeclT DeclT | FixgrpT FixgrpT | TypeT TypeT
           | ExprT ExprT | CaseT | PatT PatT
           | ModuleT
           deriving (Show)

data DeclT   = TermDT | FixgrpDT
             deriving (Show)
data FixgrpT = ItemFT
             deriving (Show)
data TypeT   = ArrowTT | GlobTT | VaridxTT
             deriving (Show)
data ExprT   = LambdaET| ApplyET | GlobalET | ConstructorET | RelET | CaseET | LetET
             deriving (Show)
data PatT    = ConstructorPT | WildPT
             deriving (Show)


instance FromJSON WhatT where
  parseJSON (Object v) = toWhatType <$> v .: "what"
  parseJSON _          = fail "Error, WhatType : undefined !"


-- Fonction qui permet de transformer les étiquettes dans le type enuméré
toWhatType :: String -> WhatT
toWhatType strType =
  case strType of
    "case"             -> CaseT
    "module"           -> ModuleT
    "decl:term"        -> DeclT   TermDT
    "decl:fixgroup"    -> DeclT   FixgrpDT
    "fixgroup:item"    -> FixgrpT ItemFT
    "type:arrow"       -> TypeT   ArrowTT
    "type:glob"        -> TypeT   GlobTT
    "type:varidx"      -> TypeT   VaridxTT
    "expr:lambda"      -> ExprT   LambdaET
    "expr:apply"       -> ExprT   ApplyET
    "expr:global"      -> ExprT   GlobalET
    "expr:constructor" -> ExprT   ConstructorET
    "expr:rel"         -> ExprT   RelET
    "expr:case"        -> ExprT   CaseET
    "expr:let"         -> ExprT   LetET
    "pat:constructor"  -> PatT    ConstructorPT
    "pat:wild"         -> PatT    WildPT
    strError           -> error $ "Error WhatType : " ++ strError ++ " format undefined"

--------------------------------------------------------------
data Decl = Term     { nameTerm  :: String
                     , typeTerm  :: Type
                     , valueTerm :: Expr
                     }

          | Fixgroup { fixlistFixgroup :: [Fixgroup]
                     }
          deriving (Show)

instance FromJSON Decl where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         DeclT TermDT   ->
           Term <$>
           v .: "name"  <*>
           v .: "type"  <*>
           v .: "value"

         DeclT FixgrpDT ->
           Fixgroup <$>
           v .: "fixlist"

         _                  -> fail "Error, Decl : case undefined !"

  parseJSON _          = fail "Error, Decl : undefined !"


data Fixgroup = Item { nameItem  :: String
                     , typeItem  :: Type
                     , valueItem :: Expr
                     }
              deriving (Show)

instance FromJSON Fixgroup where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         FixgrpT ItemFT ->
           Item <$>
           v .: "name"  <*>
           v .: "type"  <*>
           v .: "value"

         _              -> fail "Error, Fixgroup : case undefined !"

  parseJSON _          = fail "Error, Fixgroup : undefined !"


data Type = Arrow  { leftArrow :: Type
                   , rightLeft :: Type
                   }

          | Glob   { nameGlob :: String
                   , argsGlob :: [Type]
                   }

          | Varidx { nameVaridx :: Integer
                   }

          deriving (Show)

instance FromJSON Type where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         TypeT ArrowTT  ->
           Arrow <$>
           v .: "left"  <*>
           v .: "right"

         TypeT GlobTT   ->
           Glob <$>
           v .: "name"  <*>
           v .: "args"

         TypeT VaridxTT ->
           Varidx <$>
           v .: "name"

         _              -> fail "Error, Type : case undefined !"

  parseJSON _          = fail "Error, Type : undefined !"

data Expr = Lambda       { argnamesLambda :: [String]
                         , bodyLambda     :: Expr
                         }

          | Apply        { funcApply :: Expr
                         , argsApply :: [Expr]
                         }

          | Global       { nameGlobal :: String
                         }

          | ConstructorE { nameConstructorE :: String
                         , argsConstructorE :: [Expr]
                         }

          | Rel          { nameRel :: String
                         }

          | Case         { exprCase  :: Expr
                         , casesCase :: [Case]
                         }
          | Let          { nameLet    :: String
                         , namevalLet :: Expr
                         , bodyLet    :: Expr
                         }
          deriving (Show)

instance FromJSON Expr where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         ExprT LambdaET      ->
           Lambda <$>
           v .: "argnames"  <*>
           v .: "body"

         ExprT ApplyET       ->
           Apply <$>
           v .: "func"  <*>
           v .: "args"

         ExprT GlobalET      ->
           Global <$>
           v .: "name"

         ExprT ConstructorET ->
           ConstructorE <$>
           v .: "name"  <*>
           v .: "args"

         ExprT RelET         ->
           Rel <$>
           v .: "name"

         ExprT CaseET        ->
           Case <$>
           v .: "expr"  <*>
           v .: "cases"

         ExprT LetET         ->
           Let <$>
           v .: "name"    <*>
           v .: "nameval" <*>
           v .: "body"

         _                   -> fail "Error, Expr : case undefined !"


  parseJSON _          = fail "Error, Expr : undefined !"

data Case = C { patC  :: Pat
              , bodyC :: Expr
              }
            deriving (Show)

instance FromJSON Case where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         CaseT ->
           C <$>
           v .: "pat"  <*>
           v .: "body"

         _     -> fail "Error, Case : case undefined !"

  parseJSON _          = fail "Error, Case : undefined !"

data Pat = ConstructorP { nameConstructorP     :: String
                        , argnamesConstructorP :: [String]
                        }
         | WildP
           deriving (Show)

instance FromJSON Pat where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         PatT ConstructorPT ->
           ConstructorP <$>
           v .: "name"  <*>
           v .: "argnames"
         PatT WildPT        -> return WildP

         _                  -> fail "Error, Pat : case undefined !"

  parseJSON _          = fail "Error, Pat : undefined !"

data Module = Mod { nameMod         :: String
                  , needMagicMod    :: Bool
                  , needDummyMod    :: Bool
                  , usedModulesMod  :: [ModuleUsed]
                  , declarationsMod :: [Decl]
                  }
              deriving (Show)
type ModuleUsed = String
type FileName   = String

instance FromJSON Module where
  parseJSON (Object v) =
    do what <- toWhatType <$> v .: "what"
       case what of
         ModuleT ->
           Mod <$>
           v .: "name"         <*>
           v .: "need_magic"   <*>
           v .: "need_dummy"   <*>
           v .: "used_modules" <*>
           v .: "declarations"

         _       -> fail "Error, Module : case undefined !"

  parseJSON _          = fail "Error, Module : undefined !"


data ASTCoq = ModuleAST Module
            | TypeAST Type
            | ExprAST Expr
            | CaseAST Case
            | DeclAST Decl
            | FixgroupAST Fixgroup
            | PatAST Pat


--------------------- Monad MyEither  -----------------------------------
-- Définition du type Monadique, représente le resultat de transformation.
-- Cela marche comme la monade Maybe. La différence vient du fait que lorsque l'on echoue
-- on peut l'associé à un message d'erreur.
-- Pour ce faire on definie un nouveau type, qui est un wrapper sur le type Either
newtype MyEither a = MyEith (Either String a)
  deriving (Show)

instance Functor MyEither where
  fmap f (MyEith me) = case me of
      Left l  -> MyEith $ Left l
      Right r -> MyEith $ Right (f r)

instance Applicative MyEither where
  pure  = MyEith . Right
  (MyEith f) <*> (MyEith (Right r)) = case f of
    Left l -> MyEith $ Left l
    Right f' -> MyEith $ Right (f' r)
  _ <*> (MyEith (Left l))           = MyEith $ Left l

instance Monad MyEither where
  return = pure
  fail = MyEith . Left
  (MyEith me) >>= f = case me of
    Left l  -> MyEith $ Left l
    Right r -> f r

-- Fonction qui permet de transformer une monade Maybe en une monade MyEither
-- @param strErr : Représente le message d'erreur, si la monade est dans un etat d'echec
-- @param maybe  : Représente la monade Maybe qui sera convertit
-- @return : On retourne une entité MyEither construite à partir des param fixé
toMyEith :: String -> Maybe a -> MyEither a
toMyEith strErr Nothing  = fail strErr
toMyEith _      (Just x) = return x

-- Même principe que pour la fonction toMyEith, à la différence que le message d'erreur n'est pas renseigné
toMyEith' :: Maybe a -> MyEither a
toMyEith' = toMyEith ""

-- Getteur, permet d'extraire du type MyEither l'objet de type either
myEither :: MyEither a -> Either String a
myEither (MyEith x) = x

-- Cette fonction permet d'ajouter un message d'erreur si le contexte générale est dans un etat d'echec
-- @param trace : Message d'erreur à ajouter
-- @param myEith : Représente le contexte, si l'etat est dans un mode echec, on ajoute un message d'erreur
-- @return : Retourne le contexte, avec potentiellement l'ajout d'un nouveau message
addTraceMyEith :: String -> MyEither b -> MyEither b
addTraceMyEith trace (MyEith (Left x)) = fail $ trace ++ "\n" ++ x
addTraceMyEith _  x                    = x

-- Cet opérateur permet de combiner different contexte. La combinaison ce fait comme un 'ou'.
-- Si le contexte de gauche echoue, on renvoie le contexte de droite, et inversemement.
infixr 2 |||
(|||) :: MyEither a -> MyEither a -> MyEither a
(MyEith (Left err)) ||| y = addTraceMyEith err y
x ||| _       = x

------------------- Module ASTC -----------------------------------------
-- Définition des différente entité correspondant à l'arbre syntaxique du sous-ensemble C
data FunctionC = FunctionC { prototypeFun :: PrototypeC
                           , bodyFun      :: [InstructionC]
                           }
               deriving (Show, Eq)

data PrototypeC = PrototypeC { nameProto       :: String
                             , returnTypeProto :: TypeC
                             , argsProto       :: [DeclVarC]
                             }
                deriving (Show, Eq)


data InstructionC = CSC            { cSC :: ControlStructC
                                   }
                  | ExprC          { exprC :: ExprC
                                   }

                  | DeclVarC       { declVarC :: DeclVarC
                                   }

                  | ReturnC        { returnC :: ExprC
                                   }
                  deriving (Show, Eq)

data ControlStructC = Iter   { iterC :: IterativeStructC
                             }

                    | Cond   { condC :: ConditionalStructC
                             }
                    deriving (Show, Eq)

data IterativeStructC = WhileC { conditionWhile :: ExprC
                               , bodyWhile      :: [InstructionC]
                               }
                      deriving (Show, Eq)

data ConditionalStructC = IfThenElseC { conditionIf :: ExprC
                                      , bodyThen    :: [InstructionC]
                                      , bodyElse    :: [InstructionC]
                                      }

                        deriving (Show, Eq)

data ExprC = ApplyFunC   { funApplyFun  :: String
                         , argsApplyFun :: [ExprC]
                         }
           | BinOp       { leftBO      :: ExprC
                         , symboleBO   :: String
                         , rightBO     :: ExprC
                         }

           | VarC        { varC :: VarC
                         }

           | ValC        { valC :: ValC
                         }
           deriving (Show, Eq)


data DeclC = VarCDecl     { varCDecl :: DeclVarC
                          }
           | FunDecl      { funDecl :: FunctionC
                          }
           | ObjectDecl   { objectDecl :: DeclObjectC
                          }
           | ProtoDecl    { protoDecl :: PrototypeC
                          }
           | CommDecl     { commDecl :: CommentaryC
                          }
           deriving (Show, Eq)



data DeclVarC = VarEmptyDecl { lvalueEDecl :: VarC
                             }
              | VarDecl      { lvalueDecl :: VarC
                             , rvalueDecl :: ExprC
                             }

              deriving (Show, Eq)

data DeclObjectC = StructDecl { nameStructDecl :: String
                              , bodyStructDecl :: [DeclVarC]
                              }

                 | EnumDecl { nameEnumDecl :: String
                            , bodyEnumDecl :: [String]
                            }
                 deriving (Show, Eq)

data ModuleC = ModuleC FileName FileH FileC
             deriving (Show, Eq)

data FileC = FileC { includeFC :: [IncludeC]
                   , bodyFC    :: [DeclC]
                   }
           deriving (Show, Eq)

type FileH = FileC

data IncludeC = GloIncC String
              | LocIncC String
              deriving (Show, Eq, Read)

data TypeC = TypeC String
           | TypeCFun [TypeC]
           deriving (Show, Eq)


type CommentaryC      = String
type DataC            = String
type NameC            = String
type MacroIncBegin    = String
type MacroIncEnd      = String

type VarC  = (NameC, TypeC)
type ValC  = (DataC, TypeC)

----------- Definition du type Parser ----------------------
data ParseError = PE_NOT_IF_THEN_ELSE
type Parser a b = (a -> MyEither b)

typeVoid :: TypeC
typeVoid = TypeC "void"

bodyLambdaM :: Expr -> MyEither Expr
bodyLambdaM (Lambda _ b) = (MyEith . Right) b
bodyLambdaM _            = fail "bodyLambdaM, fail : "

lvalueOfDeclV :: DeclVarC -> VarC
lvalueOfDeclV (VarEmptyDecl v) = v
lvalueOfDeclV (VarDecl v _)    = v

isDeclFun :: DeclC -> Bool
isDeclFun (FunDecl _) = True
isDeclFun _           = False

isDeclObjectC :: DeclC -> Bool
isDeclObjectC (ObjectDecl _) = True
isDeclObjectC _              = False

isTypeCFun :: TypeC -> Bool
isTypeCFun (TypeCFun _) = True
isTypeCFun _            = False

typeRetTypeC :: TypeC -> TypeC
typeRetTypeC (TypeCFun t) = last t
typeRetTypeC t@TypeC{}    = t

elmTypeC :: TypeC -> [TypeC]
elmTypeC (TypeCFun t) = t
elmTypeC t@TypeC{}    = [t]

prototypeOfDeclC :: [DeclC] -> [PrototypeC]
prototypeOfDeclC ldecl = map (prototypeFun . funDecl)  lfun
  where lfun = filter isDeclFun ldecl

objectCOfDeclC :: [DeclC] -> [DeclObjectC]
objectCOfDeclC ldecl = map objectDecl (filter isDeclObjectC ldecl)

clearPref :: NameCoq -> NameC
clearPref = last . (splitOn ".")

addDeclCToFileC :: FileC -> [DeclC] -> FileH
addDeclCToFileC (FileC incC's declC'ss) declC's = FileC incC's (declC's ++ declC'ss)

addDeclCToModCFileH :: ModuleC -> [DeclC] -> ModuleC
addDeclCToModCFileH (ModuleC n fH fC) declC's = ModuleC n (addDeclCToFileC fH declC's) fC
------------- Module Env -----------------------------------
-- Ce module permet de definir les structures modélisant l'environnement du traducteur.
-- Definition des synonymes de types utilisés pour définir l'environnement
type NameCoqType      = String
type NameCoqFun       = String
type NameCoqConstruct = String
type SymbolBinOp      = String
type NameCFun         = String
type NameCoq          = String
-------------------------------------------------------------------------------
-- Cette objet represente les differente formes que l'on peux prendre dans l'environnement utilisateur
-- Forme NewTypeEUI :
---- Cela représente ...

-- Forme NewFunEUI
---- Cela représente ...

-- Forme NewVarEUI
---- Cela représente ...

-- Forme OpBinEUI
---- Cela représente ...

-- Forme IncludeListEUI
---- Cela représente ...

data EnvUserItem = NewTypeEUI { nameCoqNNEUI :: NameCoq
                              , pTypeNNEUI   :: EnvG -> Parser (Either Type Expr) TypeC
                              , pValCNNEUI   :: [(NameCoqConstruct, NameCoq -> EnvG -> Parser Expr ExprC)]
                              }

                 | NewFunEUI { nameCoqNNEUI :: NameCoq
                             , nameCNNEUI   :: NameC
                             , pTypeNNEUI   :: EnvG -> Parser (Either Type Expr) TypeC
                             , nArgsNNEUI   :: Int
                             }

                 | NewVarEUI { nameCoqNVEUI :: NameCoq
                             , newDeclVarC  :: DeclVarC
                             }
                 | OpBinEUI   { nameCoqOBEUI :: NameCoq
                              , nameCOBEUI   :: SymbolBinOp
                              , pTypeOBEUI   :: EnvG -> Parser (Either Type Expr) TypeC
                              }

                 | IncludeListEUI { nameModuleEUI  :: ModuleUsed
                                  , includeListEUI :: [(NameCoq, [IncludeC])]
                                  }

data EnvArgs = EnvArgs { importOpt  :: [String]
                       , gloIncOpt  :: [String]
                       , locIncOpt  :: [String]
                       , headerOpt  :: Maybe String
                       , outputOpt  :: Maybe String
                       , outputHOpt :: Maybe String
                       , inputOpt   :: String
                       }

data EnvTypeItem = ETI { idET :: String, funET :: EnvG -> Parser (Either Type Expr) TypeC }
instance Eq EnvTypeItem where
  (ETI id' _) == (ETI id'' _) = id' == id''

instance Show EnvTypeItem where
  show (ETI id' _) = id'

type    EnvVarC       = [DeclVarC]
type    EnvType       = [EnvTypeItem]
type    EnvChangeName = [(NameCoqFun, NameCFun)]
newtype EnvValC       = EVC { evc :: [(NameCoqConstruct, EnvG -> Parser Expr ExprC)]}
newtype EnvFunApply   = EFA [EnvG -> Parser Expr ExprC]
type    EnvG          = (EnvVarC, EnvType, EnvValC, EnvFunApply, EnvChangeName)
type    EnvUser       = [EnvUserItem]

instance Show EnvValC where
  show (EVC l) = show $ map fst l

----------------------------------------------
type EnvIncG     = [(String, [IncludeC])]
type EnvBinOp    = [(NameCoqFun, SymbolBinOp)]
newtype EnvCondC = EC [EnvG -> ExprC -> Parser Pat ([DeclVarC], Maybe ExprC)]
----------------------------------------------
type ExprValC = ExprC

lookupEnvV :: Expr -> EnvG -> MyEither (Parser Expr ExprValC)
lookupEnvV (ConstructorE k _) env@(_,_,ev,_,_) = toMyEith "lookupEnvV undefined, constructor case" $ lookup (clearPref k) (evc ev) <*> return env
lookupEnvV _ _                               = fail "lookupEnvV undefined"

lookupEnvT :: String -> EnvG -> MyEither (Parser (Either Type Expr) TypeC)
lookupEnvT k env@(_,et,_,_,_) = toMyEith ("lookupEnvT undefined, key : " ++ (clearPref k)) $ lookup (clearPref k) $ map (\(ETI k' f )-> (k', f env)) et

lookupEnvVar :: NameCoq -> EnvG -> MyEither TypeC
lookupEnvVar name (ev,_,_,_,_) = toMyEith "lookupEnvVar undefined" $  lookup (clearPref name) (map lvalueOfDeclV ev)

lookupEnvN :: NameCoq -> EnvG -> MyEither NameC
lookupEnvN nCoq (_,_,_,_,en) = toMyEith "lookupEnvN undefined" $ lookup (clearPref nCoq) en

lookupEnvBinOp :: NameCoq -> EnvBinOp -> MyEither NameC
lookupEnvBinOp k envbop = toMyEith "lookupEnvBinOp undefined" $ lookup (clearPref k) envbop

lookupEnvInc :: ModuleUsed -> ModuleUsed -> EnvUser -> MyEither [IncludeC]
lookupEnvInc mu minc eu =  toMyEith "lookupEnvInc undefined" body
  where inclL   = filter (\x -> case x of {IncludeListEUI{} -> True; _ -> False}) eu
        itemInc = find ((mu ==) . nameModuleEUI) inclL
        body    = do einc <- includeListEUI <$> itemInc
                     lookup minc einc

lookupEnvIncG :: ModuleUsed -> EnvIncG -> MyEither [IncludeC]
lookupEnvIncG minc einc =  toMyEith "lookupEnvInc undefined" $ lookup minc einc

-- lookupReverseEnvN :: NameC -> EnvG -> MyEither NameCoq
-- lookupReverseEnvN nC (_,_,_,_,en) = toMyEith "lookupReverseEnvN undefined" $ lookup (clearPref nC) $ map (\(a,b) -> (b,a)) en
-- existNameCEnvN :: NameC -> EnvG -> Bool
-- existNameCEnvN nC env = (isRight . myEither) . lookupReverseEnvN nC env

runParserEnvT :: String -> EnvG -> Parser (Either Type Expr) TypeC
runParserEnvT k env eith = join $ (\f -> f eith) <$> lookupEnvT k env

runParserEnvV :: EnvG -> Parser Expr ExprValC
runParserEnvV env expr = join $ (\f -> f expr) <$> lookupEnvV expr env

runParserEnvFApply :: EnvG -> Parser Expr ExprC
runParserEnvFApply env@(_,_,_,EFA efa,_) c = foldr (\f fs -> f env c ||| fs) (fail "runParserEnvFApply undefined") efa

runParserEnvCondC :: EnvG -> EnvCondC -> ExprC -> Parser Pat ([DeclVarC], Maybe ExprC)
runParserEnvCondC env (EC envc) expr pat = foldr (\f fs -> f env expr pat ||| fs) (fail "runParserEnvCondC undefined") envc

retConstPType :: String -> TypeC -> EnvTypeItem
retConstPType nameCoq typeC = ETI nameCoq (\_ _ -> return typeC)

retValC :: DataC -> NameCoq -> Expr -> EnvG -> MyEither ExprC
retValC dataC nC c@(ConstructorE _ args) env = ValC . (,) dataC <$> pCaseIfArgs
  where pCaseIfArgs = bool (parseExprOfTypeC env c)  -- case False
                           (parseTypeC env (Glob nC [])) $ null args
retValC _ _ _ _                              = fail "retValC undefined"

mergeEnvG :: EnvG -> EnvG -> EnvG
mergeEnvG (ev, et, ec, ef, en) (ev', et', _, _, _) = (ev' `union` ev, et `union` et', ec, ef, en)

addVarsCEnvG :: EnvG -> EnvVarC -> EnvG
addVarsCEnvG (v, t, ec, ef, en) e = (e ++ v, t, ec, ef, en)

addTypeCEnvT :: EnvG -> EnvType -> EnvG
addTypeCEnvT (v, t, ec, ef, en) e = (v, t `union` e, ec, ef, en)

envTypeG :: EnvG -> EnvType
envTypeG (_, envt, _, _, _) = envt
----------------------

envUser :: EnvUser
envUser = [ NewTypeEUI "bool"    pBoolC      [("Coq_true", pTrueValC), ("Coq_false", pFalseValC)]
          , NewTypeEUI "unit"    pVoidC      [("Coq_tt", pVoidValC)]
          , NewTypeEUI "nat"     pUnsignedC  [("O", pOValC), ("S", pSValC)]
          , NewTypeEUI "coq_LLI" pLLIOfTypeC []
          , NewTypeEUI "index"   pUint32C    []
          , NewTypeEUI "page"    pUintPtrC   []
          , NewTypeEUI "vaddr"   pUintPtrC   []
          , NewTypeEUI "level"   pUint32C    []
          , NewTypeEUI "count"   pUint32C    []

          , OpBinEUI "orb"      "||" pBoolC
          , OpBinEUI "eqb"      "==" pBoolC
          , OpBinEUI "andb"     "&&" pBoolC
          --, OpBinEUI "sub"      "-"  pUnsignedC
          , OpBinEUI "gtbLevel" ">"  pBoolC
          , OpBinEUI "gtbIndex" ">"  pBoolC
          , OpBinEUI "ltbIndex" "<"  pBoolC
          , OpBinEUI "gebIndex" ">=" pBoolC
          , OpBinEUI "lebIndex" "<=" pBoolC
          , OpBinEUI "indexEq"  "==" pBoolC
          , OpBinEUI "levelEq"  "==" pBoolC
          , OpBinEUI "gebCount" ">=" pBoolC


          --, NewFunEUI "coq_Kidx"          "Kidx"                         pUint32C      0
          , NewFunEUI "negb"              "!"                            pBoolC        1
          , NewFunEUI "ret"               ""                             pRetOfTypeC   1
          , NewFunEUI "i"                 ""                             pIOfTypeC     1

          ----------------------------- Mal --------------------------------------------

          , NewFunEUI "nb_level"          "nb_level"                     pUnsignedC    0
          , NewFunEUI "table_size"        "table_size"                   pUnsignedC    0
          , NewFunEUI "fstLevel"          "fstLevel"                     pUint32C      0
          {-
          , NewFunEUI "writePhyEntry"     "writePhysicalWithLotsOfFlags" pVoidC        8
          , NewFunEUI "writeVirEntry"     "writePhysical"                pVoidC        3
          , NewFunEUI "readPhyEntry"      "readPhysical"                 pUintPtrC     2
          , NewFunEUI "readVirEntry"      "readPhysical"                 pUintPtrC     2
          , NewFunEUI "getNbLevel"        "getNbIndex"                   pUint32C      0
          , NewFunEUI "getIndexOfAddr"    "getIndexOfAddr"               pUint32C      2
          , NewFunEUI "readPhysical"      "readPhysical"                 pUintPtrC     2
          , NewFunEUI "writePhysical"     "writePhysical"                pVoidC        3
          , NewFunEUI "readVirtual"       "readPhysical"                 pUintPtrC     2
          , NewFunEUI "writeVirtual"      "writePhysical"                pVoidC        3
          , NewFunEUI "fetchVirtual"      "readVirtual"                  pUintPtrC     2
          , NewFunEUI "storeVirtual"      "writeVirtual"                 pVoidC        3
          , NewFunEUI "readIndex"         "readIndex"                    pUint32C      2
          , NewFunEUI "writeIndex"        "writeIndex"                   pVoidC        3
          , NewFunEUI "readPresent"       "readPresent"                  pBoolC        2
          , NewFunEUI "writePresent"      "writePresent"                 pVoidC        3
          , NewFunEUI "readAccessible"    "readAccessible"               pBoolC        2
          , NewFunEUI "writeAccessible"   "writeAccessible"              pVoidC        3
          , NewFunEUI "derivated"         "derivated"                    pBoolC        2
          , NewFunEUI "writePDflag"       "writePDflag"                  pVoidC        3
          , NewFunEUI "readPDflag"        "readPDflag"                   pBoolC        2
          , NewFunEUI "getCurPartition"   "get_current_pd"               pUintPtrC     0
          , NewFunEUI "defaultPhysical"   "defaultAddr"                  pUintPtrC     0
          , NewFunEUI "defaultVirtual"    "defaultAddr"                  pUintPtrC     0
          , NewFunEUI "getMaxIndex"       "getMaxIndex"                  pUint32C      0
          , NewFunEUI "addressEqualsPhy"  "addressEquals"                pBoolC        2
          , NewFunEUI "addressEqualsVir"  "addressEquals"                pBoolC        2
          , NewFunEUI "getIndex"          "toAddr"                       pUint32C      1
          , NewFunEUI "checkRights"       "checkRights"                  pBoolC        3
          , NewFunEUI "levelPred"         "sub"                          pUint32C      1
          , NewFunEUI "levelSucc"         "inc"                          pUint32C      1
          , NewFunEUI "indexZero"         "indexZero"                    pUint32C      0
          , NewFunEUI "indexKernel"       "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexPR"           "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexPD"           "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexSh1"          "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexSh2"          "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexSh3"          "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexPRP"          "kernelIndex"                  pUint32C      0
          , NewFunEUI "indexPred"         "sub"                          pUint32C      1
          , NewFunEUI "indexSucc"         "inc"                          pUint32C      1
          , NewFunEUI "levelToCountProd3" "levelToCountProd3"            pUint32C      1
          , NewFunEUI "countZero"         "indexZero"                    pUint32C      0
          , NewFunEUI "countSucc"         "inc"                          pUint32C      1 -}
          ]
  where pBoolC _     = const $ return (TypeC "uint32_t")
        pUintPtrC _  = const $ return (TypeC "uintptr_t")
        pUint32C  _  = const $ return (TypeC "uint32_t")
        pVoidC _     = const $ return (TypeC "void")
        pUnsignedC _ = const $ return (TypeC "unsigned")
        pUndefTypeC _ _ = fail ""
        pRetOfTypeC env x = case x of
          Right (Apply _ [arg]) -> parseExprOfTypeC env arg
          _                     -> fail "pRetOfTypeC undefined"
        pIOfTypeC env x = case x of
          Right (Apply _ [arg]) -> parseExprOfTypeC env arg
          _                     -> fail "pIOfTypeC undefined"
        pLLIOfTypeC env x = case x of
          Left (Glob _ [arg]) -> parseTypeC env arg
          _                   -> fail "pLLIOfTypeC undefined"
------------------------ Parser ValC ----------------------------------------------------
        pTrueValC nt env c@(ConstructorE _ []) = retValC "1" nt c env
        pTrueValC _ _ _                        = fail "pTrueValC undefined"
        pFalseValC nt env c@(ConstructorE _ []) = retValC "0" nt c env
        pFalseValC _ _ _                        = fail "pFalseValC undefined"
        pVoidValC nt env c@(ConstructorE _ []) = retValC "" nt c env
        pVoidValC _ _ _                        = fail "pVoidValC undefined"
        pOValC nt env c@(ConstructorE _ [])    = retValC "0" nt c env
        pOValC _ _ _    = fail "pOValC undefined"
        pSValC nt env (ConstructorE _ [p]) =
          do res <- parseExprC env p
             case res of
               ValC (d, t) -> return $ ValC (show $ 1 + (read d :: Int), t)
               expr        -> do tC <- parseTypeC env tCoq
                                 return $ BinOp (ValC ("1", tC)) "+" expr
             where tCoq = Glob nt []
        pSValC _ _ _                      = fail "pSValC undefined"


envFunApply :: EnvUser ->  EnvFunApply
envFunApply eu = EFA [ pRet
                     , pI
                     , parseOpBinOPC (envBinOp eu)
                     ]
  where pRet env (Apply (Global "Hardware.ret") [expr]) = parseExprC env expr
        pRet _ _                                        = fail "pRet undefined"
        pI env (Apply (Global "Parameters.i") [expr]) = parseExprC env expr
        pI _ _                                        = fail "pI undefined"

envChangeName :: EnvUser -> EnvChangeName
envChangeName = foldr (\x y -> case x of { NewFunEUI nCoq nC _ _ -> [(nCoq, nC)]; _ -> []} ++ y) []

envBinOp :: EnvUser -> EnvBinOp
envBinOp = foldr (\x y -> case x of { OpBinEUI nCoq symb _ -> [(nCoq, symb)]; _ -> []} ++ y) []

envCondC :: EnvCondC
envCondC = EC [pPatOExprC, pPatTrueExprC, pPatFalseExprC, pPatSExprC]

envIncG :: ModuleUsed -> EnvUser -> MyEither EnvIncG
envIncG mu eu = toMyEith "envIncG construct undefined" body
    where inclL = filter (\x -> case x of {IncludeListEUI{} -> True; _ -> False}) eu
          body = includeListEUI <$> find ((mu ==) . nameModuleEUI) inclL

envType :: EnvUser -> EnvType
envType = foldr (\x y -> case x of
                   NewTypeEUI tCoq tC ev                 -> ETI tCoq tC : foldr (\(tCoq',_) y' -> ETI tCoq' tC : y') [] ev
                   OpBinEUI nCoq _ tC                    -> [ETI nCoq tC]
                   NewFunEUI nCoq _ tC n                 -> [ETI nCoq (f n tC)] -- TypeCFun tC
                   NewVarEUI _ _                         -> []
                   _                                     -> []
                   --NewVarEUI nCoq (VarEmptyDecl (_, tC)) -> [retConstPType nCoq tC]
                   --NewVarEUI nCoq (VarDecl (_, tC) _)    -> [retConstPType nCoq tC]
                ++ y) []
    where f :: Int -> (EnvG -> Parser (Either Type Expr) TypeC) -> (EnvG -> Parser (Either Type Expr) TypeC)
          f n p env e = TypeCFun . (replicate n (TypeC "") ++) . (:[]) <$> p env e

envValC :: EnvUser -> EnvValC
envValC = EVC . foldr (\x y -> case x of {NewTypeEUI nt _ ev -> map (\(n, f) -> (n, f nt) ) ev; _ -> []} ++ y) []

envVarC :: EnvUser -> EnvVarC
envVarC _ = []

envArgs :: OA.Parser EnvArgs
envArgs = EnvArgs <$> imports <*> gloincs <*> locincs <*> header <*> output <*> outputH <*> input
    where input   = strArgument $  metavar "INPUT.json"
                                <> help "Coq module (extracted as JSON) to compile into C code"
          imports = many $ strOption $
                         short   'm'
                      <> long    "module"
                      <> metavar "MODULE.json"
                      <> help    "load function names and types from the given Coq MODULE (extracted as JSON)"
          gloincs = many $ strOption $
                         short   'i'
                      <> long    "include"
                      <> metavar "INCL.h"
                      <> help    "add #include directive for this (global) header file"
          locincs = many $ strOption $
                         short   'I'
                      <> long    "locinclude"
                      <> metavar "INCL.h"
                      <> help    "add #include directive for this local header file"
          header  = optional $ strOption $
                         long    "header"
                      <> metavar "FILE"
                      <> help    "add the content of FILE to the header of the generated C file"
          output  = optional $ strOption $
                         short   'o'
                      <> long    "output"
                      <> metavar "FILE.c"
                      <> help    "output extracted C code into FILE.c (defaults to INPUT.c)"
          outputH = optional $ strOption $
                         short   'O'
                      <> long    "output-h"
                      <> metavar "FILE.h"
                      <> help    "output extracted C code headers into FILE.h"

envG :: EnvUser -> EnvG
envG eu = (envVarC eu, envType eu, envValC eu, envFunApply eu, envChangeName eu)

------------------------------------------

parseMacroTypeC :: Parser Type TypeC
parseMacroTypeC type' = TypeC <$> f type'
  where f :: Parser Type String
        f t = case t of
          Glob n []      -> parseNameC n
          Glob n (p:ps)  -> do nm <- parseNameC n
                               p1 <- f p
                               pn <- concatMapM (\p' -> (", "++) <$> f p') ps
                               return $ nm ++ "(" ++ p1 ++ pn ++ ")"
          _              -> fail "parseMacroTypeC undefined"



parseError :: ParseError -> String
parseError PE_NOT_IF_THEN_ELSE = "The pattern matching is not possible to parse in the conditionnal struct"

parseOpBinOPC :: EnvBinOp -> EnvG -> Parser Expr ExprC
parseOpBinOPC envb env (Apply (Global op) args) = if length args /= 2
                                                  then fail "parseOpBinOPC : Arguments > 2"
                                                  else do yop <- lookupEnvBinOp op envb
                                                          le  <- parseExprC env (head args)
                                                          re  <- parseExprC env (args !! 1)
                                                          return $ BinOp le yop re
parseOpBinOPC _ _  _                            = fail "parseOpBinOPC undefined"


parseTypeC :: EnvG -> Parser Type TypeC
parseTypeC env tp = do r <- pType tp
                       return (if length r == 1
                               then head r
                               else TypeCFun r
                              )
  where pType :: Parser Type [TypeC]
        pType t = case t of
          Varidx{}          -> fail "Exception, error variable type"
          Glob n _          -> (:[]) <$> (runParserEnvT n env (Left t) ||| parseMacroTypeC t)
          Arrow l@Arrow{} r -> (:) <$> (TypeCFun <$> pType l) <*> pType r
          Arrow l r         -> (++) <$> pType l <*> pType r


parseNameC :: Parser NameCoq NameC
parseNameC nameCoq = if nameC == nameCoq'
                       then return nameC
                       else fail $ "parseNameC fail, the identifier is not correct : " ++ nameCoq
  where nameCoq' = clearPref nameCoq
        pattern' = "[a-zA-Z_](_?[a-zA-Z0-9]+)*" :: String
        nameC    = (nameCoq' =~ pattern') :: String


parseExprOfTypeC :: EnvG -> Parser Expr TypeC
parseExprOfTypeC env expr = case expr of
  Rel name               -> lookupEnvVar name env                                 -- Regarde dans l'environnement des variables locales
  Global name            -> typeRetTypeC <$> runParserEnvT name env (Right expr)  -- Regarde dans l'environnement des types
  Apply (Global fname) _ -> typeRetTypeC <$> runParserEnvT fname env (Right expr) -- idem
  Apply (Rel fname) _    -> typeRetTypeC <$> lookupEnvVar fname env               -- idem
  ConstructorE name _    -> runParserEnvT name env (Right expr)                   -- idem
  _                      -> fail "parseExprOfTypeC undefined"                     -- Echoue en renvoyant Nothing

parseDeclVarC :: EnvG -> Parser (Either Decl Expr) (EnvG, DeclVarC)
parseDeclVarC env decl = case decl of
    Left (Term n t@Glob{} (Lambda _ e@ConstructorE{})) -> do typeV <- parseTypeC env t
                                                             expr  <- parseExprC env e
                                                             n'    <- parseNameC n
                                                             let var = (n', typeV)
                                                             return ( addTypeCEnvT env [retConstPType n' typeV]
                                                                    , VarDecl var expr
                                                                    )
    Right (Let n expr _)                               -> do rvalue   <- parseExprC env expr
                                                             lvalType <- parseExprOfTypeC env expr
                                                             if lvalType == typeVoid
                                                             then fail "Exception parseDeclVar, the type of rvalue is void"
                                                             else do n' <- parseNameC n
                                                                     let var = VarDecl (n', lvalType) rvalue -- Pas de reference sur la variable
                                                                     return (addVarsCEnvG env [var], var)
    _                            -> fail $ "parseDeclVarC undefined, decl : "  ++ take debugRender (show decl)


parseDeclC :: EnvG -> Parser Decl [DeclC]
parseDeclC e decl = case decl of
  Term{}     -> (:[]) . VarCDecl . snd <$> parseDeclVarC e (Left decl) |||
                (:[]) . FunDecl <$> parseFunctionC e decl
  Fixgroup{} -> map FunDecl <$> parseFunctionCFX e decl

parseDeclH :: EnvG -> Parser Decl (EnvG, [DeclC])
parseDeclH e decl = case decl of
  Term{}     ->  parseDVar ||| parseProtoT
  Fixgroup{} ->  parseProtoTFX e transf
  where parseDVar = do (eg,d) <- parseDeclVarC e (Left decl)
                       let var = VarEmptyDecl (lvalueDecl d)
                       return (addVarsCEnvG eg [var], [VarCDecl var])
        parseProtoT   = parsePrototypeC e (Left decl) >>= (\(eg,p) -> return (eg, [ProtoDecl p]))
        transf        = map Right (fixlistFixgroup decl) :: [Either Decl Fixgroup]
        parseProtoTFX env d = case d of
          []   -> return (env, [])
          x:xs -> do (en1, proto) <- parsePrototypeC env x
                     let envn1 = addTypeCEnvT env (envTypeG en1)
                     (en, protos) <- parseProtoTFX envn1 xs
                     return (addTypeCEnvT envn1 (envTypeG en), ProtoDecl proto:protos)

parseAllDeclC :: EnvG -> Parser [Decl] [DeclC]
parseAllDeclC e = concatMapM (parseDeclC e)

parseAllDeclH :: EnvG -> Parser [Decl] (EnvG, [DeclC])
parseAllDeclH env decl = case decl of
  []   -> return (env, [])
  d:ds -> do (e1, ds1) <- parseDeclH env d
             (en, dsn) <- parseAllDeclH env ds
             return (mergeEnvG e1 en, ds1 ++ dsn)

parsePrototypeC :: EnvG -> Parser (Either Decl Fixgroup) (EnvG, PrototypeC)
parsePrototypeC env term  =
  case term of
    (Left (Term n t v))  -> construct n t v
    (Right (Item n t v)) -> construct n t v
    _                    -> fail "parsePrototypeC undefined"
  where construct n' t' v' = do typeC      <- parseTypeC env t'
                                argsProto' <- args n' (elmTypeC typeC) v'
                                name       <- parseNameC n'
                                let retProto = typeRetTypeC typeC
                                return ( addTypeCEnvT env [retConstPType name typeC]
                                       , PrototypeC name retProto argsProto'
                                       )
        args n' t (Lambda prm _) = if length t - 1 == length prm
                                   then mergeParam t prm
                                   else fail $ "Exception, detection of partiel application (definition) : function " ++ n'
        args _ _ _               = fail "parsePrototypeC - args, undefined"
        mergeParam = zipWithM (\a b -> VarEmptyDecl . flip (,) a <$> parseNameC b)

parseFunctionC :: EnvG -> Parser Decl FunctionC
parseFunctionC env t@Term{} = do prototype    <- snd <$> parsePrototypeC env (Left t)
                                 body         <- bodyLambdaM (valueTerm t)
                                 instructions <- parseInstructionC (addVarsCEnvG env (argsProto prototype)) body
                                 return $ FunctionC prototype instructions
parseFunctionC _ _          = fail "parseFunctionC undefined"


parseFunctionCFX :: EnvG -> Parser Decl [FunctionC]
parseFunctionCFX env (Fixgroup l) = mapM (parseFunctionC env) lterm
  where reformate (Item n t v) = Term n t v
        lterm = map reformate l
parseFunctionCFX _ _              = fail "parseFunctionCFX"


parseInstructionC :: EnvG -> Parser Expr [InstructionC]
parseInstructionC env expr = case expr of
  Rel{}          -> multicase
  Global{}       -> multicase
  ConstructorE{} -> multicase
  Apply{}        -> parseMonadBlockC env expr ||| multicase
  Case{}         -> (:[]) . CSC . Cond <$> parseConditionnalC envCondC env expr
  (Let _ _ body) -> do (newEnv, varLet) <- parseDeclVarC env (Right expr)
                       instructions     <- parseInstructionC newEnv body
                       return $ DeclVarC varLet : instructions
  _              -> fail $ "parseInstructionC undefined " ++ take debugRender (show expr)
  where multicase = do typeC <- parseExprOfTypeC env expr
                       if typeC == typeVoid
                       then (:[]) . ExprC <$> parseExprC env expr
                       else (:[]) . ReturnC <$> parseExprC env expr

parseMonadBlockC :: EnvG -> Parser Expr [InstructionC]
parseMonadBlockC env exprCoq = case exprCoq of
  Apply (Global "Hardware.bind") [expr, Lambda [nameV] ninstr] ->
    do rvalue <- parseExprC env expr
       typeRvalue <- parseExprOfTypeC env expr
       if nameV == "_"
       then (ExprC rvalue:) <$> parseInstructionC env ninstr
       else do nameC <- parseNameC nameV
               let lvalue = (nameC, typeRvalue)
                   newEnv = addVarsCEnvG env [VarEmptyDecl lvalue]
               (DeclVarC (VarDecl lvalue rvalue):) <$> parseInstructionC newEnv ninstr

  _                                                            -> fail "parseMonadBlockC undefined"

parseConditionnalC :: EnvCondC -> EnvG -> Parser Expr ConditionalStructC
parseConditionnalC envC env match = condC . cSC . head <$> if length (casesCase match) == 1 then fail "parseConditionnalC, error : l case < 1" else pC' match
  where pC' :: Parser Expr [InstructionC]
        pC' (Case expr cases) = case cases of
          []            -> return []
          [C pat body]  ->   -- else only
              do exprC'      <- parseExprC env expr
                 (declVM, _) <- runParserEnvCondC env envC exprC' pat
                 pInstr declVM body
          C pat body:cs ->
              do exprC'          <- parseExprC env expr
                 (declVM, exprM) <- runParserEnvCondC env envC exprC' pat
                 exprB           <- toMyEith' exprM
                 bThen           <- pInstr declVM body
                 bElse           <- pC' $ Case expr cs
                 return $ ((:[]) . CSC . Cond) $ IfThenElseC exprB bThen bElse
        pC' _                 = fail "parseConditionnalC undefined"
        pInstr :: [DeclVarC] -> Parser Expr [InstructionC]
        pInstr declV' body' = (++) (map DeclVarC declV') <$> parseInstructionC (addVarsCEnvG env declV') body'

----------  Pattern Conditionnal  -------------------------------
pPatOExprC :: EnvG -> ExprC -> Parser Pat ([DeclVarC], Maybe ExprC)
pPatOExprC _ match (ConstructorP "Datatypes.O" []) = return ([], return $ BinOp match "==" (ValC ("0", TypeC "unsigned")))
pPatOExprC _ _ _                                   = fail "pPatOExprC undefined"

pPatSExprC :: EnvG -> ExprC -> Parser Pat ([DeclVarC], Maybe ExprC)
pPatSExprC _ match (ConstructorP "Datatypes.S" [p]) = do p' <- parseNameC p
                                                         return ([VarDecl (p', TypeC "unsigned") (BinOp match "-" (ValC ("1", TypeC "uint32_t")))], Nothing)
pPatSExprC _ _ _                                    = fail "pPatSExprC undefined"

pPatTrueExprC :: EnvG -> ExprC -> Parser Pat ([DeclVarC], Maybe ExprC)
pPatTrueExprC _ match (ConstructorP "Datatypes.Coq_true" []) = return ([], return match)
pPatTrueExprC _ _ _                                          = fail "pPatTrueExprC undefined"

pPatFalseExprC :: EnvG -> ExprC ->  Parser Pat ([DeclVarC], Maybe ExprC)
pPatFalseExprC _ match (ConstructorP "Datatypes.Coq_false" []) = return ([], return $ BinOp match "==" (ValC ("0", TypeC "uint32_t")))
pPatFalseExprC _ _ _                                           = fail "pPatFalseExprC undefined"
----------------------------------------------------------

parseExprC :: EnvG -> Parser Expr ExprC
parseExprC env expr = case expr of
  Rel{}          -> VarC <$> parseVarC env expr
  Global{}       -> VarC <$> parseVarC env expr ||| parseApplyFunC env expr
  ConstructorE{} -> parseValC env expr
  Apply{}        -> parseApplyFunC env expr
  _              -> fail $ "parseExprC undefined" ++ take (debugRender + 200) (show expr)

parseVarC :: EnvG -> Parser Expr VarC
parseVarC env expr = case expr of
 (Rel var)    -> construct var $ lookupEnvVar var env
 (Global var) -> construct var (lookupEnvVar var env) ||| construct' var
 _            -> fail "parseVarC undefined"
 where construct var getType = do nVar    <- parseNameC var
                                  typeVar <- getType
                                  return (nVar, typeVar)
       construct' var = do let tm = runParserEnvT var env (Right expr)
                           t <- tm
                           if length (elmTypeC t) == 1
                           then fail "parseVarC fail, it's a function"
                           else construct var tm

parseValC :: EnvG -> Parser Expr ExprValC
parseValC env c@ConstructorE{} = runParserEnvV env c
parseValC _ _                  = fail "parseValC undefined"

parseApplyFunC :: EnvG -> Parser Expr ExprC
parseApplyFunC env apply = runParserEnvFApply env apply ||| pApplyFunC
  where pApplyFunC = case apply of
          (Apply fun args) -> do f      <- transformFun fun
                                 a      <- transformArgs args
                                 status <- checkApplyPart f args
                                 if status
                                 then construct f a
                                 else fail $ "Exception, detection of partiel application (application) : function " ++ f
          (Global fun)     -> construct fun []
          _                -> fail "parseApplyFunC undefined"
        transformFun (Global f) = return f
        transformFun (Rel f)    = return f
        transformFun _          = fail "It's not function"
        transformFun' f = lookupEnvN f env ||| parseNameC f
        transformArgs = mapM (parseExprC env)
        checkApplyPart nfun a = do t <- runParserEnvT nfun env (Right apply)
                                   return $ length a == length (elmTypeC t) - 1
        construct f a = do f' <- transformFun' f
                           return $ ApplyFunC f' a

parseFileC :: EnvArgs -> EnvG -> Parser Module FileC
parseFileC eargs eg m = do ldecl <- parseAllDeclC eg (declarationsMod m)
                           return $ FileC includes ldecl
  where includes = map GloIncC (gloIncOpt eargs)
                ++ map LocIncC (locIncOpt eargs)

parseFileH :: EnvArgs -> EnvG -> Parser Module (EnvG, FileH)
parseFileH eargs eg m = do (env, ldecl) <- parseAllDeclH eg (declarationsMod m)
                           return (env, FileC includes ldecl)
  where includes = map GloIncC (gloIncOpt eargs) ++ map LocIncC (locIncOpt eargs)

parseModuleC :: EnvArgs -> EnvG -> Parser Module (EnvG, ModuleC)
parseModuleC eargs env m = do (envH, fileH) <- parseFileH eargs env m
                              fileC         <- parseFileC eargs envH m
                              return (envH, ModuleC nm fileH fileC)
  where nm = nameMod m

parseMacroInc :: FileName -> (MacroIncBegin, MacroIncEnd)
parseMacroInc name = ( "#ifndef " ++ define ++ "\n"
                    ++ "#define " ++ define ++ "\n"
                     , "#endif\n")
  where define = "__" ++ map ((\ x -> if x == '.' then '_' else x) . toUpper) name ++ "_H__"

parseImportC :: EnvG -> Parser Module (EnvG, DeclC)
parseImportC env m = do (nenv, declH's) <- parseAllDeclH env (declarationsMod m)
                        let comDeclH's = concatMap ((++"\n") . showDeclC) declH's
                            header = "************* Expected symbols from " ++ nameMod m ++  " *************\n"
                            footer = replicate (length header) '*'
                        return (nenv, CommDecl $ header ++ comDeclH's ++ footer)

parseAllImportC :: EnvG -> Parser [Module] (EnvG, [DeclC])
parseAllImportC env m's = case m's of
  []   -> return (env, [])
  m:ms -> do (e1, ds1) <- parseImportC env m
             (en, dsn) <- parseAllImportC env ms
             return (mergeEnvG e1 en, ds1 : dsn)

-------------- Show -------------------------------
type Indent = Int

showCommentaryC :: CommentaryC -> String
showCommentaryC commentary = "/*\n" ++ commentary ++ "\n*/"

showIndent :: Indent -> String
showIndent indent = if indent < 0 then "" else replicate indent '\t'


showIncludeC :: IncludeC -> String
showIncludeC inc = case inc of
  (GloIncC name) -> "#include <" ++ name ++ ">" ++ "\n"
  (LocIncC name) -> "#include \"" ++ name ++ "\"" ++ "\n"

showVarC :: Indent -> VarC -> String
showVarC ind (n, _) = showIndent ind ++  n

showValC :: Indent -> ValC -> String
showValC ind (n, _) = showIndent ind ++  n

showExprC :: Indent -> ExprC -> String
showExprC ind (ValC l)                 = showValC ind l
showExprC ind (VarC v)                 = showVarC ind v
showExprC ind (ApplyFunC nameFun args) = showIndent ind ++ nameFun ++ "(" ++ intercalate ", " (map (showExprC 0) args) ++ ")"
showExprC ind (BinOp el symb er)       = showIndent ind ++ "(" ++ showExprC 0 el ++ " " ++ symb ++ " " ++ showExprC 0 er ++ ")"

showTypeC :: NameC -> TypeC -> String
showTypeC _ (TypeC typeC) = typeC
showTypeC n (TypeCFun typeC@(t:ts)) = ret ++ idt ++ args
    where ret = showTypeC "" $ last typeC
          idt = "(*" ++ n ++ ")"
          args = "(" ++ showTypeC "" t ++ foldr (\x y -> ", " ++ showTypeC "" x ++ y) "" (init' ts) ++ ")"
          init' l = if null l then [] else init l
showTypeC _ _                       = error "error, showTypeC"


showPrototypeC :: PrototypeC -> String
showPrototypeC (PrototypeC name ret args) =
  showTypeC name ret ++ " " ++ name ++ "(" ++ intercalate ", " (map (showDeclVarC eDECLVARINPROTOTYPE) args) ++ ")"

showOnlyPrototypeC :: PrototypeC -> String
showOnlyPrototypeC pro = "extern " ++ showPrototypeC pro ++ ";"

eDECLVARINPROTOTYPE :: Int
eDECLVARINPROTOTYPE = -1

showDeclVarC :: Indent -> DeclVarC -> String
showDeclVarC ind decl = case decl of
  (VarEmptyDecl (n, t)) -> let isFun = isTypeCFun t in begin ++ extern ++ identif isFun t n ++ end
  (VarDecl (n, t) expr) -> let isFun = isTypeCFun t in begin ++ constI isFun ++ showTypeC n t ++ " " ++ n ++ " = " ++ showExprC 0 expr ++ end
  where begin  = showIndent ind
        end    = if ind == 0 then ";" else ""
        extern = if ind == 0 then "extern " else ""
        constI isFun' = if isFun' then "" else "const "
        identif isFun' t' n' =  if isFun' then showTypeC n' t' else constI isFun' ++ showTypeC "" t' ++ " " ++ n'


showConditionnalC :: Indent -> ConditionalStructC -> String
showConditionnalC ind (IfThenElseC cond ifB elseB) = showIndent ind ++
  "if (" ++ showExprC 0 cond ++ ") {\n" ++
    concatMap (showInstructionC (ind + 1)) ifB ++ showIndent ind ++
  "} else {\n" ++
    concatMap (showInstructionC (ind + 1)) elseB ++ showIndent ind ++
  "}"

showControlStructC :: Indent -> ControlStructC -> String
showControlStructC ind (Cond struct) = showConditionnalC ind struct
showControlStructC _ (Iter _)      = undefined

showFunctionC :: FunctionC -> String
showFunctionC (FunctionC proto body) =
  showPrototypeC proto ++
  "\n{\n" ++
    concatMap (showInstructionC 1) body ++
  "}"

debugRender :: Int
debugRender = 120

showDeclC :: DeclC -> String
showDeclC decl = case decl of
  VarCDecl var    -> showDeclVarC 0 var
  FunDecl fun     -> showFunctionC fun
  ProtoDecl proto -> showOnlyPrototypeC proto
  CommDecl comm   -> showCommentaryC comm
  _               -> error $ "showDeclC undefined" ++ take debugRender (show decl)


showInstructionC :: Indent -> InstructionC -> String
showInstructionC ind instr =
  case instr of
  (CSC csc)          -> showControlStructC ind csc ++ "\n"
  (ExprC expr)       -> showExprC ind expr ++ ";\n"
  (DeclVarC declVar) -> showDeclVarC ind declVar ++ ";\n"
  (ReturnC expr)     -> showIndent ind ++ "return " ++ showExprC 0 expr ++ ";\n"

showFileC :: FileC -> String
showFileC (FileC incl body) = concatMap showIncludeC incl ++ "\n" ++
                              concatMap (\x ->  showDeclC x ++ "\n\n") body

showFileH :: FileName -> FileH -> String
showFileH name (FileC incl body) =
  let (begin, end) = parseMacroInc name
      includes     = concatMap showIncludeC incl
  in
  begin ++ "\n" ++
  includes ++ (if null includes then "" else "\n") ++
  concatMap (\d -> showDeclC d ++ "\n") body ++ "\n" ++
  end ++ "\n"

type ShowFileC = String
type ShowFileH = String
showModuleC :: ModuleC -> (ShowFileH, ShowFileC)
showModuleC (ModuleC name fileH fileC) =
  ( showFileH name fileH
  , showFileC fileC
  )


------------------- Main Module ----------------------

type ContentFile = String
type ImportFile  = ContentFile

extractor :: EnvArgs -> [ImportFile] -> ContentFile -> MyEither (ModuleUsed, (ShowFileH, ShowFileC))
extractor eargs impFile's contFile = do astImpFile's           <- mapM decodeModule impFile's
                                        astCoqFile             <- decodeModule contFile
                                        (envImp's, declComm's) <- parseAllImportC env astImpFile's
                                        (_, m)                 <- parseModuleC eargs (mergeEnvG env envImp's) astCoqFile
                                        let moduleC = addDeclCToModCFileH m declComm's
                                        return $ (nameMod astCoqFile, showModuleC moduleC)
  where decodeModule content = MyEith $ (eitherDecode (BS.pack content) :: Either String Module)
        env = envG envUser

main :: IO ()
main = do namePrg   <- getProgName
          eargs     <- OA.execParser (opts namePrg)
          fileImp's <- mapM readFile $ importOpt eargs
          fileCoq   <- readFile $ inputOpt eargs
          let (nm, (contentFileH, contentFileC)) = eith . myEither $ extractor eargs fileImp's fileCoq
              output = fromMaybe (nm ++ ".c") (outputOpt eargs)
          writeFile output contentFileC
          case outputHOpt eargs of
            Just fileH -> writeFile fileH contentFileH
            Nothing    -> return ()

  where eith = either error id
        opts nm = OA.info (OA.helper <*> envArgs)
                  ( OA.fullDesc
                    <> OA.progDesc (  "Compile Coq source code written in a specific monadic style "
                                   ++ "(and extracted to its JSON representation by the standard "
                                   ++ "Coq extracting facility) into the corresponding C code" )
                    <> OA.header (nm ++ " - a simple compiler from Coq monadic code into C")
                  )
