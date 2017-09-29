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

module Language.Coq.ExtractedAST where

import           Control.Arrow    (first)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Default
import           Data.List        (elemIndex, find, isPrefixOf)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe, isJust, mapMaybe)
import qualified Data.Set         as Set

-- | Type (and sub-types) of the elements of the CoqAST as named in
-- JSON document
data WhatT = DeclT DeclT
           | FixgrpT FixgrpT
           | TypeT TypeT
           | ExprT ExprT
           | CaseT
           | PatT PatT
           | ModuleT
    deriving (Show,Eq)

data DeclT   = TermDT | FixgrpDT
    deriving (Show,Eq)
data FixgrpT = ItemFT
    deriving (Show,Eq)
data TypeT   = ArrowTT | GlobTT | VaridxTT
    deriving (Show,Eq)
data ExprT   = LambdaET| ApplyET | GlobalET | ConstructorET | RelET | CaseET | LetET
    deriving (Show,Eq)
data PatT    = ConstructorPT | WildPT
    deriving (Show,Eq)

-- | Parse the type of an element in the JSON document
toWhatType :: String -> Parser WhatT
toWhatType strType =
  case strType of
    "case"             -> pure   CaseT
    "module"           -> pure   ModuleT
    "decl:term"        -> pure $ DeclT   TermDT
    "decl:fixgroup"    -> pure $ DeclT   FixgrpDT
    "fixgroup:item"    -> pure $ FixgrpT ItemFT
    "type:arrow"       -> pure $ TypeT   ArrowTT
    "type:glob"        -> pure $ TypeT   GlobTT
    "type:varidx"      -> pure $ TypeT   VaridxTT
    "expr:lambda"      -> pure $ ExprT   LambdaET
    "expr:apply"       -> pure $ ExprT   ApplyET
    "expr:global"      -> pure $ ExprT   GlobalET
    "expr:constructor" -> pure $ ExprT   ConstructorET
    "expr:rel"         -> pure $ ExprT   RelET
    "expr:case"        -> pure $ ExprT   CaseET
    "expr:let"         -> pure $ ExprT   LetET
    "pat:constructor"  -> pure $ PatT    ConstructorPT
    "pat:wild"         -> pure $ PatT    WildPT
    strError           -> fail $ "Error: WhatType \"" ++ strError ++ "\" undefined"

instance FromJSON WhatT where
  parseJSON (Object v) = toWhatType =<< v .: "what"
  parseJSON _          = fail "Error: WhatType undefined"

type Name = String
type FunName = Name
type ArgName = Name

-- | Apply a function on all the names of an AST
class Renamable a where
    rename :: (Name -> Name) -> a -> a

data Decl = Term     { nameTerm  :: Name
                     , typeTerm  :: Type
                     , valueTerm :: Expr
                     }
          | Fixgroup { fixItems  :: [FixItem]
                     }
    deriving (Show,Eq)

instance Renamable Decl where
    rename f (Term n t v)  = Term (f n) (rename f t) (rename f v)
    rename f (Fixgroup gs) = Fixgroup (map (rename f) gs)

instance FromJSON Decl where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      DeclT TermDT   -> Term <$> v .: "name" <*> v .: "type" <*> v .: "value"
      DeclT FixgrpDT -> Fixgroup <$> v .: "fixlist"
      _              -> fail "Error: Decl case undefined"
  parseJSON _        =  fail "Error: Decl kind undefined"

data FixItem = FixItem { nameItem  :: Name
                       , typeItem  :: Type
                       , valueItem :: Expr
                       }
    deriving (Show,Eq)

instance Renamable FixItem where
    rename f (FixItem n t v) = FixItem (f n) (rename f t) (rename f v)

instance FromJSON FixItem where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      FixgrpT ItemFT -> FixItem <$> v .: "name" <*> v .: "type" <*> v .: "value"
      _              -> fail "Error: Fixgroup case undefined"
  parseJSON _        =  fail "Error: Fixgroup kind undefined"

data Type = Arrow  { leftArrow  :: Type
                   , rightArrow :: Type
                   }
          | Glob   { nameGlob :: Name
                   , argsGlob :: [Type]
                   }
          | Varidx { nameVaridx :: Integer
                   }
    deriving (Show,Eq)

instance Renamable Type where
    rename f (Arrow l r) = Arrow (rename f l) (rename f r)
    rename f (Glob n ts) = Glob (f n) (map (rename f) ts)
    rename _          v  = v

instance FromJSON Type where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      TypeT ArrowTT  -> Arrow  <$> v .: "left" <*> v .: "right"
      TypeT GlobTT   -> Glob   <$> v .: "name" <*> v .: "args"
      TypeT VaridxTT -> Varidx <$> v .: "name"
      _              -> fail "Error: Type case undefined"
  parseJSON _        =  fail "Error: Type kind undefined"

data Expr = Lambda       { argnamesLambda :: [Name]
                         , bodyLambda     :: Expr
                         }
          | Apply        { funcApply :: Expr
                         , argsApply :: [Expr]
                         }
          | Global       { nameGlobal :: Name
                         }
          | ConstructorE { nameConstructorE :: Name
                         , argsConstructorE :: [Expr]
                         }
          | Rel          { nameRel :: Name
                         }
          | Case         { exprCase  :: Expr
                         , casesCase :: [Case]
                         }
          | Let          { nameLet    :: Name
                         , namevalLet :: Expr
                         , bodyLet    :: Expr
                         }
    deriving (Show,Eq)

instance Renamable Expr where
    rename f (Lambda ns e)       = Lambda (map f ns) (rename f e)
    rename f (Apply fun as)      = Apply (rename f fun) (map (rename f) as)
    rename f (Global n)          = Global (f n)
    rename f (ConstructorE n as) = ConstructorE (f n) (map (rename f) as)
    rename f (Rel n)             = Rel (f n)
    rename f (Case e cs)         = Case (rename f e) (map (rename f) cs)
    rename f (Let n v b)         = Let (f n) (rename f v) (rename f b)

instance FromJSON Expr where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      ExprT LambdaET      -> Lambda       <$> v .: "argnames" <*> v .: "body"
      ExprT ApplyET       -> Apply        <$> v .: "func" <*> v .: "args"
      ExprT GlobalET      -> Global       <$> v .: "name"
      ExprT ConstructorET -> ConstructorE <$> v .: "name" <*> v .: "args"
      ExprT RelET         -> Rel          <$> v .: "name"
      ExprT CaseET        -> Case         <$> v .: "expr" <*> v .: "cases"
      ExprT LetET         -> Let          <$> v .: "name" <*> v .: "nameval" <*> v .: "body"
      _                   -> fail "Error: Expr case undefined"
  parseJSON _             =  fail "Error: Expr kind undefined"

data Case = C { patC  :: Pat
              , bodyC :: Expr
              }
    deriving (Show,Eq)

instance Renamable Case where
    rename f (C p b) = C (rename f p) (rename f b)

instance FromJSON Case where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      CaseT   -> C <$> v .: "pat" <*> v .: "body"
      _       -> fail "Error: Case case undefined"
  parseJSON _ =  fail "Error: Case kind undefined"

data Pat = ConstructorP { nameConstructorP     :: Name
                        , argnamesConstructorP :: [Name]
                        }
         | WildP
    deriving (Show,Eq)

instance Renamable Pat where
    rename f (ConstructorP n as) = ConstructorP (f n) (map f as)
    rename _                  w  = w

instance FromJSON Pat where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      PatT ConstructorPT -> ConstructorP <$> v .: "name" <*> v .: "argnames"
      PatT WildPT        -> return WildP
      _                  -> fail "Error: Pat case undefined"
  parseJSON _            =  fail "Error: Pat kind undefined"

type ModuleUsed = Name
type FileName   = String
data Module = Mod { nameMod         :: Name
                  , needMagicMod    :: Bool
                  , needDummyMod    :: Bool
                  , usedModulesMod  :: [ModuleUsed]
                  , declarationsMod :: [Decl]
                  }
    deriving (Show,Eq)

instance Renamable Module where
    rename f (Mod n m d mus ds) = Mod (f n) m d (map f mus) (map (rename f) ds)

instance FromJSON Module where
  parseJSON (Object v) = do
    what <- toWhatType =<< v .: "what"
    case what of
      ModuleT -> Mod <$>
        v .: "name"         <*>
        v .: "need_magic"   <*>
        v .: "need_dummy"   <*>
        v .: "used_modules" <*>
        v .: "declarations"
      _       -> fail "Error: Module case undefined"
  parseJSON _ =  fail "Error: Module kind undefined"


-- | == Cleaning up the AST

-- | Drop the full qualification of names in the given 'Renamable' for
-- all the modules of 'modNames'
--
-- >>> unqualify ["Module"] (Global "Module.Test.val")
-- Global {nameGlobal = "Test.val"}
-- >>> unqualify ["Module"] (Global "Module2.val")
-- Global {nameGlobal = "Module2.val"}
-- >>> unqualify ["Module"] (Global "Module.Module.val")
-- Global {nameGlobal = "Module.val"}
-- >>> unqualify ["M"] (Mod "M.Test" False False [] [])
-- Mod {nameMod = "Test", needMagicMod = False, needDummyMod = False, usedModulesMod = [], declarationsMod = []}
-- >>> unqualify ["M"] (Mod "Mo.Test" False False [] [])
-- Mod {nameMod = "Mo.Test", needMagicMod = False, needDummyMod = False, usedModulesMod = [], declarationsMod = []}
--
unqualify :: Renamable r => [ModuleUsed] -> r -> r
unqualify modNames = rename go
    where go n = case find (\m -> (m ++ ".") `isPrefixOf` n) modNames of
                   Just modName -> drop (length modName + 1) n
                   Nothing      -> n

-- | Rename according to the given map
--
-- >>> rewriteNames (Map.fromList [("coq_true", "true")]) (Global "coq_true")
-- Global {nameGlobal = "true"}
-- >>> rewriteNames (Map.fromList [("coq_true", "true")]) (Global "coq_false")
-- Global {nameGlobal = "coq_false"}
rewriteNames :: Renamable r => Map Name Name -> r -> r
rewriteNames nameMap = rename f
    where f n = fromMaybe n $ Map.lookup n nameMap

-- | Replace dots (qualified names) with a given string
--
-- >>> undotify "_" (Global "Module.function")
-- Global {nameGlobal = "Module_function"}
undotify :: Renamable r => String -> r -> r
undotify "."   = id
undotify subst = rename (concatMap f)
    where f '.' = subst
          f  x  = [x]

-- | Clean up the source AST
--
-- Do:
--
-- * drop module names
-- * rewrite names according to the given map
-- * replace dots by the given string
clean :: Renamable r => [ModuleUsed] -> Map Name Name -> String -> r -> r
clean mods rewrts dot = undotify dot . rewriteNames rewrts . unqualify mods

-- | Clean up the source AST of a full module
--
-- Do a standard 'clean' /after/ a first pass to remove the name of
-- the module itself.
-- This is necessary because, when extracting a module @A@, the JSON
-- extractor extracts a definition @d@ as is (not with as @A.d@ but as
-- @d@); but it extracts a definition @d'@ in a submodule @B@ as
-- @A.B.d'@.
cleanModule :: [ModuleUsed] -> Map Name Name -> String -> Module -> Module
cleanModule mods rewrts dot modul = clean mods rewrts dot $ unqualify [nameMod modul] modul

-- | Description of how Coq’s nat constructors and addition function
-- are represented in the extracted AST
-- Recursive-function bounds are of type nat, these parameters define
-- how they will be identified in the extracted AST
data CoqNat = CN { natO   :: FunName -- ^ How Coq’s O (nat) is extracted
                 , natS   :: FunName -- ^ How Coq’s S (nat) is extracted
                 , natAdd :: FunName -- ^ How Coq’s addition function on nat is extracted
                 }
    deriving (Show,Eq)

instance Default CoqNat where
    def = CN { natO   = "O"
             , natS   = "S"
             , natAdd = "add"
             }

-- | Type of the map associating function with their bound of
-- recursive calls (the name of the argument and its position)
type RecursiveBoundMap = Map FunName RecursiveBoundId
type Position = Int
type RecursiveBoundId = Position

-- | Type of a recursive-function bound and of its constant part
-- A recursive-function bound is the sum of a constant natural number
-- with some global values
type RecursiveBound      = (RecursiveBoundConst, [Name])
type RecursiveBoundConst = Integer

-- | Detect bounds for all the recursive functions of a module
--
-- Recursive functions are accepted only whenever they have an
-- argument that is bounding the number of recursive steps and when
-- they always start by checking whether that bound is 0 or not
-- When a fixitem does have a recursive bound, return the name of the
-- fixitem and the pair of the name of the argument and its position
-- in the argument list
--
-- >>> :{
--  detectBounds def (Mod "Simple" False False []
--      [Fixgroup
--          [FixItem "recursive"
--              (Arrow (Glob "nat" []) (Glob "unit" []))
--              (Lambda ["bound"]
--                  (Case (Rel "bound")
--                      [C (ConstructorP "O" []) (ConstructorE "tt" [])
--                      ,C (ConstructorP "S" ["n"]) (Apply (Global "recursive") [Rel "n"])]))]])
-- :}
-- fromList [("recursive",0)]
detectBounds :: CoqNat -> Module -> RecursiveBoundMap
detectBounds nats = Map.fromList . concatMap f . declarationsMod
    where f (Fixgroup is) = mapMaybe (detectBoundFixItem nats) is
          f _             = []

-- | Detect which argument is the bound in a FixItem
--
-- >>> :{
--  detectBoundFixItem def
--      (FixItem "recursive"
--          (Arrow (Glob "nat" []) (Glob "unit" []))
--          (Lambda ["bound"]
--              (Case (Rel "bound")
--                  [C (ConstructorP "O" []) (ConstructorE "tt" [])
--                  ,C (ConstructorP "S" ["n"]) (Apply (Global "recursive") [Rel "n"])])))
-- :}
-- Just ("recursive",0)
detectBoundFixItem :: CoqNat -> FixItem -> Maybe (FunName, RecursiveBoundId)
detectBoundFixItem CN{..} (FixItem ni _
                              (Lambda args
                                  (Case (Rel na)
                                        [C (ConstructorP o [])  _
                                        ,C (ConstructorP s [_]) _])))
    | o == natO && s == natS = do pos <- elemIndex na args
                                  pure (ni, pos)
detectBoundFixItem _ _       = Nothing

type ErrorMsg = String

-- | Extract a recursive-function bound from an expression, when
-- possible
-- Recognize an expression as a recursive-function bound if it is
-- composed of (natural numbers) additions, S, O and global values
--
-- For example, we can have \( n + 1 \), written as preferred, or even
-- some more complex expression, like \( (n+1) + ((1+m) + 2) \),
-- reduced to \( n + m + 4 \).
--
-- >>> :{
--  extractRecBoundExpr
--      def
--      (ConstructorE "S" [Global "n"])
-- :}
-- Just (1,["n"])
--
-- >>> :{
--  extractRecBoundExpr
--      def
--      (Apply
--          (Global "add")
--          [Global "n"
--          ,ConstructorE "S" [ConstructorE "O" []]])
-- :}
-- Just (1,["n"])
--
-- >>> :{
--  extractRecBoundExpr
--      def
--      (Apply
--          (Global "add")
--          [ConstructorE "S" [Global "n"]
--          ,Apply (Global "add")
--            [Apply (Global "add")
--              [ConstructorE "S" [ConstructorE "O" []]
--              ,Global "m"]
--            ,ConstructorE "S" [ConstructorE "S" [ConstructorE "O" []]]]])
-- :}
-- Just (4,["n","m"])
--
extractRecBoundExpr :: CoqNat -> Expr -> Maybe (RecursiveBoundConst, [Name])
extractRecBoundExpr CN{..} = go
    where go (Global n)                               = Just (0, [n])
          go (ConstructorE o [])        | o == natO   = Just (0, [])
          go (ConstructorE s [e])       | s == natS   = first (+1) <$> go e
          go (Apply (Global f) [e1,e2]) | f == natAdd = do (c1, ns1) <- go e1
                                                           (c2, ns2) <- go e2
                                                           pure (c1 + c2, ns1 ++ ns2)
          go _                                        = Nothing

-- | Extract and rewrite recursive-function calls
-- Extract the list of recursive functions a given function calls;
-- also extract the bound (if there are more than one call to a given
-- recursive function, they must use the same bound)
-- Rewrite the expression to remove the recursive bounds from the calls
--
-- TODO: should the behaviour be to fail when various bounds are used
-- (to report the error), or extract one randomly, or?
--
-- >>> :{
--  extractRecCalls
--      def
--      "recursive"
--      (Map.fromList [("recursive",0)])
--      (Apply (Global "recursive") [Rel "n"])
-- :}
-- Right ([],Apply {funcApply = Global {nameGlobal = "recursive"}, argsApply = []})
--
-- >>> :{
--  extractRecCalls
--      def
--      "callrec"
--      (Map.fromList [("recursive",0)])
--      (Apply (Global "recursive") [Global "bnd"])
-- :}
-- Right ([("recursive",(0,["bnd"]))],Apply {funcApply = Global {nameGlobal = "recursive"}, argsApply = []})
--
extractRecCalls :: CoqNat -> FunName -> RecursiveBoundMap -> Expr -> Either ErrorMsg ([(FunName, RecursiveBound)], Expr)
extractRecCalls cn self recs expr = deDup <$> isConsistent (go expr)
    where go :: Expr -> Either ErrorMsg ([(FunName, RecursiveBound)], Expr)
          go e@(Apply fun@(Global f) args) = do
                (nm, args') <- case Map.lookup f recs of
                           Just pos | pos < length args ->
                                      case splitAt pos args of
                                        (a1, _ : a2) | f == self  -> pure (Nothing, a1 ++ a2)
                                        (a1, a : a2) | isJust bnd -> pure (bnd,  a1 ++ a2)
                                            where bnd = extractRecBoundExpr cn a
                                        _                         -> Left $ expectedBound f pos e
                                    | otherwise         -> Left $ missingArgs f pos e
                           Nothing                      -> pure (Nothing, args)
                (calls, expr') <- drills (Apply fun) args'
                case nm of
                    Just nm' -> pure ((f, nm') : calls, expr')
                    Nothing  -> pure (calls, expr')

          go (Apply f args)        = drills (Apply f) args
          go (Lambda args body)    = drill  (Lambda args) body
          go (ConstructorE n args) = drills (ConstructorE n) args
          go (Case expr' cases)    = do (calls, exprs) <- drills id (map bodyC cases)
                                        let cases' = zipWith (\c e -> C (patC c) e) cases exprs
                                        pure (calls, Case expr' cases')
          go (Let var val body)    = do (calls,  let')  <- drill (Let var) val
                                        (calls', let'') <- drill let' body
                                        pure (calls ++ calls', let'')
          go e@(Global _)          = pure ([], e)
          go e@(Rel _)             = pure ([], e)

          drills f exprs = do (calls, exprs') <- unzip <$> traverse go exprs
                              pure (concat calls, f exprs')

          drill f e = do (calls, e') <- go e
                         pure (calls, f e')

          isConsistent r@(Right (calls, _)) = let assoc = Map.fromList calls
                                                  check = map (\(f, v) -> (f, Just v /= Map.lookup f assoc)) calls
                                              in  case find snd check of
                                                    Just (f, _) -> Left $ inconsistentCall f expr
                                                    Nothing     -> r
          isConsistent r                    = r

          deDup (as, b) = (Set.toList (Set.fromList as), b)

          missingArgs f nb e = "Not enough arguments: \"" ++ f ++ "\" expects at least " ++ show nb
                            ++ " arguments; in " ++ show e
          expectedBound f nb e = "Recursive bound argument expected: \"" ++ f ++ "\"'s "
                              ++ show nb ++"th argument should be an expression containing only "
                              ++ "natural numbers, additions and global values; in " ++ show e
          inconsistentCall f e = "Inconsistent calls of " ++ f ++ " with different recursive bounds; in "
                              ++ show e
