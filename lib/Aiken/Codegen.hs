{- |
Module      : Aiken.Codegen
Description : Haskell DSL for generating Aiken source
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

A minimal Haskell AST for the Aiken language subset
needed for code generation (tests, records, function
calls, let bindings, literals). Construct expressions
as data, render to source, let @aiken fmt@ normalize.
-}
module Aiken.Codegen
    ( -- * AST
      Expr (..)
    , Def (..)
    , Module (..)

      -- * Rendering
    , renderModule

      -- * Smart constructors
    , hex
    , int
    , var
    , record
    , call
    , list
    , (.==)

      -- * Definition constructors
    , useFrom
    , useAs
    , comment

      -- * BodyM (let-binding builder)
    , BodyM
    , bind
    , bindAs
    , runBody

      -- * ModuleM (definition accumulator)
    , ModuleM
    , emit
    , emitTest
    , runModule
    ) where

import Control.Monad.Operational
    ( Program
    , ProgramViewT (Return, (:>>=))
    , singleton
    , view
    )
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.List (intercalate)

-- ---------------------------------------------------------
-- AST
-- ---------------------------------------------------------

-- | Aiken expression.
data Expr
    = -- | Hex literal: @#"abcd"@
      Hex ByteString
    | -- | Integer literal: @42@
      Int Int
    | -- | Variable reference: @proof@, @mpf.empty@
      Var String
    | -- | Record constructor: @Name { f: v }@
      Record String [(String, Expr)]
    | -- | Function call: @fn(a, b)@
      Call String [Expr]
    | -- | Equality: @a == b@
      Eq Expr Expr
    | -- | List literal: @[x, y]@
      List [Expr]
    | -- | Let binding: @let x[: T] = e1; e2@
      Let String (Maybe String) Expr Expr
    deriving stock (Show, Eq)

-- | Top-level Aiken definition.
data Def
    = -- | Test definition: @test name() { body }@
      Test String Expr
    | -- | Use import: @use path.{A, B}@
      Use String [String]
    | -- | Use-as import: @use path as alias@
      UseAs String String
    | -- | Comment line: @// text@
      Comment String
    | -- | Blank line
      Blank
    deriving stock (Show, Eq)

-- | An Aiken module is a list of definitions.
newtype Module = Module [Def]
    deriving stock (Show, Eq)

-- ---------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------

-- | Render a module to Aiken source text.
renderModule :: Module -> String
renderModule (Module defs) =
    unlines (map renderDef defs)

-- | Render a single definition.
renderDef :: Def -> String
renderDef = \case
    Test name body ->
        "test "
            ++ name
            ++ "() {\n"
            ++ indent (renderExpr body)
            ++ "\n}"
    Use path names ->
        "use "
            ++ path
            ++ ".{"
            ++ intercalate ", " names
            ++ "}"
    UseAs path alias ->
        "use " ++ path ++ " as " ++ alias
    Comment text ->
        "// " ++ text
    Blank -> ""

-- | Render an expression.
renderExpr :: Expr -> String
renderExpr = \case
    Hex bs ->
        "#\""
            ++ C8.unpack (Base16.encode bs)
            ++ "\""
    Int n -> show n
    Var v -> v
    Record name fields ->
        name
            ++ " { "
            ++ intercalate
                ", "
                (map renderField fields)
            ++ " }"
    Call fn args ->
        fn
            ++ "("
            ++ intercalate ", " (map renderExpr args)
            ++ ")"
    Eq a b ->
        renderExpr a ++ " == " ++ renderExpr b
    List elems ->
        "["
            ++ intercalate ", " (map renderExpr elems)
            ++ trailing
            ++ "]"
      where
        trailing
            | null elems = ""
            | otherwise = ","
    Let name mTy rhs body ->
        "let "
            ++ name
            ++ tyAnn
            ++ " = "
            ++ renderExpr rhs
            ++ "\n"
            ++ renderExpr body
      where
        tyAnn = case mTy of
            Nothing -> ""
            Just ty -> ": " ++ ty

-- | Render a record field.
renderField :: (String, Expr) -> String
renderField (name, val) =
    name ++ ": " ++ renderExpr val

-- | Indent each line by 2 spaces.
indent :: String -> String
indent =
    unlines
        . map ("  " ++)
        . lines

-- ---------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------

-- | Hex literal from a 'ByteString'.
hex :: ByteString -> Expr
hex = Hex

-- | Integer literal.
int :: Int -> Expr
int = Int

-- | Variable reference.
var :: String -> Expr
var = Var

-- | Record constructor.
record :: String -> [(String, Expr)] -> Expr
record = Record

-- | Function call.
call :: String -> [Expr] -> Expr
call = Call

-- | List literal.
list :: [Expr] -> Expr
list = List

-- | Equality operator.
(.==) :: Expr -> Expr -> Expr
(.==) = Eq

infixl 4 .==

-- ---------------------------------------------------------
-- Definition constructors
-- ---------------------------------------------------------

-- | Use import with selected names.
useFrom :: String -> [String] -> Def
useFrom = Use

-- | Use import with alias.
useAs :: String -> String -> Def
useAs = UseAs

-- | Comment line.
comment :: String -> Def
comment = Comment

-- ---------------------------------------------------------
-- BodyM — let-binding builder (operational)
-- ---------------------------------------------------------

-- | Instructions for building let-binding chains.
data BodyI a where
    -- | Introduce an untyped let binding.
    Bind :: String -> Expr -> BodyI Expr
    -- | Introduce a typed let binding.
    BindAs :: String -> String -> Expr -> BodyI Expr

{- | Monad for building test bodies.  Each 'bind'
introduces a let-binding; 'runBody' folds them into
nested 'Let' expressions.
-}
type BodyM = Program BodyI

{- | Introduce an untyped let binding, return 'Var'
reference.
-}
bind :: String -> Expr -> BodyM Expr
bind name rhs = singleton (Bind name rhs)

{- | Introduce a typed let binding, return 'Var'
reference.
-}
bindAs :: String -> String -> Expr -> BodyM Expr
bindAs name ty rhs =
    singleton (BindAs name ty rhs)

-- | Finalize: fold instructions into nested 'Let'.
runBody :: BodyM Expr -> Expr
runBody m = case view m of
    Return e -> e
    Bind name rhs :>>= k ->
        Let
            name
            Nothing
            rhs
            (runBody (k (Var name)))
    BindAs name ty rhs :>>= k ->
        Let
            name
            (Just ty)
            rhs
            (runBody (k (Var name)))

-- ---------------------------------------------------------
-- ModuleM — definition accumulator (operational)
-- ---------------------------------------------------------

-- | Instructions for accumulating definitions.
data ModuleI a where
    -- | Emit a single definition.
    Emit :: Def -> ModuleI ()

{- | Monad for building modules.  Each 'emit' appends a
definition; 'runModule' collects them into a 'Module'.
-}
type ModuleM = Program ModuleI

-- | Emit a single definition.
emit :: Def -> ModuleM ()
emit d = singleton (Emit d)

-- | Emit a test definition from a 'BodyM'.
emitTest :: String -> BodyM Expr -> ModuleM ()
emitTest name body =
    emit (Test name (runBody body))

-- | Finalize: collect definitions into a 'Module'.
runModule :: ModuleM () -> Module
runModule = Module . go
  where
    go :: ModuleM () -> [Def]
    go m = case view m of
        Return () -> []
        Emit d :>>= k -> d : go (k ())
