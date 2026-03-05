{- |
Module      : Main
Description : Golden tests for Aiken.Codegen
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Renders AST values to strings and compares against
golden files in @test\/golden\/@. Set @UPDATE_GOLDEN=1@
to overwrite golden files with actual output.
-}
module Main
    ( main
    ) where

import Aiken.Codegen
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Hspec

-- ---------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------

{- | Run a golden test. Compares rendered output against
a @.golden@ file. When @UPDATE_GOLDEN=1@ is set,
writes actual output to the golden file instead.
-}
goldenExpr :: String -> Expr -> Spec
goldenExpr name expr = it name $ do
    let actual = renderExpr expr
    goldenCheck name actual

goldenDef :: String -> Def -> Spec
goldenDef name d = it name $ do
    let actual = renderDef d
    goldenCheck name actual

goldenModule :: String -> Module -> Spec
goldenModule name m = it name $ do
    let actual = renderModule m
    goldenCheck name actual

goldenCheck :: String -> String -> IO ()
goldenCheck name actual = do
    let path = "test/golden/" ++ name ++ ".golden"
    update <- lookupEnv "UPDATE_GOLDEN"
    case update of
        Just _ -> writeFile path actual
        Nothing -> do
            exists <- doesFileExist path
            if exists
                then do
                    expected <- readFile path
                    actual `shouldBe` expected
                else do
                    writeFile path actual
                    pendingWith
                        "golden file created"

-- | Helper to render a single expression.
renderExpr :: Expr -> String
renderExpr = renderModule . Module . pure . Test "_"

-- | Helper to render a single definition.
renderDef :: Def -> String
renderDef = renderModule . Module . pure

-- ---------------------------------------------------------
-- Tests
-- ---------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Expressions" $ do
        goldenExpr "hex_literal" $
            hex "\xab\xcd"

        goldenExpr "int_literal" $
            int 42

        goldenExpr "var" $
            var "mpf.empty"

        goldenExpr "empty_list" $
            list []

        goldenExpr "list_of_ints" $
            list [int 1, int 2, int 3]

        goldenExpr "record_simple" $
            record
                "Leaf"
                [ ("skip", int 0)
                , ("key", hex "\xab")
                ]

        goldenExpr "record_nested" $
            record
                "Fork"
                [ ("skip", int 0)
                ,
                    ( "neighbor"
                    , record
                        "Neighbor"
                        [ ("nibble", int 5)
                        , ("root", hex "\xde\xad")
                        ]
                    )
                ]

        goldenExpr "call_no_args" $
            call "mpf.empty" []

        goldenExpr "call_with_args" $
            call
                "mpf.insert"
                [ var "t"
                , hex "\xab"
                , hex "\xcd"
                , var "p"
                ]

        goldenExpr "eq" $
            call "mpf.root" [var "t"]
                .== hex "\xab"

        goldenExpr "let_untyped" $
            Let "x" Nothing (int 1) (var "x")

        goldenExpr "let_typed" $
            Let
                "proof"
                (Just "Proof")
                (list [])
                (var "proof")

        goldenExpr "let_chain" $
            runBody $ do
                p <-
                    bindAs "proof" "Proof" (list [])
                t <-
                    bind
                        "trie"
                        ( call
                            "mpf.insert"
                            [ var "mpf.empty"
                            , hex "\xab"
                            , hex "\xcd"
                            , p
                            ]
                        )
                pure $
                    call "mpf.root" [t]
                        .== hex "\xef"

    describe "Definitions" $ do
        goldenDef "test_simple" $
            Test "my_test" (int 1 .== int 1)

        goldenDef "use_from" $
            useFrom
                "aiken/merkle_patricia_forestry"
                ["Branch", "Fork", "Leaf"]

        goldenDef "use_as" $
            useAs
                "aiken/merkle_patricia_forestry"
                "mpf"

        goldenDef "comment" $
            comment "Auto-generated"

    describe "Modules" $ do
        goldenModule "module_empty" $
            Module []

        goldenModule "module_insert_test" $
            runModule $ do
                emit $
                    useFrom
                        "aiken/merkle_patricia_forestry"
                        [ "Branch"
                        , "Fork"
                        , "Leaf"
                        , "Proof"
                        ]
                emit $
                    useAs
                        "aiken/merkle_patricia_forestry"
                        "mpf"
                emit Blank
                emitTest "insert_and_check_root" $ do
                    p <-
                        bindAs
                            "proof"
                            "Proof"
                            ( list
                                [ record
                                    "Branch"
                                    [
                                        ( "skip"
                                        , int 0
                                        )
                                    ,
                                        ( "neighbors"
                                        , hex
                                            "\xaa"
                                        )
                                    ]
                                ]
                            )
                    t <-
                        bind
                            "trie"
                            ( call
                                "mpf.insert"
                                [ var "mpf.empty"
                                , hex "\xab"
                                , hex "\xcd"
                                , p
                                ]
                            )
                    pure $
                        call "mpf.root" [t]
                            .== hex "\xef"

        goldenModule "module_inclusion_test" $
            runModule $ do
                emit $
                    useFrom
                        "aiken/merkle_patricia_forestry"
                        ["Proof"]
                emit $
                    useAs
                        "aiken/merkle_patricia_forestry"
                        "mpf"
                emit Blank
                emitTest "has_key" $ do
                    p <-
                        bindAs
                            "proof"
                            "Proof"
                            (list [])
                    pure $
                        call
                            "mpf.has"
                            [ var "mpf.empty"
                            , hex "\xab"
                            , hex "\xcd"
                            , p
                            ]

        goldenModule "module_asset_name_test" $
            runModule $ do
                emit $
                    useFrom
                        "cardano/assets"
                        ["OutputReference"]
                emit Blank
                emitTest "asset_name_check" $
                    pure $
                        call
                            "asset_name"
                            [ record
                                "OutputReference"
                                [
                                    ( "transaction_id"
                                    , hex "\xaa\xbb"
                                    )
                                ,
                                    ( "output_index"
                                    , int 0
                                    )
                                ]
                            ]
                            .== hex "\xcc\xdd"

        goldenModule "module_mixed" $
            runModule $ do
                emit $ comment "Auto-generated"
                emit $
                    useFrom
                        "aiken/merkle_patricia_forestry"
                        ["Proof"]
                emit $
                    useAs
                        "aiken/merkle_patricia_forestry"
                        "mpf"
                emit Blank
                emitTest "test_one" $
                    pure $
                        int 1 .== int 1
                emit Blank
                emitTest "test_two" $ do
                    p <-
                        bindAs
                            "proof"
                            "Proof"
                            (list [])
                    pure $
                        call
                            "mpf.has"
                            [ var "mpf.empty"
                            , hex "\xab"
                            , hex "\xcd"
                            , p
                            ]
