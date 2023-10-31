{-# LANGUAGE OverloadedStrings #-}

module EnvSpec (main, spec) where

import qualified Data.Sequence as S
import Test.Hspec
import Text.Pandoc.Builder

import Env (convertBlock, splitTerm)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    splitTermSpec
    convertBlockSpec

splitTermSpec :: Spec
splitTermSpec = describe "splitTerm" $ do
    let makeInput = splitTerm . unMany . text

    it "parses a tag" $ do
        makeInput "Lemma."
            `shouldBe` (S.fromList [Str "Lemma"], S.empty, S.empty)

    it "parses a tag and one-word name" $ do
        makeInput "Lemma (Lambek's)."
            `shouldBe` ( S.fromList [Str "Lemma", Space]
                       , S.fromList [Str "Lambek's"]
                       , S.empty
                       )

    it "parses a tag and multi-word name" $ do
        makeInput "Lemma (Lambek's Lemma)."
            `shouldBe` ( S.fromList [Str "Lemma", Space]
                       , S.fromList [Str "Lambek's", Space, Str "Lemma"]
                       , S.empty
                       )

    it "parses a tag, name, and additional data" $ do
        makeInput "Lemma (Lambek's Lemma). \\label{eqn:1}"
            `shouldBe` ( S.fromList [Str "Lemma", Space]
                       , S.fromList [Str "Lambek's", Space, Str "Lemma"]
                       , S.fromList [Space, Str "\\label{eqn:1}"]
                       )

    it "parses a tag and additional data" $ do
        makeInput "Lemma. \\label{eqn:1}"
            `shouldBe` (S.fromList [Str "Lemma"], S.empty, S.fromList [Space, Str "\\label{eqn:1}"])


convertBlockSpec :: Spec
convertBlockSpec = describe "convertBlock" $ do
    it "extracts definitions into the environment body" $ do
        let definitions =
                para ("A " <> code "List a" <> " is one of:")
                    <> bulletList
                           [plain $ code "Nil", plain $ code "Cons a (List a)"]
                    <> para ("for any type " <> code "a" <> ".")

        let input = DefinitionList
                [ ( [ Str "Definition"
                    , Space
                    , Str "(List)."
                    , Space
                    , RawInline (Format "latex") "\\label{def:list}"
                    ]
                  , [toList definitions]
                  )
                ]

        convertBlock input
            `shouldBe` (  (  plain
                          $  (rawInline "latex" "\\begin{definition}[List]")
                          <> " "
                          <> (rawInline "latex" "\\label{def:list}")
                          )
                       <> para ("A " <> code "List a" <> " is one of:")
                       <> bulletList
                              [ plain $ code "Nil"
                              , plain $ code "Cons a (List a)"
                              ]
                       <> para
                              (  "for any type "
                              <> code "a"
                              <> "."
                              <> rawInline "latex" "\\end{definition}"
                              )
                       )

    it "packs multiple definitions into the environment body" $ do
        let
            defs =
                para
                        "A dependently-typed programming language often used for interactive theorem proving."
                    <> para
                           "A video game that doesn't mean you understand the underlying theory, according to Bob."

        let input = DefinitionList
                [([Str "Definition", Space, Str "(Agda)."], [toList defs])]

        convertBlock input
            `shouldBe` (plain $ rawInline "latex" "\\begin{definition}[Agda]")
            <>         para
                           "A dependently-typed programming language often used for interactive theorem proving."
            <>         para
                           ( "A video game that doesn't mean you understand the underlying theory, according to Bob."
                           <> rawInline "latex" "\\end{definition}"
                           )

    it "maps a list with two definitions to two theorem environments" $ do
        let def1 = para "Any type with decidable equality is a set."
        let def2 = para (link "https://homotopy.io" "Trivial" "" <> ".")
        let input = DefinitionList
                [ ([Str "Theorem", Space, Str "(Hedberg)."], [toList def1])
                , ([Str "Proof."]                          , [toList def2])
                ]

        convertBlock input
            `shouldBe` ( (plain $ rawInline "latex" "\\begin{theorem}[Hedberg]")
                       <> para
                              (  "Any type with decidable equality is a set."
                              <> rawInline "latex" "\\end{theorem}"
                              )
                       <> (plain $ rawInline "latex" "\\begin{proof}")
                       <> para
                              (  link "https://homotopy.io" "Trivial" ""
                              <> "."
                              <> rawInline "latex" "\\end{proof}"
                              )
                       )

    it "combines the closing tag into the last block of a one-line inline proof"
        $ do
              let def   = plain "Trivial."
              let input = DefinitionList [([Str "Proof."], [toList def])]

              convertBlock input
                  `shouldBe` (  (plain $ rawInline "latex" "\\begin{proof}")
                             <> (  plain
                                $  "Trivial."
                                <> (rawInline "latex" "\\end{proof}")
                                )
                             )

    it
            "combines the closing tag into the last block of a multi-line inline proof"
        $ do
              let def1 = plain "Trivial."
              let def2 = plain "Another line."
              let input =
                      DefinitionList [([Str "Proof."], [toList $ def1 <> def2])]

              convertBlock input
                  `shouldBe` (  (plain $ rawInline "latex" "\\begin{proof}")
                             <> (plain "Trivial.")
                             <> (  plain
                                $  "Another line."
                                <> (rawInline "latex" "\\end{proof}")
                                )
                             )

    it "combines the closing tag into the last block of a block proof" $ do
        let def   = para "Trivial."
        let input = DefinitionList [([Str "Proof."], [toList def])]

        convertBlock input
            `shouldBe` (  (plain $ rawInline "latex" "\\begin{proof}")
                       <> (  para
                          $  "Trivial."
                          <> (rawInline "latex" "\\end{proof}")
                          )
                       )
