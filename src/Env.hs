{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Env
    ( convertBlock
    ) where

import qualified Data.Text as T
import Text.Pandoc.JSON

-- Represents an amsthm environment.
data Env = Env 
    Tag              -- Type of environment, e.g. Definition, Theorem, etc.
    (Maybe String)   -- Name of the theorem, definition, etc.
    [Inline]         -- Additional term inlines, e.g. \label{thm:1}
    [Block]          -- Body of the environment.
    deriving (Show, Eq)

data Tag = Definition | Lemma | Theorem | Proof | Claim
    deriving (Show, Eq)

-- Aliases for terms in a Pandoc DefinitionList.
type Term = [Inline]
type Definition = [Block]

-- Converts Pandoc AST blocks into theorem blocks.
convertBlock :: Block -> [Block]
convertBlock (DefinitionList terms) = terms >>= expandTerm
    where expandTerm :: (Term, [Definition]) -> [Block]
          expandTerm t = case matchTag t of
                  Just env -> envToBlocks env
                  Nothing -> [DefinitionList [t]]
convertBlock x = [x]

-- Matches a definition pair to check if it's being overloaded as an environment.
matchTag :: (Term, [Definition]) -> Maybe Env
matchTag (term, defs) = case term of
          -- Lemma. -> ["Lemma."]
          [Str tag] -> makeEnvFromTag <$> parseTag (init tag)
              where makeEnvFromTag :: Tag -> Env
                    makeEnvFromTag t = Env t Nothing [] (concat defs)
          -- Lemma (Lambek's). -> ["Lemma", Space, "(Lambek's)."]
          -- Lemma (Lambek's). \label{lamb} -> ["Lemma", Space, "(Lambek's).", RawInline]
          (Str tag):Space:(Str n):xs -> makeEnvFromTag <$> parseTag tag
              where makeEnvFromTag :: Tag -> Env
                    makeEnvFromTag t = Env t (parseName n) xs (concat defs)
          _ -> Nothing

-- Converts an environment to a list of Pandoc AST blocks.
envToBlocks :: Env -> [Block]
envToBlocks (Env tag name xs defs) = [front] ++ defs ++ [back]
    where (front, back) = makeDelimiters tag name xs

-- Defines the first and last blocks of the TeX environment.
makeDelimiters :: Tag -> Maybe String -> [Inline] -> (Block, Block)
makeDelimiters tagText nameText xs =
    ( makeTexBlock $ "\\begin{" ++ tag ++ "}" ++ name
    , makeTexBlock $   "\\end{" ++ tag ++ "}"
    )
    where makeTexBlock :: String -> Block
          makeTexBlock s = Plain $ [RawInline (Format "tex") s] ++ xs
          tag = getLatexEnvName tagText
          name = case nameText of
                   Just n -> "[" ++ n ++ "]"
                   Nothing -> ""

-- Parses a definition list term to see if it's being used as a theorem.
parseTag :: String -> Maybe Tag
parseTag txt = case txt of
                "Claim" -> Just Claim
                "Def" -> Just Definition
                "Definition" -> Just Definition
                "Lemma" -> Just Lemma
                "Pf" -> Just Proof
                "Proof" -> Just Proof
                "Thm" -> Just Theorem
                "Theorem" -> Just Theorem
                _ -> Nothing

-- Tries to extract the title out of a parenthesized string.
-- "(Lambek's)." |-> "Lambek's"
parseName :: String -> Maybe String
parseName name = T.unpack <$> name'
    where n = T.pack name
          -- Delete trailing period.
          name' = case T.stripSuffix "." n of
                -- Try to unwrap parens.
                Just n' -> case T.stripPrefix "(" n' of
                             Just n'' -> T.stripSuffix ")" n''
                             -- If no opening paren exists, return the original name.
                             Nothing -> Just n'
                -- If no trailing period, just return the original name.
                Nothing -> Just n

-- Maps a Tag to the corresponding amsthm environment.
getLatexEnvName :: Tag -> String
getLatexEnvName e = case e of
                Claim -> "claim"
                Definition -> "definition"
                Lemma -> "lemma"
                Proof -> "proof"
                Theorem -> "theorem"
