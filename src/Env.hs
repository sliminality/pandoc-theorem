{-# OPTIONS_GHC -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Env
    ( convertBlock
    , splitTerm
    ) where

import Data.Sequence (Seq((:<|), (:|>)), (|>), (<|))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder hiding (Example)

-- | Represents a LaTeX environment.
data Env = Env
    Tag             -- ^ Type of environment, e.g. Definition, Theorem, etc.
    (Maybe Text)    -- ^ Name of the theorem, definition, etc.
    Inlines         -- ^ Additional term inlines, e.g. \label{thm:1}
    Blocks          -- ^ Body of the environment.
    deriving (Show, Eq)

-- | Type of a LaTeX environment. Corresponds to a unique environment name.
data Tag = Definition | Lemma | Theorem | Proof | Claim | Example | Assumption | Proposition
    deriving (Show, Eq)

-- Aliases for terms in a DefinitionList.
type Term = [Inline]
type Definition = [Block]

-- | Converts a DefinitionList block from the Pandoc AST into a theorem block,
-- if the definition term starts with a recognized tag.
convertBlock :: Block -> Blocks
convertBlock (DefinitionList terms) = foldMap expandTerm terms
  where
    expandTerm :: (Term, [Definition]) -> Blocks
    expandTerm (term, defs) =
        let pieces = splitTerm (S.fromList term)
        in  case makeEnv pieces defs of
                Just (Env t n rest ds) -> front <> (Many $ combineBlocks (unMany ds) back)
                    where (front, back) = makeDelimiters t n rest
                Nothing -> definitionList [(fromList term, fromList <$> defs)]
    -- If the last definition is a Para or Plain block, compress it with the final
    -- Plain block to avoid an unsightly newline.
    combineBlocks :: Seq Block -> Block -> Seq Block
    combineBlocks (xs:|>Para xs') (Plain ys) =
        xs |> Para (xs' <> ys)
    combineBlocks (xs:|>Plain xs') (Plain ys) =
        xs |> Plain (xs' <> ys)
    combineBlocks ds back = ds |> back

-- Pass all other blocks through unmodified.
convertBlock x = singleton x

-- | Creates an environment representing a LaTeX environment block.
makeEnv :: (Seq Inline, Seq Inline, Seq Inline) -> [Definition] -> Maybe Env
makeEnv (tag, name, rest) defs
    | null tag  = Nothing
    | otherwise = fmap convertTag ((parseTag . T.strip . toRawText) tag)
  where
    convertTag :: Tag -> Env
    convertTag t = Env t n (Many rest) (foldMap fromList defs)
    n = if null name
        then Nothing
        else (Just . T.strip . toRawText) name
    -- TODO: Might be useful to have this work with formatted tags, e.g. **Lemma**.
    toRawText :: Seq Inline -> Text
    toRawText S.Empty      = ""
    toRawText (Str s:<|xs) = s `T.append` (toRawText xs)
    toRawText (Space:<|xs) = " " `T.append` (toRawText xs)
    toRawText (_    :<|xs) = toRawText xs

-- Defines the first and last blocks of the TeX environment.
makeDelimiters :: Tag -> Maybe Text -> Inlines -> (Blocks, Block)
makeDelimiters tagText nameText rest =
    ( plain $ (rawInline "latex" $ "\\begin{" <> tag <> "}" <> name) <> rest
      -- Closing block may be merged into the final body block, so keep it
      -- independent of other blocks for now.
    , Plain [RawInline (Format "latex") ("\\end{" <> tag <> "}")]
    )
  where
    tag  = getLatexEnvName tagText
    name = case nameText of
        Just n  -> "[" <> n <> "]"
        Nothing -> ""

-- Maps definition terms to Env types.
-- TODO: Allow the user to define their own aliases in frontmatter.
parseTag :: Text -> Maybe Tag
parseTag txt = case txt of
    "Claim"       -> Just Claim
    "Def"         -> Just Definition
    "Definition"  -> Just Definition
    "Lemma"       -> Just Lemma
    "Pf"          -> Just Proof
    "Proof"       -> Just Proof
    "Thm"         -> Just Theorem
    "Theorem"     -> Just Theorem
    "Ex"          -> Just Example
    "Example"     -> Just Example
    "Assumption"  -> Just Assumption
    "Prop"        -> Just Proposition
    "Proposition" -> Just Proposition
    _             -> Nothing

-- Maps a Tag to the corresponding environment name (the `foo` in `\begin{foo}`).
getLatexEnvName :: Tag -> Text
getLatexEnvName e = case e of
    Claim       -> "claim"
    Definition  -> "definition"
    Lemma       -> "lemma"
    Proof       -> "proof"
    Theorem     -> "theorem"
    Example     -> "example"
    Assumption  -> "assumption"
    Proposition -> "proposition"

-- Splits term text into the metadata of a LaTeX environment.
-- TODO: Add support for nested parens, e.g. "Definition (O(n) runtime)."
splitTerm :: Seq Inline -> (Seq Inline, Seq Inline, Seq Inline)
splitTerm xs =
    let (tagName, rest) = splitAfter (checkStr (T.isSuffixOf ".")) xs
    in  case S.breakl opensParen tagName of
            (tag, S.Empty) -> (withoutPeriods tag, S.Empty, rest)
            (tag, name   ) -> (tag, trimParens . withoutPeriods $ name, rest)
  where
    withoutPeriods xs = fmap (dropSuffix ".") xs
    opensParen :: Inline -> Bool
    opensParen = checkStr (T.isPrefixOf "(")
    closesParen :: Inline -> Bool
    closesParen = checkStr (T.isSuffixOf ")")

checkStr :: (Text -> Bool) -> Inline -> Bool
checkStr f (Str s) = f s
checkStr _ _       = False

trimParens :: Seq Inline -> Seq Inline
trimParens S.Empty = S.Empty
trimParens xs = applyToLast (dropSuffix ")") $ applyToFirst (dropPrefix "(") xs

shiftPartition :: (Seq a, Seq a) -> (Seq a, Seq a)
shiftPartition (S.Empty, y:<|ys) = (S.singleton y, ys)
shiftPartition (xs     , y:<|ys) = (xs |> y, ys)
shiftPartition x                 = x

splitAfter :: (Inline -> Bool) -> Seq Inline -> (Seq Inline, Seq Inline)
splitAfter f s = shiftPartition (S.breakl f s)

applyToFirst :: (a -> a) -> Seq a -> Seq a
applyToFirst _ S.Empty  = S.Empty
applyToFirst f (x:<|xs) = (f x) <| xs

applyToLast :: (a -> a) -> Seq a -> Seq a
applyToLast _ S.Empty  = S.Empty
applyToLast f (xs:|>x) = xs |> (f x)

dropSuffix :: Text -> Inline -> Inline
dropSuffix = transformStr . T.stripSuffix

dropPrefix :: Text -> Inline -> Inline
dropPrefix = transformStr . T.stripPrefix

transformStr :: (Text -> Maybe Text) -> Inline -> Inline
transformStr f i@(Str s) = case f s of
    Just s' -> Str $ s'
    Nothing -> i
transformStr _ i = i
