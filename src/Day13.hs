{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day13 (day13) where

import Control.Monad (void)
import Data.List (foldl', sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Foreign (fromBool)
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, parseMaybe, sepBy, try, (<|>))
import Text.Megaparsec.Char (hspace1, newline, space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

data Packet = PNum Int | PList [Packet]
    deriving (Show, Eq)

instance Ord Packet where
    compare :: Packet -> Packet -> Ordering
    compare (PNum a) (PNum b) = compare a b
    compare (PList a) (PList b) = compare a b
    compare (PNum a) (PList b) = compare (PList [PNum a]) (PList b)
    compare (PList a) (PNum b) = compare (PList a) (PList [PNum b])

type Parser = Parsec Void Text

sc :: Parser ()
sc =
    L.space
        space1
        empty
        empty

-- consumes trailing whitespace and \n and maybe others (nee to look at space1)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--  consumes trailing whitespace and \n and maybe others (nee to look at space1)
symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

comma :: Parser Text
comma = symbol ","

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pPacket :: Parser Packet
pPacket = (PNum <$> try integer) <|> pList

pList :: Parser Packet
pList = brackets $ do
    elements <- sepBy pPacket comma
    return $ PList elements

outerMessage :: Parser (Packet, Packet)
outerMessage = do
    msg1 <- pPacket
    msg2 <- pPacket
    return (msg1, msg2)

day13 :: IO ()
day13 = do
    txt <- T.unlines . filter (not . T.null) . T.lines <$> TIO.readFile "./inputs/day13.txt"
    let my_parsed_input = fromJust $ parseMaybe (many outerMessage <* eof) txt
    let my_parsed_input_p2 =
            sort $
                PList [PList [PNum 6]] : PList [PList [PNum 2]] : concatMap (\(x, y) -> [x, y]) my_parsed_input
    let res = zipWith (\idx (x, y) -> if x < y then idx else 0) [1 ..] my_parsed_input
    let res2 =
            zipWith
                ( \idx x ->
                    ( if (x == PList [PList [PNum 6]])
                        || (x == PList [PList [PNum 2]])
                        then
                            idx
                        else
                            1
                    )
                )
                [1 ..]
                my_parsed_input_p2
    print $ sum res
    print $ foldl' (*) 1 res2

-- let (tst1, tst2) = fromJust $ parseMaybe (outerMessage <* eof) "[[2]]\n[[6]]\n"
-- print tst1
-- print tst2
-- print (tst1 < tst2)
-- let tst =
--        fromJust $
--            parseMaybe (many outerMessage <* eof) "[[1],[2,3,4]]\n[[1],[2,3,4]]\n[[1],[2,3,4]]\n[[1],[2,3,4]]\n"
-- print tst
