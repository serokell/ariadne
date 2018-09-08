module Ariadne.UI.Qt.AnsiToHTML
    ( spanFormat
    , simpleDocToHTML
    , csiToHTML
    ) where

import Formatting

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import qualified HTMLEntities.Builder as HTML

import qualified System.Console.ANSI.Types as AT
import qualified Text.PrettyPrint.ANSI.Leijen as PP

spanFormat :: Format a (Text -> a)
spanFormat =
  -- using span because pre leads to weird empty lines
  "<span style='\
      \font-family: monospace;\
      \white-space: pre-wrap;'><span>" % stext % "</span></span>"

-- Colors in this file are from gruvbox theme: https://github.com/morhetz/gruvbox

getColor :: [AT.SGR] -> Text
getColor xs = case colors of
  []    -> bgColor
  (c:_) -> toColor c
  where
    toColor (AT.SetColor _ _ c) = case c of
      AT.Black   -> "#cdcdcd" -- This one is not from gruvbox, but from our designs
      AT.Red     -> "#cc241d"
      AT.Green   -> "#98971a"
      AT.Yellow  -> "#d79921"
      AT.Blue    -> "#458588"
      AT.Magenta -> "#b16286"
      AT.Cyan    -> "#689d6a"
      AT.White   -> "#a89984"
    toColor _ = bgColor

    colors = filter isColor xs

    isColor = \case
      AT.SetColor {} -> True
      _ -> False

    bgColor = "#cdcdcd"

simpleDocToHTML :: PP.SimpleDoc -> Text
simpleDocToHTML sdoc = if innerHTML == "" then "" else
  toText $ format spanFormat $ innerHTML
  where
    indentation i = if i <= 0 then "" else T.replicate i " "
    innerHTML = toText $ B.toLazyText $ go sdoc
    go = \case
      PP.SFail -> error "simpleDocToHTML: impossible"
      PP.SEmpty -> mempty
      PP.SChar c x -> HTML.char c <> go x
      PP.SText _ s x -> HTML.text (fromString s) <> go x
      PP.SLine i x -> B.fromText "<br>" <> B.fromText (indentation i) <> go x
      PP.SSGR s x -> B.fromText "</span><span style=\"color: " <> B.fromText (getColor s) <> B.fromText ";\">" <> go x

csiToHTML :: Text -> Text
csiToHTML line = T.concat split
  where
    split = readString "" $ toString line

    readString :: String -> String -> [Text]
    readString acc ('\x1b':'[':xs) = (fromString $ reverse acc) : readAttr "" xs
    readString acc (x:xs) = readString (x:acc) xs
    readString acc [] = [fromString $ reverse acc]

    readAttr :: String -> String -> [Text]
    readAttr acc (x:xs)
      | x `elem` ['\x40'..'\x7e'] = (makeSpan $ csiToCSS x $ fromString $ reverse acc) : readString "" xs
      | otherwise = readAttr (x:acc) xs
      -- Just skip unsupported attributes, we don't want any garbage in HTML
    readAttr _ [] = []

    makeSpan :: Text -> Text
    makeSpan = toText . format ("</span><span style=\"" % stext % "\">")

    csiToCSS :: Char -> Text -> Text
    csiToCSS 'm' params = T.intercalate ";" $ sgnToColor <$> T.splitOn ";" params
    csiToCSS _ _ = ""

    sgnToColor = \case
      "" -> "color: "
      "0" -> "color: black"

      "30" -> "color: black"
      "31" -> "color: #cc241d"
      "32" -> "color: #98971a"
      "33" -> "color: #d79921"
      "34" -> "color: #458588"
      "35" -> "color: #b16286"
      "36" -> "color: #689d6a"
      "37" -> "color: white"

      "90" -> "color: #928374" -- brightBlack
      "91" -> "color: #9d0006" -- brightRed
      "92" -> "color: #79740e" -- brightGreen
      "93" -> "color: #b57614" -- brightYellow
      "94" -> "color: #076678" -- brightBlue
      "95" -> "color: #8f3f71" -- brightMagenta
      "96" -> "color: #417b58" -- brightCyan
      "97" -> "color: #3c3836" -- brightWhite

      _ -> ""
