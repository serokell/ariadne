module Ariadne.UI.Vty.AnsiToVty (ansiToVty, csiToVty, pprDoc) where

import Control.Monad.Trans.Writer

import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified System.Console.ANSI.Types as AT
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A

ansiToVty
  :: V.Attr
  -> Ppr.A.SimpleDoc
  -> V.Image
ansiToVty defAttr = V.vertCat . execWriter . go defAttr identity
  where
    go
      :: V.Attr
      -> (V.Image -> V.Image)
      -> Ppr.A.SimpleDoc
      -> Writer [V.Image] ()
    go attr img = \case
      Ppr.A.SFail -> error "ansiToVty: impossible"
      Ppr.A.SEmpty -> tell [img V.emptyImage]
      Ppr.A.SChar c x -> go attr (img . V.horizJoin (V.char attr c)) x
      Ppr.A.SText _ s x -> go attr (img . V.horizJoin (V.string attr s)) x
      Ppr.A.SLine i x -> do
        tell [img V.emptyImage]
        go attr (V.horizJoin (V.backgroundFill i 1)) x
      Ppr.A.SSGR s x ->
        go (V.withForeColor attr (getColor s)) img x

getColor :: [AT.SGR] -> V.Color
getColor xs = case colors of
  []    -> V.white
  (c:_) -> toColor c
  where
    toColor (AT.SetColor _ _ c) = case c of
      AT.Black   -> V.black
      AT.Red     -> V.red
      AT.Green   -> V.green
      AT.Yellow  -> V.yellow
      AT.Blue    -> V.blue
      AT.Magenta -> V.magenta
      AT.Cyan    -> V.cyan
      AT.White   -> V.white
    toColor _ = V.white

    colors = filter isColor xs

    isColor = \case
      AT.SetColor _ _ _ -> True
      _ -> False

csiToVty
  :: V.Attr
  -> T.Text
  -> V.Image
csiToVty defAttr text = V.horizCat $ uncurry V.text' <$> split
  where
    split = readString defAttr "" $ T.unpack text

    readString :: V.Attr -> String -> String -> [(V.Attr, T.Text)]
    readString attr acc ('\x1b':'[':xs) = (attr, T.pack $ reverse acc) : readAttr attr "" xs
    readString attr acc (x:xs) = readString attr (x:acc) xs
    readString attr acc [] = [(attr, T.pack $ reverse acc)]

    readAttr :: V.Attr -> String -> String -> [(V.Attr, T.Text)]
    readAttr attr acc (x:xs)
      | x `elem` ['\x40'..'\x7e'] = readString (csiToAttr x (T.pack $ reverse acc) attr) "" xs
      | otherwise = readAttr attr (x:acc) xs
    readAttr attr acc [] = [(attr, T.pack $ "\x1b[" ++ reverse acc)]

    csiToAttr :: Char -> T.Text -> V.Attr -> V.Attr
    csiToAttr 'm' params attr = foldl' sgrToAttr attr $ T.splitOn ";" params
    csiToAttr _ _ attr = attr

    sgrToAttr attr = \case
      "" -> defAttr
      "0" -> defAttr

      "30" -> attr `V.withForeColor` V.black
      "31" -> attr `V.withForeColor` V.red
      "32" -> attr `V.withForeColor` V.green
      "33" -> attr `V.withForeColor` V.yellow
      "34" -> attr `V.withForeColor` V.blue
      "35" -> attr `V.withForeColor` V.magenta
      "36" -> attr `V.withForeColor` V.cyan
      "37" -> attr `V.withForeColor` V.white

      "90" -> attr `V.withForeColor` V.brightBlack
      "91" -> attr `V.withForeColor` V.brightRed
      "92" -> attr `V.withForeColor` V.brightGreen
      "93" -> attr `V.withForeColor` V.brightYellow
      "94" -> attr `V.withForeColor` V.brightBlue
      "95" -> attr `V.withForeColor` V.brightMagenta
      "96" -> attr `V.withForeColor` V.brightCyan
      "97" -> attr `V.withForeColor` V.brightWhite

      _ -> attr

pprDoc
  :: V.Attr
  -> Int
  -> Ppr.A.Doc
  -> V.Image
pprDoc defAttr w s = ansiToVty defAttr $ Ppr.A.renderSmart 0.985 w s
