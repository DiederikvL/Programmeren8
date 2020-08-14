import Data.Char
import System.Environment
import System.IO

-- Deze kennen we nog van het encoden natuurlijk
type RLE a = [(Int, a)]

-- concatMap hebben we behandeld: deze voert op een lijst steeds dezelfde functie uit
-- uncurry is wel nieuw, deze voert een curried functie uit op een paar
-- Zie Hoogle voor voorbeelden: https://hackage.haskell.org/package/hspec-2.7.1/docs/Test-Hspec-Discover.html#v:uncurry
-- replicate neemt een argument en print het tweede argument zo vaak
-- replicate 5 'a' = "aaaaa"
unrle :: Eq a => RLE a -> [a]
unrle = concatMap (uncurry replicate)

-- Uit deze functie komt een RLE op basis van een string
-- Deze string is natuurlijk de encoded value
s_to_rle :: String -> RLE Char

-- Deze functie is ook weer recursief
-- Als we bij een lege string aankomen, is dat het eindpunt
s_to_rle "" = []

-- We behandelen de string natuurlijk als lijst van karakters
-- We kijken alleen naar de eerste waarde en controleren of dit een getal is
s_to_rle (s:ss) = if isDigit s

                  -- Als dit zo is, splitsen we de lijst op
                  then let (n,c:cs) = span isDigit (s:ss)

                       -- read cast een string naar een ander type
                       -- Zo wordt een string toch een integer, noodzakelijk voor RLE
                       -- Vervolgens gaan we verder met de rest
                       in (read n, c):(s_to_rle cs)

                  -- Als er geen getal is, is er slechts 1
                  -- TODO: er is een else nodig
                  -- Ik heb liever when, maar dat werkt niet bij mij
                  else (1, s):(s_to_rle ss)

main = do
  -- Het eerste argument is de textfile waar de te decoden tekst in staat
  args <- getArgs
  let inputFile = (head args)

  -- Deze file is de outputfile
  let outputFile = args!!1

  -- Lees de inhoud van de file
  -- readFile geeft het gelijk als IO String ipv IO Handle
  outputFile <- readFile inputFile

  -- Geef output naar de gebruiker adhv de functies
  -- TODO: geef de output volgens de practicumhandleiding
  let output = unrle (s_to_rle outputFile)

  -- Schrijf de output naar de file die is meegegeven als tweede argument
  writeFile outputFile output
