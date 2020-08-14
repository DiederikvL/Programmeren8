import System.Environment
import System.IO

-- Typecasting!
-- Dit type geeft is een lijst van combinaties van letter en aantal
-- Gebruik dus overal dit type in plaats van de ruwe versie
type RLE a = [(Int, a)]

-- Dit is het encoden zelf
-- Let op! Dit levert nog niet de gewenste output op
encode :: Eq a => [a] -> RLE a

-- Dit is heel belangrijk, omdat dit het beginpunt is
-- Immers is deze functie recursief
encode [] = []

-- Deze is vrij ingewikkeld op het eerste gezicht, maar we gaan er rustig doorheen
-- We krijgen de volledige lijst binnen van karakters, en splitsen dan met `span` op het eerste karakter.
-- Dit gebeurt er dan met een lijst bijvoorbeeld:
-- ["aaaaaabbbbbbbbcccccccaaaaaa"] -> (["aaaaaa"], ["bbbbbbbbcccccccaaaaaa"])

-- Het eerste gedeelte slaan we op, samen met de lengte van die lijst
-- We krijgen dan dit:
-- [(6, 'a')]

-- Nu gaan we verder met het toevoegen van RLE aan deze lijst
-- Dat is het gedeelte `encode diff`. In diff staat nu dus: 
-- ["bbbbbbbbcccccccaaaaaa"]

-- Dit is het recursieve gedeelte, nu gaan we door tot de lijst leeg is
-- Het resultaat is dan dit:
-- [(6,'a'),(8,'b'),(7,'c'),(6,'a')]
encode (x:xs) = 
  let (eq, diff) = span (==x) xs 
  in (1 + length eq, x) : encode diff

-- Nu willen we van deze lijst (van type RLE) een string maken
-- Dat is handiger opslaan, en daarnaast moet dat van de handleiding
encoding_to_string :: RLE Char -> String

-- `concatMap` voert een functie uit op een lijst
-- Deze functie krijgt de lijst van `encode` aangeleverd
-- De functie valt hier slecht op, omdat het een lambda-functie is
-- Inlezen kan hier: http://learnyouahaskell.com/higher-order-functions#lambdas

-- Het eerste deel van de tuple is een getal, dus dit moet worden omgezet naar een string
-- Dat is exact wat `show` doet
-- Met `++` plakken we een gedeelte achter die string, namelijk `c`
-- In c staat volgens RLE altijd een char
-- Je zou gebruik kunnen maken van `show` voor die char, maar die zet er aanhalingstekens omheen
-- Aangezien een string gewoon een lijst van karakters is, werkt dit ook
encoding_to_string = concatMap (\(n,c) -> show n ++ [c])

-- In de main functie nemen we het eerste argument in
-- Dat wordt natuurlijk een file, maar daar had ik ff geen tijd voor
-- Daar voeren we eerst encode uit, daarna casten we naar een string
-- Het resultaat printen we met een nette witregel erachter
-- Zonder witregel wordt mijn console boos
main = do

  -- Het eerste argument is de textfile waar de te encoden tekst in staat
  args <- getArgs
  let inputFile = (head args)

  -- Deze file is de outputfile
  let outputFile = args!!1
  
  -- Lees de inhoud van de file
  -- readFile geeft het gelijk als IO String ipv IO Handle
  inputText <- readFile inputFile

  -- Geef output naar de gebruiker adhv de functies
  -- TODO: geef de output volgens de practicumhandleiding
  let output = encoding_to_string (encode inputText)

  -- Geef de originele lengte weer
  let lengthOfInputFile = length inputText
  putStrLn("length of " ++ inputFile ++ ": " ++ (show lengthOfInputFile) ++ " characters.")

  -- Geef de lengte van de outputfile en de compressie
  let lengthOfOutputFile = length output
  putStrLn("length of compressed file " ++ outputFile ++ ": " ++ (show lengthOfOutputFile) ++ " characters.")

  let factor = round $ ((fromIntegral lengthOfOutputFile) / (fromIntegral lengthOfInputFile)) * 100
  putStrLn("factor: " ++ (show lengthOfOutputFile) ++ " / " ++ (show lengthOfInputFile) ++ " * 100% = " ++ (show factor) ++ "%")

  -- Schrijf de output naar de file die is meegegeven als tweede argument
  writeFile outputFile output