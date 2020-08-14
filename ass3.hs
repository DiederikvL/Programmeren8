import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function
import System.Environment
import System.IO

class Eq a => Bits a where
    zer :: a
    one :: a

instance Bits Int where
    zer = 0
    one = 1

instance Bits Bool where
    zer = False
    one = True

type Codemap a = M.Map Char [a]

-- De HuffmanTree bestaat uit Leaves en Forks,
-- die op zichczelf ook weer bestaan uit "HuffmanTrees"
data HuffmanTree  = Leaf Char Int
                  | Fork HuffmanTree HuffmanTree Int
                  deriving (Show)

-- Deze gestandaardiseerde manier maakt het mogelijk om de weight te krijgen,
-- ongeacht het type HuffmanTree
weight :: HuffmanTree -> Int
weight (Leaf _ w)    = w
weight (Fork _ _ w)  = w

-- Het mergen van twee HuffmanTrees behoudt ook de originele componenten,
-- door te mergen hoef je dus niet de leaves apart toe te voegen
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

-- TODO, FIXME: sorteren kan ook prima in deze frequency list,
-- daarnaast zijn er nettere en geschiktere methodes
freqList :: String -> [(Char, Int)]
freqList = M.toList . M.fromListWith (+) . map (flip (,) 1)

-- Deze functie bouwt recursief de tree op
-- Hierbij is de list uit freqList de input voor deze functie
buildTree :: [(Char, Int)] -> HuffmanTree
buildTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
    where  bld (t:[])    = t
           bld (a:b:cs)  = bld $ insertBy (compare `on` weight) (merge a b) cs

-- Zodra de tree is opgebouwd, moet er natuurlijk nog doorheen worden genavigeerd
-- Hierbij worden de bits geset
buildCodemap :: Bits a => HuffmanTree -> Codemap a
buildCodemap = M.fromList . buildCodelist
    where  buildCodelist (Leaf c w)    = [(c, [])]
           buildCodelist (Fork l r w)  = map (addBit zer) (buildCodelist l) ++ map (addBit one) (buildCodelist r)
             where addBit b = second (b :)

-- Deze functie is de core van de compression
-- Het converteert een string naar een Huffman tree
stringTree :: String -> HuffmanTree
stringTree = buildTree . freqList

stringCodemap :: Bits a => String -> Codemap a
stringCodemap = buildCodemap . stringTree

encode :: Bits a => Codemap a -> String -> [a]
encode m = concat . map (m M.!)

encode' :: Bits a => HuffmanTree -> String -> [a]
encode' t = encode $ buildCodemap t

decode :: Bits a => HuffmanTree -> [a] -> String
decode tree = dcd tree
    where  dcd (Leaf c _) []        = [c]
           dcd (Leaf c _) bs        = c : dcd tree bs
           dcd (Fork l r _) (b:bs)  = dcd (if b == zer then l else r) bs

-- TODO: voeg I/O toe.

main = do 
    args <- getArgs
    let inputFile = (head args)
    let outputFile = args!!1
    let htFile = args!!2

    text <- readFile inputFile
    codeMap <- stringCodemap text

    encoded <- encode codeMap text 

-- bits naar output file schrijven
--writeFile outputFile outputBits

-- hufmantree naar file schrijven
--writeFile HTFile hufmanntree

    --aantal karakters
    let lengthOfInputFile = length text
    putStrLn("length of " ++ inputFile ++ ": " ++ (show lengthOfInputFile) ++ " characters.")

-- aantal bits input ????

--aantal bits output

    --let factor = round $ ((fromIntegral lengthOfOutputFile) / (fromIntegral lengthOfInputFile)) * 100
    --putStrLn("factor: " ++ (show lengthOfOutputFile) ++ " / " ++ (show lengthOfInputFile) ++ " * 100% = " ++ (show factor) ++ "%")


