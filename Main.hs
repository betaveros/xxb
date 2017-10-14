{-# LANGUAGE OverloadedStrings, BangPatterns, MagicHash #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T (singleton)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import System.IO
import System.Environment
import Data.List (foldl')
import Data.Monoid
import Data.Word
import Data.Char
import System.Console.GetOpt

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)

import GHC.Exts
import GHC.Word

import Data.ByteString.Internal (ByteString(..), createAndTrim', unsafeCreate)

data CBSFormat = Ascii | TwoChar | Unicode deriving (Eq, Show)
data CBSModification = Plain | Fancy | Fancier deriving (Eq, Show)

{-
bsChunkBuilder :: ByteString -> Builder
bsChunkBuilder s = case B.uncons s of
    Nothing -> mempty
    Just (c0, r0) -> case B.uncons r0 of
        Nothing       -> word8HexFixed c0
        Just (c1, r1) -> word8HexFixed c0 <> word8HexFixed c1 <> char7 ' ' <> bsChunkBuilder r1

bsChunkString :: ByteString -> L.ByteString
bsChunkString s = let lbs = toLazyByteString (bsChunkBuilder s) in
    lbs `L.append` L.replicate (40 - L.length lbs) ' '
-}

fillSpacesTo :: Ptr Word8 -> Ptr Word8 -> IO ()
fillSpacesTo d0 de = go d0
    where go d | d == de = return ()
               | otherwise = do
                    poke d 32
                    fillSpacesTo (d `plusPtr` 1) de

-- Scaffolding is copied from base16-bytestring, which is, I'm assuming,
-- pretty good at what it does.
-- I'm only using a 16-byte lookup table though since I can't see the
-- difference in testing speed.
bsChunkString :: Int -> Int -> ByteString -> ByteString
bsChunkString bsize gsize (PS sfp soff slen) = unsafeCreate bsize $ \dptr ->
    withForeignPtr sfp $ \sptr ->
        enc (sptr `plusPtr` soff) dptr
    where
        enc :: Ptr Word8 -> Ptr Word8 -> IO ()
        enc sptr dptr = go gsize sptr dptr
            where
                space = 32 :: Word8
                e = sptr `plusPtr` slen
                de = dptr `plusPtr` bsize
                go :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
                go gr s d | s == e = fillSpacesTo d de
                          | otherwise = do
                    x <- fromIntegral `fmap` peek s
                    poke  d              (tlookup hexTable (x `shiftR` 4))
                    poke (d `plusPtr` 1) (tlookup hexTable (x .&. 0xf))
                    if gr == 1 then do
                        poke (d `plusPtr` 2) space
                        go gsize (s `plusPtr` 1) (d `plusPtr` 3)
                    else
                        go (gr - 1) (s `plusPtr` 1) (d `plusPtr` 2)
        tlookup :: Addr# -> Int -> Word8
        tlookup table (I# index) = W8# (indexWord8OffAddr# table index)
        !hexTable = "0123456789abcdef"#

bsBinChunkString :: Int -> Int -> ByteString -> ByteString
bsBinChunkString bsize gsize (PS sfp soff slen) = unsafeCreate bsize $ \dptr ->
    withForeignPtr sfp $ \sptr ->
        enc (sptr `plusPtr` soff) dptr
    where
        enc :: Ptr Word8 -> Ptr Word8 -> IO ()
        enc sptr dptr = go gsize sptr dptr
            where
                space = 32 :: Word8
                one = 49 :: Word8
                zero = 48 :: Word8
                e = sptr `plusPtr` slen
                de = dptr `plusPtr` bsize
                pokeBits :: Ptr Word8 -> Word8 -> Int -> IO ()
                pokeBits _ _ 8 = return ()
                pokeBits p w i = poke (p `plusPtr` i) (if testBit w i then one else zero)
                go :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
                go gr s d | s == e = fillSpacesTo d de
                          | otherwise = do
                    x <- peek s
                    pokeBits d x 0
                    if gr == 1 then do
                        poke (d `plusPtr` 8) space
                        go gsize (s `plusPtr` 1) (d `plusPtr` 9)
                    else
                        go (gr - 1) (s `plusPtr` 1) (d `plusPtr` 8)

newtype Color = Color Word8

color :: Color -> ByteString -> ByteString
color (Color c) s = mconcat ["\27[3", B.singleton c, "m", s, "\27[0m"]

data CBS = CBS (Maybe Color) ByteString -- Colored Byte String

getColored :: CBS -> ByteString
getColored (CBS (Just c) s) = color c s
getColored (CBS Nothing  s) = s

getUncolored :: CBS -> ByteString
getUncolored (CBS _ s) = s

cbsj :: Color -> ByteString -> CBS
cbsj = CBS . Just

cbsn :: ByteString -> CBS
cbsn = CBS Nothing

plainAsciiC :: Word8 -> Word8
plainAsciiC (W8# w) = W8# (indexWord8OffAddr# table (word2Int# w)) where table = "................................ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~................................................................................................................................."#

fancyAsciiC :: Word8 -> Word8
fancyAsciiC (W8# w) = W8# (indexWord8OffAddr# table (word2Int# w)) where table = ".:::::::::#::::::::::::::::::::: !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????"#

red :: Color
red = Color 49
green :: Color
green = Color 50
magenta :: Color
magenta = Color 53

plainAsciiCBS :: Word8 -> CBS
plainAsciiCBS c
    | c < 32    = cbsj green "."
    | c > 126   = cbsj red "."
    | otherwise = cbsn $ B.singleton c

fancyAsciiCBS :: Word8 -> CBS
fancyAsciiCBS 0  = cbsj magenta "."
fancyAsciiCBS 10 = cbsj magenta "#"
fancyAsciiCBS c
    | c < 32    = cbsj green ":"
    | c > 126   = cbsj red "?"
    | otherwise = cbsn $ B.singleton c

ordToBS :: Int -> ByteString
ordToBS = encodeUtf8 . T.singleton . chr

plainUnicodeCBS :: Word8 -> CBS
plainUnicodeCBS c = case c of
    32   -> cbsj magenta . ordToBS $ 0x2423
    127  -> cbsj green   . ordToBS $ 0x2421
    0x80 -> r 0x20AC
    0x81 -> r 0xFFFD
    0x82 -> r 0x201A
    0x83 -> r 0x0192
    0x84 -> r 0x201E
    0x85 -> r 0x2026
    0x86 -> r 0x2020
    0x87 -> r 0x2021
    0x88 -> r 0x02C6
    0x89 -> r 0x2030
    0x8A -> r 0x0160
    0x8B -> r 0x2039
    0x8C -> r 0x0152
    0x8D -> r 0xFFFD
    0x8E -> r 0x017D
    0x8F -> r 0xFFFD
    0x90 -> r 0xFFFD
    0x91 -> r 0x2018
    0x92 -> r 0x2019
    0x93 -> r 0x201C
    0x94 -> r 0x201D
    0x95 -> r 0x2022
    0x96 -> r 0x2013
    0x97 -> r 0x2014
    0x98 -> r 0x02DC
    0x99 -> r 0x2122
    0x9A -> r 0x0161
    0x9B -> r 0x203A
    0x9C -> r 0x0153
    0x9D -> r 0xFFFD
    0x9E -> r 0x017E
    0x9F -> r 0x0178
    _ | c < 32    -> cbsj clr . ordToBS $ 0x2400 + fromIntegral c
      | c > 126   -> cbsj red . ordToBS $ fromIntegral c
      | otherwise -> cbsn               $ B.singleton c
    where r = cbsj red . ordToBS
          clr = case c of 0  -> magenta
                          9  -> magenta
                          10 -> magenta
                          13 -> magenta
                          _  -> green

fancyUnicodeCBS :: Word8 -> CBS
fancyUnicodeCBS 0  = cbsj magenta $ ordToBS 0xb7
fancyUnicodeCBS 9  = cbsj magenta $ ordToBS 0x21e5
fancyUnicodeCBS 10 = cbsj magenta $ ordToBS 0x23ce
fancyUnicodeCBS c  = plainUnicodeCBS c

fancierUnicodeCBS :: Word8 -> CBS
fancierUnicodeCBS 0  = cbsj magenta $ ordToBS 0xb7
fancierUnicodeCBS 9  = cbsj magenta $ ordToBS 0xbb
fancierUnicodeCBS 10 = cbsj magenta $ ordToBS 0x2193
fancierUnicodeCBS c  = plainUnicodeCBS c

plainTwoCharCBS :: Word8 -> CBS
plainTwoCharCBS c
    | c < 32    = cbsj green $ "^" <> B.singleton (c + 64)
    | c > 127   = cbsj red . sb $ word8HexFixed c
    | otherwise = cbsn $ B.pack [32, c]
    where sb = L.toStrict . toLazyByteString

fancyTwoCharCBS :: Word8 -> CBS
fancyTwoCharCBS c = case c of
    9  -> cbsj magenta "\\t"
    10 -> cbsj magenta "\\n"
    13 -> cbsj magenta "\\r"
    0  -> cbsj magenta "\\0"
    _  -> plainTwoCharCBS c

fancierTwoCharCBS :: Word8 -> CBS
fancierTwoCharCBS 0 = cbsj magenta ". "
fancierTwoCharCBS c = fancyTwoCharCBS c

getCBSFormatter :: CBSFormat -> CBSModification -> Word8 -> CBS
getCBSFormatter Ascii   Plain   = plainAsciiCBS
getCBSFormatter Ascii   _       = fancyAsciiCBS
getCBSFormatter TwoChar Plain   = plainTwoCharCBS
getCBSFormatter TwoChar Fancy   = fancyTwoCharCBS
getCBSFormatter TwoChar Fancier = fancierTwoCharCBS
getCBSFormatter Unicode Plain   = plainUnicodeCBS
getCBSFormatter Unicode Fancy   = fancyUnicodeCBS
getCBSFormatter Unicode Fancier = fancierUnicodeCBS

xxbBuild :: Bool -> (ByteString -> ByteString) -> (ByteString -> ByteString) -> Word32 -> ByteString -> Builder
xxbBuild color lhf rhf i s =
    byteString (if color then "\27[34m(" else "(")
        <> word32HexFixed i
        <> byteString (if color then ")\27[0m " else ") ")
        <> byteString (lhf s)
        <> byteString "| "
        <> byteString rhs
        <> char7 '\n'
    where rhs = rhf s

xxbWith :: Bool -> Bool -> CBSFormat -> CBSModification -> Int -> Int -> Handle -> Handle -> IO ()
xxbWith color ub cf cm cols gsize inh outh = do
    let colm = if ub then 8 else 2
    let bsize = cols * colm + if gsize == 0 then 1 else (cols + gsize - 1) `quot` gsize
    let chunkf = if ub then bsBinChunkString bsize gsize else bsChunkString bsize gsize
    let printablef = case (color, cf, cm) of
            (False, Ascii, Plain) -> B.map plainAsciiC
            (False, Ascii, _    ) -> B.map fancyAsciiC
            _ -> B.concatMap $ (if color then getColored else getUncolored) . getCBSFormatter cf cm
    let cols' = fromIntegral cols :: Word32
    let loop i e
            | i >= e = return (Just i, mempty)
            | otherwise = do
                f <- hIsEOF inh
                if f then return (Nothing, mempty) else do
                    s <- B.hGet inh cols
                    if B.null s then return (Nothing, mempty) else do
                        let b = xxbBuild color chunkf printablef i s
                        lr <- loop (i + cols') e
                        return $ fmap (b <>) lr
    let jumpSize = 4096
    let outerLoop i = do
            (cont, res) <- loop i (i + jumpSize)
            hPutBuilder outh res
            case cont of
                Just nxt -> outerLoop nxt
                Nothing -> return ()
    outerLoop 0

dropPastParen :: ByteString -> ByteString
dropPastParen = B.drop 1 . C.dropWhile (/= ')')

unxxbBuilder :: ByteString -> Builder
unxxbBuilder s = case C.uncons s of
    Just ('(', r) -> unxxbBuilder (dropPastParen r)
    Just ('|', _) -> mempty
    Just (c, r) -> if isHexDigit c
        then case C.uncons r of
            Just (c', r') -> if isHexDigit c'
                then word8 (fromIntegral $ 16 * digitToInt c + digitToInt c') <> unxxbBuilder r'
                else unxxbBuilder r'
            Nothing -> mempty
        else unxxbBuilder r
    _ -> mempty

unxxbBinaryBuilder :: ByteString -> Builder
unxxbBinaryBuilder s = case C.uncons s of
    Just ('(', r) -> unxxbBinaryBuilder (dropPastParen r)
    Just ('|', _) -> mempty
    Just (c, r) -> if c == '0' || c == '1'
        then go 7 r [c]
        else unxxbBinaryBuilder r
            where
                go 0 r cs = word8 (fromIntegral . foldl' (\a d -> 2*a + d) 0 $ map digitToInt cs) <> unxxbBuilder r
                go n r cs = case C.uncons r of
                    Just (c', r') -> if c' == '0' || c' == '1'
                        then go (n - 1) r' (c' : cs)
                        else unxxbBinaryBuilder r'
                    Nothing -> mempty
    _ -> mempty

unxxb :: Handle -> Handle -> IO ()
unxxb inh outh = loop
    where loop = do
            f <- hIsEOF inh
            unless f $ do
                s <- C.hGetLine inh
                unless (B.null s) $ do
                    hPutBuilder outh (unxxbBuilder s)
                    loop

data Options = Options
    { reverseOperation :: Bool
    , useBinary        :: Bool
    , useColor         :: Maybe Bool
    , cbsFormat        :: CBSFormat
    , cbsModification  :: CBSModification
    , groupSize        :: Int
    , columns          :: Int
    , help             :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { reverseOperation = False
    , useBinary        = False
    , useColor         = Nothing
    , cbsFormat        = Ascii
    , cbsModification  = Fancy
    , groupSize        = 2
    , columns          = 16
    , help             = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['?'] ["help"]
        (NoArg (\ opts -> opts { help = True }))
        "display this help message"
    , Option ['r'] ["reverse"]
        (NoArg (\ opts -> opts { reverseOperation = True }))
        "reverse operation"
    , Option ['g'] ["group-size"]
        (ReqArg (\ c opts -> opts { groupSize = read c }) "bytes")
        "separate the group of every <bytes> bytes by spaces"
    , Option ['c'] ["cols"]
        (ReqArg (\ c opts -> opts { columns = read c }) "cols")
        "format <cols> bytes per line"
    , Option ['b'] ["binary"]
        (NoArg (\ opts -> opts { useBinary = True }))
        "dump in binary instead of hexadecimal"
    , Option ['C'] ["color"]
        (NoArg (\ opts -> opts { useColor = Just True }))
        "use console escape sequences to display color"
    , Option ['n'] ["no-color"]
        (NoArg (\ opts -> opts { useColor = Just False }))
        "don't use console escape sequences to display color"
    , Option ['u'] ["unicode"]
        (NoArg (\ opts -> opts { cbsFormat = Unicode }))
        "use unicode for printable display"
    , Option ['2'] ["two-char"]
        (NoArg (\ opts -> opts { cbsFormat = TwoChar }))
        "use two chars per byte for printable display"
    , Option ['p'] ["plain"]
        (NoArg (\ opts -> opts { cbsModification = Plain }))
        "plain modifications of printable display"
    , Option ['f'] ["fancy"]
        (NoArg (\ opts -> opts { cbsModification = Fancy }))
        "fancy modifications of printable display (default)"
    , Option ['F'] ["fancier"]
        (NoArg (\ opts -> opts { cbsModification = Fancier }))
        "fancier modifications of printable display"
    ]

helpText :: String
helpText = usageInfo "Usage: xxb [OPTION..] [infile [outfile]]" options

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ helpText))

main' :: Options -> Handle -> Handle -> IO ()
main' opts inh outh = if reverseOperation opts
    then unxxb inh outh
    else do
        color <- case useColor opts of
            Just b -> return b
            Nothing -> hIsTerminalDevice outh
        xxbWith
            color (useBinary opts)
            (cbsFormat opts) (cbsModification opts)
            (columns opts) (groupSize opts)
            inh outh

main :: IO ()
main = do
    args <- getArgs
    (opts, rargs) <- compilerOpts args
    if help opts then putStrLn helpText else
        case rargs of
            [] -> main' opts stdin stdout
            [infile] -> do
                inh <- openFile infile ReadMode
                main' opts inh stdout
            [infile, outfile] -> do
                inh <- openFile infile ReadMode
                outh <- openFile outfile WriteMode
                main' opts inh outh
            _ -> putStrLn helpText
