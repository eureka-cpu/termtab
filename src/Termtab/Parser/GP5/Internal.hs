module Termtab.Parser.GP5.Internal (
    GP5ScoreInfo (..),
    GP5MeasureHeader (..),
    GP5Track (..),
    GP5Note (..),
    GP5Beat (..),
    GP5Bar (..),
    getVersion,
    getScoreInfo,
    skipLyrics,
    skipPageSetup,
    skipRSEMaster,
    getTempoName,
    getChannels,
    getMeasureHeader,
    getTrack,
    getBeat,
    getBar,
    getInt8',
    getInt32le',
    getU8,
    getByteSizeString,
    getIntByteSizeString,
    getIntSizeString,
) where

import Control.Monad (replicateM, replicateM_, when)
import Data.Binary.Get
import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data GP5ScoreInfo = GP5ScoreInfo
    { gsiTitle :: T.Text
    , gsiSubtitle :: T.Text
    , gsiArtist :: T.Text
    , gsiAlbum :: T.Text
    , gsiLyricist :: T.Text
    , gsiComposer :: T.Text
    }
    deriving (Show, Eq)

data GP5MeasureHeader = GP5MeasureHeader
    { gmhTimeNum :: Int
    , gmhTimeDen :: Int
    , gmhKeyRoot :: Int
    , gmhKeyMode :: Int
    }
    deriving (Show, Eq)

data GP5Track = GP5Track
    { gtName :: T.Text
    , gtStrings :: [Int]
    , gtChannel :: Int
    , gtProgram :: Int
    , gtIsDrums :: Bool
    }
    deriving (Show, Eq)

data GP5Note = GP5Note
    { gnString :: Int
    , gnFret :: Int
    , gnDynamic :: Int
    }
    deriving (Show, Eq)

data GP5Beat = GP5Beat
    { gbDuration :: Int
    , gbIsRest :: Bool
    , gbNotes :: [GP5Note]
    }
    deriving (Show, Eq)

data GP5Bar = GP5Bar
    { gbarBeats :: [GP5Beat]
    }
    deriving (Show, Eq)

getU8 :: Get Int
getU8 = fromIntegral <$> getWord8

getInt8' :: Get Int
getInt8' = do
    b <- getWord8
    let n = fromIntegral b :: Int
    return (if n >= 128 then n - 256 else n)

getInt32le' :: Get Int
getInt32le' = fromIntegral <$> getInt32le

-- 1-byte length + fieldLen-byte zero-padded field
getByteSizeString :: Int -> Get T.Text
getByteSizeString fieldLen = do
    strLen <- getU8
    raw <- getByteString fieldLen
    return (TE.decodeUtf8 (BS.take strLen raw))

-- 4-byte int (= 1 + str_len) + 1-byte str_len + str_len bytes
getIntByteSizeString :: Get T.Text
getIntByteSizeString = do
    _total <- getInt32le'
    strLen <- getU8
    raw <- getByteString strLen
    return (TE.decodeUtf8 raw)

-- 4-byte length + length bytes
getIntSizeString :: Get T.Text
getIntSizeString = do
    len <- getInt32le'
    raw <- getByteString len
    return (TE.decodeUtf8 raw)

-- Returns (major, minor), e.g. (5, 10) for "FICHIER GUITAR PRO v5.10"
getVersion :: Get (Int, Int)
getVersion = do
    raw <- getByteSizeString 30
    let s = T.unpack raw
    case parseVersionSuffix s of
        Just v -> return v
        Nothing -> fail ("Unrecognised GP version string: " <> s)

parseVersionSuffix :: String -> Maybe (Int, Int)
parseVersionSuffix s =
    case reverse (words s) of
        (verStr : _) ->
            let stripped = case verStr of 'v' : rest -> rest; _ -> verStr
             in case break (== '.') stripped of
                    (majS, '.' : minS) -> case (reads majS, reads minS) of
                        ([(maj, "")], [(mn, "")]) -> Just (maj, mn)
                        _ -> Nothing
                    _ -> case reads stripped of
                        [(maj, "")] -> Just (maj, 0)
                        _ -> Nothing
        [] -> Nothing

getScoreInfo :: Get GP5ScoreInfo
getScoreInfo = do
    title <- getIntByteSizeString
    subtitle <- getIntByteSizeString
    artist <- getIntByteSizeString
    album <- getIntByteSizeString
    lyricist <- getIntByteSizeString
    composer <- getIntByteSizeString
    _copy1 <- getIntByteSizeString
    _copy2 <- getIntByteSizeString
    _tabAuthor <- getIntByteSizeString
    _instr <- getIntByteSizeString
    noticeCount <- getInt32le'
    replicateM_ noticeCount getIntByteSizeString
    return
        GP5ScoreInfo
            { gsiTitle = title
            , gsiSubtitle = subtitle
            , gsiArtist = artist
            , gsiAlbum = album
            , gsiLyricist = lyricist
            , gsiComposer = composer
            }

skipLyrics :: Get ()
skipLyrics = do
    _ <- getInt32le'
    replicateM_ 5 $ do
        _ <- getInt32le'
        _ <- getIntSizeString
        return ()

skipPageSetup :: Get ()
skipPageSetup = do
    skip 36
    replicateM_ 11 getIntByteSizeString

-- GP5.0: 0 bytes; GP5.10: 19 bytes
skipRSEMaster :: Int -> Get ()
skipRSEMaster minor = when (minor >= 10) (skip 19)

getTempoName :: Get T.Text
getTempoName = getIntByteSizeString

-- Returns list of MIDI programs (one per channel, 64 total)
getChannels :: Get [Int]
getChannels = replicateM 64 readChannel
  where
    readChannel = do
        program <- getInt32le'
        skip 7 -- volume, balance, chorus, reverb, phaser, tremolo (6 bytes) + padding (1 byte)
        return program

getMeasureHeader :: Int -> (Int, Int) -> (Int, Int) -> Get GP5MeasureHeader
getMeasureHeader idx (prevNum, prevDen) (prevRoot, prevMode) = do
    flags <- getU8
    (timeNum, timeDen) <-
        if flags `testBit` 0
            then do
                n <- getU8
                d <- getU8
                return (n, d)
            else return (prevNum, prevDen)
    when (flags `testBit` 2) (skip 1) -- repeat end count
    when (flags `testBit` 3) (skip 1) -- alternate ending
    (keyRoot, keyMode) <-
        if flags `testBit` 4
            then do
                r <- getInt8'
                m <- getU8
                return (r, m)
            else return (prevRoot, prevMode)
    -- GP5 has an extra byte after the first measure header and when marker is present
    when (idx == 0) (skip 1)
    return
        GP5MeasureHeader
            { gmhTimeNum = timeNum
            , gmhTimeDen = timeDen
            , gmhKeyRoot = keyRoot
            , gmhKeyMode = keyMode
            }

getTrack :: [Int] -> Get GP5Track
getTrack programs = do
    flags <- getU8
    name <- getByteSizeString 40
    strCount <- getInt32le'
    -- 7 string slots, each 4 bytes
    allPitches <- replicateM 7 getInt32le'
    let pitches = take strCount allPitches
    _port <- getInt32le'
    chanIdx <- getInt32le'
    _chanFx <- getInt32le'
    _frets <- getInt32le'
    _height <- getInt32le'
    skip 4 -- color
    let prog =
            if chanIdx >= 1 && chanIdx <= length programs
                then programs !! (chanIdx - 1)
                else 0
    return
        GP5Track
            { gtName = name
            , gtStrings = pitches
            , gtChannel = chanIdx
            , gtProgram = prog
            , gtIsDrums = flags `testBit` 0
            }

getBeat :: Get GP5Beat
getBeat = do
    flags <- getU8
    isRest <-
        if flags `testBit` 6 -- 0x40
            then (\s -> s == 0x02) <$> getU8
            else return False
    durRaw <- getInt8'
    when (flags `testBit` 5) (skip 1) -- 0x20: tuplet
    when (flags `testBit` 1) skipChord -- 0x02: chord diagram
    when (flags `testBit` 2) (getIntSizeString >> return ()) -- 0x04: text
    when (flags `testBit` 3) skipBeatEffects -- 0x08: beat effects
    when (flags `testBit` 4) skipMixTable -- 0x10: mix table
    stringMask <- getU8
    let strNums = [s | s <- [1 .. 7], stringMask `testBit` (s - 1)]
    notes <- mapM getNote strNums
    return
        GP5Beat
            { gbDuration = durRaw
            , gbIsRest = isRest
            , gbNotes = notes
            }

getNote :: Int -> Get GP5Note
getNote strNum = do
    flags <- getU8
    _strIdx <- getU8 -- redundant string number
    fret <- getU8
    dynamic <- getU8
    when (flags `testBit` 3) skipBend -- 0x08: bend
    when (flags `testBit` 4) (skip 4) -- 0x10: grace note
    return
        GP5Note
            { gnString = strNum
            , gnFret = fret
            , gnDynamic = fromIntegral dynamic
            }

getBar :: Get GP5Bar
getBar = do
    voice0 <- getVoice
    _voice1 <- getVoice
    return GP5Bar{gbarBeats = voice0}

getVoice :: Get [GP5Beat]
getVoice = do
    beatCount <- getInt32le'
    replicateM beatCount getBeat

skipChord :: Get ()
skipChord = do
    newFmt <- getU8
    if newFmt == 1
        then do
            skip 16
            _ <- getByteSizeString 20
            skip 4
            replicateM_ 7 (skip 4)
            skip 32
        else do
            _ <- getByteSizeString 8
            skip 2
            replicateM_ 6 (getIntByteSizeString >> return ())

skipBeatEffects :: Get ()
skipBeatEffects = do
    flags1 <- getU8
    flags2 <- getU8
    when (flags1 `testBit` 0) skipBend -- vibrato bar
    when (flags1 `testBit` 1) (skip 1) -- tap/slap/pop type
    when (flags1 `testBit` 2) (skip 2) -- stroke down+up values
    when (flags2 `testBit` 5) (skip 1) -- tremolo picking duration
    when (flags2 `testBit` 6) (skip 1) -- slide type
    when (flags2 `testBit` 7) skipHarmonic -- harmonic type

skipMixTable :: Get ()
skipMixTable = do
    instrument <- getInt8'
    skip 16 -- RSE instrument info
    volume <- getInt8'
    balance <- getInt8'
    chorus <- getInt8'
    reverb <- getInt8'
    phaser <- getInt8'
    tremolo <- getInt8'
    _tempoStr <- getIntByteSizeString
    tempo <- getInt32le'
    when (instrument >= 0) (skip 1)
    when (volume >= 0) (skip 1)
    when (balance >= 0) (skip 1)
    when (chorus >= 0) (skip 1)
    when (reverb >= 0) (skip 1)
    when (phaser >= 0) (skip 1)
    when (tremolo >= 0) (skip 1)
    when (tempo >= 0) (skip 1)
    skip 1 -- use RSE flag

skipBend :: Get ()
skipBend = do
    skip 5 -- type + value + unknown
    pointCount <- getInt32le'
    skip (pointCount * 11)

skipHarmonic :: Get ()
skipHarmonic = do
    htype <- getU8
    case htype of
        2 -> skip 3 -- semitone + accidental + octave
        3 -> skip 1 -- tapping harmonic fret
        _ -> return ()
