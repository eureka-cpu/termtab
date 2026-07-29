module Termtab.Parser.GP5 (parseGP5) where

import           Control.Monad               (forM, replicateM)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (transpose)
import qualified Data.Map.Strict             as Map
import           Termtab.Parser.GP5.Internal
import           Termtab.Types

parseGP5 :: FilePath -> IO (Either ParseError Song)
parseGP5 path = do
  bytes <- LBS.readFile path
  case runGetOrFail parseGP5File bytes of
    Left  (_, _, err)  -> return (Left (ParseErrorMalformed err))
    Right (_, _, song) -> return (Right song)

parseGP5File :: Get Song
parseGP5File = do
  (major, minor) <- getVersion
  if major /= 5
    then fail ("Expected GP5 file, got version " <> show major <> "." <> show minor)
    else return ()
  info         <- getScoreInfo
  skipLyrics
  skipPageSetup
  skipRSEMaster minor
  _tempoName   <- getTempoName
  bpm          <- getInt32le'
  skip 1       -- hide tempo flag
  _key         <- getInt32le'
  skip 1       -- octave
  programs     <- getChannels
  measureCount <- getInt32le'
  trackCount   <- getInt32le'
  headers      <- parseMeasureHeaders measureCount
  tracks       <- replicateM trackCount (getTrack programs)
  -- bars: measureCount × trackCount, row-major (measure first)
  barRows      <- forM [0 .. measureCount - 1] $ \_ ->
                    mapM (\_ -> getBar) [1 .. trackCount]
  -- transpose to [trackIdx][measureIdx]
  let barCols  = transpose barRows
  return (convertSong info bpm headers tracks barCols)

parseMeasureHeaders :: Int -> Get [GP5MeasureHeader]
parseMeasureHeaders n = go n (4, 4) (0, 0) []
  where
    go 0 _ _ acc = return (reverse acc)
    go left prevSig prevKey acc = do
      h <- getMeasureHeader (n - left) prevSig prevKey
      go (left - 1) (gmhTimeNum h, gmhTimeDen h) (gmhKeyRoot h, gmhKeyMode h) (h : acc)

convertSong :: GP5ScoreInfo -> Int -> [GP5MeasureHeader] -> [GP5Track] -> [[GP5Bar]] -> Song
convertSong info bpm headers tracks barCols = Song
  { songTitle    = gsiTitle info
  , songArtist   = gsiArtist info
  , songTempo    = Tempo bpm
  , songTracks   = zipWith convertTrack tracks barCols
  , songMeasures = zipWith convertMeasure [0..] headers
  }

convertMeasure :: Int -> GP5MeasureHeader -> Measure
convertMeasure idx h = Measure
  { measureIndex  = MeasureIndex idx
  , timeSignature = TimeSignature (gmhTimeNum h) (gmhTimeDen h)
  , keySignature  = KeySignature (gmhKeyRoot h) (if gmhKeyMode h == 0 then Major else Minor)
  , tempoChange   = Nothing
  }

convertTrack :: GP5Track -> [GP5Bar] -> Track
convertTrack t bars = Track
  { trackName       = gtName t
  , trackInstrument = if gtIsDrums t
                        then Standard (MidiProgram (gtProgram t))
                        else Guitar
                               { tuning      = map Pitch (gtStrings t)
                               , stringCount = length (gtStrings t)
                               }
  , trackChannel    = MidiChannel (gtChannel t)
  , trackBeats      = Map.fromList
      [ (MeasureIndex i, map (convertBeat (gtStrings t)) (gbarBeats bar))
      | (i, bar) <- zip [0..] bars
      ]
  }

convertBeat :: [Int] -> GP5Beat -> Beat
convertBeat stringPitches b = Beat
  { beatDuration = decodeDuration (gbDuration b) False
  , beatIsRest   = gbIsRest b
  , beatNotes    = map (convertNote stringPitches) (gbNotes b)
  }

convertNote :: [Int] -> GP5Note -> Note
convertNote stringPitches n =
  let strIdx = gnString n - 1
      basePitch = if strIdx >= 0 && strIdx < length stringPitches
                    then stringPitches !! strIdx
                    else 0
  in Note
    { notePitch    = Pitch (basePitch + gnFret n)
    , noteVelocity = Velocity (gnDynamic n * 8)
    , noteEffects  = []
    , noteString   = Just (StringIndex (gnString n))
    , noteFret     = Just (FretNumber (gnFret n))
    }

decodeDuration :: Int -> Bool -> Duration
decodeDuration raw dotted =
  let base = case raw of
        (-2) -> Whole
        (-1) -> Half
        0    -> Quarter
        1    -> Eighth
        2    -> Sixteenth
        3    -> Thirty2nd
        _    -> Quarter
  in if dotted then Dotted base else base
