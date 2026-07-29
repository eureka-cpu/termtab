module Termtab.Display (printSong) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

import           Termtab.Types

printSong :: Song -> IO ()
printSong song = do
  TIO.putStrLn ("Title:  " <> songTitle song)
  TIO.putStrLn ("Artist: " <> songArtist song)
  let Tempo bpm = songTempo song
  putStrLn ("Tempo:  " <> show bpm <> " BPM")
  putStrLn ("Tracks: " <> show (length (songTracks song)))
  mapM_ printTrack (zip [1 :: Int ..] (songTracks song))

printTrack :: (Int, Track) -> IO ()
printTrack (i, track) =
  TIO.putStrLn
    ( "  [" <> T.pack (show i) <> "] "
    <> trackName track
    <> ": " <> T.pack (show noteCount) <> " notes"
    )
  where
    noteCount = sum (map (length . beatNotes) (concat (Map.elems (trackBeats track))))
