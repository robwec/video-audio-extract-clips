import Text.Read
import Data.List.Split --splitOn
import System.Process
import Control.Monad
--import Data.Char --isDigit
--import System.Directory --doesFileExist
import System.IO --openFile for reading ANSI unicode characters
import Data.String.Utils --replace
--import Text.RegexPR
--import Data.List --intercalate
--import GHC.Stack
--import Debug.Trace
import GHC.IO.Encoding (setLocaleEncoding) --setLocaleEncoding, so I can write a freaking EM dash — to a file
unicodeReadFile :: String -> IO String
unicodeReadFile myfile = do
  handle <- openFile myfile ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  return contents

main = do
  setLocaleEncoding utf8
  clipFilesFromSpreadSheet "in" "out" "audiolist.txt"
  return()

--Extract Audio Clips in batch, using a spreadsheet pasted into a text file
----------------
--HOW IT WORKS--
----------------
--Calls a batch of sox commands to extract clips from audio file outputs.
--the text file input is a tab-delemited list with format (including header row)
  --input file  start time  end time  output clip name (use.ogg)
  --input.mp3  1:30:01.333 1:32:03 clipname.ogg
--the trim command called is like
  --sox "myinputfile.mp3" "myoutputclip.ogg" trim 12.5 =25.333
--UPDATE: just use ffmpeg. This has the added advantage of it's possible to add a switch to clip video, too.

clipFilesFromSpreadSheet :: String -> String -> String -> IO()
clipFilesFromSpreadSheet infoldername outfoldername myclipsfilename = do
  mytrackcliplist <- readTrackSheet_toSongDataList myclipsfilename
  --putStrLn $ show $ mytrackcliplist!!0
  --mapM_ sox_clip_track mytrackcliplist
  mapM_ (\x -> ffmpeg_clip_track infoldername outfoldername x) mytrackcliplist
  return()
sox_clip_track :: SongClipData -> IO()
sox_clip_track mysongdata = do
  mycommand <- return $ "sox \""++(filename mysongdata)++"\" \""++(outputtrackname mysongdata)++"\" trim "++(starttime mysongdata)++" ="++(endtime mysongdata)
  callCommand mycommand
  return()
--2017-10-27-1925-07 Because I want to extract a song from a 4-hour video and didn't want to convert it to mp3 first; that takes a while!
ffmpeg_clip_track :: String -> String -> SongClipData -> IO()
ffmpeg_clip_track infoldername outfoldername mysongdata = do
  --mycommand <- return $ "ffmpeg -ss " ++ starttime mysongdata ++ " -i \""++(filename mysongdata)++"\" -c copy -t \""++(outputtrackname mysongdata)++"\" trim "++(starttime mysongdata)++" ="++(endtime mysongdata)
    --https://superuser.com/questions/138331/using-ffmpeg-to-cut-up-video
    --can implement hybrid seeking. if seconds is more than 60, then
  mycommand <- return $ setcodeccommand infoldername outfoldername mysongdata
  --mycommand <- return $ "ffmpeg -ss " ++ hybridseektime ++ " -i \""++(filename mysongdata)++"\" -ss " ++ accurateseekstarttime ++ " -t " ++ songduration ++ " -c:a libvorbis \""++(outputtrackname mysongdata)++"\""
  --mycommand <- return $ "ffmpeg -ss " ++ hybridseektime ++ " -i \""++(filename mysongdata)++"\" -ss " ++ accurateseekstarttime ++ " -t " ++ songduration ++ " "++mycodecstring++" \""++(outputtrackname mysongdata)++"\""
  --putStrLn mycommand
  --getLine
  callCommand mycommand
  return()
setcodeccommand :: String -> String -> SongClipData -> String
setcodeccommand infoldername outfoldername mysongdata
  -- | outextension == "ogg" = "ffmpeg -ss " ++ hybridseektime ++ " -i \""++(filename mysongdata)++"\" -ss " ++ accurateseekstarttime ++ " -t " ++ songduration ++ " "++"c:a libvorbis"++" \""++(outputtrackname mysongdata)++"\""
  -- | outextension == "mp4" = "-c:v libx264 -c:a copy"
  -- | otherwise = "-c:a libvorbis"
  | outextension == "ogg" = "ffmpeg -i \""++(infoldername++"\\"++filename mysongdata)++"\" -ss " ++ (hybridseektime mysongdata) ++ " -i \""++(infoldername++"\\"++filename mysongdata)++"\" -ss " ++ (accurateseekstarttime mysongdata) ++ " -t " ++ (songduration mysongdata) ++ " "++"-map 0:0 -map 1:1? -c:a libvorbis -qscale:v 7"++" \""++(outfoldername++"\\"++outputtrackname mysongdata)++"\""
    --what was a command that copies the cover image with seeking?
    --ffmpeg -i "a.mp3" -ss 0 -i "a.mp3" -ss 2 -t 2 -c:a libvorbis -c:v libtheora -qscale:v 7 "z.ogg"
      --breaks
    --ffmpeg -i "a.mp3" -ss 0 -i "a.mp3" -t 2 -c:a libvorbis -c:v libtheora -qscale:v 7 "z.ogg"
      --works. the hybrid seek time seems to break oggs with embedded images. Whatever.
  | outextension == "mp4" = "ffmpeg -ss " ++ (hybridseektime mysongdata) ++ " -i \""++(infoldername++"\\"++filename mysongdata)++"\" -ss " ++ (accurateseekstarttime mysongdata) ++ " -t " ++ (songduration mysongdata) ++ " "++"-c:v libx264 -c:a copy"++" \""++(outfoldername++"\\"++outputtrackname mysongdata)++"\""
  | otherwise = error $ "use mp4 or ogg extension for output"
  where
  outextension = head $ reverse $ splitOn "." $ outputtrackname mysongdata
starttime_asnum :: SongClipData -> Float
starttime_asnum mysongdata = (read $ starttime mysongdata)::Float
hybridseektime :: SongClipData -> String
hybridseektime mysongdata
  | starttime < 60 = "0"
  | otherwise = show $ starttime - 60
  where
  starttime = starttime_asnum mysongdata
accurateseekstarttime :: SongClipData -> String
accurateseekstarttime mysongdata
  | (starttime_asnum mysongdata) < 60 = show $ starttime_asnum mysongdata
  | otherwise = "60"
songduration :: SongClipData -> String
songduration mysongdata = show $ ((read $ endtime mysongdata)::Float) - ((read $ starttime mysongdata)::Float)

data SongClipData = SongClipData
  {filename::String
  ,starttime::String
  ,endtime::String
  ,outputtrackname::String
  } deriving Show
--READ DATA FROM SPREADSHEET
readTrackSheet_toSongDataList :: String -> IO([SongClipData])
readTrackSheet_toSongDataList myfilename = do
  mytrackfile <- readFile myfilename
  mytrackfile <- return $ drop 1 (splitOn "\n" mytrackfile)
  --putStr $ show mytrackfile
  mytrackfile <- return $ map (\x -> splitOn "\t" x) mytrackfile
  --putStr $ show mytrackfile
  mytrackfile <- return $ map (\x -> splitSheet_toSongClipData x) mytrackfile
  --putStr $ show mytrackfile
  return mytrackfile
splitSheet_toSongClipData :: [String] -> SongClipData
splitSheet_toSongClipData mylist
  | length mylist == 4 = (makesongclipdata mylist)
  | otherwise = makesongclipdata ["","","",""]
  where
  makesongclipdata mylist = SongClipData
    {filename=(mylist!!0)
    ,starttime=hhmmss_toSeconds $ (mylist!!1)
    ,endtime=hhmmss_toSeconds $ (mylist!!2)
    ,outputtrackname=nameoutputfile mylist
    }
  nameoutputfile mylist = (mylist!!0)++" "++(replace ":" "m" (mylist!!1))++"s "++(mylist!!3)
--[][][]PROCESS START/END TIME DATA WHEN COLUMNS ARE BLANK
--CONVERT mm:ss to just seconds
hhmmss_toSeconds :: String -> String
hhmmss_toSeconds mytimestamp
  | length stamparray == 3 = (show $ (readint_pos stamparray 0)*3600 + (readint_pos stamparray 1)*60 + (readint_pos stamparray 2)) ++ (maybemilliseconds mytimestamp 2)
  | length stamparray == 2 = (show $ (readint_pos stamparray 0)*60 + (readint_pos stamparray 1)) ++ (maybemilliseconds mytimestamp 1)
  | length stamparray == 1 = (show $ (readint_pos stamparray 0)) ++ (maybemilliseconds mytimestamp 0)
  | otherwise = "0"
  where
  maybemilliseconds :: String -> Int -> String
  maybemilliseconds mytimestamp pos
    | (length $ splitOn "." $ (splitOn ":" (mytimestamp))!!pos) == 2 = "." ++ (splitOn "." $ splitOn ":" (mytimestamp)!!pos)!!1
    | otherwise = ""
  readint_pos mynumarray pos = read (head $ splitOn "." (mynumarray!!pos))::Int
  stamparray = splitOn ":" mytimestamp