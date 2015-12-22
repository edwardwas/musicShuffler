{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty)
import Control.Applicative
import Control.Monad.Random
import Control.Monad
import Prelude hiding (FilePath)

musicExtensions :: [Text]
musicExtensions = ["mp3","ogg","m4a","wav","flac","wma"]

addRandomPrep :: MonadIO m => FilePath -> m FilePath
addRandomPrep fp = do
    rs <- liftIO $ replicateM 5 (getRandomR ('a','z'))
    return $ replaceExtension (directory fp 
        </> "new" </> decodeString (rs <> "_" <> encodeString (filename fp))) "mp3"
        
musicPattern ::  Pattern Text
musicPattern = choice $ 
    map (suffix . text . mappend "." ) musicExtensions

main = sh run

convert :: MonadIO m => (FilePath, FilePath) -> m ExitCode
convert (x,y) = 
    proc "ffmpeg" ["-i", format fp x, "-ac", "2", "-ab", "192k", "-vn", "-ar", 
        "44100", "-f" ,"mp3", format fp y] empty

run :: Shell ExitCode
run = do
    mktree "new"
    fs <- pwd >>= find musicPattern
    fs' <- addRandomPrep fs
    convert (fs, fs')
