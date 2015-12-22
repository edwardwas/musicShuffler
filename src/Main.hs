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
    return $ directory fp </> decodeString (rs <> "_" <> encodeString (filename fp))
        
musicPattern ::  Pattern Text
musicPattern = choice $ 
    map (suffix . text . mappend "." ) musicExtensions

isRandomPref :: Pattern Text
isRandomPref = mappend musicPattern $ T.pack <$> prefix ((count 5 lower) <> (pure <$> char '_')) 

main = putStrLn "Not doen yet"
