{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Katip (Severity(DebugS, EmergencyS, InfoS), logTM)
import Katip.Config (runEnv')

main :: IO ()
main = runEnv' $ do
    $(logTM) DebugS "Debug message"
    $(logTM) InfoS "Info message"
    $(logTM) EmergencyS "Emergency message"
