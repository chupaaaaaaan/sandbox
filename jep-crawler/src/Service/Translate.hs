{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Translate (
    translate2Ja
) where

import RIO.Process
import Import
import Util

translate2Ja :: (HasLogFunc env, HasProcessContext env) => [Jep] -> RIO env [Jep]
translate2Ja jeps = forM jeps $ \jep -> do
    deeplResponseBody <- translate "JA" jep.summary
    let summaryJa = (\t -> t.text) <$> deeplResponseBody.translations
    return jep{summaryJa = summaryJa}
    

