{-# LANGUAGE OverloadedRecordDot #-}

module Service.Output (
    writeToCsv
) where
import Import
import Data.Csv
import RIO.File
import qualified RIO.ByteString.Lazy as BL

writeToCsv :: FilePath -> [Jep] -> RIO env ()
writeToCsv outputFilePath jeps = do
    
    let csvEncoded = BL.toStrict $ encodeWith csvEncodeOption jeps
    
    writeBinaryFile outputFilePath csvEncoded

    where
        csvEncodeOption :: EncodeOptions
        csvEncodeOption = defaultEncodeOptions{encQuoting = QuoteAll}
    
























































































































