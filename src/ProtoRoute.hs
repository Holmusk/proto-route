module ProtoRoute
       ( main
       ) where

import Language.Haskell.Ghcid  (execStream, startGhci, stopGhci)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
    let f = \_ s -> putStrLn s
    curDir <- getCurrentDirectory
    (g, _) <- startGhci "ghci" (Just curDir) f
    let execStatement s = execStream g s f
    execStatement "import Data.Text"
    execStatement "import Data.ProtoLens.Encoding"
    execStatement ":load src/ProtoExports"
    execStatement "let sr = SearchRequest {_SearchRequest'query = pack \"test\"\
                           \, _SearchRequest'_unknownFields = ([])}"
    -- Value of generated msg, and its serialized representation
    execStatement "sr"
    execStatement "encodeMessage sr"
    stopGhci g
