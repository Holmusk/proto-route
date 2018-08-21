module ProtoRoute.Ghcid
    ( runGhci
    ) where

import Language.Haskell.Ghcid (execStream, startGhci, stopGhci)
import System.Directory (getCurrentDirectory)

-- Should runGhci have the following type signature and start off like so?
runGhci :: String -> IO ()
runGhci stuff = do
    let f _ = putStrLn
    curDir <- getCurrentDirectory
    (g, _) <- startGhci "ghci" (Just curDir) f
    let runCmd s = execStream g s f
    runCmd ":set -XOverloadedStrings"
    runCmd "import Data.ProtoLens.Encoding"
    runCmd ":load src/ProtoExports"
    runCmd ("let testMsg = " ++ stuff)
    runCmd "encodeMessage testMsg"
    stopGhci g
