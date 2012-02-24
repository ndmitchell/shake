module Development.Shake.Report
       ( buildReport -- :: String -> FilePath -> IO ()
       ) where
import Text.StringTemplate as T

import System.FilePath
import Paths_shake


-- | Generates an HTML report given some build system
-- profiling data in JSON format.
buildReport :: String -> FilePath -> IO ()
buildReport json out = do
  htmlDir <- getDataFileName "html"
  report  <- readFile $ htmlDir </> "report.html"
  bcss    <- readFile $ htmlDir </> "bootstrap.min.css"
  brcss   <- readFile $ htmlDir </> "bootstrap-responsive.min.css"
  bjs     <- readFile $ htmlDir </> "bootstrap.min.js"
  jquery  <- readFile $ htmlDir </> "jquery-1.6.4.min.js"
  flot    <- readFile $ htmlDir </> "jquery.flot.min.js"
  shakejs <- readFile $ htmlDir </> "shake.js"

  let t = T.setAttribute "bootstrapcss" bcss
        $ T.setAttribute "bootstraprcss" brcss
        $ T.setAttribute "bootstrapjs" bjs
        $ T.setAttribute "jquery" jquery
        $ T.setAttribute "flot" flot
        $ T.setAttribute "shakejs" shakejs
        $ T.setAttribute "shakedump" json
        $ T.newSTMP report :: StringTemplate String
  writeFile out $ T.render t
