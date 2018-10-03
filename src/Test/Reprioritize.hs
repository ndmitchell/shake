
module Test.Reprioritize(main) where

import Development.Shake
import Test.Type


main = shakeTest_ test $ do
    file <- newResource "log.txt" 1
    let log x = withResource file 1 $ liftIO $ appendFile "log.txt" x
    "*.p0" %> \out -> do
        log "0"
        writeFile' out ""
    "*.p1" %> \out -> do
        reprioritize 1
        log "1"
        writeFile' out ""
    "*.p2" %> \out -> do
        reprioritize 2
        log "2"
        writeFile' out ""


test build = do
    build ["clean"]
    build ["foo.p1","bar.p1","baz.p0","qux.p2"]
    assertContents "log.txt" "0211"
