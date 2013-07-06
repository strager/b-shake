
-- | The main entry point that calls all the default rules
module B.Shake.Shake(shake) where

import B.Shake.Types
import B.Shake.Timing
import B.Shake.Core

import B.Shake.Directory
import B.Shake.File
import B.Shake.Rerun


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build. The function will throw
--   an exception if the build fails.
--
--   To use command line flags to modify 'ShakeOptions' see 'Development.Shake.shakeArgs'.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts r = do
    addTiming "Function shake"
    run opts $ do
        r
        defaultRuleFile
        defaultRuleDirectory
        defaultRuleRerun
    return ()
