{-# LANGUAGE CPP #-}

-- | The limit on how many files the test process can have open at once.
module FileLimit (raiseFileLimit) where

#if !defined(mingw32_HOST_OS)
import           Control.Monad         (void)
import           System.Posix.Resource (Resource (..), ResourceLimit (..), ResourceLimits (..),
                                        getResourceLimit, setResourceLimit)
import           UnliftIO.Exception    (tryIO)
#endif

-- | Raise the soft limit on open files by @n@, or as far as the hard limit
-- allows. A limit that can't be raised is left as it is. On Windows this does
-- nothing.
raiseFileLimit :: Int -> IO ()
#if defined(mingw32_HOST_OS)
raiseFileLimit _ = return ()
#else
raiseFileLimit n = void . tryIO $ do
  limits <- getResourceLimit ResourceOpenFiles
  case softLimit limits of
    ResourceLimit soft -> do
      let wanted = soft + toInteger n
          raised = case hardLimit limits of
            ResourceLimit hard -> min hard wanted
            _ -> wanted
      setResourceLimit ResourceOpenFiles limits { softLimit = ResourceLimit raised }
    _ -> return ()
#endif
