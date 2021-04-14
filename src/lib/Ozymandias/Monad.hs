{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ozymandias.Monad
  ( -- * The @Oz@ monad
    Oz (..),
    runOz,
    problem,
    catch,

    -- * Re-exports
    liftIO,
  )
where

import qualified Control.Exception as E
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Ozymandias.Problem

-- | A type for actions which have effects and might fail with a
-- 'Problem'.
newtype Oz a = Oz (ExceptT Problem IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an @Oz@ action.
runOz :: Oz a -> IO (Either Problem a)
runOz (Oz ma) = runExceptT ma

-- | Terminate a computation early with a 'Problem'.
problem :: Problem -> Oz a
problem = Oz . throwE

-- | Catch an @IO@ exception
catch :: E.Exception e => Oz a -> (e -> Oz a) -> Oz a
catch ma handler = Oz . ExceptT $ runOz ma `E.catch` (runOz . handler)
