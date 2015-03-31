{-|
Module      : Language.SAL
Description : Public API for sal-lang, a package for representing and generating
              SAL models and syntax.
Copyright   : (c) Galois Inc, 2015
License     : MIT
Maintainer  : Benjamin F Jones <bjones@galois.com>
Stability   : experimental
Portability : Yes

See "Language.SAL.Syntax" for details on the syntax representation.

TODO -- better package-wide description here
-}

module Language.SAL (module X) where

import Language.SAL.Syntax  as X
import Language.SAL.Pretty  as X
import Language.SAL.Helpers as X
