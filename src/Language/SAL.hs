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
-}

module Language.SAL
  ( -- * SAL Syntax representation
    module Language.SAL.Syntax
    -- * Pretty printer
  , module Language.SAL.Pretty
  ) where

import Language.SAL.Syntax
import Language.SAL.Pretty
