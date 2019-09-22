{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients, testGroup )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified NonEmptyContainers.SeqConversions
import qualified NonEmptyContainers.SeqNE

--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients $
         testGroup "non-empty-containers"
                   [ NonEmptyContainers.SeqConversions.tests
                   , NonEmptyContainers.SeqNE.tests
                   ]

-- that's all, folks! ----------------------------------------------------------
