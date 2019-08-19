{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Forge.Generate
import Forge.Lucid
import Test.Hspec
import Data.Functor.Identity
import Lucid
import Forge.Internal.Types

main :: IO ()
main =
  hspec
    (describe
       "Lucid"
       (do it
             "View"
             (shouldBe
                (renderText
                   (runIdentity
                      (view
                         @Lucid
                         ((,) <$> FieldForm (pure IntegerField) <*>
                          FieldForm (pure TextField)))))
                "<input name=\"/l/m/\" type=\"number\"><input name=\"/r/\">")))
