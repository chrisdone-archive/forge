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
                "<input name=\"/l/m/\" type=\"number\"><input name=\"/r/\">")
           it
             "Sequence"
             (shouldBe
                (renderText
                   (runIdentity
                      (view
                         @Lucid
                         ((,) <$>
                          traverse
                            (const (FieldForm (pure IntegerField)))
                            [1 :: Int .. 3] <*>
                          FieldForm (pure TextField)))))
                "<input name=\"/l/m/l/m/\" type=\"number\">\
                \<input name=\"/l/m/r/l/m/\" type=\"number\">\
                \<input name=\"/l/m/r/r/l/m/\" type=\"number\">\
                \<input name=\"/r/\">")))
