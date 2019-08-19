{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Validation
import           Forge.Generate
import           Forge.Internal.Types
import           Forge.Lucid
import           Lucid
import           Test.Hspec

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
                \<input name=\"/r/\">")
           it
             "Input parsing"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/" (TextInput "5"))
                         (FieldForm (pure IntegerField)))))
                (Success 5))
           it
             "Missing input"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate @Lucid mempty (FieldForm (pure IntegerField)))))
                (Failure [MissingInput "/"]))
           it
             "Missing input [multiple]"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         mempty
                         (FieldForm (pure IntegerField) *>
                          FieldForm (pure TextField)))))
                (Failure [MissingInput "/l/m/", MissingInput "/r/"]))
           it
             "Invalid input type"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/" (FileInput ""))
                         (FieldForm (pure IntegerField) *>
                          FieldForm (pure TextField)))))
                (Failure
                   [ MissingInput (Key {unKey = "/l/m/"})
                   , MissingInput (Key {unKey = "/r/"})
                   ]))
           it
             "Invalid input format"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/" (TextInput "x"))
                         (FieldForm (pure IntegerField)))))
                (Failure [InvalidInputFormat "/" (TextInput "x")]))
           it
             "Form parsing"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/p/" (TextInput "6"))
                         (ParseForm
                            (\i ->
                               pure
                                 (if i > 5
                                    then Right (i * 2)
                                    else Left
                                           (InvalidInputFormat
                                              "/"
                                              (FileInput ""))))
                            (FieldForm (pure IntegerField))))))
                (Success 12))
           it
             "Form parsing fail"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/p/" (TextInput "5"))
                         (ParseForm
                            (\i ->
                               pure
                                 (if i > 5
                                    then Right i
                                    else Left
                                           (InvalidInputFormat
                                              "/"
                                              (FileInput ""))))
                            (FieldForm (pure IntegerField))))))
                (Failure [InvalidInputFormat (Key {unKey = "/"}) (FileInput "")]))))
