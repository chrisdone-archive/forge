{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Validation
import           Forge.Generate
import           Forge.Internal.Types
import           Forge.Lucid
import           Forge.Verify
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
                         (verified
                            ((,) <$>
                             FieldForm DynamicFieldName (pure IntegerField) <*>
                             FieldForm DynamicFieldName (pure TextField))))))
                "<input name=\"/l/m/\" type=\"number\"><input name=\"/r/\">")
           it
             "View named"
             (shouldBe
                (renderText
                   (runIdentity
                      (view
                         @LucidUnverified
                         $$($$(verify
                                 [|| let namedForm :: Form LucidUnverified (Integer, Text)
                                         namedForm = ((,) <$>
                                           FieldForm (StaticFieldName "foo") (pure IntegerField) <*>
                                           FieldForm (StaticFieldName "bar") (pure TextField))
                                     in namedForm
                                  ||])))))
                "<input name=\"foo\" type=\"number\"><input name=\"bar\">")
           it
             "Sequence"
             (shouldBe
                (renderText
                   (runIdentity
                      (view
                         @Lucid
                         (verified
                            ((,) <$>
                             traverse
                               (const
                                  (FieldForm
                                     DynamicFieldName
                                     (pure IntegerField)))
                               [1 :: Int .. 3] <*>
                             FieldForm DynamicFieldName (pure TextField))))))
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
                         (verified
                            (FieldForm DynamicFieldName (pure IntegerField))))))
                (Success 5))
           it
             "Missing input"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         mempty
                         (verified
                            (FieldForm DynamicFieldName (pure IntegerField))))))
                (Failure [MissingInput "/"]))
           it
             "Missing input [multiple]"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         mempty
                         (verified
                            (FieldForm DynamicFieldName (pure IntegerField) *>
                             FieldForm DynamicFieldName (pure TextField))))))
                (Failure [MissingInput "/l/m/", MissingInput "/r/"]))
           it
             "Invalid input type"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/" (FileInput ""))
                         (verified
                            (FieldForm DynamicFieldName (pure IntegerField) *>
                             FieldForm DynamicFieldName (pure TextField))))))
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
                         (verified
                            (FieldForm DynamicFieldName (pure IntegerField))))))
                (Failure [InvalidInputFormat "/" (TextInput "x")]))
           it
             "Form parsing"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/p/" (TextInput "6"))
                         (verified
                            (ParseForm
                               (\i ->
                                  pure
                                    (if i > 5
                                       then Right (i * 2)
                                       else Left
                                              (InvalidInputFormat
                                                 "/"
                                                 (FileInput ""))))
                               (FieldForm DynamicFieldName (pure IntegerField)))))))
                (Success 12))
           it
             "Form parsing fail"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @Lucid
                         (M.singleton "/p/" (TextInput "5"))
                         (verified
                            (ParseForm
                               (\i ->
                                  pure
                                    (if i > 5
                                       then Right i
                                       else Left
                                              (InvalidInputFormat
                                                 "/"
                                                 (FileInput ""))))
                               (FieldForm DynamicFieldName (pure IntegerField)))))))
                (Failure [InvalidInputFormat (Key {unKey = "/"}) (FileInput "")]))))
