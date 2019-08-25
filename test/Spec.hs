{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
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
       (do thTests
           it
             "View"
             (shouldBe
                (renderText
                   (view
                      @'Verified
                      @_
                      @(Html ())
                      @Field
                      @Error
                      (verified
                         ((,) <$> FieldForm DynamicFieldName IntegerField <*>
                          FieldForm DynamicFieldName TextField))))
                "<input name=\"/l/m/\" type=\"number\"><input name=\"/r/\">")
           it
             "Sequence"
             (shouldBe
                (renderText
                   (view
                      @'Verified
                      @_
                      @(Html ())
                      @Field
                      @Error
                      (verified
                         ((,) <$>
                          traverse
                            (const (FieldForm DynamicFieldName IntegerField))
                            [1 :: Int .. 3] <*>
                          FieldForm DynamicFieldName TextField))))
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
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
                         (M.singleton "/" (TextInput "5"))
                         (verified (FieldForm DynamicFieldName IntegerField)))))
                (Success 5))
           it
             "Missing input"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
                         mempty
                         (verified (FieldForm DynamicFieldName IntegerField)))))
                (Failure [MissingInput "/"]))
           it
             "Missing input [multiple]"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
                         mempty
                         (verified
                            (FieldForm DynamicFieldName IntegerField *>
                             FieldForm DynamicFieldName TextField)))))
                (Failure [MissingInput "/l/m/", MissingInput "/r/"]))
           it
             "Invalid input type"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
                         (M.singleton "/" (FileInput ""))
                         (verified
                            (FieldForm DynamicFieldName IntegerField *>
                             FieldForm DynamicFieldName TextField)))))
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
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
                         (M.singleton "/" (TextInput "x"))
                         (verified (FieldForm DynamicFieldName IntegerField)))))
                (Failure [InvalidInputFormat "/" (TextInput "x")]))
           it
             "Form parsing"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
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
                               (FieldForm DynamicFieldName IntegerField))))))
                (Success 12))
           it
             "Floor"
             (shouldBe
                (renderText
                   (generatedView
                      (runIdentity
                         (generate
                            @'Verified
                            @Identity
                            @(Html ())
                            @Field
                            @Error
                            (M.singleton "/p/f/" (TextInput "1"))
                            (verified
                               (ParseForm
                                  (\i ->
                                     pure
                                       (if i > 5
                                          then Right (i * 2)
                                          else Left
                                                 (InvalidInputFormat
                                                    "/wibble"
                                                    (FileInput ""))))
                                  (FloorForm
                                     (\merr v ->
                                        ( v <>
                                          case merr of
                                            Nothing -> mempty
                                            Just err ->
                                              case err of
                                                InvalidInputFormat {} ->
                                                  p_ "invalid input format"
                                                MissingInput {} ->
                                                  p_ "missing input!"
                                        , Nothing))
                                     (FieldForm DynamicFieldName IntegerField))))))))
                "<input name=\"/p/f/\" type=\"number\"><p>invalid input format</p>")
           it
             "Ceiling"
             (shouldBe
                (renderText
                   (generatedView
                      (runIdentity
                         (generate
                            @'Verified
                            @Identity
                            @(Html ())
                            @Field
                            @Error
                            (M.singleton "/c/p/" (TextInput "1"))
                            (verified
                               (CeilingForm
                                  (\merr v ->
                                     ( v <>
                                       ul_
                                         (mapM_
                                            (\err ->
                                               case err of
                                                 InvalidInputFormat {} ->
                                                   li_ "invalid input format"
                                                 MissingInput {} ->
                                                   li_ "missing input!")
                                            merr)
                                     , []))
                                  (ParseForm
                                     (\i ->
                                        pure
                                          (if i > 5
                                             then Right (i * 2)
                                             else Left
                                                    (InvalidInputFormat
                                                       "/wibble"
                                                       (FileInput ""))))
                                     (FieldForm DynamicFieldName IntegerField))))))))
                "<input name=\"/c/p/\" type=\"number\"><ul><li>invalid input format</li></ul>")
           it
             "Form parsing fail"
             (shouldBe
                (generatedValue
                   (runIdentity
                      (generate
                         @'Verified
                         @Identity
                         @(Html ())
                         @Field
                         @Error
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
                               (FieldForm DynamicFieldName IntegerField))))))
                (Failure [InvalidInputFormat (Key {unKey = "/"}) (FileInput "")]))))

thTests :: Spec
thTests =
           it
             "View named"
             (shouldBe
                (renderText
                   (
                      (view @'Unverified @_ @(Html ()) @Field @Error
                         $$($$(verify
                                 [|| let namedForm = ((,) <$>
                                           FieldForm (StaticFieldName "foo") IntegerField <*>
                                           FieldForm (StaticFieldName "bar") TextField)
                                     in namedForm
                                  ||])))))
                "<input name=\"foo\" type=\"number\"><input name=\"bar\">")
