{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Validation
import           Forge.Generate
import           Forge.Internal.Types
import           Forge.Lucid
import           Forge.Verify
import           Lucid
import           Prelude hiding (floor, ceiling)
import           Test.Hspec

data MyError
  = PasswordsMismatch Text Text
  | LucidError Error

data MyError2
  = LucidError2 Error
  | NumberTooLow Integer

instance FormError MyError where
  missingInputError = LucidError . missingInputError
  invalidInputFormat k = LucidError . invalidInputFormat k

instance FormError MyError2 where
  missingInputError = LucidError2 . missingInputError
  invalidInputFormat k = LucidError2 . invalidInputFormat k

main :: IO ()
main =
  hspec
    (describe
       "Lucid"
       (do thTests
           simpleView
           missingInputs
           invalidInputs
           nameStability
           floor
           ceiling
           parseFail
           multiples))

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

missingInputs :: Spec
missingInputs = do
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

invalidInputs :: Spec
invalidInputs = do it
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

inputParsing :: Spec
inputParsing = do
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

floor :: Spec
floor =
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
                   @MyError
                   (M.fromList
                      [ ("/p/l/m/f/e/", (TextInput "letmein"))
                      , ("/p/r/f/e/", (TextInput "letmein!"))
                      ])
                   (verified
                      (ParseForm
                         (\(a, b) ->
                            pure
                              (if a == b
                                 then Right a
                                 else Left (PasswordsMismatch a b)))
                         (let flooring =
                                FloorForm
                                  (\merr v ->
                                     ( v <>
                                       case merr of
                                         Nothing -> mempty
                                         Just err ->
                                           case err of
                                             PasswordsMismatch _ _ ->
                                               p_ "passwords do not match"
                                             LucidError er ->
                                               case er of
                                                 InvalidInputFormat {} ->
                                                   p_ "invalid input format"
                                                 MissingInput {} ->
                                                   p_ "missing input!"
                                     , Nothing))
                           in (((,) <$>
                                flooring
                                  (MapErrorForm
                                     LucidError
                                     (FieldForm DynamicFieldName TextField)) <*>
                                flooring
                                  (MapErrorForm
                                     LucidError
                                     (FieldForm DynamicFieldName TextField)))))))))))
       "<input value=\"letmein\" name=\"/p/l/m/f/e/\"><p>passwords do not match</p><input value=\"letmein!\" name=\"/p/r/f/e/\"><p>passwords do not match</p>")

parseFail :: Spec
parseFail =
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
                              else Left (InvalidInputFormat "/" (FileInput ""))))
                      (FieldForm DynamicFieldName IntegerField))))))
       (Failure [InvalidInputFormat (Key {unKey = "/"}) (FileInput "")]))

ceiling :: Spec
ceiling =
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
                   @MyError2
                   (M.singleton "/c/p/e/" (TextInput "1"))
                   (verified
                      (CeilingForm
                         (\merr v ->
                            ( v <>
                              ul_
                                (mapM_
                                   (\err ->
                                      case err of
                                        NumberTooLow _ -> li_ "number too low!"
                                        LucidError2 er ->
                                          case er of
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
                                    else Left (NumberTooLow i)))
                            (MapErrorForm
                               LucidError2
                               (FieldForm DynamicFieldName IntegerField)))))))))
       "<input value=\"1\" name=\"/c/p/e/\" type=\"number\"><ul><li>number too low!</li></ul>")

nameStability :: Spec
nameStability =
  describe
    "Name stability"
    (do it
          "Sequence 1"
          (shouldBe
             (renderText
                (view
                   @'Verified
                   @_
                   @(Html ())
                   @Field
                   @Error
                   (verified
                      ((,,) <$> FieldForm DynamicFieldName TextField <*>
                       traverse
                         (const (FieldForm DynamicFieldName IntegerField))
                         [1 :: Int .. 1] <*>
                       FieldForm DynamicFieldName TextField))))
             "<input name=\"/l/l/m/\">\
                           \<input name=\"/l/r/l/m/\" type=\"number\">\
                           \<input name=\"/r/\">")
        it
          "Sequence 9"
          (shouldBe
             (renderText
                (view
                   @'Verified
                   @_
                   @(Html ())
                   @Field
                   @Error
                   (verified
                      ((,,) <$> FieldForm DynamicFieldName TextField <*>
                       traverse
                         (const (FieldForm DynamicFieldName IntegerField))
                         [1 :: Int .. 9] <*>
                       FieldForm DynamicFieldName TextField))))
             "<input name=\"/l/l/m/\">\
                           \<input name=\"/l/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/l/r/r/r/r/r/r/r/r/r/l/m/\" type=\"number\">\
                           \<input name=\"/r/\">"))

simpleView :: SpecWith ()
simpleView =
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

multiples :: Spec
multiples =
  describe
    "Multiples"
    (do it
          "Empty"
          (shouldBe
             (renderText
                (view @'Verified @_ @(Html ()) @Field @Error basicNumericState))
             "<input name=\"/s/m/\" type=\"number\">")
        it
          "Missing input"
          (shouldBe
             ((\Generated {generatedView, generatedValue} ->
                 Generated
                   {generatedValue, generatedView = renderText generatedView})
                (runIdentity
                   (generate
                      @'Verified
                      @Identity
                      @(Html ())
                      @Field
                      @Error
                      (M.fromList [("/s/m/", TextInput "2")])
                      basicNumericState)))
             (Generated
                { generatedView =
                    mconcat
                      [ "<input value=\"2\" name=\"/s/m/\" type=\"number\">" -- set
                      , "<input name=\"/i/1/l/m/\" type=\"number\"><input name=\"/i/1/r/\">" -- 1
                      , "<input name=\"/i/2/l/m/\" type=\"number\"><input name=\"/i/2/r/\">" -- 2
                      ]
                , generatedValue =
                    Failure
                      [ MissingInput (Key {unKey = "/i/1/l/m/"})
                      , MissingInput (Key {unKey = "/i/1/r/"})
                      , MissingInput (Key {unKey = "/i/2/l/m/"})
                      , MissingInput (Key {unKey = "/i/2/r/"})
                      ]
                }))
        it
          "Fully satisfied inputs"
          (shouldBe
             ((\Generated {generatedView, generatedValue} ->
                 Generated
                   {generatedValue, generatedView = renderText generatedView})
                (runIdentity
                   (generate
                      @'Verified
                      @Identity
                      @(Html ())
                      @Field
                      @Error
                      (M.fromList
                         [ ("/s/m/", TextInput "2")
                         , ("/i/1/l/m/", TextInput "666")
                         , ("/i/1/r/", TextInput "Hello!")
                         , ("/i/2/l/m/", TextInput "123")
                         , ("/i/2/r/", TextInput "World!")
                         ])
                      basicNumericState)))
             (Generated
                { generatedView =
                    "<input value=\"2\" name=\"/s/m/\" type=\"number\">\
                    \<input value=\"666\" name=\"/i/1/l/m/\" type=\"number\">\
                    \<input value=\"Hello!\" name=\"/i/1/r/\">\
                    \<input value=\"123\" name=\"/i/2/l/m/\" type=\"number\">\
                    \<input value=\"World!\" name=\"/i/2/r/\">"
                , generatedValue = Success [(666, "Hello!"), (123, "World!")]
                })))
  where
    basicNumericState =
      verified
        (ManyForm
           (\setView views -> setView <> mconcat views)
           (fmap
              (Set.fromList . enumFromTo 1)
              -- Above: We generate an ordered list here. However:
              -- this input could be a TextField producing an
              -- [Integer] value, thereby allowing the client-side to
              -- delete with random access, or re-order formlets, etc.
              (FieldForm DynamicFieldName IntegerField))
           ((,) <$> FieldForm DynamicFieldName IntegerField <*>
            FieldForm DynamicFieldName TextField))
