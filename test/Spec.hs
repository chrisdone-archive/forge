{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
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
                                           FieldForm (StaticFieldName "foo") (IntegerField Nothing) <*>
                                           FieldForm (StaticFieldName "bar") (TextField Nothing))
                                     in namedForm
                                  ||])))))
                "<input pattern=\"[0-9]*\" name=\"foo\" type=\"text\"><input name=\"bar\">")

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
                (verified (FieldForm DynamicFieldName (IntegerField Nothing))))))
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
                   (FieldForm DynamicFieldName (IntegerField Nothing) *>
                    FieldForm DynamicFieldName (TextField Nothing))))))
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
                                 (M.singleton "/" (pure (FileInput "")))
                                 (verified
                                    (FieldForm DynamicFieldName (IntegerField Nothing) *>
                                     FieldForm DynamicFieldName (TextField Nothing))))))
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
                                 (M.singleton "/" (pure (TextInput "x")))
                                 (verified (FieldForm DynamicFieldName (IntegerField Nothing))))))
                        (Failure [InvalidInputFormat "/" (pure (TextInput "x"))]))

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
                (M.singleton "/" (pure (TextInput "5")))
                (verified (FieldForm DynamicFieldName (IntegerField Nothing))))))
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
                (M.singleton "/p/" (pure (TextInput "6")))
                (verified
                   (ParseForm
                      (\i ->
                         pure
                           (if i > 5
                              then Right (i * 2)
                              else Left
                                     (InvalidInputFormat
                                        "/"
                                        (pure (FileInput "")))))
                      (FieldForm DynamicFieldName (IntegerField Nothing)))))))
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
                      [ ("/p/l/m/f/e/", (pure (TextInput "letmein")))
                      , ("/p/r/f/e/", (pure (TextInput "letmein!")))
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
                                                 InvalidMultiselectKey {} ->
                                                   p_ "Invalid multiselect"
                                     , Nothing))
                           in (((,) <$>
                                flooring
                                  (MapErrorForm
                                     LucidError
                                     (FieldForm
                                        DynamicFieldName
                                        (TextField Nothing))) <*>
                                flooring
                                  (MapErrorForm
                                     LucidError
                                     (FieldForm
                                        DynamicFieldName
                                        (TextField Nothing))))))))))))
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
                (M.singleton "/p/" (pure (TextInput "5")))
                (verified
                   (ParseForm
                      (\i ->
                         pure
                           (if i > 5
                              then Right i
                              else Left (InvalidInputFormat "/" (pure (FileInput "")))))
                      (FieldForm DynamicFieldName (IntegerField Nothing)))))))
       (Failure [InvalidInputFormat (Key {unKey = "/"}) (pure (FileInput ""))]))

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
                   (M.singleton "/c/p/e/" (pure (TextInput "1")))
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
                                              li_ "missing input!"
                                            InvalidMultiselectKey {} ->
                                              p_ "Invalid multiselect")
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
                               (FieldForm DynamicFieldName (IntegerField Nothing))))))))))
       "<input pattern=\"[0-9]*\" value=\"1\" name=\"/c/p/e/\" type=\"text\"><ul><li>number too low!</li></ul>")

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
                      ((,,) <$> FieldForm DynamicFieldName (TextField Nothing) <*>
                       traverse
                         (const (FieldForm DynamicFieldName (IntegerField Nothing)))
                         [1 :: Int .. 1] <*>
                       FieldForm DynamicFieldName (TextField Nothing)))))
             "<input name=\"/l/l/m/\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/l/m/\" type=\"text\">\
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
                      ((,,) <$> FieldForm DynamicFieldName (TextField Nothing) <*>
                       traverse
                         (const (FieldForm DynamicFieldName (IntegerField Nothing)))
                         [1 :: Int .. 9] <*>
                       FieldForm DynamicFieldName (TextField Nothing)))))
             "<input name=\"/l/l/m/\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/r/r/r/r/l/m/\" type=\"text\">\
                           \<input pattern=\"[0-9]*\" name=\"/l/r/r/r/r/r/r/r/r/r/l/m/\" type=\"text\">\
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
                ((,) <$> FieldForm DynamicFieldName (IntegerField Nothing) <*>
                 FieldForm DynamicFieldName (TextField Nothing)))))
       "<input pattern=\"[0-9]*\" name=\"/l/m/\" type=\"text\"><input name=\"/r/\">")

multiples :: Spec
multiples =
  describe
    "Multiples"
    (do it
          "Empty"
          (shouldBe
             (renderText
                (view
                   @'Verified
                   @_
                   @(Html ())
                   @Field
                   @Error
                   (basicNumericState mempty)))
             "<input pattern=\"[0-9]*\" name=\"/s/m/\" type=\"text\">")
        it
          "Empty with defaults"
          (shouldBe
             (renderText
                (view
                   @'Verified
                   @_
                   @(Html ())
                   @Field
                   @Error
                   (basicNumericState [(123, "foo")])))
             "<input pattern=\"[0-9]*\" name=\"/s/m/\" type=\"text\">\
             \<input pattern=\"[0-9]*\" value=\"123\" name=\"/i/1/l/m/\" type=\"text\">\
             \<input value=\"foo\" name=\"/i/1/r/\">")
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
                      (M.fromList [("/s/m/", pure (TextInput "2"))])
                      (basicNumericState mempty))))
             (Generated
                { generatedView =
                    mconcat
                      [ "<input pattern=\"[0-9]*\" value=\"2\" name=\"/s/m/\" type=\"text\">" -- set
                      , "<input pattern=\"[0-9]*\" name=\"/i/1/l/m/\" type=\"text\"><input name=\"/i/1/r/\">" -- 1
                      , "<input pattern=\"[0-9]*\" name=\"/i/2/l/m/\" type=\"text\"><input name=\"/i/2/r/\">" -- 2
                      ]
                , generatedValue =
                    Failure
                      [ MissingInput (Key {unKey = "/i/1/l/m/"})
                      , MissingInput (Key {unKey = "/i/1/r/"})
                      , MissingInput (Key {unKey = "/i/2/l/m/"})
                      , MissingInput (Key {unKey = "/i/2/r/"})
                      ]
                }))
        let fullySatisfied defaults =
              it
                ("Fully satisfied inputs, defaults = " ++ show defaults)
                (shouldBe
                   ((\Generated {generatedView, generatedValue} ->
                       Generated
                         { generatedValue
                         , generatedView = renderText generatedView
                         })
                      (runIdentity
                         (generate
                            @'Verified
                            @Identity
                            @(Html ())
                            @Field
                            @Error
                            (M.fromList
                               [ ("/s/m/", pure (TextInput "2"))
                               , ("/i/1/l/m/", pure (TextInput "666"))
                               , ("/i/1/r/", pure (TextInput "Hello!"))
                               , ("/i/2/l/m/", pure (TextInput "123"))
                               , ("/i/2/r/", pure (TextInput "World!"))
                               ])
                            (basicNumericState defaults))))
                   (Generated
                      { generatedView =
                          "<input pattern=\"[0-9]*\" value=\"2\" name=\"/s/m/\" type=\"text\">\
                          \<input pattern=\"[0-9]*\" value=\"666\" name=\"/i/1/l/m/\" type=\"text\">\
                          \<input value=\"Hello!\" name=\"/i/1/r/\">\
                          \<input pattern=\"[0-9]*\" value=\"123\" name=\"/i/2/l/m/\" type=\"text\">\
                          \<input value=\"World!\" name=\"/i/2/r/\">"
                      , generatedValue =
                          Success [(666, "Hello!"), (123, "World!")]
                      }))
        fullySatisfied mempty
        fullySatisfied (pure (123, "foo")))
  where
    basicNumericState defaults =
      verified
        (ManyForm
           (\setView views -> setView <> mconcat views)
           (fmap
              (enumFromTo 1)
              -- Above: We generate an ordered list here. However:
              -- this input could be a (TextField Nothing) producing an
              -- [Integer] value, thereby allowing the client-side to
              -- delete with random access, or re-order formlets, etc.
              (FieldForm DynamicFieldName (IntegerField Nothing)))
           (\mdefault ->
              ((,) <$>
               FieldForm DynamicFieldName (IntegerField (fmap fst mdefault)) <*>
               FieldForm DynamicFieldName (TextField (fmap snd mdefault))))
           defaults)
