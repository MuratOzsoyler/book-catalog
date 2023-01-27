module Frontend.UI where

import Prelude

import Common.BookProps (BookProps)
import Control.Monad.Except (runExceptT)
import Data.Array (foldl, notElem)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String (fromCodePointArray, length, toCodePointArray, toLower)
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (cb, (!:=))
import Deku.Attributes (klass_)
import Deku.Control (blank, text_, (<#~>))
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useHot)
import Deku.Listeners (click, textInput_)
import Effect.Aff (joinFiber, launchAff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Frontend.BookOperations (BookOpResult(..), getBook)
import Frontend.BookProps (collectBookProperties)
import Frontend.BookPropsInput (bookPropsInputScene)
import Frontend.UI.UIUtils (disabled, errorPanel, infoPanel, sameIdAndName, value)
import Network.RemoteData (RemoteData(..))
import Partial.Unsafe (unsafePartial)
import QualifiedDo.Alt as Alt

data ClickStatus
  = NotClicked
  | Clicked (RemoteData String (Either String BookProps /\ Either String BookOpResult))

data ISBNStatus
  = NotEntered
  | HasInvalidChar
  | LengthError
  | ChecksumError
  | Valid String

derive instance Generic ISBNStatus _

instance Show ISBNStatus where
  show i = genericShow i

bookPropsInputPage :: forall lock payload. Domable lock payload
bookPropsInputPage = Deku.do
  setISBN /\ iSBN <- useHot NotEntered
  setRemoteData /\ remoteData <- useHot NotClicked

  D.div (klass_ "form row")
    [ D.div (klass_ "col-sm-3") [ D.span (klass_ "h3 fw-bold") [ text_ "KİTAP KATALOĞU" ], D.span (klass_ "fw-bold") [ text_ "0.0.5.1" ] ]
    , D.fieldset (klass_ "col-sm-6")
        [ D.div (klass_ "input-group")
            [ D.input
                Alt.do
                  D.Xtype !:= "text"
                  klass_ "form-control"
                  D.Placeholder !:= "ISBN" -- Aranacak kitabın ISBN numarasını giriniz"
                  sameIdAndName "isbn"

                  D.Maxlength !:= "13"
                  D.Required !:= "required"
                  D.Pattern !:= """^\d*$"""
                  value $ iSBN
                    # filter
                        ( case _ of
                            NotEntered -> true
                            Valid _ -> true
                            _ -> false
                        )
                    <#> case _ of
                      Valid s -> s
                      _ -> ""
                  textInput_ \_v -> do
                    setRemoteData NotClicked
                    let v = toLower _v
                    Console.log $ "isbn: \"" <> v <> "\""
                    setISBN case length v of
                      0 -> NotEntered
                      10 ->
                        let
                          v9 = String.take 9 v
                          vx = String.drop 9 v
                        in
                          if notNumeric v9 || notNumeric vx && vx /= "x" then HasInvalidChar
                          else if check10 v then Valid v
                          else ChecksumError
                      13 ->
                        if notNumeric v then HasInvalidChar
                        else if check13 v then Valid v
                        else ChecksumError
                      _ -> LengthError
                []
            , D.button
                Alt.do
                  disabled $ not <<< isValidISBN <$> iSBN
                  klass_ "btn btn-primary btn-outline-secondary"
                  click $ iSBN <#> \_isbn -> cb $ const case _isbn of
                    Valid isbn -> do
                      let isbn13 = if String.length isbn /= 13 then isbn10to13 isbn else isbn
                      setISBN $ Valid isbn13
                      setRemoteData $ Clicked $ NotAsked
                      fbr <- launchAff $ sequential $ Tuple
                        <$> parallel (collectBookProperties isbn13)
                        <*> parallel (runExceptT $ getBook isbn13)
                      setRemoteData $ Clicked Loading
                      launchAff_ do
                        result <- joinFiber fbr
                        liftEffect $ setRemoteData $ Clicked $ Success result
                    _ -> pure unit

                [ text_ "Ara" ]
            ]
        ]
    , ((remoteData <#> \r i -> r /\ i) <*> iSBN) <#~> case _ of
        _ /\ NotEntered -> errorPanel "ISBN giriniz"
        _ /\ HasInvalidChar -> errorPanel
          """Hatalı karakterler girilmiş.\n
(Sadece 10 haneli ISBN'lerin en son hanesi 'x' ya da 'X' ile bitebilir.
Bunun dışında rakamdan başkası kullanılamaz.)"""
        _ /\ LengthError -> errorPanel """ISBN ya 10 ya da 13 haneli olmalıdır"""
        _ /\ ChecksumError -> errorPanel "ISBN hatalı. Girişinizi kontrol ediniz. Yanlış giriş yapmadıysanız bu bir ISBN olamaz."
        NotClicked /\ Valid _ -> blank
        Clicked NotAsked /\ _ -> infoPanel "Sorgulanıyor"
        Clicked Loading /\ _ -> infoPanel "İndiriyor"
        Clicked (Failure err) /\ _ -> errorPanel $ "ISBN sorgulanırken hata oluştu: " <> err
        Clicked (Success (Left collErr /\ Left getErr)) /\ _ -> errorPanel
          $ "Özellik seçenekleri alınırken/ayrıştırılırken hata: \"" <> collErr
              <> "\", Kitap sorgulanırken hata: \""
              <> getErr
              <> "\""
        Clicked (Success (Left collErr /\ _)) /\ _ -> errorPanel $ "Özellik seçenekleri alınırken/ayrıştırılırken hata: \"" <> collErr
        Clicked (Success (_ /\ Left getErr)) /\ _ -> errorPanel $ "\", Kitap sorgulanırken hata: \"" <> getErr
        Clicked (Success (Right props /\ Right bookProps)) /\ Valid isbn -> Deku.do
          D.div (D.Id !:= "result")
            [ bookPropsInputScene props
                ( case bookProps of
                    OK bp -> Just bp
                    _ -> Nothing
                )
                isbn
            ]
    ]
  where
  isValidISBN = case _ of
    Valid _ -> true
    _ -> false
  digitsStr = "0123456789"
  digits = toCodePointArray digitsStr
  notNumeric = any (_ `notElem` digits) <<< toCodePointArray
  check10 = toCodePointArray
    >>> flip foldl (Tuple 0 10)
      ( \(Tuple r i) cp ->
          let
            ch = fromCodePointArray [ cp ]
            dgt = unsafePartial $ fromJust $ case ch of
              "x" -> Just 10
              _ -> Int.fromString ch
            prod = dgt * i
            sm = r + prod
          in
            Tuple sm $ i - 1
      )
    >>> fst
    >>> (_ `mod` 11)
    >>> (_ == 0)
  check13 isbn =
    let
      isbn12 = String.take 12 isbn
      isbn13 = unsafePartial $ fromJust $ Int.fromString $ String.drop 12 isbn
    in
      isbn13ChkDgt isbn12 == isbn13
  isbn13ChkDgt isbn12 =
    toCodePointArray isbn12 #
      flip foldl (Tuple 0 1)
        ( \(Tuple r n) cp ->
            let
              dgt = unsafePartial $ fromJust $ Int.fromString $ fromCodePointArray [ cp ]
              prod = dgt * n
              sm = r + prod
            in
              Tuple sm $ (n + 2) `mod` 4
        )
        >>> fst
        >>> (_ `mod` 10)
        >>> (\chkd -> if chkd == 0 then 0 else 10 - chkd)
  isbn10to13 isbn =
    let
      isbn9 = String.take 9 isbn
      isbn12 = "978" <> isbn9
      chkDgt = isbn13ChkDgt isbn12
    in
      isbn12 <> String.take 1 (String.drop chkDgt digitsStr)
