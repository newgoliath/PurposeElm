-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Browser
import Html exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , early_time: Time.Posix
  , counter: Int
  , offset: Int
  , fast_time: Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc
    (Time.millisToPosix 0)
    (Time.millisToPosix 0)
    0
    0
    (Time.millisToPosix 0)
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime

                , early_time = (
                    Time.millisToPosix (
                      (Time.posixToMillis newTime) + 1000
                    )
                  )
                , counter = model.counter + 1
                , offset = 1000 * (
                      0.5 * toFloat(model.counter + 1) |> ceiling
                    )

                , fast_time = (
                    Time.millisToPosix (
                        (Time.posixToMillis newTime) + model.offset
                      )
                  )
                }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    second = String.fromInt (Time.toSecond model.zone model.time)
    early_second = String.fromInt (Time.toSecond model.zone (model.early_time))
    fast_second = String.fromInt (Time.toSecond model.zone (model.fast_time))
  in
  h1 [] [ text (
    "second: " ++ second
    ++ " early_second: "   ++ early_second
    ++ " counter: " ++ String.fromInt model.counter
    ++ " fast_second: " ++ fast_second
    ) ]
