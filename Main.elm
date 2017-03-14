import Html exposing (..)
import Html.Attributes exposing (style)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Int


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Something


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Something ->
      (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  Html.div
          [ style
              [ ( "position", "relative" )
              , ( "paddingTop", "20px" )
              , ( "marginLeft", "20px" )
              , ( "width", "200px" )
              , ( "height", "auto" )
              ]
          ]
          [
            Html.div [][Html.text ("hello " ++ (toString model))]
          ]
