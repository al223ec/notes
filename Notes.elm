module Notes exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Note =
    { id : Int, body : String, timestamp : Int }


type alias NoteList =
    { notes : List Note }


init : () -> ( NoteList, Cmd Msg )
init _ =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 123 }
            , { id = 2, body = "Second note...", timestamp = 0 }
            , { id = 3, body = "Third note...", timestamp = 0 }
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> NoteList -> ( NoteList, Cmd Msg )
update msg noteList =
    ( noteList, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : NoteList -> Sub Msg
subscriptions noteList =
    Sub.none



-- VIEW


view : NoteList -> Html Msg
view noteList =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button" ] [ text "New" ]
            , button [ class "toolbar-button" ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search.. " ] []
            ]
        , div [ class "note-container" ]
            [ viewNoteSelectors noteList
            , div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text "Timestamp here..." ]
                , textarea [ class "note-editor-input" ] [ text "First note..." ]
                ]
            ]
        ]


viewNoteSelectors : NoteList -> Html Msg
viewNoteSelectors noteList =
    div [ class "note-selectors" ]
        (List.map (\note -> viewNoteSelector note) noteList.notes)


viewNoteSelector : Note -> Html Msg
viewNoteSelector note =
    div [ class "note-selector" ]
        [ p [ class "note-selector-title" ] [ text note.body ]
        , p [ class "note-selector-timestamp" ] [ text (String.fromInt note.timestamp) ]
        ]
