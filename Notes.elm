module Notes exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time


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
    { notes : List Note, selectedNoteId : Int, searchNoteText : String }


init : () -> ( NoteList, Cmd Msg )
init _ =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 1 }
            , { id = 2, body = "Second note...", timestamp = 23430 }
            , { id = 3, body = "Third note...", timestamp = 4983490228633471814 }
            ]
      , selectedNoteId = 1
      , searchNoteText = ""
      }
    , Task.perform InitializeNotesTimestamps Time.now
    )



-- UPDATE


type Msg
    = InitializeNotesTimestamps Time.Posix
    | SelectNote Int
    | UpdateSelectedNoteBody String
    | UpdateSelectedNoteTimestamp Time.Posix
    | ClickNew
    | CreateNote Time.Posix
    | ClickDelete
    | InputSearch String


update : Msg -> NoteList -> ( NoteList, Cmd Msg )
update msg noteList =
    case msg of
        InitializeNotesTimestamps time ->
            ( { noteList | notes = List.map (\note -> { note | timestamp = Time.posixToMillis time }) noteList.notes }
            , Cmd.none
            )

        SelectNote id ->
            ( { noteList | selectedNoteId = id }, Cmd.none )

        UpdateSelectedNoteBody newText ->
            case getSelectedNote noteList of
                Nothing ->
                    ( noteList, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == noteList.selectedNoteId then
                                { note | body = newText }
                            else
                                note

                        newNotes =
                            List.map updateSelectedNote noteList.notes
                    in
                        ( { noteList | notes = newNotes }
                        , Task.perform UpdateSelectedNoteTimestamp Time.now
                        )

        UpdateSelectedNoteTimestamp newTime ->
            case getSelectedNote noteList of
                Nothing ->
                    ( noteList, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == noteList.selectedNoteId then
                                { note | timestamp = Time.posixToMillis newTime }
                            else
                                note

                        newNotes =
                            List.map updateSelectedNote noteList.notes
                    in
                        ( { noteList | notes = newNotes }, Cmd.none )

        ClickNew ->
            ( noteList, Time.now |> Task.perform CreateNote )

        CreateNote newTime ->
            let
                newTimestamp =
                    Time.posixToMillis newTime

                newId =
                    newTimestamp
            in
                ( { noteList
                    | notes = [ { id = newId, body = "", timestamp = newTimestamp } ] ++ noteList.notes
                    , selectedNoteId = newId
                  }
                , Cmd.none
                )

        ClickDelete ->
            let
                newNotes =
                    List.filter (\note -> note.id /= noteList.selectedNoteId) noteList.notes

                firstVisibleNote =
                    getFirstVisibleNote newNotes noteList.searchNoteText
            in
                case firstVisibleNote of
                    Nothing ->
                        ( { noteList | notes = newNotes }, Cmd.none )

                    Just availableNote ->
                        ( { noteList | notes = newNotes, selectedNoteId = availableNote.id }, Cmd.none )

        InputSearch searchNoteText ->
            let
                firstVisibleNote =
                    getFirstVisibleNote noteList.notes searchNoteText
            in
                case firstVisibleNote of
                    Nothing ->
                        ( { noteList | searchNoteText = searchNoteText, selectedNoteId = -1 }, Cmd.none )

                    Just availableNote ->
                        ( { noteList | searchNoteText = searchNoteText, selectedNoteId = availableNote.id }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : NoteList -> Sub Msg
subscriptions noteList =
    Sub.none



-- VIEW


view : NoteList -> Html Msg
view noteList =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button", onClick ClickNew ] [ text "New" ]
            , button [ class "toolbar-button", onClick ClickDelete ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search.. ", onInput InputSearch ] []
            ]
        , div [ class "note-container" ]
            [ viewNoteSelectors noteList
            , viewNoteEditor noteList
            ]
        ]


viewNoteSelectors : NoteList -> Html Msg
viewNoteSelectors noteList =
    div [ class "note-selectors" ]
        (noteList.notes
            |> transformNotes noteList.searchNoteText
            |> List.map (\note -> viewNoteSelector note noteList.selectedNoteId)
        )


viewNoteSelector : Note -> Int -> Html Msg
viewNoteSelector note selectedNoteId =
    div [ classList [ ( "note-selector", True ), ( "active", note.id == selectedNoteId ) ], onClick (SelectNote note.id) ]
        [ p [ class "note-selector-title" ] [ text (formatTitle note.body) ]
        , p [ class "note-selector-timestamp" ] [ text (formatTimeStamp note.timestamp) ]
        ]


viewNoteEditor : NoteList -> Html Msg
viewNoteEditor noteList =
    case getSelectedNote noteList of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text (formatTimeStamp selectedNote.timestamp) ]
                , textarea [ class "note-editor-input", onInput UpdateSelectedNoteBody, value selectedNote.body ] []
                ]



-- HELPERS


getFirstVisibleNote : List Note -> String -> Maybe Note
getFirstVisibleNote notes searchText =
    notes
        |> transformNotes searchText
        |> List.head


transformNotes : String -> List Note -> List Note
transformNotes searchNoteText notes =
    notes
        |> List.filter (\note -> String.contains (String.toLower searchNoteText) (String.toLower note.body))
        |> List.sortBy .timestamp
        |> List.reverse


sortNotes : List Note -> List Note
sortNotes notes =
    notes |> List.sortBy .timestamp |> List.reverse


getSelectedNote : NoteList -> Maybe Note
getSelectedNote noteList =
    noteList.notes
        |> transformNotes noteList.searchNoteText
        |> List.filter (\note -> note.id == noteList.selectedNoteId)
        |> List.head


formatTitle : String -> String
formatTitle body =
    let
        maxLength =
            20

        length =
            String.length body
    in
        if length > maxLength then
            String.left (maxLength - 3) body ++ "..."
        else if length == 0 then
            "New note"
        else
            body


formatTimeStamp : Int -> String
formatTimeStamp timestamp =
    let
        time =
            Time.millisToPosix timestamp

        hour =
            String.fromInt (Time.toHour Time.utc time)

        minute =
            String.fromInt (Time.toMinute Time.utc time)

        second =
            String.fromInt (Time.toSecond Time.utc time)
    in
        hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"
