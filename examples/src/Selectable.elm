module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, style, value)
import Html.Events exposing (onInput, onClick)
import Html.Events.Extra.Mouse as Mouse

import Calendar2
import Date 
import Fixtures
import Dict exposing (Dict)
import Time 
import Time.Extra
import String
import Keyboard
import Browser.Events
import Html.Events.Extra.Mouse


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model_ =
    Sub.batch
        [ Sub.map SetCalendarState (Calendar2.subscriptions model_.calendarState)
        , Keyboard.downs CancelEventPreview
        ]


type alias Model =
    { calendarState : Calendar2.State
    , events : Dict String Event
    , eventExtendAmount : Time.Posix
    , eventPreview : Maybe EventPreview
    , curEventId : String
    , selectedEvent : Maybe Event
    }


type alias Event =
    { id : String
    , title : String
    , start : Time.Posix
    , end : Time.Posix
    }


type alias EventPreview =
    { event : Event
    , position : Mouse.Event
    , showDialog : Bool
    }


base_model : Model
base_model =
     { calendarState = Calendar2.init Calendar2.Week Fixtures.viewing
      , events =
            Fixtures.events
                |> List.map (\event -> ( event.id, event ))
                |> Dict.fromList
      --, eventExtendAmount = 0
      , eventExtendAmount = (Time.millisToPosix 0)
      , eventPreview = Nothing
      , curEventId =
            Fixtures.events
                --|> List.map (Result.withDefault 0 << String.toInt << .id)
                |> List.map (Maybe.withDefault 0 << String.toInt << .id)
                |> List.sortWith flippedComparison
                |> List.head
                |> Maybe.withDefault (List.length Fixtures.events)
               -- |> toString
                |> String.fromInt
      , selectedEvent = Nothing
      }



init : () -> ( Model, Cmd Msg )
init _ =
      ( base_model
    , Cmd.none
    )


type Msg
    = SetCalendarState Calendar2.Msg
    | CreateEventTitle String
    | AddEventPreviewToEvents
    | CancelEventPreview Keyboard.RawKey


type CalendarMsg
    = SelectDate Time.Posix Mouse.Event
    | CreateEventPreview Time.Posix Mouse.Event
    | ExtendEventPreview Time.Posix  Mouse.Event
    | ShowCreateEventDialog Time.Posix Mouse.Event
    | SelectEvent String
    --| ExtendingEvent String Time.Posix
    | ExtendingEvent String Int
    --| ExtendEvent String Time.Posix
    | ExtendEvent String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    ( pureUpdate msg model_, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model_ =
    case msg of
        SetCalendarState calendarMsg ->
            let
                ( updatedCalendar, maybeMsg ) =
                    Calendar2.update eventConfig timeSlotConfig calendarMsg model_.calendarState

                newModel =
                    { model_ | calendarState = updatedCalendar }
            in
                case maybeMsg of
                    Nothing ->
                        newModel

                    Just updateMsg ->
                        updateCalendar updateMsg newModel

        CreateEventTitle title ->
            model_
                |> changeEventPreviewTitle title

        AddEventPreviewToEvents ->
            model_
                |> addEventPreviewToEvents
                |> removeEventPreview

        CancelEventPreview keyCode ->
            let
               key = Keyboard.anyKeyOriginal  keyCode
            in
            case key of
                --27 ->
                Just Keyboard.Alt ->
                    model_
                        |> removeEventPreview
                Just _ ->
                    model_
                Nothing ->
                    model_


updateCalendar : CalendarMsg -> Model -> Model
updateCalendar msg model_ =
        let _  = Debug.log "calendarMsg:" msg in
        --case Debug.log "calendarMsg:" msg of
        case  msg of
        SelectDate date xy ->
            model_
                |> createEventPreview date xy 60
                |> showCreateEventDialog

        CreateEventPreview date xy ->
            model_
                |> createEventPreview date xy 30

        ExtendEventPreview date xy ->
            model_
                |> extendEventPreview date xy

        ShowCreateEventDialog date xy ->
            model_
                |> extendEventPreview date xy
                |> showCreateEventDialog

        SelectEvent eventId ->
            model_
                |> selectEvent eventId

        ExtendingEvent _ timeDiff ->
            --{ model_ | eventExtendAmount = timeDiff }
            { model_ | eventExtendAmount = (Time.millisToPosix timeDiff) }

        ExtendEvent eventId timeDiff ->
            let
                maybeEvent =
                    Dict.get eventId model_.events

                newEnd end = 
                    --Date.toTime end
                    Time.posixToMillis end
                        --|> (+) (Time.posixToMillis timeDiff)
                        |> (+) timeDiff
                        --|> Date.fromTime
                        |> Time.millisToPosix 

                extendEvent event =
                    { event | end = newEnd event.end }

                updateEvents event =
                    Dict.insert eventId (extendEvent event) model_.events
            in
                case maybeEvent of
                    Nothing ->
                        model_

                    Just event ->
                        { model_ | events = updateEvents event }


--createEventPreview : Date -> Mouse.Position -> Int -> Model -> Model
--createEventPreview : Time.Posix -> (Float, Float) -> Int -> Model -> Model
createEventPreview : Time.Posix -> Mouse.Event -> Int -> Model -> Model
createEventPreview date xy minutes model_ =
    let _  = Debug.log "createEventPreview:" date in
    let
        newEvent =
            --Event (newEventId model.curEventId) "" date (Date.Extra.add Date.Extra.Minute minutes date)
            --Event (newEventId model.curEventId) "" date (Time.Extra.add Time.Extra.Minute minutes date)
            Event (newEventId model_.curEventId) "" date (Time.Extra.add Time.Extra.Millisecond minutes Time.utc  date)

        eventPreview =
            { event = newEvent
            , position = xy
            , showDialog = False
            }
    in
        { model_ | eventPreview = Just eventPreview }


selectEvent : String -> Model -> Model
selectEvent eventId model_ =
    --let _  = Debug.log "selectEvent:" eventId in
    { model_ | selectedEvent = Dict.get eventId model_.events }


showCreateEventDialog : Model -> Model
showCreateEventDialog model_ =
    { model_ | eventPreview = Maybe.map toggleEventPreviewDialog model_.eventPreview }


toggleEventPreviewDialog : EventPreview -> EventPreview
toggleEventPreviewDialog eventPreview =
    { eventPreview | showDialog = not eventPreview.showDialog }


changeEventPreviewTitle : String -> Model -> Model
changeEventPreviewTitle title model_ =
    let
        changeEventTitle event =
            { event | title = title }

        changePreviewTitle preview =
            { preview | event = changeEventTitle preview.event }
    in
        { model_ | eventPreview = Maybe.map changePreviewTitle model_.eventPreview }


--extendEventPreview : Date -> Mouse.Position -> Model -> Model
--extendEventPreview : Time.Posix -> (Float, Float) -> Model -> Model
extendEventPreview : Time.Posix -> Mouse.Event -> Model -> Model
extendEventPreview date xy model_ =
    let _  = Debug.log "extendEventPreview:" date in
    let
        extend ({ event, position } as eventPreview) =
            { eventPreview | event = { event | end = Debug.log "finalEnd" date } }
    in
        { model_ | eventPreview = Maybe.map extend model_.eventPreview }


addEventPreviewToEvents : Model -> Model
addEventPreviewToEvents model_ =
    let
        defaultEmptyTitle event =
            { event
                | title =
                    if event.title == "" then
                        "(No Title)"
                    else
                        event.title
            }

        addToEvents event =
            Dict.insert event.id (Debug.log "newEvent" (defaultEmptyTitle event)) model_.events
    in
        { model_
            | events =
                Maybe.map (addToEvents << .event) model_.eventPreview
                    |> Maybe.withDefault model_.events
            , curEventId = (newEventId model_.curEventId)
        }


removeEventPreview : Model -> Model
removeEventPreview model_ =
    { model_ | eventPreview = Nothing }


newEventId : String -> String
newEventId eventId =
    String.toInt eventId
        --|> Result.withDefault 0
        |> Maybe.withDefault 0
        |> (+) 1
        --|> toString
        |> String.fromInt


view : Model -> Html Msg
view model_ =
    let
        events =
            Dict.values model_.events
        selectedEvent = model_.selectedEvent
    in
        div []
            [ case model_.eventPreview of
                Just preview ->
                    viewCreateEvent preview

                Nothing ->
                    text ""
            , Html.map SetCalendarState (Calendar2.view viewConfig events model_.calendarState)
            ]




px : String -> String
px str =
    str ++ "px"



abc : a -> b -> ( a, b )
abc l r =
    (l,r)

 {--
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
    --}

viewCreateEvent : EventPreview -> Html Msg
viewCreateEvent ({ event, position, showDialog } as preview) =
    let
        duration =
            --Date.Extra.diff Date.Extra.Minute event.start event.end
            Time.Extra.diff  Time.Extra.Minute Time.utc event.start event.end

        height =
            duration
                // 30
                |> (*) 20
                --|> toString
                |> String.fromInt
    in
        div
            [ class "event-preview"
            --, --style
            {--
                [ "position" => "absolute"
                , "top" => px (toString position.y)
                , "left" => px (toString position.x)
                , "height" => px height
                , "z-index" => "2"
                ]
                --}
                , style "position"  "absolute"
                --, style "top"   (px (String.fromInt position.y))
                , style "top"   (px (String.fromFloat (Tuple.second position.offsetPos)))
                , style "left"  (px (String.fromFloat (Tuple.first position.offsetPos)))
                , style "height"   (px height)
                , style "z-index"   "2"
                --]
            ]
            [ if showDialog then
                viewCreateEventDialog preview
              else
                text ""
            , text "New Event"
            ]


viewCreateEventDialog : EventPreview -> Html Msg
viewCreateEventDialog { event, position } =
    div
        [ class "create-event-dialog"
        ]
        [ h3 [ class "create-event-title" ] [ text "Create event" ]
        , input
            [ onInput CreateEventTitle
            , value event.title
            , class "create-event-input"
            ]
            []
        , button
            [ onClick AddEventPreviewToEvents
            , class "create-event-button"
            ]
            [ text "Create Event" ]
        ]


viewConfig : Calendar2.ViewConfig Event
viewConfig =
    Calendar2.viewConfig
        { toId = .id
        , title = .title
        , start = .start
        , end = .end
        , event =
            \event isSelected ->
                --let _  = Debug.log "viewConfig/event:" isSelected in
                Calendar2.eventView
                    { nodeName = "div"
                    , classes =
                        [ ( "elm-calendar--event-content", True )
                        , ( "elm-calendar--event-content--is-selected", isSelected )
                        ]
                    , children =
                        [ div []
                            [ text <| event.title ]
                        ]
                    }
        }


eventConfig : Calendar2.EventConfig CalendarMsg
eventConfig =
    Calendar2.eventConfig
        { onClick = \eventId -> Just <| SelectEvent eventId
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onDragStart = \_ -> Nothing
        , onDragging = \eventId timeDiff -> Just <| ExtendingEvent eventId timeDiff
        , onDragEnd = \eventId timeDiff -> Just <| ExtendEvent eventId timeDiff
        }


timeSlotConfig : Calendar2.TimeSlotConfig CalendarMsg
timeSlotConfig =
    Calendar2.timeSlotConfig
        { onClick = \date xy -> Just <| SelectDate date xy
        , onMouseEnter = \_ _ -> Nothing
        , onMouseLeave = \_ _ -> Nothing
        , onDragStart = \date xy -> Just <| CreateEventPreview date xy
        , onDragging = \date xy -> Just <| ExtendEventPreview date xy
        , onDragEnd = \date xy -> Just <| ShowCreateEventDialog date xy
        }


flippedComparison : comparable -> comparable -> Order
flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
