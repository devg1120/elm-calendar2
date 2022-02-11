module Calendar2.Internal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
-- import Date exposing (Date)
-- import Date.Extra
import Config exposing (ViewConfig, EventConfig, TimeSlotConfig)
import Calendar2.Agenda as Agenda
import Calendar2.Day as Day
import Calendar2.Month as Month
import Calendar2.Week as Week
import Calendar2.Msg exposing (Msg(..), TimeSpan(..))
-- import Mouse
-- import Time exposing (Time)
import Time 
import Time.Extra
import Date
import Html.Events.Extra.Mouse as Mouse 
import Browser.Events
import Json.Decode


type alias State =
    { timeSpan : TimeSpan
    , viewing : Time.Posix
    , dragState : Maybe Drag
    , selected : Maybe String
    }

type alias Position = (Float, Float)

type alias Drag =
    -- { start : Mouse.Position
    -- , current : Mouse.Position
    { start : Mouse.Event
    , current : Mouse.Event
    , kind : DragKind
    }


type DragKind
    = Event String
    | TimeSlot Time.Posix


-- init : TimeSpan -> Date -> State
init : TimeSpan -> Time.Posix -> State
init timeSpan viewing =
    { timeSpan = timeSpan
    , viewing = viewing
    , dragState = Nothing
    , selected = Nothing
    }


update : EventConfig msg -> TimeSlotConfig msg -> Msg -> State -> ( State, Maybe msg )
update eventConfig timeSlotConfig msg state =
    -- case Debug.log "msg" msg of
    case msg of
        PageBack ->
            ( state
                |> page -1
            , Nothing
            )

        PageForward ->
            ( state
                |> page 1
            , Nothing
            )

        ChangeTimeSpan timeSpan ->
            ( state
                |> changeTimeSpan timeSpan
            , Nothing
            )

        TimeSlotClick date xy ->
            ( state
            , timeSlotConfig.onClick date xy
            )

        TimeSlotMouseEnter date xy ->
            ( state
            , timeSlotConfig.onMouseEnter date xy
            )

        TimeSlotMouseLeave date xy ->
            ( state
            , timeSlotConfig.onMouseLeave date xy
            )

        TimeSlotDragStart date xy ->
            ( { state | dragState = Just { start = xy, current = xy, kind = TimeSlot date } }
            , timeSlotConfig.onDragStart date xy
            )

        TimeSlotDragging date me ->
            ( { state | dragState = (Maybe.map (\{ start, kind } -> Drag start me kind) state.dragState) }
            , timeSlotConfig.onDragging (getNewDateBasedOnPosition date me state) me 
            )

        TimeSlotDragEnd date xy ->
            ( { state | dragState = Nothing }
            , timeSlotConfig.onDragEnd (getNewDateBasedOnPosition date xy state) xy
            )

        EventClick eventId ->
            ( { state | selected = Just eventId }
            , eventConfig.onClick eventId
            )

        EventMouseEnter eventId ->
            ( state
            , eventConfig.onMouseEnter eventId
            )

        EventMouseLeave eventId ->
            ( state
            , eventConfig.onMouseLeave eventId
            )

        EventDragStart eventId xy ->
            ( { state | dragState = Just { start = xy, current = xy, kind = Event eventId } }
            , eventConfig.onDragStart eventId
            )

        EventDragging eventId me ->
            ( { state | dragState = (Maybe.map (\{ start, kind } -> Drag start me kind) state.dragState) }
            , eventConfig.onDragging eventId (getTimeDiffForPosition me state)
            --, eventConfig.onDragging eventId (Time.millisToPosix (getTimeDiffForPosition me state))
            )

        EventDragEnd eventId xy ->
            ( { state | dragState = Nothing }
            , eventConfig.onDragEnd eventId (getTimeDiffForPosition xy state)
            --, eventConfig.onDragEnd eventId (Time.millisToPosix (getTimeDiffForPosition xy state))
            )


-- getNewDateBasedOnPosition : Date -> Mouse.Event -> State -> Date
getNewDateBasedOnPosition : Time.Posix -> Mouse.Event -> State -> Time.Posix
getNewDateBasedOnPosition date xy state =
    -- let
    --     timeDiff =
    --         getTimeDiffForPosition xy state
    --             |> floor
    -- in
    --     Date.Extra.add Date.Extra.Millisecond timeDiff date
    date


-- getTimeDiffForPosition : Mouse.Position -> State -> Time
-- getTimeDiffForPosition : Mouse.Event -> State -> Time.Posix
getTimeDiffForPosition : Mouse.Event -> State -> number
getTimeDiffForPosition xy state =
    let
        timeDiff { start, current } =
            (current.y - start.y)
                // 20
                |> toFloat
                -- |> (*) Time.toMinute
                |> (*) 30
    in
        case state.timeSpan of
            Month ->
                0

            _ ->
                case state.dragState of
                    Just drag ->
                        -- timeDiff drag
                        1

                    Nothing ->
                        0


page : Int -> State -> State
page step state =
    let
        { timeSpan, viewing } =
            state
    in
        case timeSpan of
            Week ->
                -- { state | viewing = Date.Extra.add Date.Extra.Week step viewing }
                { state | viewing = Time.Extra.add Time.Extra.Week step Time.utc viewing }

            Day ->
                -- { state | viewing = Date.add Date.Days step viewing }
                { state | viewing = Time.Extra.add Time.Extra.Day step Time.utc viewing }

            _ ->
                -- { state | viewing = Date.add Date.Months step viewing }
                { state | viewing = Time.Extra.add Time.Extra.Month step Time.utc viewing }


changeTimeSpan : TimeSpan -> State -> State
changeTimeSpan timeSpan state =
    { state | timeSpan = timeSpan }


view : ViewConfig event -> List event -> State -> Html Msg
view config events { viewing, timeSpan, selected } =
    let
        calendarView =
            case timeSpan of
                Month ->
                    --let _ = Debug.log "Internal calendarView Month" events in
                    Month.view config events selected viewing

                Week ->
                    Week.view config events selected viewing

                Day ->
                    Day.view config events selected viewing

                Agenda ->
                    Agenda.view config events viewing
    in
        div
            [ class "elm-calendar--container"
            , Html.Attributes.draggable "false"
            ]
            [ div [ class "elm-calendar--calendar" ]
                [ viewToolbar viewing timeSpan
                , calendarView
                ]
            ]


viewToolbar : Time.Posix -> TimeSpan -> Html Msg
viewToolbar viewing timeSpan =
    div [ class "elm-calendar--toolbar" ]
        [ viewPagination
        , viewTitle viewing
        , viewTimeSpanSelection timeSpan
        ]


viewTitle : Time.Posix -> Html Msg
viewTitle viewing =
    div [ class "elm-calendar--month-title" ]
        -- [ h2 [] [ text <| Date.Extra.toFormattedString "MMMM yyyy" viewing ] ]
        [ h2 [] [ text <| Date.format "MMMM yyyy" (Date.fromPosix Time.utc viewing) ] ]


viewPagination : Html Msg
viewPagination =
    div [ class "elm-calendar--paginators" ]
        [ button [ class "elm-calendar--button", onClick PageBack ] [ text "back" ]
        , button [ class "elm-calendar--button", onClick PageForward ] [ text "next" ]
        ]


viewTimeSpanSelection : TimeSpan -> Html Msg
viewTimeSpanSelection timeSpan =
    div [ class "elm-calendar--time-spans" ]
        [ button [ class "elm-calendar--button", onClick (ChangeTimeSpan Month) ] [ text "Month" ]
        , button [ class "elm-calendar--button", onClick (ChangeTimeSpan Week) ] [ text "Week" ]
        , button [ class "elm-calendar--button", onClick (ChangeTimeSpan Day) ] [ text "Day" ]
        , button [ class "elm-calendar--button", onClick (ChangeTimeSpan Agenda) ] [ text "Agenda" ]
        ]


-- subscriptions : State -> Sub Msg
subscriptions : State -> Sub (Mouse.Event ->Msg)
subscriptions state =
    case state.dragState of
        Just dragState ->
            case dragState.kind of
                TimeSlot date ->
                    Sub.batch
                         [ Browser.Events.onMouseMove (Json.Decode.succeed (TimeSlotDragging date))
                        , Browser.Events.onMouseUp (Json.Decode.succeed (TimeSlotDragEnd date))
                        ]

                Event eventId ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (Json.Decode.succeed  (EventDragging eventId))
                        , Browser.Events.onMouseUp  (Json.Decode.succeed  (EventDragEnd eventId))
                        ]

        _ ->
            Sub.none
