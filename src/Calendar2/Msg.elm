module Calendar2.Msg exposing (..)

-- import Date exposing (Date)
-- import Mouse
import Time

import Html.Events.Extra.Mouse as Mouse

type TimeSpan
    = Month
    | Week
    | Day
    | Agenda


type Msg
    = PageBack
    | PageForward
    | ChangeTimeSpan TimeSpan
    | TimeSlotClick Time.Posix Mouse.Event
    | TimeSlotMouseEnter Time.Posix Mouse.Event
    | TimeSlotMouseLeave Time.Posix Mouse.Event
    | TimeSlotDragStart Time.Posix Mouse.Event
    | TimeSlotDragging Time.Posix Mouse.Event
    | TimeSlotDragEnd Time.Posix Mouse.Event
    | EventClick String
    | EventMouseEnter String
    | EventMouseLeave String
    | EventDragStart String Mouse.Event
    | EventDragging String Mouse.Event
    | EventDragEnd String Mouse.Event
