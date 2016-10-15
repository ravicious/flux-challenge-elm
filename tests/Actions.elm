module Actions exposing (RosterScrollAction(..), applyRosterScrollAction)

import Roster exposing (Roster)


type RosterScrollAction
    = ScrollUp
    | ScrollDown


applyRosterScrollAction : RosterScrollAction -> Roster a -> Roster a
applyRosterScrollAction action roster =
    case action of
        ScrollUp ->
            Roster.scrollUp roster

        ScrollDown ->
            Roster.scrollDown roster
