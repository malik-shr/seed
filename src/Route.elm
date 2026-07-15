module Route exposing
    ( RouteInfo
    , fromUrl
    , shareUrl
    , sidebarTabPath
    )

import Model exposing (SidebarTab(..))
import List
import Url exposing (Url)


type alias RouteInfo =
    { saveId : Maybe String
    , tab : SidebarTab
    }


fromUrl : Url -> RouteInfo
fromUrl url =
    let
        segments =
            url.path
                |> String.split "/"
                |> List.filter (\segment -> segment /= "")
                |> stripAppPrefix
    in
    case segments of
        "s" :: saveId :: tabSegment :: _ ->
            { saveId = Just saveId
            , tab = tabFromSegment tabSegment
            }

        "s" :: saveId :: [] ->
            { saveId = Just saveId
            , tab = StatisticsTab
            }

        tabSegment :: _ ->
            { saveId = Nothing
            , tab = tabFromSegment tabSegment
            }

        [] ->
            { saveId = Nothing
            , tab = StatisticsTab
            }


sidebarTabPath : Maybe String -> SidebarTab -> String
sidebarTabPath saveId tab =
    case saveId of
        Just savedId ->
            case tab of
                StatisticsTab ->
                    "/s/" ++ savedId

                _ ->
                    "/s/" ++ savedId ++ "/" ++ tabToSegment tab

        Nothing ->
            "/" ++ tabToSegment tab


shareUrl : String -> Maybe String -> SidebarTab -> String
shareUrl appBaseUrl saveId tab =
    appBaseUrl ++ sidebarTabPath saveId tab


stripAppPrefix : List String -> List String
stripAppPrefix segments =
    case segments of
        "seed" :: rest ->
            rest

        _ ->
            segments


tabFromSegment : String -> SidebarTab
tabFromSegment segment =
    case segment of
        "production" ->
            ProductionTab

        "jobs" ->
            JobsTab

        "buildings" ->
            BuildingsTab

        _ ->
            StatisticsTab


tabToSegment : SidebarTab -> String
tabToSegment tab =
    case tab of
        StatisticsTab ->
            "statistics"

        ProductionTab ->
            "production"

        JobsTab ->
            "jobs"

        BuildingsTab ->
            "buildings"
