module Route exposing
    ( RouteInfo
    , fromUrl
    , shareUrl
    , sidebarTabPath
    )

import Model exposing (SidebarTab(..))
import List
import String
import Url exposing (Url)


type alias RouteInfo =
    { saveId : Maybe String
    , tab : SidebarTab
    }


fromUrl : Url -> RouteInfo
fromUrl url =
    let
        segments =
            routeSegments url
                |> String.split "/"
                |> List.filter (\segment -> segment /= "")
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

        _ ->
            { saveId = Nothing
            , tab = StatisticsTab
            }


sidebarTabPath : Maybe String -> SidebarTab -> String
sidebarTabPath saveId tab =
    case saveId of
        Just savedId ->
            "/s/" ++ savedId ++ sidebarTabSuffix tab

        Nothing ->
            case tab of
                StatisticsTab ->
                    "/"

                _ ->
                    "/" ++ tabToSegment tab


shareUrl : String -> Maybe String -> SidebarTab -> String
shareUrl appBaseUrl saveId tab =
    appBaseUrl ++ "/#" ++ sidebarTabPath saveId tab


sidebarTabSuffix : SidebarTab -> String
sidebarTabSuffix tab =
    case tab of
        StatisticsTab ->
            ""

        _ ->
            "/" ++ tabToSegment tab


routeSegments : Url -> String
routeSegments url =
    case url.fragment of
        Just fragment ->
            fragment

        Nothing ->
            url.path
                |> String.split "/"
                |> List.filter (\segment -> segment /= "")
                |> stripAppPrefix


stripAppPrefix : List String -> String
stripAppPrefix segments =
    case segments of
        "seed" :: rest ->
            String.join "/" rest

        _ ->
            String.join "/" segments


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
