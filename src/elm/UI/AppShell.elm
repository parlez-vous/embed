module UI.AppShell exposing (appShell)

import Ant.Css
import Css exposing (..)
import Css.Media as Media exposing (withMedia)
import Css.ModernNormalize as NormalizeCss
import Html exposing (Html)
import Html.Styled as Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attr exposing (css)


type alias MediaQueries =
    { extraSmall : Style
    , small : Style
    , medium : Style
    , large : Style
    }


extraSmallMediaQueries : Style
extraSmallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.maxWidth (px 375) ] ]
        [ maxWidth (pct 95)
        ]


smallMediaQueries : Style
smallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 376), Media.maxWidth (px 640) ] ]
        [ maxWidth (pct 94)
        ]


mediumMediaQueries : Style
mediumMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 641), Media.maxWidth (px 1007) ] ]
        [ maxWidth (px 600)
        ]


largeMediaQueries : Style
largeMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 1008) ] ]
        [ maxWidth (px 800)
        ]


mediaQueries : MediaQueries
mediaQueries =
    { extraSmall = extraSmallMediaQueries
    , small = smallMediaQueries
    , medium = mediumMediaQueries
    , large = largeMediaQueries
    }


appShell : Styled.Html msg -> Html msg
appShell contents =
    toUnstyled <|
        Styled.div
            [ Attr.class "parlez-container"
            , css
                [ maxWidth (px 800)
                , marginRight auto
                , marginLeft auto
                , mediaQueries.extraSmall
                , mediaQueries.small
                , mediaQueries.medium
                , mediaQueries.large
                ]
            ]
            [ fromUnstyled NormalizeCss.globalHtml
            , fromUnstyled Ant.Css.defaultStyles
            , contents
            ]
