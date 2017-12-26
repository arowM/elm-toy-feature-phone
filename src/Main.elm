module Main exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (attribute, class)
import Html.Events as Events


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { pre = ""
      , target = Nothing
      , post = ""
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { pre : String
    , target : Maybe ActiveChar
    , post : String
    }


type ActiveChar
    = EditableChar
        { origin : Char
        , count : Int
        }
    | UneditableChar Char


type Button
    = Button1
    | Button2
    | Button3
    | Button4
    | Button5
    | Button6
    | Button7
    | Button8
    | Button9
    | Button0



-- UPDATE


type Msg
    = ClickNum Char
    | ClickDel
    | ClickRight
    | ClickLeft


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickNum c ->
            case model.target of
                Nothing ->
                    ( { model
                        | target =
                            Just <|
                                EditableChar
                                    { origin = c
                                    , count = 1
                                    }
                      }
                    , Cmd.none
                    )

                Just (EditableChar o) ->
                    if o.origin == c then
                        ( { model
                            | target =
                                Just <|
                                    EditableChar
                                        { o
                                            | count = o.count + 1
                                        }
                          }
                        , Cmd.none
                        )
                    else
                        ( { model
                            | pre = model.pre ++ String.fromChar (showActiveChar <| EditableChar o)
                            , target =
                                Just <|
                                    EditableChar
                                        { origin = c
                                        , count = 1
                                        }
                          }
                        , Cmd.none
                        )

                Just (UneditableChar uneditable) ->
                    ( { model
                        | pre = model.pre ++ String.fromChar uneditable
                        , target =
                            Just <|
                                EditableChar
                                    { origin = c
                                    , count = 1
                                    }
                      }
                    , Cmd.none
                    )

        ClickDel ->
            case String.uncons model.post of
                Just ( c, str ) ->
                    ( { model
                        | target = Just <| UneditableChar c
                        , post = str
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | target = Nothing
                      }
                    , Cmd.none
                    )

        ClickLeft ->
            case String.uncons <| String.reverse model.pre of
                Just ( c, rev ) ->
                    ( { model
                        | pre = String.reverse rev
                        , target = Just <| UneditableChar c
                        , post =
                            case model.target of
                                Nothing ->
                                    model.post

                                Just a ->
                                    String.cons (showActiveChar a) model.post
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ClickRight ->
            case String.uncons model.post of
                Just ( c, str ) ->
                    ( { model
                        | target = Just <| UneditableChar c
                        , post = str
                        , pre =
                            case model.target of
                                Nothing ->
                                    model.pre

                                Just a ->
                                    model.pre ++ String.fromChar (showActiveChar a)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case model.target of
                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                        Just a ->
                            ( { model
                                | target = Nothing
                                , pre = model.pre ++ String.fromChar (showActiveChar a)
                              }
                            , Cmd.none
                            )



-- VIEW


view : Model -> Html Msg
view model =
    wrap_
        [ class "center"
        ]
        [ wrap
            [ wrap_
                [ class "display"
                ]
                [ renderDisplayContent model
                ]
            , wrap
                [ pack_
                    [ class "row"
                    , class "justifyAround"
                    ]
                    [ wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick ClickDel
                            ]
                            [ Html.text "DEL"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "row"
                            ]
                            [ pack_
                                [ class "button button-left"
                                , Events.onClick ClickLeft
                                ]
                                [ Html.text "<"
                                ]
                            , pack_
                                [ class "button button-right"
                                , Events.onClick ClickRight
                                ]
                                [ Html.text ">"
                                ]
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            ]
                            [ Html.text ""
                            ]
                        ]
                    ]
                , pack_
                    [ class "row"
                    , class "justifyAround"
                    ]
                    [ wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'あ'
                            ]
                            [ Html.text "1 あ"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'か'
                            ]
                            [ Html.text "2 か"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'さ'
                            ]
                            [ Html.text "3 さ"
                            ]
                        ]
                    ]
                , pack_
                    [ class "row"
                    , class "justifyAround"
                    ]
                    [ wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'た'
                            ]
                            [ Html.text "4 た"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'な'
                            ]
                            [ Html.text "5 な"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'は'
                            ]
                            [ Html.text "6 は"
                            ]
                        ]
                    ]
                , pack_
                    [ class "row"
                    , class "justifyAround"
                    ]
                    [ wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'ま'
                            ]
                            [ Html.text "7 ま"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'や'
                            ]
                            [ Html.text "8 や"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'ら'
                            ]
                            [ Html.text "9 ら"
                            ]
                        ]
                    ]
                , pack_
                    [ class "row"
                    , class "justifyAround"
                    ]
                    [ wrap
                        [ pack_
                            [ class "button"
                            ]
                            []
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            , Events.onClick <| ClickNum 'わ'
                            ]
                            [ Html.text "0 わ"
                            ]
                        ]
                    , wrap
                        [ pack_
                            [ class "button"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


renderDisplayContent : Model -> Html Msg
renderDisplayContent model =
    pack
        [ Html.span
            [ class "pack"
            ]
            [ Html.text model.pre
            ]
        , case model.target of
            Nothing ->
                Html.span
                    [ class "pack"
                    , class "active"
                    ]
                    [ Html.text "\x3000"
                    ]

            Just c ->
                Html.span
                    [ class "pack"
                    , class "active"
                    ]
                    [ Html.text <| String.fromChar <| showActiveChar c
                    ]
        , Html.span
            [ class "pack"
            ]
            [ Html.text model.post
            ]
        ]



-- Helper View functions


pack : List (Html msg) -> Html msg
pack =
    pack_ []


pack_ : List (Attribute msg) -> List (Html msg) -> Html msg
pack_ attrs children =
    Html.div
        (class "pack" :: attrs)
        children


wrap : List (Html msg) -> Html msg
wrap =
    wrap_ []


wrap_ : List (Attribute msg) -> List (Html msg) -> Html msg
wrap_ attrs children =
    Html.div
        (class "wrap" :: attrs)
        children


wrappedText : String -> Html msg
wrappedText str =
    wrap
        [ Html.text str
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helper functions


showActiveChar : ActiveChar -> Char
showActiveChar obj =
    case obj of
        EditableChar { origin, count } ->
            case origin of
                'あ' ->
                    case count % 5 of
                        1 ->
                            'あ'

                        2 ->
                            'い'

                        3 ->
                            'う'

                        4 ->
                            'え'

                        _ ->
                            'お'

                'か' ->
                    case count % 5 of
                        1 ->
                            'か'

                        2 ->
                            'き'

                        3 ->
                            'く'

                        4 ->
                            'け'

                        _ ->
                            'こ'

                'さ' ->
                    case count % 5 of
                        1 ->
                            'さ'

                        2 ->
                            'し'

                        3 ->
                            'す'

                        4 ->
                            'せ'

                        _ ->
                            'そ'

                'た' ->
                    case count % 5 of
                        1 ->
                            'た'

                        2 ->
                            'ち'

                        3 ->
                            'つ'

                        4 ->
                            'て'

                        _ ->
                            'と'

                'な' ->
                    case count % 5 of
                        1 ->
                            'な'

                        2 ->
                            'に'

                        3 ->
                            'ぬ'

                        4 ->
                            'ね'

                        _ ->
                            'の'

                'は' ->
                    case count % 5 of
                        1 ->
                            'は'

                        2 ->
                            'ひ'

                        3 ->
                            'ふ'

                        4 ->
                            'へ'

                        _ ->
                            'ほ'

                'ま' ->
                    case count % 5 of
                        1 ->
                            'ま'

                        2 ->
                            'み'

                        3 ->
                            'む'

                        4 ->
                            'め'

                        _ ->
                            'も'

                'や' ->
                    case count % 3 of
                        1 ->
                            'や'

                        2 ->
                            'ゆ'

                        _ ->
                            'よ'

                'ら' ->
                    case count % 5 of
                        1 ->
                            'ら'

                        2 ->
                            'り'

                        3 ->
                            'る'

                        4 ->
                            'れ'

                        _ ->
                            'ろ'

                'わ' ->
                    case count % 3 of
                        1 ->
                            'わ'

                        2 ->
                            'を'

                        _ ->
                            'ん'

                _ ->
                    'ん'

        UneditableChar c ->
            c



-- Utility functions


ariaSelected : Bool -> Attribute Msg
ariaSelected b =
    attribute "aria-selected" <|
        if b then
            "true"
        else
            "false"


ariaHidden : Bool -> Attribute Msg
ariaHidden b =
    attribute "aria-hidden" <|
        if b then
            "true"
        else
            "false"


role : String -> Attribute Msg
role =
    attribute "role"


result : (l -> x) -> (r -> x) -> Result l r -> x
result onErr onOk res =
    case res of
        Err l ->
            onErr l

        Ok r ->
            onOk r


maybe : a -> (b -> a) -> Maybe b -> a
maybe def f =
    Maybe.withDefault def << Maybe.map f
