module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Errors exposing (Errors)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder, rows, selected, src, style, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, string)
import List
import Parser exposing ((|.), (|=), Parser)
import Partition exposing (Partition(..))
import Set
import String


type Partitioner
    = ByName
    | ByManaValue
    | ByColor
    | ByLandness
    | ByType
    | ByColorness


pname : Partitioner -> String
pname part =
    "by "
        ++ (case part of
                ByName ->
                    "name"

                ByManaValue ->
                    "mana value"

                ByColor ->
                    "color"

                ByLandness ->
                    "land/non-land"

                ByType ->
                    "type"

                ByColorness ->
                    "mono/multi color"
           )


pname_ : String -> Partitioner
pname_ s =
    case s of
        "by name" ->
            ByName

        "by mana value" ->
            ByManaValue

        "by color" ->
            ByColor

        "by land/non-land" ->
            ByLandness

        "by type" ->
            ByType

        "by mono/multi color" ->
            ByColorness

        _ ->
            ByName


pfunc : Partitioner -> Card -> String
pfunc part =
    case part of
        ByName ->
            .name

        ByManaValue ->
            .cmc

        ByColor ->
            .color >> String.concat

        ByLandness ->
            landiness

        ByType ->
            .type_line

        ByColorness ->
            colorness


porder : Partitioner -> Int
porder part =
    case part of
        ByName ->
            1

        _ ->
            -1


pshow : Partitioner -> Card -> Html msg
pshow part c =
    span [ title (pname part ++ ": " ++ pfunc part c) ] <|
        case part of
            ByName ->
                [ em [ class "tooltip" ]
                    [ text <| c.name
                    , span [ class "tooltiptext" ] [ img [ src c.image_uri, attribute "loading" "lazy" ] [] ]
                    ]
                ]

            ByColor ->
                let
                    colors =
                        case c.color of
                            [] ->
                                [ "c" ]

                            cc ->
                                cc
                in
                List.map (\color -> span [ class "ms", class "ms-cost", class ("ms-" ++ String.toLower color) ] []) colors

            ByManaValue ->
                [ span [ class "ms", class ("ms-" ++ c.cmc) ] [] ]

            ByType ->
                List.map (\t -> span [ class "ms", class "ms-fw", class ("ms-" ++ String.toLower t) ] []) (String.split " " c.type_line)

            ByColorness ->
                let
                    multi =
                        if colorness c == "multicolor" then
                            [ class "ms-duo-color", class "ms-grad" ]

                        else
                            []
                in
                [ span ([ class "ms", class "ms-multicolor", class "ms-duo" ] ++ multi) [] ]

            _ ->
                [ text <| pfunc part c ]


classifyOn : Partitioner -> List Card -> List (List Card)
classifyOn field =
    let
        classification a b =
            pfunc field a == pfunc field b
    in
    Partition.classifyBy classification
        >> List.map (List.sortBy (pfunc field))


partition : Partitioner -> Partition Card -> Partition Card
partition field p =
    case p of
        One ps ->
            Many <| List.map One <| classifyOn field ps

        Many ts ->
            Many (List.map (partition field) ts)


type alias Card =
    { color : List String, cmc : String, name : String, type_line : String, image_uri : String }


unlines : List String -> String
unlines ls =
    List.foldr (\x y -> x ++ "\n" ++ y) "" ls


showCard : List Partitioner -> Card -> Html msg
showCard parts c =
    if List.isEmpty parts then
        text "a card"

    else
        parts
            |> List.sortBy porder
            |> List.map (\p -> pshow p c)
            |> List.intersperse (text " ")
            |> span []


deckExample : String
deckExample =
    unlines
        [ "4 Counterspell"
        , "20 Island"
        , "4 Ponder"
        , "4 Preordain"
        ]


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "Deck partitioner" ]
        , p []
            [ text "Companion app for post: “"
            , a [ href "http://blog.florius.com.ar/" ] [ text "Deckchecks, Heuristics and Decision Trees" ]
            , text "”"
            ]
        , viewInner m
        , if Errors.isEmpty m.errors then
            span [] []

          else
            div []
                [ hr [] []
                , h3 [] [ text "Errors" ]
                , ul [] (List.map (\l -> li [] [ text l ]) (Errors.toStrings m.errors))
                ]
        ]


viewInner : Model -> Html Msg
viewInner model =
    case model.status of
        Editting ->
            form [ onSubmit LoadCards ]
                [ button [ type_ "submit" ] [ text "Load" ]
                , textarea
                    [ placeholder "Write or paste here your favourite deck to partition"
                    , value model.content
                    , style "width" "100%"
                    , rows 30
                    , onInput Change
                    ]
                    []
                ]

        Showing ->
            let
                cards =
                    lookupCards model.db model.deck
                        |> List.filter (\c -> c.type_line /= "Card")

                tree =
                    partitionBy model.partition cards
            in
            div []
                [ button [ onClick Edit ] [ text "<< Change deck" ]
                , span [] <|
                    if List.length cards < List.length model.deck then
                        [ text <| "❌ Missing " ++ String.fromInt (List.length model.deck - List.length cards) ++ " card(s)" ]

                    else
                        []
                , div []
                    [ viewPartitions model.partition
                    , button [ onClick (Partitions PAdd) ] [ text "➕" ]
                    ]
                , div [] [ button [ onClick FindBest ] [ text "Find best partition ", em [] [ text "(this will take a while)" ] ] ]
                , showPartition model.partition tree
                , div [] [ text <| "Total cost: ", strong [] [ text <| String.fromFloat (Partition.cost tree) ] ]
                ]


viewPartitions : PartitionModel -> Html Msg
viewPartitions parts =
    span [] <|
        if List.isEmpty parts then
            [ text "Add partitions 👉" ]

        else
            List.indexedMap viewPartition parts
                |> List.intersperse (text " > ")


viewPartition : Int -> Partitioner -> Html Msg
viewPartition i p =
    span []
        [ Html.select
            [ onInput (\newP -> Partitions (PChange i (pname_ newP))) ]
            (List.map (\this -> viewOption (p == this) this) partitions)
        , button [ onClick (Partitions (PRemove i)) ] [ text "🗑" ]
        ]


permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    List.map ((::) y) (permutations ys)
            in
            List.concatMap f (select xs)


subsequences : List a -> List (List a)
subsequences xs =
    [] :: subsequencesNonEmpty xs


subsequencesNonEmpty : List a -> List (List a)
subsequencesNonEmpty list =
    case list of
        [] ->
            []

        first :: rest ->
            let
                f ys r =
                    ys :: (first :: ys) :: r
            in
            [ first ] :: List.foldr f [] (subsequencesNonEmpty rest)


select : List a -> List ( a, List a )
select list =
    case list of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xs)


viewOption : Bool -> Partitioner -> Html Msg
viewOption sel p =
    option
        [ value <| pname p, selected sel ]
        [ text <| pname p ]


partitionBy : List Partitioner -> List Card -> Partition Card
partitionBy pts cards =
    List.foldl partition (One cards) pts


showPartition : List Partitioner -> Partition Card -> Html msg
showPartition parts p =
    Partition.renderPartition (showCard parts) p
        |> List.map (div [] << List.singleton)
        |> Html.pre [ style "cursor" "default" ]


type Status
    = Editting
    | Showing


type alias PartitionModel =
    List Partitioner


type alias Model =
    { content : String
    , errors : Errors
    , status : Status
    , db : Dict CardName Card
    , deck : List CardName
    , partition : PartitionModel
    }


type Msg
    = GotCard CardName (Result Http.Error Card)
    | Change String
    | LoadCards
    | Edit
    | FindBest
    | Partitions PartitionMsg


type PartitionMsg
    = PAdd
    | PRemove Int
    | PChange Int Partitioner


type alias MultiResult error value =
    { ok : value
    , err : error
    }


mapMR : (error1 -> error2) -> (value1 -> value2) -> MultiResult error1 value1 -> MultiResult error2 value2
mapMR fErr fVal mr =
    { ok = fVal mr.ok, err = fErr mr.err }


accumulate : List (Result e a) -> MultiResult (List e) (List a)
accumulate xs =
    { ok = List.filterMap Result.toMaybe xs
    , err = List.filterMap errToMaybe xs
    }


errToMaybe : Result e a -> Maybe e
errToMaybe mb =
    case mb of
        Err e ->
            Just e

        _ ->
            Nothing


type alias CardName =
    String


parserRunSimple : Parser a -> String -> Result String a
parserRunSimple parser input =
    Parser.run parser input
        |> Result.mapError (\_ -> "coudn't parse [" ++ input ++ "]")


parseDeck : String -> MultiResult (List String) (List CardName)
parseDeck content =
    let
        cards =
            String.split "\n" content
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)
                |> List.map (parserRunSimple cardsParser)
                |> accumulate
    in
    mapMR identity multiply cards


multiply : List ( Int, a ) -> List a
multiply =
    uncurry List.repeat
        |> List.concatMap


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( a, b ) =
    f a b


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> List.member c [ ' ', '\t', '\n', '\u{000D}', '┌', '─', '─' ])


cardsParser : Parser ( Int, String )
cardsParser =
    Parser.succeed Tuple.pair
        |. whitespace
        |= Parser.oneOf
            [ Parser.int
            , Parser.succeed 1
            ]
        |. Parser.oneOf
            [ Parser.symbol "x"
            , Parser.succeed ()
            ]
        |. whitespace
        |= notEmpty
            (Parser.map String.trim <|
                Parser.getChompedString <|
                    Parser.succeed ()
                        |. Parser.chompWhile (\c -> c /= '$')
            )
        |. Parser.oneOf
            [ Parser.symbol "$"
                |. Parser.chompUntilEndOr "\n"
            , Parser.succeed ()
            ]
        |. Parser.end


notEmpty : Parser String -> Parser String
notEmpty =
    Parser.andThen
        (\s ->
            if String.isEmpty s then
                Parser.problem "the string was empty"

            else
                Parser.succeed s
        )


loadCard : CardName -> Cmd Msg
loadCard name =
    Http.get
        { url = "https://api.scryfall.com/cards/named?fuzzy=" ++ name
        , expect = Http.expectJson (GotCard name) cardDecoder
        }


type alias OneFaceCard =
    { color : List String, name : String, type_line : String, image_uri : String }


oneFaceCardDecoder : Decoder OneFaceCard
oneFaceCardDecoder =
    Json.Decode.map4 (\color_ name_ type_line_ image_uri_ -> { color = color_, name = name_, type_line = type_line_, image_uri = image_uri_ })
        (Json.Decode.map (Maybe.withDefault []) <| Json.Decode.maybe <| Json.Decode.field "colors" colorsDecode)
        (Json.Decode.field "name" string)
        (Json.Decode.map typeClosure <| Json.Decode.field "type_line" string)
        (Json.Decode.field "image_uris" <| Json.Decode.field "png" string)


multiFaceCardDecoder : Decoder OneFaceCard
multiFaceCardDecoder =
    Json.Decode.andThen
        (\faces ->
            case faces of
                [] ->
                    Json.Decode.fail "is not multi faced"

                face :: _ ->
                    Json.Decode.succeed face
        )
        (Json.Decode.field "card_faces" (Json.Decode.list oneFaceCardDecoder))


cardDecoder : Decoder Card
cardDecoder =
    Json.Decode.map2 (\cmc_ of_ -> { color = of_.color, cmc = cmc_, name = of_.name, type_line = of_.type_line, image_uri = of_.image_uri })
        (Json.Decode.map String.fromInt <| Json.Decode.field "cmc" Json.Decode.int)
        (Json.Decode.oneOf [ multiFaceCardDecoder, oneFaceCardDecoder ])


typeClosure : String -> String
typeClosure type_line =
    if String.startsWith "Basic Land" type_line || String.startsWith "Land" type_line then
        "Land"

    else
        String.split "—" type_line
            |> List.head
            |> Maybe.map String.trim
            |> Maybe.withDefault type_line
            |> String.replace "Legendary " ""


colorsDecode : Decoder (List String)
colorsDecode =
    Json.Decode.list string
        |> Json.Decode.map List.sort


subtract : List comparable -> List comparable -> List comparable
subtract a b =
    List.filter (\x -> not <| List.member x b) a


defaultPartition : Partitioner
defaultPartition =
    ByManaValue


namePartition : Partitioner
namePartition =
    ByName


partitions : List Partitioner
partitions =
    ByName :: partitionsWithoutName


partitionsWithoutName : List Partitioner
partitionsWithoutName =
    [ ByManaValue
    , ByColor
    , ByLandness
    , ByType
    , ByColorness
    ]


landiness : Card -> String
landiness c =
    case c.type_line of
        "Land" ->
            "Land    "

        _ ->
            "Non-Land"


colorness : Card -> String
colorness c =
    if List.length c.color > 1 then
        "multicolor"

    else
        "monocolored"


updatePartition : PartitionMsg -> PartitionModel -> PartitionModel
updatePartition msg model =
    case msg of
        PAdd ->
            List.append model [ defaultPartition ]

        PRemove i ->
            removeAt i model

        PChange i new ->
            updateAt i (\_ -> new) model


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                head ++ fn x :: xs

            _ ->
                list


removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l

    else
        case List.drop index l of
            [] ->
                l

            _ :: rest ->
                List.take index l ++ rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.status ) of
        ( Change c, Editting ) ->
            ( { model | content = c }, Cmd.none )

        ( Edit, Showing ) ->
            ( { model | status = Editting }, Cmd.none )

        ( Partitions pmsg, Showing ) ->
            ( { model | partition = updatePartition pmsg model.partition }, Cmd.none )

        ( LoadCards, Editting ) ->
            let
                res =
                    parseDeck model.content

                errors =
                    if List.isEmpty res.err then
                        Errors.init

                    else
                        List.foldl Errors.add Errors.init res.err

                missingCards =
                    subtract res.ok (Dict.keys model.db)
            in
            ( { model
                | status = Showing
                , errors = errors
                , deck = res.ok
              }
            , Cmd.batch <| List.map loadCard <| unique missingCards
            )

        ( GotCard name res, _ ) ->
            case res of
                Err _ ->
                    ( { model | errors = Errors.add ("Coudn't get [" ++ name ++ "]") model.errors }, Cmd.none )

                Ok card ->
                    ( { model | db = Dict.insert name card model.db }, Cmd.none )

        ( FindBest, _ ) ->
            ( { model | partition = findBestPartition model }, Cmd.none )

        _ ->
            ( { model | errors = Errors.add "Shoudnt have gootten a message in this state" model.errors }, Cmd.none )


findBestPartition : Model -> List Partitioner
findBestPartition model =
    let
        cards =
            lookupCards model.db model.deck
    in
    (\x -> List.concatMap subsequences (permutations x)) partitionsWithoutName
        |> List.map (\p -> p ++ [ namePartition ])
        |> List.sortBy (\parts -> Partition.cost <| partitionBy parts cards)
        |> List.head
        |> Maybe.withDefault []


unique : List comparable -> List comparable
unique =
    Set.fromList >> Set.toList


lookupCards : Dict CardName Card -> List CardName -> List Card
lookupCards db cards =
    List.filterMap (\card -> Dict.get card db) cards


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = deckExample
      , errors = Errors.init
      , status = Editting
      , db = Dict.empty
      , deck = []
      , partition = []
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
