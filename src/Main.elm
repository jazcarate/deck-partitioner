module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Errors exposing (Errors)
import Html exposing (..)
import Html.Attributes exposing (placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, string)
import List
import Parser exposing ((|.), (|=), Parser)
import Partition exposing (Partition(..))
import Set
import String


classifyOn : Partitioner a -> List a -> List (List a)
classifyOn field =
    let
        classification a b =
            field.pfunc a == field.pfunc b
    in
    Partition.classifyBy classification
        >> List.map (List.sortBy field.pfunc)


partition : Partitioner a -> Partition a -> Partition a
partition field p =
    case p of
        One ps ->
            Many <| List.map One <| classifyOn field ps

        Many ts ->
            Many (List.map (partition field) ts)


type alias Card =
    { color : String, cmc : String, name : String, type_line : String }


unlines : List String -> String
unlines ls =
    List.foldr (\x y -> x ++ "\n" ++ y) "" ls


showCard : Card -> String
showCard =
    .name


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
        [ viewInner m
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
                [ textarea [ placeholder deckExample, value model.content, onInput Change ] []
                , button [ type_ "submit" ] [ text "Load" ]
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
                [ button [ onClick Edit ] [ text "<< Edit" ]
                , div []
                    [ span [] <| List.indexedMap viewPartition model.partition
                    , span [] [ text "by name" ]
                    ]
                , div [] [ button [ onClick (Partitions PAdd) ] [ text "add partition" ] ]
                , div [] [ button [ onClick FindBest ] [ text "Find best partition" ] ]
                , div [] [ text <| "Total cost: " ++ String.fromFloat (Partition.cost tree) ]
                , span [] <|
                    if List.length cards < List.length model.deck then
                        [ text <| "missing " ++ String.fromInt (List.length model.deck - List.length cards) ++ " cards" ]

                    else
                        [ text <| "complete!" ]
                , showPartition tree
                ]


viewPartition : Int -> Partitioner Card -> Html Msg
viewPartition i p =
    span []
        [ Html.select
            []
            (List.map (\this -> viewOption i (p.pname == this.pname) this) partitions)
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


viewOption : Int -> Bool -> Partitioner Card -> Html Msg
viewOption i sel p =
    option
        [ onClick (Partitions (PChange i p)), selected sel ]
        [ text <| p.pname ]


partitionBy : List (Partitioner Card) -> List Card -> Partition Card
partitionBy pts cards =
    let
        allPts =
            List.append pts [ namePartition ]
    in
    List.foldl partition (One cards) allPts


showPartition : Partition Card -> Html msg
showPartition p =
    Partition.renderPartition showCard p
        |> unlines
        |> text
        |> List.singleton
        |> Html.pre []


type Status
    = Editting
    | Showing


type alias PartitionModel =
    List (Partitioner Card)


type alias Model =
    { content : String
    , errors : Errors
    , status : Status
    , db : Dict CardName Card
    , deck : List CardName
    , partition : PartitionModel
    }


type alias Comparable =
    String


type alias Partitioner a =
    { pfunc : a -> Comparable, pname : String }


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
    | PChange Int (Partitioner Card)


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


cardDecoder : Decoder Card
cardDecoder =
    Json.Decode.map4 (\color_ cmc_ name_ type_line_ -> { color = color_, cmc = cmc_, name = name_, type_line = type_line_ })
        (Json.Decode.map (Maybe.withDefault "") <| Json.Decode.maybe <| Json.Decode.field "colors" colorsDecode)
        (Json.Decode.map String.fromInt <| Json.Decode.field "cmc" Json.Decode.int)
        (Json.Decode.field "name" string)
        (Json.Decode.map typeClosure <| Json.Decode.field "type_line" string)


typeClosure : String -> String
typeClosure type_line =
    if String.startsWith "Basic Land" type_line || String.startsWith "Land" type_line then
        "Land"

    else
        String.split "—" type_line
            |> List.head
            |> Maybe.map String.trim
            |> Maybe.withDefault type_line


colorsDecode : Decoder String
colorsDecode =
    Json.Decode.list string
        |> Json.Decode.map (List.sort >> String.concat)


subtract : List comparable -> List comparable -> List comparable
subtract a b =
    List.filter (\x -> not <| List.member x b) a


defaultPartition : Partitioner Card
defaultPartition =
    { pfunc = .cmc, pname = "by mana value" }


namePartition : Partitioner Card
namePartition =
    { pfunc = .name, pname = "by name" }


partitions : List (Partitioner Card)
partitions =
    [ defaultPartition
    , { pfunc = .color, pname = "by color" }
    , { pfunc = .type_line, pname = "by type" }
    , { pfunc = landiness, pname = "land/non-land" }
    , { pfunc = colorness, pname = "mono/multi colored" }
    ]


landiness : Card -> String
landiness c =
    case c.type_line of
        "Land" ->
            "Land"

        _ ->
            "Non-Land"


colorness : Card -> String
colorness c =
    if String.length c.color > 1 then
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


findBestPartition : Model -> List (Partitioner Card)
findBestPartition model =
    let
        cards =
            lookupCards model.db model.deck
    in
    (\x -> List.concatMap subsequences (permutations x)) partitions
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
