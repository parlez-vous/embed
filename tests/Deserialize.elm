module Deserialize exposing (suite)

{-| Tests for complex JSON parsing
-}

import Expect
import Api.Input as Input
import Api.Input.Comment as Comment 
import Json.Decode as D exposing (Decoder)
import RemoteData 
import Test exposing (..)
import Time



json : String
json = """{
    "data": {
        "comments": [
            {
                "id": "ckhxpcv2l0056b7t28lc2qnii",
                "anon_author_name": "unbeatable-caped-terror",
                "body": "A top-level comment",
                "votes": 0,
                "post_id": "ckhxpc7sh0032b7t24cmlli2j",
                "created_at": 1606326555981,
                "updated_at": 1606326555981,
                "parent_comment_id": null,
                "author_id": null,
                "author": null,
                "replies": []
            },
            {
                "id": "ckhxpde9v0069b7t2tuddh7lm",
                "anon_author_name": "infamous-sir-master",
                "body": "Another top-level comment",
                "votes": 0,
                "post_id": "ckhxpc7sh0032b7t24cmlli2j",
                "created_at": 1606326580868,
                "updated_at": 1606326580868,
                "parent_comment_id": null,
                "author_id": null,
                "author": null,
                "replies": [
                    {
                        "id": "ckhxpdorx0077b7t2y28g02dj",
                        "anon_author_name": "criminal-tomorrow-storm",
                        "body": "Yo this is a reply",
                        "votes": 0,
                        "post_id": "ckhxpc7sh0032b7t24cmlli2j",
                        "created_at": 1606326594477,
                        "updated_at": 1606326594478,
                        "parent_comment_id": "ckhxpde9v0069b7t2tuddh7lm",
                        "author_id": null,
                        "author": null,
                        "replies": [
                            {
                                "id": "ckhxt2pj11340b7t2st4bjg0f",
                                "anon_author_name": "jolly-arduous-fang",
                                "body": "Yo this is a level 2 reply",
                                "votes": 0,
                                "post_id": "ckhxpc7sh0032b7t24cmlli2j",
                                "created_at": 1606332800702,
                                "updated_at": 1606332800702,
                                "parent_comment_id": "ckhxpdorx0077b7t2y28g02dj",
                                "author_id": null,
                                "author": null,
                                "replies": null
                            }
                        ]
                    }
                ]
            }
        ],
        "siteVerified": false,
        "postId": "ckhxpc7sh0032b7t24cmlli2j",
        "leafIds": [
            "ckhxpcv2l0056b7t28lc2qnii",
            "ckihx0xo80137bit2zl2611mu"
        ]
    }
}"""



decoder : Decoder Input.InitialCommentTree
decoder =
    Input.apiResponseDecoder Input.commentTreeDecoder


suite : Test
suite =
    describe "Deserialize tests"
        [ test "Correctly converts comment graph into a valid comment tree" <|
            \_ ->
                let
                    expectedValue =
                        { siteVerified = False
                        , postId = "ckhxpc7sh0032b7t24cmlli2j"
                        , leafIds =
                            [ "ckhxpcv2l0056b7t28lc2qnii"
                            , "ckihx0xo80137bit2zl2611mu"
                            ]
                        , comments =
                            [ { id = "ckhxpcv2l0056b7t28lc2qnii"
                              , anonymousAuthorName = "unbeatable-caped-terror"
                              , body = "A top-level comment"
                              , votes = 0
                              , createdAt = Time.millisToPosix 1606326555981
                              , replies =
                                    RemoteData.Success
                                        (Comment.Replies [ ])
                              }
                            , { id = "ckhxpde9v0069b7t2tuddh7lm"
                              , anonymousAuthorName = "infamous-sir-master"
                              , body = "Another top-level comment"
                              , votes = 0
                              , createdAt = Time.millisToPosix 1606326580868
                              , replies =
                                  RemoteData.Success <|
                                    Comment.Replies
                                        [ { id = "ckhxpdorx0077b7t2y28g02dj"
                                          , anonymousAuthorName = "criminal-tomorrow-storm"
                                          , body = "Yo this is a reply"
                                          , votes = 0
                                          , createdAt = Time.millisToPosix 1606326594477
                                          , replies = 
                                                RemoteData.Success <|
                                                    Comment.Replies
                                                        [ { id = "ckhxt2pj11340b7t2st4bjg0f"
                                                          , anonymousAuthorName = "jolly-arduous-fang"
                                                          , body = "Yo this is a level 2 reply"
                                                          , votes = 0
                                                          , createdAt = Time.millisToPosix 1606332800702
                                                          , replies = RemoteData.NotAsked
                                                          }
                                                        ]
                                          }
                                        ]
                              }
                            ]

                        }

                    result = D.decodeString decoder json
                in
                Expect.equal result (Ok expectedValue)

        ]
