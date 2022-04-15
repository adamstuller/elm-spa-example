module Main exposing (main)

import Alt exposing (add, basicParser, emptyFooter, initRouter, join)
import Api exposing (Cred)
import Article.Slug exposing (Slug)
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Dom exposing (Error(..))
import Flip exposing (flip)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Home as Home
import Page.Login as Login
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Page.NotFound as NotFound
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Username exposing (Username)
import Viewer exposing (Viewer)


title : String
title =
    "Conduit"


main =
    NotFound.initPageWidget (basicParser "whatever")
        |> flip join (Register.initPageWidget (basicParser "register"))
        |> flip add (Settings.initPageWidget (basicParser "settings"))
        |> flip add (Login.initPageWidget (basicParser "login"))
        |> flip add (Home.initPageWidget Home.parser)
        |> flip add (Profile.initPageWidget Profile.parser)
        |> flip add (Article.initPageWidget Article.parser)
        |> flip add (Editor.initPageWidgetNew Editor.parserNew)
        |> flip add (Editor.initPageWidgetEdit Editor.parserEdit)
        |> initRouter title Page.viewNavbar emptyFooter
        |> Browser.application
