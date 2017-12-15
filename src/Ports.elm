port module Ports exposing (..)

import Json.Decode exposing (..)

port onDataLoadSuccess : (String -> m) -> Sub m

type alias DataLoadFileNotFound =
    { dataFileName : String
    }

port onDataLoadFileNotFound : (DataLoadFileNotFound -> m) -> Sub m

type alias DataLoadError =
    { error : Value
    }

port onDataLoadError : (DataLoadError -> m) -> Sub m

type alias DataSaveError =
    { dataFileName : String
    , error : Value
    }

port onDataSaveError : (DataSaveError -> m) -> Sub m

type alias DataSaveSuccess =
    { dataFileName : String
    }

port onDataSaveSuccess : (DataSaveSuccess -> m) -> Sub m

port saveData : Value -> Cmd m

port loadData : () -> Cmd m

port openURL : String -> Cmd m

type alias DesktopNotif =
    { title : String
    , body : String
    , isGreen : Bool
    }

port desktopNotification : DesktopNotif -> Cmd m

port quit : () -> Cmd m

port closeWindow : () -> Cmd m

type alias CopyInput =
    { nodeId : String
    , data : String
    }

port copyToClipboard : CopyInput -> Cmd m

port onCopiedToClipboard : (String -> m) -> Sub m
