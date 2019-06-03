module Messages exposing (..)

import Common exposing (BuildResult)
import Http exposing (Error)
import Material
import Model exposing (Build, BuildDef, PersistedData)
import Time exposing (Time)
import Ports exposing (..)
import Material.Snackbar as Snackbar

type ABVMsg
    = ABCancelClicked
    | ABOkClicked
    | ABBambooServerUrlChanged String
    | ABBambooUsernameChanged String
    | ABBambooPasswordChanged String
    | ABBambooPlanChanged String
    | ABTabClicked Int
    | ABTravisServerUrlChanged String
    | ABTravisTokenChanged String
    | ABTravisRepoChanged String
    | ABTravisBranchChanged String
    | ABGitlabServerUrlChanged String
    | ABGitlabTokenChanged String
    | ABGitlabProjectChanged String
    | ABGitlabBranchChanged String
    | ABImportTextChanged String


type BVMsg
    = BVAddBuildClicked
    | BVEditClicked Build
    | BVDeleteClicked Build
    | BVAboutClicked
    | BVPrefsClicked
    | BVPrefsToggleNotif
    | BVPrefsPollingChanged String
    | BVPrefsExternalToolChanged String
    | BVNowReceived Time
    | BVBuildClicked Build
    | BVCloseClicked
    | BVCopyClicked Build
    | BVFilterChanged String
    | BVClearFilter
    | BVShareClicked Build
    | BVShareAllClicked
    | BVShowFilterClicked
    | BVFilterFocusResult
    | BVTagsClicked Build
    | BVDeleteTagClicked String
    | BVTagsChanged String
    | BVTagsKeyUp Int
    | BVRaiseTag String
    | BVBuildHover Bool Build
    | BVTagClicked String

type Msg
    = BuildsViewMsg BVMsg
    | AddBuildViewMsg ABVMsg
    | Tick Time
    | FetchResult BuildDef (Result Http.Error (BuildResult, BuildDef))
    | Mdl (Material.Msg Msg)
    | OnDataLoadError DataLoadError
    | OnDataLoadFileNotFound DataLoadFileNotFound
    | OnDataLoadSuccess String
    | OnDataSaveError DataSaveError
    | OnDataSaveSuccess DataSaveSuccess
    | Snackbar (Snackbar.Msg Int)
    | OpenUrl String
    | CopyToClipboard String
    | OnCopiedToClipboard String
    | LayoutTabClicked Int
