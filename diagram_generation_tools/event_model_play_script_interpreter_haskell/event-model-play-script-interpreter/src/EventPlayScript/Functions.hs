module EventPlayScript.Functions where

import Data.Text
import Data.Text.Conversions

import EventPlayScript.Data
import qualified EventPlayScript.InternalFunctions as I
import qualified EventPlayScript.Script.Data as SD
import qualified EventPlayScript.Script.Functions as SF

-----------------Function Declarations-----------------
filter :: EventPlayScript -> SD.FilterParameter -> EventPlayScript

----------------Function Implementations----------------
filter eventPlayScript filterParameter =
    let filteredScript = SF.hideUnnecessaryLines (script eventPlayScript) filterParameter
        updatedUsers = I.filterDefinitionsByScript filteredScript (users eventPlayScript) I.ActorName
        updatedSystems = I.filterDefinitionsByScript filteredScript (systems eventPlayScript) I.ActorName
        updatedDataTypes = I.filterDefinitionsByScript filteredScript (dataTypes eventPlayScript) I.LineContent
    in eventPlayScript { script = filteredScript
                       , users = updatedUsers
                       , systems = updatedSystems
                       , dataTypes = updatedDataTypes }
