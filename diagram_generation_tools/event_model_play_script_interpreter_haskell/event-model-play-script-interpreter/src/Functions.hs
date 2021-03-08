module Functions where

import Data.Text
import Data.Text.Conversions

import Data
import InternalFunctions
import InternalDomains.ScriptDomain.Data
import InternalDomains.ScriptDomain.Functions

filter :: EventPlayScript -> FilterParameter -> EventPlayScript
filter eventPlayScript filterParameter =
    let filteredScript = hideUnnecessaryLines (script eventPlayScript) filterParameter
        updatedUsers = filterDefinitionsByScript filteredScript (users eventPlayScript) ActorName
        updatedSystems = filterDefinitionsByScript filteredScript (systems eventPlayScript) ActorName
        updatedDataTypes = filterDefinitionsByScript filteredScript (dataTypes eventPlayScript) LineContent
    in eventPlayScript { script = filteredScript
                       , users = updatedUsers
                       , systems = updatedSystems
                       , dataTypes = updatedDataTypes }
