module Functions (Functions.filter) where

import Data.Text
import Data.Text.Conversions

import Data
import Internal.Script

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

data SearchParameter = ActorName | LineContent

filterDefinitionsByScript :: Script -> [ Definition ] -> SearchParameter -> [ Definition ]
filterDefinitionsByScript script definitions ActorName = Prelude.filter (\x -> (actorInScript script) (name x)) definitions
filterDefinitionsByScript script word LineContent = Prelude.filter (\x -> (mentionedInScript script) (name x)) word
