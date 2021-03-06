module Functions ( Functions.filter ) where

import Data.Text
import Data.Text.Conversions

import Data
import Internal.Script

filter :: EventPlayScript -> FilterParameter -> EventPlayScript
filter eventPlayScript filterParameter =
    let script' = (script eventPlayScript)
        filteredScript = hideUnnecessaryLines script' filterParameter
        updatedUsers = filterDefinitionsUsingNewScript script' (users eventPlayScript)
        updatedSystems = filterDefinitionsUsingNewScript script' (systems eventPlayScript)
        updatedDataTypes = filterDataTypesUsingNewScript script' (dataTypes eventPlayScript)
    in eventPlayScript { script = filteredScript
                       , users = updatedUsers
                       , systems = updatedSystems
                       , dataTypes = updatedDataTypes }

filterDataTypesUsingNewScript :: Script -> [ DataType ] -> [ DataType ]
filterDataTypesUsingNewScript script dataTypes =
    Prelude.filter (scriptReferencesDataType script) dataTypes

scriptReferencesDataType :: Script -> DataType -> Bool
scriptReferencesDataType script dataType =
    Prelude.any (isDataTypeInsideActorScriptLine dataType) script

isDataTypeInsideActorScriptLine :: DataType -> ActorScriptLine -> Bool
isDataTypeInsideActorScriptLine dataType (HiddenLine hiddenSymbol) = False
isDataTypeInsideActorScriptLine dataType (Line _ scriptLine) =
    case scriptLine of (Event event) -> isDataTypeReferencedInEvent dataType event
                       (Broadcast broadcast) -> isDataTypeReferencedInBroadcast dataType broadcast

isDataTypeReferencedInEvent :: DataType -> Event -> Bool
isDataTypeReferencedInEvent dataType (ReceivedBroadcast broadcast) = isDataTypeReferencedInBroadcast dataType broadcast
isDataTypeReferencedInEvent dataType (Action actionContent) = isDataTypeReferencedInActionContent dataType actionContent

isDataTypeReferencedInActionContent :: DataType -> ActionContent -> Bool
isDataTypeReferencedInActionContent dataType actionContent =
    if toText (name dataType) `isInfixOf` toText actionContent then True else False

isDataTypeReferencedInBroadcast :: DataType -> Broadcast -> Bool
isDataTypeReferencedInBroadcast dataType (REQUEST _ _ parameters ) =
    if toText (name dataType) `isInfixOf` toText parameters then True else False
isDataTypeReferencedInBroadcast dataType (RESPONSE _ _ response) =
    if toText (name dataType) `isInfixOf` toText response then True else False

filterDefinitionsUsingNewScript :: Script -> [ Definition ] -> [ Definition ]
filterDefinitionsUsingNewScript script definitions =
    Prelude.filter (scriptReferencesDefinition script) definitions

scriptReferencesDefinition :: Script -> Definition -> Bool
scriptReferencesDefinition script definition =
    Prelude.any (isDefinitionInsideActorScriptLine definition) script

isDefinitionInsideActorScriptLine :: Definition -> ActorScriptLine -> Bool
isDefinitionInsideActorScriptLine definition (HiddenLine hiddenSymbol) = False
isDefinitionInsideActorScriptLine definition (Line actor _) =
    if toText (name definition) `isInfixOf` toText actor
    then True
    else False
