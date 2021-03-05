module Functions ( parseInputFile
                 , parseInputText
                 , validateRawEventPlayText
                 , parseValidEventPlayText
                 , applyFilter
                 , getEventPlayScriptText ) where

import Data
import Types

parseInputFile :: InputFile -> Either FileAccessError InputText

parseInputFile inputFilePath = Left "parseInputFile is unimplemented"

parseInputText :: InputText -> Either TextParsingError RawEventPlayText

parseInputText inputText = Left "parseInputText is unimplemented"

validateRawEventPlayText :: RawEventPlayText -> Either EventPlayValidationError ValidEventPlayText

validateRawEventPlayText rawEventPlayText = Left "validateRawEventPlayText is unimplemented"

parseValidEventPlayText :: ValidEventPlayText -> Either PlayParseError EventPlayScript

parseValidEventPlayText validEventPlayText = Left "parseValidEventPlayText is unimplemented"

applyFilter :: EventPlayScript -> Filter -> Either FilterError EventPlayScript

applyFilter eventPlayScript filter = Left "applyFilter is unimplemented"

getEventPlayScriptText :: EventPlayScript -> ValidEventPlayText

getEventPlayScriptText eventPlayScript = ValidEventPlayText { validUsersSection = []
                                                            , validSystemsSection = []
                                                            , validDataTypesSection = []
                                                            , validSettingSection = ""
                                                            , validScopeSection = ""
                                                            , validScriptSection = [] }

getEventPlayOutputText :: ValidEventPlayText -> OutputText

getEventPlayOutputText validEventPlayText = "getEventPlayOutputText is unimplemented"
