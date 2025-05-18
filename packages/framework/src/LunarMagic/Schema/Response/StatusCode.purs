module LunarMagic.Schema.Response.StatusCode where

import Type.Data.Maybe (Just, Maybe, Nothing)

data HttpResponseStatus

foreign import data Continue :: HttpResponseStatus
foreign import data Switching_Protocols :: HttpResponseStatus
foreign import data Processing :: HttpResponseStatus
foreign import data Early_Hints :: HttpResponseStatus
foreign import data OK :: HttpResponseStatus
foreign import data Created :: HttpResponseStatus
foreign import data Accepted :: HttpResponseStatus
foreign import data Non_Authoritative_Information :: HttpResponseStatus
foreign import data No_Content :: HttpResponseStatus
foreign import data Reset_Content :: HttpResponseStatus
foreign import data Partial_Content :: HttpResponseStatus
foreign import data Multi_Status :: HttpResponseStatus
foreign import data Already_Reported :: HttpResponseStatus
foreign import data IM_Used :: HttpResponseStatus
foreign import data Multiple_Choices :: HttpResponseStatus
foreign import data Moved_Permanently :: HttpResponseStatus
foreign import data Found :: HttpResponseStatus
foreign import data See_Other :: HttpResponseStatus
foreign import data Not_Modified :: HttpResponseStatus
foreign import data Use_Proxy :: HttpResponseStatus
foreign import data Temopary_Redirect :: HttpResponseStatus
foreign import data Permanent_Redirect :: HttpResponseStatus
foreign import data Bad_Request :: HttpResponseStatus
foreign import data Unauthorized :: HttpResponseStatus
foreign import data Payment_Required  :: HttpResponseStatus
foreign import data Forbidden :: HttpResponseStatus
foreign import data Not_Found :: HttpResponseStatus
foreign import data Method_Not_Allowed :: HttpResponseStatus
foreign import data Not_Acceptable :: HttpResponseStatus
foreign import data Proxy_Authentication_Required :: HttpResponseStatus
foreign import data Request_Timeout :: HttpResponseStatus
foreign import data Conflict :: HttpResponseStatus
foreign import data Gone :: HttpResponseStatus
foreign import data Length_Required :: HttpResponseStatus
foreign import data Precondition_Failed :: HttpResponseStatus
foreign import data Payload_Too_Large :: HttpResponseStatus
foreign import data URI_Too_Long :: HttpResponseStatus
foreign import data Unsupported_Media_Type :: HttpResponseStatus
foreign import data Range_Not_Satisfiable :: HttpResponseStatus
foreign import data Expectation_Failed :: HttpResponseStatus
foreign import data I'm_a_teapot :: HttpResponseStatus
foreign import data Misdirected_Request :: HttpResponseStatus
foreign import data Unprocessable_Content :: HttpResponseStatus
foreign import data Locked :: HttpResponseStatus
foreign import data Faile_Dependency :: HttpResponseStatus
foreign import data Too_Early :: HttpResponseStatus
foreign import data Upgrade_Required  :: HttpResponseStatus
foreign import data Precondition_Required :: HttpResponseStatus
foreign import data Too_Many_Requests :: HttpResponseStatus
foreign import data Request_Header_Fields_Too_Large :: HttpResponseStatus
foreign import data Unavailable_For_Legal_Reasons :: HttpResponseStatus
foreign import data Internal_Server_Error :: HttpResponseStatus
foreign import data Not_Implemented :: HttpResponseStatus
foreign import data Bad_Gateway :: HttpResponseStatus
foreign import data Service_Unavailable :: HttpResponseStatus
foreign import data Gateway_Timeout :: HttpResponseStatus
foreign import data HTTP_Version_Not_Supported :: HttpResponseStatus
foreign import data Variant_Also_Negotiates :: HttpResponseStatus
foreign import data Insufficient_Storage :: HttpResponseStatus
foreign import data Loop_Detected :: HttpResponseStatus
foreign import data Not_Extended :: HttpResponseStatus
foreign import data Network_Authentication_Required :: HttpResponseStatus

class FromStatusCode :: HttpResponseStatus -> Symbol -> Constraint
class FromStatusCode a code | a -> code

instance FromStatusCode Continue "100"
instance FromStatusCode Switching_Protocols "101"
instance FromStatusCode Processing "102"
instance FromStatusCode Early_Hints "103"
instance FromStatusCode OK "200"
instance FromStatusCode Created "201"
instance FromStatusCode Accepted "202"
instance FromStatusCode Non_Authoritative_Information "203"
instance FromStatusCode No_Content "204"
instance FromStatusCode Reset_Content "205"
instance FromStatusCode Partial_Content "206"
instance FromStatusCode Multi_Status "207"
instance FromStatusCode Already_Reported "208"
instance FromStatusCode IM_Used "226"
instance FromStatusCode Multiple_Choices "300"
instance FromStatusCode Moved_Permanently "301"
instance FromStatusCode Found "302" 
instance FromStatusCode See_Other "303"
instance FromStatusCode Not_Modified "304"
instance FromStatusCode Use_Proxy "305"
instance FromStatusCode Temopary_Redirect "307"
instance FromStatusCode Permanent_Redirect "308"
instance FromStatusCode Bad_Request "400"
instance FromStatusCode Unauthorized "401"
instance FromStatusCode Payment_Required "402"
instance FromStatusCode Forbidden "403"
instance FromStatusCode Not_Found "404"
instance FromStatusCode Method_Not_Allowed "405"
instance FromStatusCode Not_Acceptable "406"  
instance FromStatusCode Proxy_Authentication_Required "407"
instance FromStatusCode Request_Timeout "408"
instance FromStatusCode Conflict "409"
instance FromStatusCode Gone "410"
instance FromStatusCode Length_Required "411"
instance FromStatusCode Precondition_Failed "412"
instance FromStatusCode Payload_Too_Large "413"
instance FromStatusCode URI_Too_Long "414"
instance FromStatusCode Unsupported_Media_Type "415"
instance FromStatusCode Range_Not_Satisfiable "416"
instance FromStatusCode Expectation_Failed "417"
instance FromStatusCode I'm_a_teapot "418"
instance FromStatusCode Misdirected_Request "421"
instance FromStatusCode Unprocessable_Content "422"
instance FromStatusCode Locked "423"
instance FromStatusCode Faile_Dependency "424"
instance FromStatusCode Too_Early "425"
instance FromStatusCode Upgrade_Required "426"
instance FromStatusCode Precondition_Required "428"
instance FromStatusCode Too_Many_Requests "429"
instance FromStatusCode Request_Header_Fields_Too_Large "431"
instance FromStatusCode Unavailable_For_Legal_Reasons "451"
instance FromStatusCode Internal_Server_Error "500"
instance FromStatusCode Not_Implemented "501"
instance FromStatusCode Bad_Gateway "502"
instance FromStatusCode Service_Unavailable "503"
instance FromStatusCode Gateway_Timeout "504"
instance FromStatusCode HTTP_Version_Not_Supported "505"
instance FromStatusCode Variant_Also_Negotiates "506"
instance FromStatusCode Insufficient_Storage "507"
instance FromStatusCode Loop_Detected "508"
instance FromStatusCode Not_Extended "510"
instance FromStatusCode Network_Authentication_Required "511"

class ToStatusCode :: Symbol -> Maybe HttpResponseStatus -> Constraint
class ToStatusCode code a | code -> a

instance ToStatusCode "100" (Just Continue)
else instance ToStatusCode "101" (Just Switching_Protocols)
else instance ToStatusCode "102" (Just Processing)
else instance ToStatusCode "103" (Just Early_Hints)
else instance ToStatusCode "200" (Just OK)
else instance ToStatusCode "201" (Just Created)
else instance ToStatusCode "202" (Just Accepted)
else instance ToStatusCode "203" (Just Non_Authoritative_Information)
else instance ToStatusCode "204" (Just No_Content)
else instance ToStatusCode "205" (Just Reset_Content)
else instance ToStatusCode "206" (Just Partial_Content)
else instance ToStatusCode "207" (Just Multi_Status)
else instance ToStatusCode "208" (Just Already_Reported)
else instance ToStatusCode "226" (Just IM_Used)
else instance ToStatusCode "300" (Just Multiple_Choices)
else instance ToStatusCode "301" (Just Moved_Permanently)
else instance ToStatusCode "302" (Just Found)
else instance ToStatusCode "303" (Just See_Other)
else instance ToStatusCode "304" (Just Not_Modified)
else instance ToStatusCode "305" (Just Use_Proxy)
else instance ToStatusCode "307" (Just Temopary_Redirect)
else instance ToStatusCode "308" (Just Permanent_Redirect)
else instance ToStatusCode "400" (Just Bad_Request)
else instance ToStatusCode "401" (Just Unauthorized) 
else instance ToStatusCode "402" (Just Payment_Required)
else instance ToStatusCode "403" (Just Forbidden)
else instance ToStatusCode "404" (Just Not_Found)
else instance ToStatusCode "405" (Just Method_Not_Allowed)
else instance ToStatusCode "406" (Just Not_Acceptable)
else instance ToStatusCode "407" (Just Proxy_Authentication_Required)
else instance ToStatusCode "408" (Just Request_Timeout)
else instance ToStatusCode "409" (Just Conflict)
else instance ToStatusCode "410" (Just Gone)
else instance ToStatusCode "411" (Just Length_Required)
else instance ToStatusCode "412" (Just Precondition_Failed)
else instance ToStatusCode "413" (Just Payload_Too_Large)
else instance ToStatusCode "414" (Just URI_Too_Long)
else instance ToStatusCode "415" (Just Unsupported_Media_Type)
else instance ToStatusCode "416" (Just Range_Not_Satisfiable)
else instance ToStatusCode "417" (Just Expectation_Failed)
else instance ToStatusCode "418" (Just I'm_a_teapot)
else instance ToStatusCode "421" (Just Misdirected_Request)
else instance ToStatusCode "422" (Just Unprocessable_Content)
else instance ToStatusCode "423" (Just Locked)
else instance ToStatusCode "424" (Just Faile_Dependency)
else instance ToStatusCode "425" (Just Too_Early)
else instance ToStatusCode "426" (Just Upgrade_Required)
else instance ToStatusCode "428" (Just Precondition_Required)
else instance ToStatusCode "429" (Just Too_Many_Requests)
else instance ToStatusCode "431" (Just Request_Header_Fields_Too_Large)
else instance ToStatusCode "451" (Just Unavailable_For_Legal_Reasons)
else instance ToStatusCode "500" (Just Internal_Server_Error)
else instance ToStatusCode "501" (Just Not_Implemented)
else instance ToStatusCode "502" (Just Bad_Gateway)
else instance ToStatusCode "503" (Just Service_Unavailable)
else instance ToStatusCode "504" (Just Gateway_Timeout)
else instance ToStatusCode "505" (Just HTTP_Version_Not_Supported)
else instance ToStatusCode "506" (Just Variant_Also_Negotiates)
else instance ToStatusCode "507" (Just Insufficient_Storage)
else instance ToStatusCode "508" (Just Loop_Detected)
else instance ToStatusCode "510" (Just Not_Extended)
else instance ToStatusCode "511" (Just Network_Authentication_Required)
else instance ToStatusCode other Nothing
