module LunarMagic.HttpStatus.Types where

import Type.Data.Maybe (Just, Maybe, Nothing)

data HttpStatus

foreign import data Continue :: HttpStatus
foreign import data Switching_Protocols :: HttpStatus
foreign import data Processing :: HttpStatus
foreign import data Early_Hints :: HttpStatus
foreign import data OK :: HttpStatus
foreign import data Created :: HttpStatus
foreign import data Accepted :: HttpStatus
foreign import data Non_Authoritative_Information :: HttpStatus
foreign import data No_Content :: HttpStatus
foreign import data Reset_Content :: HttpStatus
foreign import data Partial_Content :: HttpStatus
foreign import data Multi_Status :: HttpStatus
foreign import data Already_Reported :: HttpStatus
foreign import data IM_Used :: HttpStatus
foreign import data Multiple_Choices :: HttpStatus
foreign import data Moved_Permanently :: HttpStatus
foreign import data Found :: HttpStatus
foreign import data See_Other :: HttpStatus
foreign import data Not_Modified :: HttpStatus
foreign import data Use_Proxy :: HttpStatus
foreign import data Temopary_Redirect :: HttpStatus
foreign import data Permanent_Redirect :: HttpStatus
foreign import data Bad_Request :: HttpStatus
foreign import data Unauthorized :: HttpStatus
foreign import data Payment_Required :: HttpStatus
foreign import data Forbidden :: HttpStatus
foreign import data Not_Found :: HttpStatus
foreign import data Method_Not_Allowed :: HttpStatus
foreign import data Not_Acceptable :: HttpStatus
foreign import data Proxy_Authentication_Required :: HttpStatus
foreign import data Request_Timeout :: HttpStatus
foreign import data Conflict :: HttpStatus
foreign import data Gone :: HttpStatus
foreign import data Length_Required :: HttpStatus
foreign import data Precondition_Failed :: HttpStatus
foreign import data Payload_Too_Large :: HttpStatus
foreign import data URI_Too_Long :: HttpStatus
foreign import data Unsupported_Media_Type :: HttpStatus
foreign import data Range_Not_Satisfiable :: HttpStatus
foreign import data Expectation_Failed :: HttpStatus
foreign import data I'm_a_teapot :: HttpStatus
foreign import data Misdirected_Request :: HttpStatus
foreign import data Unprocessable_Content :: HttpStatus
foreign import data Locked :: HttpStatus
foreign import data Faile_Dependency :: HttpStatus
foreign import data Too_Early :: HttpStatus
foreign import data Upgrade_Required :: HttpStatus
foreign import data Precondition_Required :: HttpStatus
foreign import data Too_Many_Requests :: HttpStatus
foreign import data Request_Header_Fields_Too_Large :: HttpStatus
foreign import data Unavailable_For_Legal_Reasons :: HttpStatus
foreign import data Internal_Server_Error :: HttpStatus
foreign import data Not_Implemented :: HttpStatus
foreign import data Bad_Gateway :: HttpStatus
foreign import data Service_Unavailable :: HttpStatus
foreign import data Gateway_Timeout :: HttpStatus
foreign import data HTTP_Version_Not_Supported :: HttpStatus
foreign import data Variant_Also_Negotiates :: HttpStatus
foreign import data Insufficient_Storage :: HttpStatus
foreign import data Loop_Detected :: HttpStatus
foreign import data Not_Extended :: HttpStatus
foreign import data Network_Authentication_Required :: HttpStatus

class FromStatusCode :: forall k. k -> Symbol -> Constraint
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

instance FromStatusCode "100" "100"
instance FromStatusCode "101" "101"
instance FromStatusCode "102" "102"
instance FromStatusCode "103" "103"
instance FromStatusCode "200" "200"
instance FromStatusCode "201" "201"
instance FromStatusCode "202" "202"
instance FromStatusCode "203" "203"
instance FromStatusCode "204" "204"
instance FromStatusCode "205" "205"
instance FromStatusCode "206" "206"
instance FromStatusCode "207" "207"
instance FromStatusCode "208" "208"
instance FromStatusCode "226" "226"
instance FromStatusCode "300" "300"
instance FromStatusCode "301" "301"
instance FromStatusCode "302" "302"
instance FromStatusCode "303" "303"
instance FromStatusCode "304" "304"
instance FromStatusCode "305" "305"
instance FromStatusCode "307" "307"
instance FromStatusCode "308" "308"
instance FromStatusCode "400" "400"
instance FromStatusCode "401" "401"
instance FromStatusCode "402" "402"
instance FromStatusCode "403" "403"
instance FromStatusCode "404" "404"
instance FromStatusCode "405" "405"
instance FromStatusCode "406" "406"
instance FromStatusCode "407" "407"
instance FromStatusCode "408" "408"
instance FromStatusCode "409" "409"
instance FromStatusCode "410" "410"
instance FromStatusCode "411" "411"
instance FromStatusCode "412" "412"
instance FromStatusCode "413" "413"
instance FromStatusCode "414" "414"
instance FromStatusCode "415" "415"
instance FromStatusCode "416" "416"
instance FromStatusCode "417" "417"
instance FromStatusCode "418" "418"
instance FromStatusCode "421" "421"
instance FromStatusCode "422" "422"
instance FromStatusCode "423" "423"
instance FromStatusCode "424" "424"
instance FromStatusCode "425" "425"
instance FromStatusCode "426" "426"
instance FromStatusCode "428" "428"
instance FromStatusCode "429" "429"
instance FromStatusCode "431" "431"
instance FromStatusCode "451" "451"
instance FromStatusCode "500" "500"
instance FromStatusCode "501" "501"
instance FromStatusCode "502" "502"
instance FromStatusCode "503" "503"
instance FromStatusCode "504" "504"
instance FromStatusCode "505" "505"
instance FromStatusCode "506" "506"
instance FromStatusCode "507" "507"
instance FromStatusCode "508" "508"
instance FromStatusCode "510" "510"
instance FromStatusCode "511" "511"

instance FromStatusCode 100 "100"
instance FromStatusCode 101 "101"
instance FromStatusCode 102 "102"
instance FromStatusCode 103 "103"
instance FromStatusCode 200 "200"
instance FromStatusCode 201 "201"
instance FromStatusCode 202 "202"
instance FromStatusCode 203 "203"
instance FromStatusCode 204 "204"
instance FromStatusCode 205 "205"
instance FromStatusCode 206 "206"
instance FromStatusCode 207 "207"
instance FromStatusCode 208 "208"
instance FromStatusCode 226 "226"
instance FromStatusCode 300 "300"
instance FromStatusCode 301 "301"
instance FromStatusCode 302 "302"
instance FromStatusCode 303 "303"
instance FromStatusCode 304 "304"
instance FromStatusCode 305 "305"
instance FromStatusCode 307 "307"
instance FromStatusCode 308 "308"
instance FromStatusCode 400 "400"
instance FromStatusCode 401 "401"
instance FromStatusCode 402 "402"
instance FromStatusCode 403 "403"
instance FromStatusCode 404 "404"
instance FromStatusCode 405 "405"
instance FromStatusCode 406 "406"
instance FromStatusCode 407 "407"
instance FromStatusCode 408 "408"
instance FromStatusCode 409 "409"
instance FromStatusCode 410 "410"
instance FromStatusCode 411 "411"
instance FromStatusCode 412 "412"
instance FromStatusCode 413 "413"
instance FromStatusCode 414 "414"
instance FromStatusCode 415 "415"
instance FromStatusCode 416 "416"
instance FromStatusCode 417 "417"
instance FromStatusCode 418 "418"
instance FromStatusCode 421 "421"
instance FromStatusCode 422 "422"
instance FromStatusCode 423 "423"
instance FromStatusCode 424 "424"
instance FromStatusCode 425 "425"
instance FromStatusCode 426 "426"
instance FromStatusCode 428 "428"
instance FromStatusCode 429 "429"
instance FromStatusCode 431 "431"
instance FromStatusCode 451 "451"
instance FromStatusCode 500 "500"
instance FromStatusCode 501 "501"
instance FromStatusCode 502 "502"
instance FromStatusCode 503 "503"
instance FromStatusCode 504 "504"
instance FromStatusCode 505 "505"
instance FromStatusCode 506 "506"
instance FromStatusCode 507 "507"
instance FromStatusCode 508 "508"
instance FromStatusCode 510 "510"
instance FromStatusCode 511 "511"

class ToStatusCode :: Symbol -> Maybe HttpStatus -> Constraint
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
else instance ToStatusCode _1 Nothing

class StatusCodeInt :: forall k. k -> Constraint
class StatusCodeInt code where
  statusCodeInt :: Int

instance statusCodeIntContinue :: StatusCodeInt Continue where
  statusCodeInt = 100

instance statusCodeIntSwitching_Protocols :: StatusCodeInt Switching_Protocols where
  statusCodeInt = 101

instance statusCodeIntProcessing :: StatusCodeInt Processing where
  statusCodeInt = 102

instance statusCodeIntEarly_Hints :: StatusCodeInt Early_Hints where
  statusCodeInt = 103

instance statusCodeIntOK :: StatusCodeInt OK where
  statusCodeInt = 200

instance statusCodeIntCreated :: StatusCodeInt Created where
  statusCodeInt = 201

instance statusCodeIntAccepted :: StatusCodeInt Accepted where
  statusCodeInt = 202

instance statusCodeIntNon_Authoritative_Information :: StatusCodeInt Non_Authoritative_Information where
  statusCodeInt = 203

instance statusCodeIntNo_Content :: StatusCodeInt No_Content where
  statusCodeInt = 204

instance statusCodeIntReset_Content :: StatusCodeInt Reset_Content where
  statusCodeInt = 205

instance statusCodeIntPartial_Content :: StatusCodeInt Partial_Content where
  statusCodeInt = 206

instance statusCodeIntMulti_Status :: StatusCodeInt Multi_Status where
  statusCodeInt = 207

instance statusCodeIntAlready_Reported :: StatusCodeInt Already_Reported where
  statusCodeInt = 208

instance statusCodeIntIM_Used :: StatusCodeInt IM_Used where
  statusCodeInt = 226

instance statusCodeIntMultiple_Choices :: StatusCodeInt Multiple_Choices where
  statusCodeInt = 300

instance statusCodeIntMoved_Permanently :: StatusCodeInt Moved_Permanently where
  statusCodeInt = 301

instance statusCodeIntFound :: StatusCodeInt Found where
  statusCodeInt = 302

instance statusCodeIntSee_Other :: StatusCodeInt See_Other where
  statusCodeInt = 303

instance statusCodeIntNot_Modified :: StatusCodeInt Not_Modified where
  statusCodeInt = 304

instance statusCodeIntUse_Proxy :: StatusCodeInt Use_Proxy where
  statusCodeInt = 305

instance statusCodeIntTemopary_Redirect :: StatusCodeInt Temopary_Redirect where
  statusCodeInt = 307

instance statusCodeIntPermanent_Redirect :: StatusCodeInt Permanent_Redirect where
  statusCodeInt = 308

instance statusCodeIntBad_Request :: StatusCodeInt Bad_Request where
  statusCodeInt = 400

instance statusCodeIntUnauthorized :: StatusCodeInt Unauthorized where
  statusCodeInt = 401

instance statusCodeIntPayment_Required :: StatusCodeInt Payment_Required where
  statusCodeInt = 402

instance statusCodeIntForbidden :: StatusCodeInt Forbidden where
  statusCodeInt = 403

instance statusCodeIntNot_Found :: StatusCodeInt Not_Found where
  statusCodeInt = 404

instance statusCodeIntMethod_Not_Allowed :: StatusCodeInt Method_Not_Allowed where
  statusCodeInt = 405

instance statusCodeIntNot_Acceptable :: StatusCodeInt Not_Acceptable where
  statusCodeInt = 406

instance statusCodeIntProxy_Authentication_Required :: StatusCodeInt Proxy_Authentication_Required where
  statusCodeInt = 407

instance statusCodeIntRequest_Timeout :: StatusCodeInt Request_Timeout where
  statusCodeInt = 408

instance statusCodeIntConflict :: StatusCodeInt Conflict where
  statusCodeInt = 409

instance statusCodeIntGone :: StatusCodeInt Gone where
  statusCodeInt = 410

instance statusCodeIntLength_Required :: StatusCodeInt Length_Required where
  statusCodeInt = 411

instance statusCodeIntPrecondition_Failed :: StatusCodeInt Precondition_Failed where
  statusCodeInt = 412

instance statusCodeIntPayload_Too_Large :: StatusCodeInt Payload_Too_Large where
  statusCodeInt = 413

instance statusCodeIntURI_Too_Long :: StatusCodeInt URI_Too_Long where
  statusCodeInt = 414

instance statusCodeIntUnsupported_Media_Type :: StatusCodeInt Unsupported_Media_Type where
  statusCodeInt = 415

instance statusCodeIntRange_Not_Satisfiable :: StatusCodeInt Range_Not_Satisfiable where
  statusCodeInt = 416

instance statusCodeIntExpectation_Failed :: StatusCodeInt Expectation_Failed where
  statusCodeInt = 417

instance statusCodeIntI'm_a_teapot :: StatusCodeInt I'm_a_teapot where
  statusCodeInt = 418

instance statusCodeIntMisdirected_Request :: StatusCodeInt Misdirected_Request where
  statusCodeInt = 421

instance statusCodeIntUnprocessable_Content :: StatusCodeInt Unprocessable_Content where
  statusCodeInt = 422

instance statusCodeIntLocked :: StatusCodeInt Locked where
  statusCodeInt = 423

instance statusCodeIntFaile_Dependency :: StatusCodeInt Faile_Dependency where
  statusCodeInt = 424

instance statusCodeIntToo_Early :: StatusCodeInt Too_Early where
  statusCodeInt = 425

instance statusCodeIntUpgrade_Required :: StatusCodeInt Upgrade_Required where
  statusCodeInt = 426

instance statusCodeIntPrecondition_Required :: StatusCodeInt Precondition_Required where
  statusCodeInt = 428

instance statusCodeIntToo_Many_Requests :: StatusCodeInt Too_Many_Requests where
  statusCodeInt = 429

instance statusCodeIntRequest_Header_Fields_Too_Large :: StatusCodeInt Request_Header_Fields_Too_Large where
  statusCodeInt = 431

instance statusCodeIntUnavailable_For_Legal_Reasons :: StatusCodeInt Unavailable_For_Legal_Reasons where
  statusCodeInt = 451

instance statusCodeIntInternal_Server_Error :: StatusCodeInt Internal_Server_Error where
  statusCodeInt = 500

instance statusCodeIntNot_Implemented :: StatusCodeInt Not_Implemented where
  statusCodeInt = 501

instance statusCodeIntBad_Gateway :: StatusCodeInt Bad_Gateway where
  statusCodeInt = 502

instance statusCodeIntService_Unavailable :: StatusCodeInt Service_Unavailable where
  statusCodeInt = 503

instance statusCodeIntGateway_Timeout :: StatusCodeInt Gateway_Timeout where
  statusCodeInt = 504

instance statusCodeIntHTTP_Version_Not_Supported :: StatusCodeInt HTTP_Version_Not_Supported where
  statusCodeInt = 505

instance statusCodeIntVariant_Also_Negotiates :: StatusCodeInt Variant_Also_Negotiates where
  statusCodeInt = 506

instance statusCodeIntInsufficient_Storage :: StatusCodeInt Insufficient_Storage where
  statusCodeInt = 507

instance statusCodeIntLoop_Detected :: StatusCodeInt Loop_Detected where
  statusCodeInt = 508

instance statusCodeIntNot_Extended :: StatusCodeInt Not_Extended where
  statusCodeInt = 510

instance statusCodeIntNetwork_Authentication_Required :: StatusCodeInt Network_Authentication_Required where
  statusCodeInt = 511

instance statusCodeIntSymbol100 :: StatusCodeInt "100" where
  statusCodeInt = 100

instance statusCodeIntSymbol101 :: StatusCodeInt "101" where
  statusCodeInt = 101

instance statusCodeIntSymbol102 :: StatusCodeInt "102" where
  statusCodeInt = 102

instance statusCodeIntSymbol103 :: StatusCodeInt "103" where
  statusCodeInt = 103

instance statusCodeIntSymbol200 :: StatusCodeInt "200" where
  statusCodeInt = 200

instance statusCodeIntSymbol201 :: StatusCodeInt "201" where
  statusCodeInt = 201

instance statusCodeIntSymbol202 :: StatusCodeInt "202" where
  statusCodeInt = 202

instance statusCodeIntSymbol203 :: StatusCodeInt "203" where
  statusCodeInt = 203

instance statusCodeIntSymbol204 :: StatusCodeInt "204" where
  statusCodeInt = 204

instance statusCodeIntSymbol205 :: StatusCodeInt "205" where
  statusCodeInt = 205

instance statusCodeIntSymbol206 :: StatusCodeInt "206" where
  statusCodeInt = 206

instance statusCodeIntSymbol207 :: StatusCodeInt "207" where
  statusCodeInt = 207

instance statusCodeIntSymbol208 :: StatusCodeInt "208" where
  statusCodeInt = 208

instance statusCodeIntSymbol226 :: StatusCodeInt "226" where
  statusCodeInt = 226

instance statusCodeIntSymbol300 :: StatusCodeInt "300" where
  statusCodeInt = 300

instance statusCodeIntSymbol301 :: StatusCodeInt "301" where
  statusCodeInt = 301

instance statusCodeIntSymbol302 :: StatusCodeInt "302" where
  statusCodeInt = 302

instance statusCodeIntSymbol303 :: StatusCodeInt "303" where
  statusCodeInt = 303

instance statusCodeIntSymbol304 :: StatusCodeInt "304" where
  statusCodeInt = 304

instance statusCodeIntSymbol305 :: StatusCodeInt "305" where
  statusCodeInt = 305

instance statusCodeIntSymbol307 :: StatusCodeInt "307" where
  statusCodeInt = 307

instance statusCodeIntSymbol308 :: StatusCodeInt "308" where
  statusCodeInt = 308

instance statusCodeIntSymbol400 :: StatusCodeInt "400" where
  statusCodeInt = 400

instance statusCodeIntSymbol401 :: StatusCodeInt "401" where
  statusCodeInt = 401

instance statusCodeIntSymbol402 :: StatusCodeInt "402" where
  statusCodeInt = 402

instance statusCodeIntSymbol403 :: StatusCodeInt "403" where
  statusCodeInt = 403

instance statusCodeIntSymbol404 :: StatusCodeInt "404" where
  statusCodeInt = 404

instance statusCodeIntSymbol405 :: StatusCodeInt "405" where
  statusCodeInt = 405

instance statusCodeIntSymbol406 :: StatusCodeInt "406" where
  statusCodeInt = 406

instance statusCodeIntSymbol407 :: StatusCodeInt "407" where
  statusCodeInt = 407

instance statusCodeIntSymbol408 :: StatusCodeInt "408" where
  statusCodeInt = 408

instance statusCodeIntSymbol409 :: StatusCodeInt "409" where
  statusCodeInt = 409

instance statusCodeIntSymbol410 :: StatusCodeInt "410" where
  statusCodeInt = 410

instance statusCodeIntSymbol411 :: StatusCodeInt "411" where
  statusCodeInt = 411

instance statusCodeIntSymbol412 :: StatusCodeInt "412" where
  statusCodeInt = 412

instance statusCodeIntSymbol413 :: StatusCodeInt "413" where
  statusCodeInt = 413

instance statusCodeIntSymbol414 :: StatusCodeInt "414" where
  statusCodeInt = 414

instance statusCodeIntSymbol415 :: StatusCodeInt "415" where
  statusCodeInt = 415

instance statusCodeIntSymbol416 :: StatusCodeInt "416" where
  statusCodeInt = 416

instance statusCodeIntSymbol417 :: StatusCodeInt "417" where
  statusCodeInt = 417

instance statusCodeIntSymbol418 :: StatusCodeInt "418" where
  statusCodeInt = 418

instance statusCodeIntSymbol421 :: StatusCodeInt "421" where
  statusCodeInt = 421

instance statusCodeIntSymbol422 :: StatusCodeInt "422" where
  statusCodeInt = 422

instance statusCodeIntSymbol423 :: StatusCodeInt "423" where
  statusCodeInt = 423

instance statusCodeIntSymbol424 :: StatusCodeInt "424" where
  statusCodeInt = 424

instance statusCodeIntSymbol425 :: StatusCodeInt "425" where
  statusCodeInt = 425

instance statusCodeIntSymbol426 :: StatusCodeInt "426" where
  statusCodeInt = 426

instance statusCodeIntSymbol428 :: StatusCodeInt "428" where
  statusCodeInt = 428

instance statusCodeIntSymbol429 :: StatusCodeInt "429" where
  statusCodeInt = 429

instance statusCodeIntSymbol431 :: StatusCodeInt "431" where
  statusCodeInt = 431

instance statusCodeIntSymbol451 :: StatusCodeInt "451" where
  statusCodeInt = 451

instance statusCodeIntSymbol500 :: StatusCodeInt "500" where
  statusCodeInt = 500

instance statusCodeIntSymbol501 :: StatusCodeInt "501" where
  statusCodeInt = 501

instance statusCodeIntSymbol502 :: StatusCodeInt "502" where
  statusCodeInt = 502

instance statusCodeIntSymbol503 :: StatusCodeInt "503" where
  statusCodeInt = 503

instance statusCodeIntSymbol504 :: StatusCodeInt "504" where
  statusCodeInt = 504

instance statusCodeIntSymbol505 :: StatusCodeInt "505" where
  statusCodeInt = 505

instance statusCodeIntSymbol506 :: StatusCodeInt "506" where
  statusCodeInt = 506

instance statusCodeIntSymbol507 :: StatusCodeInt "507" where
  statusCodeInt = 507

instance statusCodeIntSymbol508 :: StatusCodeInt "508" where
  statusCodeInt = 508

instance statusCodeIntSymbol510 :: StatusCodeInt "510" where
  statusCodeInt = 510

instance statusCodeIntSymbol511 :: StatusCodeInt "511" where
  statusCodeInt = 511

instance statusCodeInt100 :: StatusCodeInt 100 where
  statusCodeInt = 100

instance statusCodeInt101 :: StatusCodeInt 101 where
  statusCodeInt = 101

instance statusCodeInt102 :: StatusCodeInt 102 where
  statusCodeInt = 102

instance statusCodeInt103 :: StatusCodeInt 103 where
  statusCodeInt = 103

instance statusCodeInt200 :: StatusCodeInt 200 where
  statusCodeInt = 200

instance statusCodeInt201 :: StatusCodeInt 201 where
  statusCodeInt = 201

instance statusCodeInt202 :: StatusCodeInt 202 where
  statusCodeInt = 202

instance statusCodeInt203 :: StatusCodeInt 203 where
  statusCodeInt = 203

instance statusCodeInt204 :: StatusCodeInt 204 where
  statusCodeInt = 204

instance statusCodeInt205 :: StatusCodeInt 205 where
  statusCodeInt = 205

instance statusCodeInt206 :: StatusCodeInt 206 where
  statusCodeInt = 206

instance statusCodeInt207 :: StatusCodeInt 207 where
  statusCodeInt = 207

instance statusCodeInt208 :: StatusCodeInt 208 where
  statusCodeInt = 208

instance statusCodeInt226 :: StatusCodeInt 226 where
  statusCodeInt = 226

instance statusCodeInt300 :: StatusCodeInt 300 where
  statusCodeInt = 300

instance statusCodeInt301 :: StatusCodeInt 301 where
  statusCodeInt = 301

instance statusCodeInt302 :: StatusCodeInt 302 where
  statusCodeInt = 302

instance statusCodeInt303 :: StatusCodeInt 303 where
  statusCodeInt = 303

instance statusCodeInt304 :: StatusCodeInt 304 where
  statusCodeInt = 304

instance statusCodeInt305 :: StatusCodeInt 305 where
  statusCodeInt = 305

instance statusCodeInt307 :: StatusCodeInt 307 where
  statusCodeInt = 307

instance statusCodeInt308 :: StatusCodeInt 308 where
  statusCodeInt = 308

instance statusCodeInt400 :: StatusCodeInt 400 where
  statusCodeInt = 400

instance statusCodeInt401 :: StatusCodeInt 401 where
  statusCodeInt = 401

instance statusCodeInt402 :: StatusCodeInt 402 where
  statusCodeInt = 402

instance statusCodeInt403 :: StatusCodeInt 403 where
  statusCodeInt = 403

instance statusCodeInt404 :: StatusCodeInt 404 where
  statusCodeInt = 404

instance statusCodeInt405 :: StatusCodeInt 405 where
  statusCodeInt = 405

instance statusCodeInt406 :: StatusCodeInt 406 where
  statusCodeInt = 406

instance statusCodeInt407 :: StatusCodeInt 407 where
  statusCodeInt = 407

instance statusCodeInt408 :: StatusCodeInt 408 where
  statusCodeInt = 408

instance statusCodeInt409 :: StatusCodeInt 409 where
  statusCodeInt = 409

instance statusCodeInt410 :: StatusCodeInt 410 where
  statusCodeInt = 410

instance statusCodeInt411 :: StatusCodeInt 411 where
  statusCodeInt = 411

instance statusCodeInt412 :: StatusCodeInt 412 where
  statusCodeInt = 412

instance statusCodeInt413 :: StatusCodeInt 413 where
  statusCodeInt = 413

instance statusCodeInt414 :: StatusCodeInt 414 where
  statusCodeInt = 414

instance statusCodeInt415 :: StatusCodeInt 415 where
  statusCodeInt = 415

instance statusCodeInt416 :: StatusCodeInt 416 where
  statusCodeInt = 416

instance statusCodeInt417 :: StatusCodeInt 417 where
  statusCodeInt = 417

instance statusCodeInt418 :: StatusCodeInt 418 where
  statusCodeInt = 418

instance statusCodeInt421 :: StatusCodeInt 421 where
  statusCodeInt = 421

instance statusCodeInt422 :: StatusCodeInt 422 where
  statusCodeInt = 422

instance statusCodeInt423 :: StatusCodeInt 423 where
  statusCodeInt = 423

instance statusCodeInt424 :: StatusCodeInt 424 where
  statusCodeInt = 424

instance statusCodeInt425 :: StatusCodeInt 425 where
  statusCodeInt = 425

instance statusCodeInt426 :: StatusCodeInt 426 where
  statusCodeInt = 426

instance statusCodeInt428 :: StatusCodeInt 428 where
  statusCodeInt = 428

instance statusCodeInt429 :: StatusCodeInt 429 where
  statusCodeInt = 429

instance statusCodeInt431 :: StatusCodeInt 431 where
  statusCodeInt = 431

instance statusCodeInt451 :: StatusCodeInt 451 where
  statusCodeInt = 451

instance statusCodeInt500 :: StatusCodeInt 500 where
  statusCodeInt = 500

instance statusCodeInt501 :: StatusCodeInt 501 where
  statusCodeInt = 501

instance statusCodeInt502 :: StatusCodeInt 502 where
  statusCodeInt = 502

instance statusCodeInt503 :: StatusCodeInt 503 where
  statusCodeInt = 503

instance statusCodeInt504 :: StatusCodeInt 504 where
  statusCodeInt = 504

instance statusCodeInt505 :: StatusCodeInt 505 where
  statusCodeInt = 505

instance statusCodeInt506 :: StatusCodeInt 506 where
  statusCodeInt = 506

instance statusCodeInt507 :: StatusCodeInt 507 where
  statusCodeInt = 507

instance statusCodeInt508 :: StatusCodeInt 508 where
  statusCodeInt = 508

instance statusCodeInt510 :: StatusCodeInt 510 where
  statusCodeInt = 510

instance statusCodeInt511 :: StatusCodeInt 511 where
  statusCodeInt = 511
