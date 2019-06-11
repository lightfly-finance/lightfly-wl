(* ::Package:: *)

BeginPackage["Lightfly`"]

Stock::usage="get stock data..."

Begin["`Private`"]


BuildQueryString[params_] := StringJoin[Riffle[
    KeyValueMap[
        (#1 <> "=" <> #2) & , 
        KeySort[params]
    ], 
    "&"
]]

TokenSign[uri_, params_, config_] := Hash[
    BuildQueryString[
        Append[
            Association[params],
            Normal[Append[
                Association[config],
                {
                    "path_info" -> uri,
                    "sign_date" -> DateString["ISODate"]
                }
            ]]
        ]
    ],
    "SHA256", 
    "HexString"
]

Fetch[uri_, params_, config_] := URLRead[
    HTTPRequest[
        "http://localhost:8000" <> uri,
        <|
            "Query" -> params, 
            "Headers" -> {
                "X-App-Id" -> Association[config][["app_id"]],
                "X-Token" -> TokenSign[uri, params, config],
                "X-Software" -> "math"
            }
        |>
    ], 
    {"StatusCode", "Body"}
]

FinanceGet[uri_, config_] := ImportString[
    Fetch[uri, {}, config]["Body"], 
    {"CSV", "Dataset"}, 
    "HeaderLines" -> 1
]

Stock["hs300", config_] := FinanceGet["/api/stock/hs300", config]

End[]

EndPackage[]

