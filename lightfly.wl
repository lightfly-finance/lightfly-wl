(* ::Package:: *)

BeginPackage["Lightfly`"]

Stock::usage="get stock data..."
FinanceGet::usage="get finance data."

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

FinanceGet[uri_, params_, config_] := ImportString[
    Fetch[uri, params, config]["Body"], 
    {"CSV", "Dataset"}, 
    "HeaderLines" -> 1
]

noPamarsMap = <|
    "hs300" -> "/api/stock/hs300",
    "stock.index" -> "/api/stock/index",
    "hgs.trade.realtime" -> "/api/stock/hgs/trade/realtime",
    "hgtong.top10" -> "/api/stock/hgtong/top10",
    "sgtong.top10" -> "/api/stock/sgtong/top10",
    "ggtong.top10" -> "/api/stock/ggtong/top10",
    "sh.index.component" -> "/api/stock/component/shindex",
    "sh.consumption.index.component" -> "/api/stock/component/shconsumptionindex",
    "sh50.index.component" -> "/api/stock/component/sh50index",
    "sh.medicine.index.component" -> "/api/stock/component/shmedicineindex",
    "sz.index.component" -> "/api/stock/component/szindex",
    "sz.composite.index.component" -> "/api/stock/component/szcompositeindex",
    "zz500.index.component" -> "/api/stock/component/zz500index"
|>

paramsMap = <|
    "daily.history" -> "/api/stock/history/daily",
    "stock.realtime" -> "/api/stock/realtime",
    "indicator.main" -> "/api/stock/indicator/main",
    "profitability" -> "/api/stock/indicator/profitability",
    "solvency" -> "/api/stock/indicator/solvency",
    "growth.ability" -> "/api/stock/indicator/growthability"

|>
Stock[s_ /; KeyExistsQ[noPamarsMap, s], c_] :=  FinanceGet[noPamarsMap[[s]], {}, c]

Stock[s_ /; KeyExistsQ[paramsMap, s], p_, c_] :=  FinanceGet[paramsMap[[s]], p, c]

End[]

EndPackage[]

