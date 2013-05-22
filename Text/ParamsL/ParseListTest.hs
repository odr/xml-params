{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.ParamsL.ParseListTest where

import Data.Monoid
import qualified Data.Text as T
import Text.Multiline(ml)

import Text.ParamsL.ParseList

        
test :: String -> T.Text -> IO ()
test s t = mapM_ (putStrLn . T.unpack) $ applyParam (getParamsL s) t

tTest :: String -> T.Text        
tTest s = T.intercalate "\t" $ "rownum,rownum++ = {rownum},{rownum++}" : map (\t->mconcat [t,"={",t,"}"]) (pNames $ getParamsL s)

test1 s = test s (tTest s)
        
sTest :: String
sTest = [ml|
                [mode] ? [[X]] 
                *
                [n, mode] ? [
                    [-3, -],
                    [-2, -],
                    [-1, -], [-1, X],
                    [ 0, -], [ 0, X],
                    [ 1, -], [ 1, X]
                ]
                * [x, xname, qn, drus] ? [ 
                    [1, GrafikRailForeign,  ZPL_XI_RAIL_DLV,    `График ж/д отгр эксп[]`], 
                    [2, GrafikPipe,         ZPL_XI_PIPE_DLV,    График труб отгр], 
                    [3, GrafikRiver,        ZPL_XI_RIVER_DLV,   График речн отгр], 
                    [4, GrafikRailRF,       ZPL_XI_RAILRF_DLV,  График ж/д отгр РФ],
                    [5, GrafikTank,         ZPL_XI_TANK_DLV,    График танкерных отгр] 
                ]  
                * [x, plantype, nxp] ? [ 
                    [1, A, 1], [1, C, 2], 
                    [2, A, 3], [2, C, 4],
                    [3, A, 5], [3, C, 6], 
                    [4, A, 7], [4, C, 8], 
                    [5, _, 9] 
                ]  
                * [x, mode] ? [
                    [1, -], [1, X],
                    [2, -], [2, X],
                    [3, -], [3, X],
                    [4, -],
                    [5, -]
                ]
                * [dton, sf, sn, st] ? [[{add_months(curday,{n})}, {(({n}+3)*9+{nxp}-1)*2+1}, {{sf}+1}, {{sf}+2}]]
        |]

sTest2 = [ml|
        [n,rcv,sig1,sig2,sig3,ip,s1,s2,p2,i1,i2,xsl1] ? 
                [[Приемный акт,УЦП,{p1}.*{sig2},getPriemnyAkt xmlns,{sig1},IP_Sch3AskTerminals,BSP_DS_TERMINAL, BSP_DS_KPW, -, SI_RPK1CWS_SI,  SI_TerminalsUniversal_AI, -]]
            * [snd,p1] ? [
                    [КНТ,P_KALININGRAD],
                    [РПК Высоцк,P_VYSOTSK]
                ]
    |]
{- тесты

sTest2 :: String
sTest2 = [ml|
        [component,addr] ? [[BC_INBOXUCP2, datastream], [BC_INBOXUCP, datastream_test]] 
        *
        [name,addr] ? [
                [Почтовые получения,    datastream     ],
                [Почтовые получения,    datastream_test],
                [ЛП505,                 datastream     ],
                [ЛПxxx,                 datastream     ]
            ]
    |]
    
sTest21 = [ml|
        [component,addr] ? [[BC_INBOXUCP2, datastream], [BC_INBOXUCP, datastream_test]
    ] 
    |]
sTest22 = [ml|
        [name,addr] ? [
                [Почтовые получения,    datastream     ],
                [Почтовые получения,    datastream_test],
                [ЛП505,                 datastream     ],
                [ЛПxxx,                 datastream     ]
            ]
    |]
    

sTest :: String
sTest = [ml|
    [snd,snd_i] ? [[BSP_DS_ISU_PER, ZTTN_XI]] * [rcv,alrt] ? [[BSP_DS_PETRONICS,error]] * [rcv_p,desc] ? [[P_VOLGA_NP,ВолгаНП],[P_PNP,ПермьНП],[P_CNP,ЦНП],[P_URALNP,УралНП]] + 
    [x,x] ? [[BSP_DS_ISU_PER, ZTTN_XI]] * [x,x,x,x] ? [[BSP_DS_KPW,exist,-,УЦП],[BSP_EXP,exist,-,ЦУП],[BSP_DS_TERMINAL,error,RPK,РПК Высоцк]]
|]
    
sTest2 =[ml|
    [bsfrom,bsto,ptxt,scenario] ? [ [BSD_ISU_NEFTEHIM,BSD_DS_KPW,ИСУ НЕФТЕХИМ,DS.NHZ] ] ;
                                   [runDT,dname,qn,drus] ? [ 
                                               [{curday}T08:00:00,ShipsRaw,Z_GET_AVIZO,Отгрузки сырья в адрес завода],
                                               [{curday}T08:00:00,ReceiptRaw,Z_GET_PM,Поступление сырья] 
            ] * 
            [dfrom,dto,sf,sn,st] ? [
                    [{curday-40},{curday-30},1,2,3],
                    [{curday-29},{curday-19},4,5,6],
                    [{curday-18},{curday},7,8,9]
            ]
|]            

sTest3 = [ml|
                          [currm,prevm,nextm] ? [[{trunc(curday-1,'M')}, {add_months({currm},-1)}, {add_months({currm},1)}]] 
                        * [n,nn,nnn,d1,d2] ? [
                                [1,  2,  3,  {prevm},      {{prevm}+10}],
                                [3,  4,  5,  {{prevm}+10}, {{prevm}+20}],
                                [5,  6,  7,  {{prevm}+20}, {currm}],
                                [7,  8,  9,  {currm},      {{currm}+10}],
                                [9,  10, 11, {{currm}+10}, {{currm}+20}],
                                [11, 12, 50, {{currm}+20}, {nextm}]
                            ]
|]

sTest4 = [ml|
    [bsfrom,bsfrom2,bsto,dat] ? [ [BSP_DS_PETRONICS,BSP_DS_KPW,BSP_DS_AND,{curday-1}] ]
                    * [rn,pfrom,abon,nabon] ? [
                            [1, P_NO,      967,   Югнефтепродукт],
                            [2, P_SZNP,    982,   Северозападнефтепродукт],
                            [3, P_PNP,     1018,  Пермнефтепродукт],
                            [4, P_NVNP,    1079,  Нижневолжскнефтепродукт],
                            [5, P_CNP,     5234,  Центрнефтепродукт],
                            [6, P_VOLGANP, 81038, Волганефтепродукт],
                            [7, P_URALNP,  88370, Уралнефтепродукт]
                        ]
|]

sTest5 = [ml|
        [s,snd,snd_i,rcv,rcv_i,sig] ? [[Burgas ISU Pererabotka,BSP_ISU_PERERAB_BLG, ZTTN_XI, IP_Delta_Ttn, MI_TTN_Request_AA, -]] 
        * [rcv2, rcv2_i, desc] ? [[BSP_DS_KPW, MI_Ships_AI, UCP], [BSP_EXP, MI_Ships_AI, CUP], [BSP_DS_DSO, dummy_interface, DSO]]
        + [x,x,x,x,x,x,x,x,x] ? [[Modul Postavka, BSP_DS_ISU_PER, ZTTN_XI, IP_Delta_Ttn, MI_TTN_Request_AA, <OPER */>, BSP_DS_DSO, dummy_interface, DSO]]
|]

tTest4 :: T.Text
tTest4 = "<sch:eval currentStepNumber=\"{if {rn}==1 then 2 else if {rn}==2 then 10 else 1}\" xmlns:sch=\"http://lukoil.ru/scheduler\">"
tTest41 :: T.Text
tTest41 = "<sch:eval currentStepNumber=\"{if rownum==1 then 2 else if rownum==2 then 10 else 1}\" xmlns:sch=\"http://lukoil.ru/scheduler\">"
-}
