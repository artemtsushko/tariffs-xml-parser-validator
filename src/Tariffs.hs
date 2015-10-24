-----------------------------------------------------------------------------
-- |
-- Module      :  Tariffs
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
--
-----------------------------------------------------------------------------

module Tariffs (
    Tariffs(..),
    Tariff
) where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN


{-Type decls-}

newtype Tariffs             = Tariffs [Tariff] deriving (Eq,Show)
data    Tariff              = Tariff TariffAttrs TariffName Operator Payroll
                              CallPrices SmsPrice Parameters deriving (Eq)
data    TariffAttrs         = TariffAttrs { tariffId :: String }
                              deriving (Eq,Show)
newtype TariffName          = TariffName String deriving (Eq,Show)
newtype Operator            = Operator String deriving (Eq,Show)
newtype Payroll             = Payroll String deriving (Eq,Show)
data    CallPrices          = CallPrices WithinNetwork ToOtherNetworks
                              ToFixedLineNumbers deriving (Eq,Show)
newtype WithinNetwork       = WithinNetwork String deriving (Eq,Show)
newtype ToOtherNetworks     = ToOtherNetworks String deriving (Eq,Show)
newtype ToFixedLineNumbers  = ToFixedLineNumbers String deriving (Eq,Show)
newtype SmsPrice            = SmsPrice String deriving (Eq,Show)
data    Parameters          = Parameters FavoiriteNumbers Pricing SubscribeFee
                              deriving (Eq,Show)
newtype FavoiriteNumbers    = FavoiriteNumbers String deriving (Eq,Show)
data    Pricing             = Pricing { pricingUnit :: PricingUnit }
                              deriving (Eq,Show)
data    PricingUnit         = Second  |  Minute deriving (Eq,Show)
newtype SubscribeFee        = SubscribeFee String deriving (Eq,Show)


{-Instance decls-}
instance Show Tariff where
    show (Tariff
        (TariffAttrs id)
        (TariffName name)
        (Operator operator)
        (Payroll payroll)
        (CallPrices
            (WithinNetwork within)
            (ToOtherNetworks other)
            (ToFixedLineNumbers fixedline)
        )
        (SmsPrice sms)
        (Parameters
            (FavoiriteNumbers favourites)
            (Pricing  { pricingUnit = units} )
            (SubscribeFee subscribeFee)
        )
        ) = "Tariff "  ++ show name ++ "\n"
            ++ "Id: "  ++ show id ++ "\n"
            ++ "Operator: " ++ operator ++ "\n"
            ++ "Payroll: " ++ payroll ++ "\n"
            ++ "Call prices:\n"
            ++ " - within network: " ++ within ++ "\n"
            ++ " - to other networks: " ++ other ++ "\n"
            ++ " - to fixed line numbers: " ++ fixedline ++ "\n"
            ++ "Sms price: " ++ sms ++ "\n"
            ++ "Favourite numbers: " ++ favourites ++ "\n"
            ++ "Pricing unit: " ++ show units ++ "\n"
            ++ "Subscribing fee: " ++ subscribeFee ++ "\n"
instance HTypeable Tariffs where
    toHType x = Defined "tariffs" [] []
instance XmlContent Tariffs where
    toContents (Tariffs a) =
        [CElem (Elem (N "tariffs") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["tariffs"]
        ; interior e $ return Tariffs `apply` many parseContents
        } `adjustErr` ("in <tariffs>, "++)

instance HTypeable Tariff where
    toHType x = Defined "tariff" [] []
instance XmlContent Tariff where
    toContents (Tariff as a b c d e f) =
        [CElem (Elem (N "tariff") (toAttrs as)
            (toContents a ++
             toContents b ++ toContents c ++ toContents d ++
             toContents e ++ toContents f)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["tariff"]
        ; interior e $ return (Tariff (fromAttrs as))
                       `apply` parseContents `apply` parseContents
                       `apply` parseContents `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <tariff>, "++)
instance XmlAttributes TariffAttrs where
    fromAttrs as =
        TariffAttrs
          { tariffId = definiteA fromAttrToStr "tariff" "id" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "id" (tariffId v)
        ]

instance HTypeable TariffName where
    toHType x = Defined "tariffName" [] []
instance XmlContent TariffName where
    toContents (TariffName a) =
        [CElem (Elem (N "tariffName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["tariffName"]
        ; interior e $ return TariffName
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <tariffName>, "++)

instance HTypeable Operator where
    toHType x = Defined "operator" [] []
instance XmlContent Operator where
    toContents (Operator a) =
        [CElem (Elem (N "operator") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["operator"]
        ; interior e $ return Operator `apply` (text `onFail` return "")
        } `adjustErr` ("in <operator>, "++)

instance HTypeable Payroll where
    toHType x = Defined "payroll" [] []
instance XmlContent Payroll where
    toContents (Payroll a) =
        [CElem (Elem (N "payroll") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["payroll"]
        ; interior e $ return Payroll `apply` (text `onFail` return "")
        } `adjustErr` ("in <payroll>, "++)

instance HTypeable CallPrices where
    toHType x = Defined "callPrices" [] []
instance XmlContent CallPrices where
    toContents (CallPrices a b c) =
        [CElem (Elem (N "callPrices") [] (toContents a ++ toContents b ++
                                          toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["callPrices"]
        ; interior e $ return CallPrices `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <callPrices>, "++)

instance HTypeable WithinNetwork where
    toHType x = Defined "withinNetwork" [] []
instance XmlContent WithinNetwork where
    toContents (WithinNetwork a) =
        [CElem (Elem (N "withinNetwork") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["withinNetwork"]
        ; interior e $ return WithinNetwork
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <withinNetwork>, "++)

instance HTypeable ToOtherNetworks where
    toHType x = Defined "toOtherNetworks" [] []
instance XmlContent ToOtherNetworks where
    toContents (ToOtherNetworks a) =
        [CElem (Elem (N "toOtherNetworks") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["toOtherNetworks"]
        ; interior e $ return ToOtherNetworks
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <toOtherNetworks>, "++)

instance HTypeable ToFixedLineNumbers where
    toHType x = Defined "toFixedLineNumbers" [] []
instance XmlContent ToFixedLineNumbers where
    toContents (ToFixedLineNumbers a) =
        [CElem (Elem (N "toFixedLineNumbers") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["toFixedLineNumbers"]
        ; interior e $ return ToFixedLineNumbers
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <toFixedLineNumbers>, "++)

instance HTypeable SmsPrice where
    toHType x = Defined "smsPrice" [] []
instance XmlContent SmsPrice where
    toContents (SmsPrice a) =
        [CElem (Elem (N "smsPrice") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["smsPrice"]
        ; interior e $ return SmsPrice `apply` (text `onFail` return "")
        } `adjustErr` ("in <smsPrice>, "++)

instance HTypeable Parameters where
    toHType x = Defined "parameters" [] []
instance XmlContent Parameters where
    toContents (Parameters a b c) =
        [CElem (Elem (N "parameters") [] (toContents a ++ toContents b ++
                                          toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["parameters"]
        ; interior e $ return Parameters `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <parameters>, "++)

instance HTypeable FavoiriteNumbers where
    toHType x = Defined "favoiriteNumbers" [] []
instance XmlContent FavoiriteNumbers where
    toContents (FavoiriteNumbers a) =
        [CElem (Elem (N "favoiriteNumbers") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["favoiriteNumbers"]
        ; interior e $ return FavoiriteNumbers
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <favoiriteNumbers>, "++)

instance HTypeable Pricing where
    toHType x = Defined "pricing" [] []
instance XmlContent Pricing where
    toContents as =
        [CElem (Elem (N "pricing") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["pricing"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <pricing>, "++)
instance XmlAttributes Pricing where
    fromAttrs as =
        Pricing
          { pricingUnit = definiteA fromAttrToTyp "pricing" "unit" as
          }
    toAttrs v = catMaybes
        [ toAttrFrTyp "unit" (pricingUnit v)
        ]

instance XmlAttrType PricingUnit where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "second" = Just Second
            translate "minute" = Just Minute
            translate _ = Nothing
    toAttrFrTyp n Second = Just (N n, str2attr "second")
    toAttrFrTyp n Minute = Just (N n, str2attr "minute")

instance HTypeable SubscribeFee where
    toHType x = Defined "subscribeFee" [] []
instance XmlContent SubscribeFee where
    toContents (SubscribeFee a) =
        [CElem (Elem (N "subscribeFee") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["subscribeFee"]
        ; interior e $ return SubscribeFee
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <subscribeFee>, "++)
