{*******************************************************************************
*                                                                              *
*  pasStripe - Stripe Interfaces for Delphi                                    *
*                                                                              *
*  https://github.com/gmurt/pasStripe                                          *
*                                                                              *
*  Copyright 2024 Graham Murt                                                  *
*                                                                              *                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit pasStripe;

interface

uses Classes, SysUtils, System.Json, System.Generics.Collections,
  pasStripe.Constants;

type
  TpsPaymentMethodType = (pmDirectDebit, pmCard);
  TpsPaymentMethodsTypes = set of TpsPaymentMethodType;
  TpsCheckoutMode = (cmSetup, cmPayment, cmSubscription);
  TpsRecurring = (None, Daily, Weekly, Monthly, Yearly);
  TpsCurrency = (scUnknown, scGbp, scEur, scUsd);
  TpsFutureUsage = (psOnSession, psOffSession);

  TpsListParamsDateFilter = record
    LessThan: TDateTime;
    LessThanOrEqual: TDateTime;
    GreaterThanOrEqual: TDateTime;
    GreaterThan: TDateTime;
  end;

  IpsBaseList<T> = interface(IInterface)
    ['{EB23D8A8-2FE4-4562-B64F-6B41D9DB116E}']
    function GetCount: integer;
    function GetItem(AIndex: integer): T;
    function GetEnumerator: TList<T>.TEnumerator;
    function Add(const Value: T): Integer;
    property Count: integer read GetCount;
    property Items[index: integer]: T read GetItem; default;
  end;


  IpsMetaDataRecord = interface
    ['{9AF60898-83A8-41D9-8907-5A864184FE3C}']
    function GetName: string;
    function GetValue: string;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: string);
    property Name: string read GetName write SetName;
    property Value: string read GetValue write SetValue;
  end;

  IpsMetadata = interface
    ['{3D53944B-F0FA-4485-A0DB-923047025E7C}']
    function GetValue(AName: string): string;
    procedure SetValue(AName: string; const Value: string);
    function FindMetaData(AName: string): IpsMetaDataRecord;
    function AddMetaData(AName, AValue: string): IpsMetaDataRecord;
    procedure Clear;
    procedure Enumerate(ACallback: TProc<IpsMetaDataRecord>);
    procedure LoadFromJson(AJson: TJSONObject);
    property Value[AName: string]: string read GetValue write SetValue; default;
  end;

  IpsBaseObject = interface
    ['{8BDFD4A0-135A-4759-892D-CAD9DA96765B}']
  end;

  IpsBaseObjectWithMetaData = interface
    ['{C1E6245B-026C-4C5C-8708-1B0400EE7053}']
    function GetMetadata: IpsMetadata;
    procedure LoadFromJson(AJson: string);
    property MetaData: IpsMetadata read GetMetaData;
  end;
  
  IpsBaseParams = interface
    ['{641C7ADD-BD13-4546-9BFA-500BEBE2FC5D}']
    function GetAsString: string;
    procedure PopulateStrings(AStrings: TStrings);
    property AsString: string read GetAsString;
  end;

  IpsBaseParamsWithMetaData = interface(IpsBaseParams)
    ['{95E7941F-8945-456A-B8C0-566BA36FB71F}']
    function GetMetadata: IpsMetadata;
    property MetaData: IpsMetadata read GetMetaData;
  end;

  IpsUpdateChargeParams = interface(IpsBaseParamsWithMetaData)
    ['{AB596017-6B83-4B2D-B819-39406DEBC2FF}']
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    property Description: string read GetDescription write SetDescription;
  end;

  IpsCreateChargeParams = interface(IpsUpdateChargeParams)
    ['{E79EB463-E9BB-49C6-9556-C23B6CEB7179}']
    function GetAmount: integer;
    function GetApplicationFeeAmount: integer;
    function GetConfirm: Boolean;
    function GetCurrency: TpsCurrency;
    function GetCustomer: string;
    function GetPaymentMethod: string;
    procedure SetAmount(const Value: integer);
    procedure SetApplicationFeeAmount(const Value: integer);
    procedure SetConfirm(const Value: Boolean);
    procedure SetCurrency(const Value: TpsCurrency);
    procedure SetCustomer(const Value: string);
    procedure SetPaymentMethod(const Value: string);
    property Amount: integer read GetAmount write SetAmount;
    property ApplicationFeeAmount: integer read GetApplicationFeeAmount write SetApplicationFeeAmount;
    property Confirm: Boolean read GetConfirm write SetConfirm;
    property Currency: TpsCurrency read GetCurrency write SetCurrency;
    property Customer: string read GetCustomer write SetCustomer;
    property PaymentMethod: string read GetPaymentMethod write SetPaymentMethod;
  end;

  IpsCreateRefundParams = interface(IpsBaseParamsWithMetaData)
    ['{E1CACBF3-570E-4DB0-AF6F-BCBFDE21ED68}']
    function GetAmount: integer;
    function GetCharge: string;
    function GetReason: string;
    procedure SetAmount(const Value: integer);
    procedure SetCharge(const Value: string);
    procedure SetReason(const Value: string);
    property Amount: integer read GetAmount write SetAmount;
    property Charge: string read GetCharge write SetCharge;
    property Reason: string read GetReason write SetReason;
  end;

  IpsAddressParams = interface(IpsBaseParams)
    ['{97631BE3-F828-4B6C-A427-4D7D9DDC199B}']
    function GetCity: string;
    procedure SetCity(const Value: string);
    property City: string read GetCity write SetCity;
  end;

  IpsCompanyParams = interface(IpsBaseParams)
    ['{6C4EBA25-CB5F-4084-9408-7FB85DFA9592}']
    function GetName: string;
    function GetAddress: IpsAddressParams;
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
    property Address: IpsAddressParams read GetAddress;
  end;

  IpsUpdateAccountParams = interface(IpsBaseParamsWithMetadata)
    ['{1F59D6F4-35C5-4594-BA5B-14C35381C45E}']
    function GetBusinessType: string;
    function GetEmail: string;
    function GetCompany: IpsCompanyParams;
    procedure SetBusinessType(const Value: string);
    procedure SetEmail(const Value: string);
    property BusinessType: string read GetBusinessType write SetBusinessType;
    property Email: string read GetEmail write SetEmail;
    property Company: IpsCompanyParams read GetCompany;
  end;

  IpsCreateAccountParams = interface(IpsUpdateAccountParams)
    ['{EB2C81BF-C96B-4656-ABE6-47CFB1EC87BE}']
  end;

  IpsUpdateCustomerParams = interface(IpsBaseParamsWithMetadata)
    ['{2C4074A5-C529-488E-B451-266819193B4B}']
    function GetDescription: string;
    function GetName: string;
    function GetEmail: string;
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure SetEmail(const Value: string);
    property Description: string read GetDescription write SetDescription;
    property Name: string read GetName write SetName;
    property Email: string read GetEmail write SetEmail;
  end;

  IpsCheckoutLineItem = interface
    ['{FEA2035B-1C8D-400F-9568-FA868CA523AF}']
    function GetAmount: integer;
    function GetCurrency: TpsCurrency;
    function GetDescription: string;
    function GetPriceID: string;
    function GetQty: integer;
    function GetRecurring: TpsRecurring;
    function GetTaxID: string;
    procedure SetAmount(const Value: integer);
    procedure SetCurrency(const Value: TpsCurrency);
    procedure SetDescription(const Value: string);
    procedure SetPriceID(const Value: string);
    procedure SetQty(const Value: integer);
    procedure SetRecurring(const Value: TpsRecurring);
    procedure SetTaxID(const Value: string);
    property Amount: integer read GetAmount write SetAmount;
    property Currency: TpsCurrency read GetCurrency write SetCurrency;
    property Description: string read GetDescription write SetDescription;
    property Qty: integer read GetQty write SetQty;
    property Recurring: TpsRecurring read GetRecurring write SetRecurring;
    property PriceID: string read GetPriceID write SetPriceID;
    property TaxID: string read GetTaxID write SetTaxID;
  end;

  IpsCheckoutLineItems = interface
    ['{CB18B2B8-C47C-44F3-8CB5-A8ED9C9E1A54}']
    function AddLineItem(ADescription: string; AQty: integer; APence: integer; const ATaxRate: string = ''; const ARecurring: TpsRecurring = None): IpsCheckoutLineItem; overload;
    function AddLineItem(APriceID: string; AQty: integer; const ATaxRate: string = ''): IpsCheckoutLineItem; overload;
    procedure PopulateStrings(AStrings: TStrings);
  end;

  IpsCreateCheckoutParams = interface(IpsBaseParamsWithMetaData)
    ['{942DADE1-A2C6-4C59-95C9-14AAB1F4FE76}']
    function GetApplicationFeeAmount: integer;
    function GetMode: TpsCheckoutMode;
    function GetCurrency: TpsCurrency;
    function GetCustomerEmail: string;
    function GetPaymentMethods: TpsPaymentMethodsTypes;
    function GetCancelUrl: string;
    function GetSuccessUrl: string;
    function GetLineItems: IpsCheckoutLineItems;
    procedure SetMode(const Value: TpsCheckoutMode);
    procedure SetCurrency(const Value: TpsCurrency);
    procedure SetCustomerEmail(const Value: string);
    procedure SetPaymentMethods(const Value: TpsPaymentMethodsTypes);
    procedure SetCancelUrl(const Value: string);
    procedure SetSuccessUrl(const Value: string);
    procedure SetApplicationFeeAmount(const Value: integer);
    property ApplicationFeeAmount: integer read GetApplicationFeeAmount write SetApplicationFeeAmount;
    property Mode: TpsCheckoutMode read GetMode write SetMode;
    property Currency: TpsCurrency read GetCurrency write SetCurrency;
    property CustomerEmail: string read GetCustomerEmail write SetCustomerEmail;
    property PaymentMethods: TpsPaymentMethodsTypes read GetPaymentMethods write SetPaymentMethods;
    property CancelUrl: string read GetCancelUrl write SetCancelUrl;
    property SuccessUrl: string read GetSuccessUrl write SetSuccessUrl;
    property LineItems: IpsCheckoutLineItems read GetLineItems;
  end;

  IpsCreatePaymentIntentParams = interface(IpsBaseParamsWithMetaData)
    ['{61777E17-66F7-4A7B-A289-1F5B35B90C67}']
    function GetApplicationFeeAmount: integer;
    function GetConfirm: Boolean;
    function GetCurrency: TpsCurrency;
    function GetCustomer: string;
    function GetPaymentMethod: string;
    procedure SetApplicationFeeAmount(const Value: integer);
    procedure SetConfirm(const Value: Boolean);
    procedure SetCurrency(const Value: TpsCurrency);
    procedure SetCustomer(const Value: string);
    procedure SetPaymentMethod(const Value: string);
    property ApplicationFeeAmount: integer read GetApplicationFeeAmount write SetApplicationFeeAmount;
    property Confirm: Boolean read GetConfirm write SetConfirm;
    property Currency: TpsCurrency read GetCurrency write SetCurrency;
    property Customer: string read GetCustomer write SetCustomer;
    property PaymentMethod: string read GetPaymentMethod write SetPaymentMethod;
  end;

  IpsPaymentIntent = interface(IpsBaseObjectWithMetaData)
    ['{2757C336-B837-4090-B6DF-449DE58F286D}']
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCreated: TDateTime;
    function GetId: string;
    function GetPaid: Boolean;
    function GetMetaData: IpsMetadata;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    property MetaData: IpsMetadata read GetMetaData;
    property id: string read GetId;
    property Amount: integer read GetAmount;
    property Paid: Boolean read GetPaid;
    property ApplicationFeeAmount: integer read GetApplicationFee;
    property Created: TDateTime read GetCreated;
    property ClientSecret: string read GetClientSecret;
    property PaymentMethod: string read GetPaymentMethod;
  end;

  IpsSetupIntent = interface(IpsBaseObjectWithMetaData)
    ['{A6B0F997-03DC-43EB-A698-97FBEE4E701D}']
    function GetID: string;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    property ID: string read GetID;
    property ClientSecret: string read GetClientSecret;
    property PaymentMethod: string read GetPaymentMethod;
  end;

  IpsPaymentMethod = interface(IpsBaseObjectWithMetaData)
    ['{FC786B81-180B-4187-88D0-7C5BA1C81578}']
    function GetID: string;
    function GetCustomer: string;
    function GetExpiryMonth: integer;
    function GetExpiryYear: integer;
    function GetExpiryStr: string;
    function GetLast4: string;
    function GetBrand: string;
    function GetJson: string;
    procedure LoadFromJson(AJson: TJsonObject);
    property ID: string read GetID;
    property Customer: string read GetCustomer;
    property ExpiryMonth: integer read GetExpiryMonth;
    property ExpiryYear: integer read GetExpiryYear;
    property ExpiryStr: string read GetExpiryStr;
    property Last4: string read GetLast4;
    property Brand: string read GetBrand;
    property Json: string read GetJson;
  end;

  IpsCheckoutSession = interface(IpsBaseObjectWithMetaData)
    ['{3CB53C71-B7FE-4127-9E83-96BA40E5120E}']
    function GetID: string;
    function GetCreated: TDateTime;
    function GetPaymentIntentID: string;
    function GetSetupIntentID: string;
    function GetJson: string;
    function GetPaymentStatus: string;
    function GetStatus: string;
    function GetUrl: string;
    function GetMetadata: IpsMetadata;
    procedure SetID(const Value: string);
    procedure SetPaymentIntentID(const Value: string);
    procedure SetSetupIntentID(const Value: string);
    procedure SetJson(const Value: string);
    procedure SetPaymentStatus(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetUrl(const Value: string);

    procedure Clear;
    property ID: string read GetID write SetID;
    property Created: TDateTime read GetCreated;
    property Url: string read GetUrl write SetUrl;
    property PaymentIntentID: string read GetPaymentIntentID write SetPaymentIntentID;
    property SetupIntentID: string read GetSetupIntentID write SetSetupIntentID;
    property Status: string read GetStatus write SetStatus;
    property PaymentStatus: string read GetPaymentStatus write SetPaymentStatus;
    property Metadata: IpsMetadata read GetMetadata;
    property Json: string read GetJson write SetJson;
    procedure LoadFromJson(AData: string);
    property Url: string read GetUrl;
  end;

  IpsChargeListOptions = interface
    ['{63EA59D8-289A-4C8D-BF1F-5D6AD738D005}']
    function GetFromDate: TDateTime;
    function GetToDate: TDateTime;
    function GetPaymentIntentID: string;
    function GetLimit: integer;
    function GetQuery: string;
    procedure SetFromDate(const Value: TDateTime);
    procedure SetLimit(const Value: integer);
    procedure SetToDate(const Value: TDateTime);
    procedure SetQuery(const Value: string);
    procedure SetPaymentIntentID(const Value: string);
    property FromDate: TDateTime read GetFromDate write SetFromDate;
    property ToDate: TDateTime read GetToDate write SetToDate;
    property PaymentIntentID: string read GetPaymentIntentID write SetPaymentIntentID;
    property Limit: integer read GetLimit write SetLimit;
    property Query: string read GetQuery write SetQuery;
  end;

  IpsCharge = interface(IpsBaseObjectWithMetaData)
    ['{121E1B25-253F-4CED-89D4-295166916424}']
    function GetID: string;
    function GetAmount: integer;
    function GetCardBrand: string;
    function GetCreated: TDateTime;
    function GetCurrency: string;
    function GetCustomer: string;
    function GetDescription: string;
    function GetJson: string;
    function GetLast4: string;
    function GetRefunded: integer;
    function GetStatus: string;
    function GetPaymentIntentID: string;
    function GetMetaData: IpsMetaData;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    procedure LoadFromJson(AJson: string); overload;
    property Description: string read GetDescription;
    property ID: string read GetID;
    property Json: string read GetJson;
    property Customer: string read GetCustomer;
    property Amount: integer read GetAmount;
    property Created: TDateTime read GetCreated;
    property Status: string read GetStatus;
    property PaymentIntentID: string read GetPaymentIntentID;
    property Currency: string read GetCurrency;
    property Last4: string read GetLast4;
    property CardBrand: string read GetCardBrand;
    property Refunded: integer read GetRefunded;
    property MetaData: IpsMetaData read GetMetaData;
  end;

  IpsChargeList = interface(IpsBaseList<IpsCharge>)
    ['{CAED85A6-F41D-482F-9928-70F2ABE8431F}']
  end;

  IpsCustomer = interface
    ['{8687A786-45D8-4797-80D2-E260345A6FB0}']
    function GetDescription: string;
    function GetEmail: string;
    function GetID: string;
    function GetJson: string;
    function GetMetaData: IpsMetaData;
    function GetName: string;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    property ID: string read GetID;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Email: string read GetEmail;
    property Json: string read GetJson;
    property MetaData: IpsMetaData read GetMetaData;
  end;

  IpsInvoiceListOptions = interface
    ['{A0985C12-6DAC-4831-80A0-CEE8AB45B89F}']
    function GetCustomer: string;
    procedure SetCustomer(const Value: string);
    property Customer: string read GetCustomer write SetCustomer;
  end;

  IpsInvoice = interface
    ['{1F6A3A85-DE3C-41EF-8426-10F898129867}']
    function GetJson: string;
    function GetID: string;
    function GetPdfUrl: string;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    property ID: string read GetID;
    property PdfUrl: string read GetPdfUrl;
    property Json: string read GetJson;
  end;

  IpsInvoiceList = interface(IpsBaseList<IpsInvoice>)
    ['{F4231DF3-D9F1-4111-8786-0342F4780B2B}']
  end;


  IpsAccount = interface(IpsBaseObjectWithMetaData)
    ['{748FD42C-EBDA-4DE3-A55F-E304E4797EA1}']
    function GetChargesEnabled: Boolean;
    function GetID: string;
    function GetJson: string;
    function GetName: string;
    procedure LoadFromJson(AJson: string);
    property ID: string read GetID;
    property ChargesEnabled: Boolean read GetChargesEnabled;
    property Name: string read GetName;
    property Json: string read GetJson;
  end;

  IPasStripe = interface
    ['{2E285C68-FBCB-4C38-96EA-13EAF8C6B7B1}']
    function GetAccountID: string;
    function GetAccount: IpsAccount;
    function CreateAccount(AName, AEmail: string; AMetaData: TStrings): IpsAccount; overload;
    function CreateAccount(AParams: IpsCreateAccountParams): IpsAccount; overload;

    function UpdateAccount(AId: string; Params: IpsUpdateAccountParams): IpsAccount;

    function TestCredentials: Boolean;
    function GetLastError: string;
    function GetCharge(AChargeID: string; const AExpandCustomer: Boolean = False): IpsCharge;
    function GetCharges(const AOptions: IpsChargeListOptions = nil): IpsChargeList;
    function CreateCharge(AChargeParams: IpsCreateChargeParams): IpsCharge;
    function UpdateCharge(AChargeID: string; AChargeParams: IpsUpdateChargeParams): IpsCharge;
    function RefundCharge(AChargeID, AReason: string; AAmount: integer): Boolean;
    function CreatePaymentIntent(AAmountPence: integer; ADesc, ACurrency: string; AMetaData: TStrings; AApplicationFee: integer): IpsPaymentIntent;
    function GetPaymentIntent(AID: string): IpsPaymentIntent;
    function ConfirmSetupIntent(ASetupIntentID: string): IpsSetupIntent;
    function CancelPaymentIntent(APaymentIntentID: string): IpsPaymentIntent;
    function GetSetupIntent(ASetupIntentID: string): IpsSetupIntent;
    function CreateSetupIntent(const ACustID: string = ''): IpsSetupIntent; overload;
    function CreateSetupIntent(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent; overload;
    function AddCard(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent; deprecated;
    function CreateCustomer(AName, AEmail, ADescription: string; AMeta: TStrings): IpsCustomer;
    function Getcustomer(AID: string): IpsCustomer;
    function UpdateCustomer(AID: string; AParams: IpsUpdateCustomerParams): IpsCustomer;
    procedure SaveCustomer(AID: string; ANameValues: TStrings);
    function GetPaymentMethod(AID: string): IpsPaymentMethod;
    function AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
    function GetPaymentMethods(ACustID: string): string;
    function GetCheckoutSession(ASessionID: string): IpsCheckoutSession;
    function GenerateCheckoutSession(AParams: IpsCreateCheckoutParams): IpsCheckoutSession;
    function GetLoginLink(AAccount: string): string;
    function GetInvoice(AID: string): IpsInvoice;
    property AccountID: string read GetAccountID;
    property LastError: string read GetLastError;
  end;

  IpsFactory = interface
    ['{8E62ED42-EE6C-470B-A1D3-D0AB04483E51}']
    function PasStripe(ASecretKey: string; const AAccount: string = ''): IPasStripe;
    function Account: IpsAccount;
    function PaymentMethod: IpsPaymentMethod;
    function Customer: IpsCustomer;
    function Charge: IpsCharge;
    function ChargeList: IpsChargeList;
    function ChargeListOptions: IpsChargeListOptions;
    function Invoice: IpsInvoice;
    function InvoiceList: IpsInvoiceList;
    function PaymentIntent: IpsPaymentIntent;
    function SetupIntent: IpsSetupIntent;
    function CheckoutSession: IpsCheckoutSession;

    function CreateAccountParams: IpsCreateAccountParams;
    function CreateChargeParams(AAmount: integer; ACurrency: TpsCurrency): IpsCreateChargeParams;
    function CreateCheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): IpsCreateCheckoutParams;
    function CreateRefundParams: IpsCreateRefundParams;

    function UpdateAccountParams: IpsUpdateAccountParams;
    function UpdateChargeParams: IpsUpdateChargeParams;
    function UpdateCustomerParams: IpsUpdateCustomerParams;
    function CreatePaymentIntentParams(AAmount: integer; ADesc: string; ACurrency: TpsCurrency): IpsCreatePaymentIntentParams;
  end;

  function TpsFactory: IpsFactory;

implementation

uses
  pasStripe.Factory;

var
  psInternalFactory: IpsFactory;

function TpsFactory: IpsFactory;
begin
  Result := psInternalFactory;
end;


initialization

  psInternalFactory := pasStripe.Factory.TpsFactory.Create;


end.
