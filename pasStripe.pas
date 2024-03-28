unit pasStripe;

interface

uses Classes, SysUtils, System.Json, System.Generics.Collections;

type

  TpsFactory = class;

  TpsPaymentMethodType = (pmDirectDebit, pmCard);
  TpsPaymentMethodsTypes = set of TpsPaymentMethodType;

  TpsCheckoutMode = (cmSetup, cmPayment, cmSubscription);

  TpsCurrency = (scUnknown, scGbp, scEur, scUsd);

  TpsMetaDataRecord = class
    Name:  string;
    Value: string;
  end;

  IpsMetadata = interface
    ['{3D53944B-F0FA-4485-A0DB-923047025E7C}']
    function GetValue(AName: string): string;
    procedure SetValue(AName: string; const Value: string);
    function FindMetaData(AName: string): TpsMetaDataRecord;
    function AddMetaData(AName, AValue: string): TpsMetaDataRecord;
    function GetAsJson: string;
    procedure LoadFromJson(AJson: TJsonObject);
    procedure Enumerate(ACallback: TProc<TpsMetaDataRecord>);
    procedure Clear;
    property Value[AName: string]: string read GetValue write SetValue; default;
    property AsJson: string read GetAsJson;
  end;

  IpsPaymentIntent = interface
    ['{2757C336-B837-4090-B6DF-449DE58F286D}']
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCreated: TDateTime;
    function GetId: string;
    function GetPaid: Boolean;
    function GetMetaData(AName: string): string;
    function GetClientSecret: string;
    function GetPaymentMethod: string;

    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
    property MetaData[AName: string]: string read GetMetaData;
    property id: string read GetId;
    property Amount: integer read GetAmount;
    property Paid: Boolean read GetPaid;
    property ApplicationFeeAmount: integer read GetApplicationFee;
    property Created: TDateTime read GetCreated;
    property ClientSecret: string read GetClientSecret;
    property PaymentMethod: string read GetPaymentMethod;
  end;

  IpsSetupIntent = interface
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

  IpsPaymentMethod = interface
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

  IpsCheckoutParams = interface
    ['{32083FB8-DD97-493E-ADD1-A5B6362B5988}']
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCancelUrl: string;
    function GetClientReferenceID: string;
    function GetCurrency: string;
    function GetCustomer: string;
    function GetDescription: string;
    function GetEmail: string;
    function GetMetaData: IpsMetaData;
    function GetMode: TpsCheckoutMode;
    function GetPaymentMethodsTypes: TpsPaymentMethodsTypes;
    function GetSuccessUrl: string;
    function GetPriceID: string;
    function GetTaxID: string;
    procedure SetAmount(const Value: integer);
    procedure SetApplicationFee(const Value: integer);
    procedure SetCancelUrl(const Value: string);
    procedure SetClientReferenceID(const Value: string);
    procedure SetCurrency(const Value: string);
    procedure SetCustomer(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetMode(const Value: TpsCheckoutMode);
    procedure SetPriceID(const Value: string);
    procedure SetPaymentMethodsTypes(const Value: TpsPaymentMethodsTypes);
    procedure SetSuccessUrl(const Value: string);
    procedure SetTaxID(const Value: string);
    property Currency: string read GetCurrency write SetCurrency;
    property Email: string read GetEmail write SetEmail;
    property Customer: string read GetCustomer write SetCustomer;
    property Description: string read GetDescription write SetDescription;
    property Mode: TpsCheckoutMode read GetMode write SetMode;
    property SuccessUrl: string read GetSuccessUrl write SetSuccessUrl;
    property CancelUrl: string read GetCancelUrl write SetCancelUrl;
    property Amount: integer read GetAmount write SetAmount;
    property PaymentMethods: TpsPaymentMethodsTypes read GetPaymentMethodsTypes write SetPaymentMethodsTypes;
    property MetaData: IpsMetaData read GetMetaData;
    property  ApplicationFee: integer read GetApplicationFee write SetApplicationFee;
    property ClientReferenceID: string read GetClientReferenceID write SetClientReferenceID;
    property PriceID: string read GetPriceID write SetPriceID;
    property TaxID: string read GetTaxID write SetTaxID;
  end;


  IpsCheckoutSession = interface
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

    procedure LoadFromJson(AData: string);
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
  end;

  IpsChargeParams = interface
    ['{BF3F0CD0-29C9-4ACE-9604-793F0295E04B}']
    function GetAmount: integer;
    function GetCurrency: string;
    function GetPaymentMethodID: string;
    function GetCustomerID: string;
    function GetDescription: string;
    function GetMetadata: IpsMetadata;
    procedure SetPaymentMethodID(const Value: string);
    procedure SetCustomerID(const Value: string);
    procedure SetDescription(const Value: string);
    property Amount: integer read GetAmount;
    property Currency: string read GetCurrency;
    property PaymentMethodID: string read GetPaymentMethodID write SetPaymentMethodID;
    property CustomerID: string read GetCustomerID write SetCustomerID;
    property Description: string read GetDescription write SetDescription;
    property Metadata: IpsMetadata read GetMetadata;
  end;

  TpsListParamsDateFilter = record
    LessThan: TDateTime;
    LessThanOrEqual: TDateTime;
    GreaterThanOrEqual: TDateTime;
    GreaterThan: TDateTime;
  end;

  IpsBaseListParams = interface
    ['{70003C24-F0B8-4A9E-A5C5-4300811E6A91}']
    function GetLimit: integer;
    procedure SetLimit(const Value: integer);

    procedure PopulateParamStrings(AStrings: TStrings);

    //property Limit: tps read GetLimit write SetLimit;
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

  IpsCharge = interface
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
    procedure LoadFromJson(AJson: TJsonObject);
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

  TpsChargeList = class(TList<IpsCharge>);

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

  TpsInvoiceList = class(TList<IpsInvoice>);


  IpsAccount = interface
    ['{748FD42C-EBDA-4DE3-A55F-E304E4797EA1}']
    function GetChargesEnabled: Boolean;
    function GetID: string;
    function GetJson: string;
    function GetMetaData: IpsMetadata;
    function GetName: string;
    procedure LoadFromJson(AJson: string);
    property ID: string read GetID;
    property ChargesEnabled: Boolean read GetChargesEnabled;
    property Name: string read GetName;
    property Json: string read GetJson;
    property MetaData: IpsMetaData read GetMetaData;
  end;

  IPasStripe = interface
    ['{2E285C68-FBCB-4C38-96EA-13EAF8C6B7B1}']
    function GetAccountID: string;
    function GetAccount: IpsAccount;
    function CreateAccount(AName, AEmail: string; AMetaData: TStrings): IpsAccount;
    function TestCredentials: Boolean;
    function GetLastError: string;

    function GetCharge(AChargeID: string; const AExpandCustomer: Boolean = False): IpsCharge;
    function GetCharges(const AOptions: IpsChargeListOptions = nil): TpsChargeList;
    function CreateCharge(AChargeParams: IpsChargeParams; var AError: string): IpsCharge;
    function UpdateCharge(AChargeID: string; ADescription: string): IpsCharge;
    function RefundCharge(AChargeID: string; var AError: string): Boolean;


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
    procedure SaveCustomer(AID: string; ANameValues: TStrings);

    function GetPaymentMethod(AID: string): IpsPaymentMethod;
    function AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
    function GetPaymentMethods(ACustID: string): string;


    function GetCheckoutSession(ASessionID: string): IpsCheckoutSession;
    function GenerateCheckoutSession(AParams: IpsCheckoutParams): IpsCheckoutSession;

    function GenerateSubscriptionCheckout(AParams: IpsCheckoutParams): string;


    function GetLoginLink(AAccount: string): string;

    function GetInvoice(AID: string): IpsInvoice;


    property AccountID: string read GetAccountID;
    property LastError: string read GetLastError;
  end;


  TpsFactory = class
  public
    class function PasStripe(ASecretKey, AAccount: string): IPasStripe;
    class function Account: IpsAccount;
    class function Metadata: IpsMetadata;
    class function PaymentMethod: IpsPaymentMethod;
    class function Customer: IpsCustomer;
    class function Charge: IpsCharge;
    class function ChargeListOptions: IpsChargeListOptions;
    class function Invoice: IpsInvoice;
    class function InvoiceList: TpsInvoiceList;

    class function PaymentIntent: IpsPaymentIntent;
    class function SetupIntent: IpsSetupIntent;
    class function CheckoutSession: IpsCheckoutSession;
    class function CheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): IpsCheckoutParams;

    class function ChargeParams(AAmount: integer; ACurrency: string): IpsChargeParams;

  end;


implementation

uses
  pasStripe.Core,
  pasStripe.Account,
  pasStripe.Metadata,
  pasStripe.PaymentMethod,
  pasStripe.Customer,
  pasStripe.Charge,
  pasStripe.Intents,
  pasStripe.Checkout,
  pasStripe.Invoice,
  pasStripe.Utils;

{ TpsFactory }

class function TpsFactory.Account: IpsAccount;
begin
  Result := TpsAccount.Create;
end;

class function TpsFactory.Charge: IpsCharge;
begin
  Result := TpsCharge.Create;
end;

class function TpsFactory.ChargeListOptions: IpsChargeListOptions;
begin
  Result := TpsChargeListOptions.Create;
end;

class function TpsFactory.ChargeParams(AAmount: integer; ACurrency: string): IpsChargeParams;
begin
  Result := TpsChargeParams.Create(AAmount, ACurrency);
end;

class function TpsFactory.CheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): IpsCheckoutParams;
begin
  Result := TpsCheckoutParams.Create;
  Result.Mode := AMode;
  Result.Currency := CurrencyToString(ACurrency);
end;

class function TpsFactory.CheckoutSession: IpsCheckoutSession;
begin
  Result := TpsCheckoutSession.Create;
end;

class function TpsFactory.Customer: IpsCustomer;
begin
  Result := TpsCustomer.Create;
end;

class function TpsFactory.Invoice: IpsInvoice;
begin
  Result := TpsInvoice.Create;
end;

class function TpsFactory.InvoiceList: TpsInvoiceList;
begin
  Result := TpsInvoiceList.Create;
end;

class function TpsFactory.Metadata: IpsMetadata;
begin
  Result := TpsMetadata.Create;
end;

class function TpsFactory.PasStripe(ASecretKey, AAccount: string): IPasStripe;
begin
  Result := TPasStripe.Create(ASecretKey, AAccount);
end;

class function TpsFactory.PaymentIntent: IpsPaymentIntent;
begin
  Result := TpsPaymentIntent.Create;
end;

class function TpsFactory.PaymentMethod: IpsPaymentMethod;
begin
  Result := TpsPaymentMethod.Create;
end;

class function TpsFactory.SetupIntent: IpsSetupIntent;
begin
  Result := TpsSetupIntent.Create;
end;


end.
