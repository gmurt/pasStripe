unit pasStripe;

interface

uses Classes, SysUtils, System.Json, System.Generics.Collections;

type
  TpsPaymentMethodType = (pmDirectDebit, pmCard);
  TpsPaymentMethodsTypes = set of TpsPaymentMethodType;
  TpsCheckoutMode = (cmSetup, cmPayment, cmSubscription);
  TpsRecurring = (rDaily, rWeekly, rMonthly, rYearly);
  TpsCurrency = (scUnknown, scGbp, scEur, scUsd);
  TpsParamName = (amount, application_fee_amount, customer, currency, description, email, mode, name, priceId,
                  successUrl, taxId, cancelUrl);
  TpsParamNames = set of TpsParamName;

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

  TpsBaseParams = class
  strict private
    FParams: TStrings;
  protected
    function GetInteger(AParam: TpsParamName): integer;
    function GetString(AParam: TpsParamName): string;
    function GetMetaData(AName: string): string;
    procedure SetString(AParam: TpsParamName; const AValue: string);
    procedure SetInteger(AParam: TpsParamName; const AValue: integer);
    procedure SetMetaData(AName, AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PopulateStrings(AStrings: TStrings); virtual;
    property MetaData[AName: string]: string read GetMetaData write SetMetaData;
  end;

  TpsUpdateChargeParams = class(TpsBaseParams)
  public
    property Description: string index Ord(description) read GetString write SetString;
  end;

  TpsCreateChargeParams = class(TpsUpdateChargeParams)
  public
    property Amount: integer index Ord(amount) read GetInteger write SetInteger;
    property ApplicationFeeAmount: integer index(application_fee_amount) read GetInteger write SetInteger;
    property Currency: string index Ord(currency) read GetString write SetString;
    property Customer: string index Ord(customer) read GetString write SetString;
    property Description: string index Ord(description) read GetString write SetString;
  end;

  TpsUpdateCustomerParams = class(TpsBaseParams)
  public
    property Name: string index Ord(name) read GetString write SetString;
    property Description: string index Ord(description) read GetString write SetString;
  end;

  TpsCheckoutLineItem = class
    FCurrency: TpsCurrency;
    FDescription: string;
    FAMount: integer;
    FQty: integer;
    FTaxID: string;
    FRecurring: TpsRecurring;
  end;

  TpsCheckoutLineItemList = class(TObjectList<TpsCheckoutLineItem>)
  public
    procedure PopulateStrings(AStrings: TStrings);
    procedure AddLineItem(ADesc: string; ACurrency: TpsCurrency; AAmount, AQty: integer; ATaxID: string; ARecurring: TpsRecurring);
  end;

  TpsCreateCheckoutParams = class(TpsBaseParams)
  private
    FLineItems: TpsCheckoutLineItemList;
    FPaymentMethodTypes: TpsPaymentMethodsTypes;
    function GetMode: TpsCheckoutMode;
    procedure SetMode(const AValue: TpsCheckoutMode);
    function GetCurrency: TpsCurrency;
    procedure SetCurrency(const Value: TpsCurrency);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PopulateStrings(AStrings: TStrings); override;
    property ApplicationFeeAmount: integer index Ord(application_fee_amount) read GetInteger write SetInteger;
    property Email: string index Ord(email) read GetString write SetString;
    property Description: string index Ord(description) read GetString write SetString;
    property Name: string index Ord(name) read GetString write SetString;
    property Mode: TpsCheckoutMode read GetMode write SetMode;
    property Currency: TpsCurrency read GetCurrency write SetCurrency;
    property PaymentMethods: TpsPaymentMethodsTypes read FPaymentMethodTypes write FPaymentMethodTypes;
    property PriceID: string index Ord(priceId) read GetString write SetString;
    property SuccessUrl: string index Ord(successUrl) read GetString write SetString;
    property CancelUrl: string index Ord(cancelUrl) read GetString write SetString;
    property TaxID: string index Ord(taxId) read GetString write SetString;
    property LineItems: TpsCheckoutLineItemList read FLineItems;
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

  TpsListParamsDateFilter = record
    LessThan: TDateTime;
    LessThanOrEqual: TDateTime;
    GreaterThanOrEqual: TDateTime;
    GreaterThan: TDateTime;
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


    function CreateCharge(AChargeParams: TpsCreateChargeParams): IpsCharge;
    function UpdateCharge(AChargeID: string; AChargeParams: TpsUpdateChargeParams): IpsCharge;
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
    function UpdateCustomer(AID: string; AParams: TpsUpdateCustomerParams): IpsCustomer;
    procedure SaveCustomer(AID: string; ANameValues: TStrings);

    function GetPaymentMethod(AID: string): IpsPaymentMethod;
    function AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
    function GetPaymentMethods(ACustID: string): string;


    function GetCheckoutSession(ASessionID: string): IpsCheckoutSession;


    function GenerateCheckoutSession(AParams: TpsCreateCheckoutParams): IpsCheckoutSession;

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


    class function CreateCheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): TpsCreateCheckoutParams;


    class function CreateChargeParams(AAmount: integer; ACurrency: string): TpsCreateChargeParams;
    class function UpdateChargeParams: TpsUpdateChargeParams;

    class function UpdateCustomerParams: TpsUpdateCustomerParams;

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
  pasStripe.Utils,
  pasStripe.Params;


{ TpsBaseParams }

constructor TpsBaseParams.Create;
begin
  FParams := TStringList.Create;
end;

destructor TpsBaseParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TpsBaseParams.GetInteger(AParam: TpsParamName): integer;
begin
  Result := StrToIntDef(GetString(AParam), 0);
end;

function TpsBaseParams.GetMetaData(AName: string): string;
begin
  Result := FParams.Values['metadata['+AName+']'];
end;

function TpsBaseParams.GetString(AParam: TpsParamName): string;
begin
  Result := FParams.Values[ParamToString(AParam)];
end;

procedure TpsBaseParams.PopulateStrings(AStrings: TStrings);
begin
  AStrings.Assign(FParams);
end;

procedure TpsBaseParams.SetInteger(AParam: TpsParamName; const AValue: integer);
begin
  SetString(AParam, AValue.ToString);
end;

procedure TpsBaseParams.SetMetaData(AName, AValue: string);
begin
  FParams.Values['metadata['+AName+']'] := AValue;
end;

procedure TpsBaseParams.SetString(AParam: TpsParamName; const AValue: string);
begin
  FParams.Values[ParamToString(AParam)] := AValue;
end;



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

class function TpsFactory.CreateChargeParams(AAmount: integer; ACurrency: string): TpsCreateChargeParams;
begin
  Result := TpsCreateChargeParams.Create;
  Result.amount := AAmount;
  Result.currency := ACurrency;
end;

class function TpsFactory.CreateCheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): TpsCreateCheckoutParams;
begin
  Result := TpsCreateCheckoutParams.Create;
  Result.Mode := AMode;
  Result.Currency := ACurrency;
end;

class function TpsFactory.UpdateChargeParams: TpsUpdateChargeParams;
begin
  Result := TpsUpdateChargeParams.Create;
end;

class function TpsFactory.CheckoutSession: IpsCheckoutSession;
begin
  Result := TpsCheckoutSession.Create;
end;

class function TpsFactory.Customer: IpsCustomer;
begin
  Result := TpsCustomer.Create;
end;

class function TpsFactory.UpdateCustomerParams: TpsUpdateCustomerParams;
begin
  Result := TpsUpdateCustomerParams.Create;
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


{ TpsCreateCheckoutParams }

constructor TpsCreateCheckoutParams.Create;
begin
  inherited Create;
  FLineItems := TpsCheckoutLineItemList.Create(True);
end;

destructor TpsCreateCheckoutParams.Destroy;
begin
  FLineItems.Free;
  inherited;
end;

function TpsCreateCheckoutParams.GetCurrency: TpsCurrency;
begin
  Result := StringToCurrency(GetString(TpsParamName.currency));
end;

function TpsCreateCheckoutParams.GetMode: TpsCheckoutMode;
begin
  Result := StringToCheckoutMode (GetString(TpsParamName.mode));
end;


procedure TpsCreateCheckoutParams.PopulateStrings(AStrings: TStrings);
var
  APaymentMethod: TpsPaymentMethodType;
  AIndex: integer;
begin
  inherited;
  AIndex := 0;
  for APaymentMethod in FPaymentMethodTypes do
  begin
    if APaymentMethod = pmDirectDebit then AStrings.Values['payment_method_types[' + AIndex.ToString + ']'] :='bacs_debit';
    if APaymentMethod = pmCard then AStrings.Values['payment_method_types[' + AIndex.ToString + ']'] := 'card';
    Inc(AIndex);
  end;
  FLineItems.PopulateStrings(AStrings);
end;

procedure TpsCreateCheckoutParams.SetCurrency(const Value: TpsCurrency);
begin
  SetString(TpsParamName.currency, CurrencyToString(Value));
end;

procedure TpsCreateCheckoutParams.SetMode(const AValue: TpsCheckoutMode);
begin
  SetString(TpsParamName.mode, CheckoutModeToString(AValue));
end;


{ TpsCheckoutLineItemList }

procedure TpsCheckoutLineItemList.AddLineItem(ADesc: string; ACurrency: TpsCurrency; AAmount, AQty: integer; ATaxID: string; ARecurring: TpsRecurring);
var
  AItem: TpsCheckoutLineItem;
begin
  AItem := TpsCheckoutLineItem.Create;
  AItem.FCurrency := ACurrency;
  AItem.FDescription := ADesc;
  AItem.FAMount := AAmount;
  AItem.FQty := AQty;
  AItem.FTaxID := ATaxID;
  AItem.FRecurring := ARecurring;
  Add(AItem);
end;

procedure TpsCheckoutLineItemList.PopulateStrings(AStrings: TStrings);
var
  AItem: TpsCheckoutLineItem;
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
  begin
    AItem := Items[ICount];
    AStrings.Values['line_items['+ICount.ToString+'][price_data][currency]'] := CurrencyToString(AItem.FCurrency);
    AStrings.Values['line_items['+ICount.ToString+'][price_data][product_data][name]'] := AItem.FDescription;
    AStrings.Values['line_items['+ICount.ToString+'][price_data][unit_amount]'] := AItem.FAMount.ToString;
    AStrings.Values['line_items['+ICount.ToString+'][price_data][recurring][interval]'] := IntervalToString(AItem.FRecurring);
    AStrings.Values['line_items['+ICount.ToString+'][quantity]'] := AItem.FQty.ToString;
    AStrings.Values['line_items['+ICount.ToString+'][tax_rates][]'] := AItem.FTaxID;
  end;
end;

end.
