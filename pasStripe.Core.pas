unit pasStripe.Core;

interface

uses pasStripe.Json, Classes, System.Generics.Collections, pasStripe, Net.HttpClient;

type
  THttpVerb = (httpGet, httpPost, httpDelete);


  TPasStripe = class(TInterfacedObject, IPasStripe)
  private
    FSecretKey: string;
    FAccount: string;
    FLastError: string;
    function GetAccountID: string;
    function GetLastError: string;
    function HttpAction(AVerb: THttpVerb; AMethod: string; AParams: TStrings): IHTTPResponse; overload;
    function HttpAction(AVerb: THttpVerb; AMethod: string; AParams: string): IHTTPResponse; overload;
    function Get(AMethod: string; AParams: TStrings): string;
    function Post(AMethod: string; AParams: TStrings): string;
    function GenerateSubscriptionCheckout(ACheckoutParams: IpsCheckoutParams): string;

  protected
    function TestCredentials: Boolean;
    function CreateAccountSession(AAccount: string): string;
    function GetLoginLink(AAccount: string): string;

    function AddCard(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent;

    function GetCheckoutSession(ASessionID: string): IpsCheckoutSession;

    function CreateCustomer(AName, AEmail, ADescription: string; AMeta: TStrings): IpsCustomer;
    function Getcustomer(AID: string): IpsCustomer;
    procedure SaveCustomer(AID: string; ANameValues: TStrings);

    function GetAccount: IpsAccount;
    function CreateAccount(AName, AEmail: string; AMetaData: TStrings): IpsAccount;

    function CreatePaymentIntent(AAmountPence: integer; ADesc, ACurrency: string; AMetaData: TStrings; AApplicationFee: integer): IpsPaymentIntent;
    function CancelPaymentIntent(APaymentIntentID: string): string;

    function GetPaymentIntent(AID: string): IpsPaymentIntent;
    function GetPaymentMethod(AID: string): IpsPaymentMethod;

    function GetSetupIntent(AID: string): IpsSetupIntent;

    function AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
    function CreateCharge(AChargeParams: IpsChargeParams; var AError: string): IpsCharge;
    function GetCharge(AChargeID: string; const AExpandCustomer: Boolean = False): IpsCharge;
    function GetCharges(const AOptions: IpsChargeListOptions = nil): TpsChargeList;

    function GetInvoice(AID: string): IpsInvoice;

    function RefundCharge(AChargeID: string; var AError: string): Boolean;
    function UpdateCharge(AChargeID: string; ADescription: string): IpsCharge;

    function ExpireSession(ASessionID: string): IpsCheckoutSession;

    function GenerateCheckoutSession(AOptions: IpsCheckoutParams): IpsCheckoutSession;

    function DeleteAccount(AAccount: string): Boolean;
    function GetData(AResource: string; const AParams: TStrings = nil): string;

    property AccountID: string read GetAccountID;
  public
    constructor Create(ASecretKey: string; AAccount: string);


  end;



implementation

uses SysUtils, DateUtils, Math, pasStripe.Utils, System.Json;

const
  C_ACCOUNTS = 'accounts';
  C_DEFAULT_LIMIT = 100;

{ TPasStripe }

function TPasStripe.AddCard(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent;
var
  AParams: TStrings;
  AStrCvc: string;
  AData: string;
  AJson: TJsonObject;

begin
  Result := TpsFactory.SetupIntent;

  AStrCvc := ACvc.ToString;
  while Length(AStrCvc) < 3 do
    AStrCvc := '0' + AStrCvc;
  AParams := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    AParams.Values['customer'] := ACustID;
    AParams.Values['payment_method_data[type]'] := 'card';
    AParams.Values['payment_method_data[card][number]'] := ANum;
    AParams.Values['payment_method_data[card][exp_month]'] := AMonth.ToString;
    AParams.Values['payment_method_data[card][exp_year]'] := AYear.ToString;
    AParams.Values['payment_method_data[card][cvc]'] := AStrCvc;
    AParams.Values['payment_method_options[card][moto]'] := 'true';
    AParams.Values['confirm'] := 'true';
    AData := Post('setup_intents', AParams);
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

function TPasStripe.AttachPaymentMethodToCustomer(ACustID, APaymentMethodID
  : string): string;
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['customer'] := ACustID;
    Result := Post('payment_methods/' + APaymentMethodID + '/attach', AParams);
  finally
    AParams.Free;
  end;

end;

function TPasStripe.CancelPaymentIntent(APaymentIntentID: string): string;
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    Result := Post('payment_intents/'+APaymentIntentID+'/cancel', AParams);
  finally
    AParams.Free;
  end;
end;

constructor TPasStripe.Create(ASecretKey, AAccount: string);
begin
  inherited Create;
  FSecretKey := ASecretKey;
  FAccount := AAccount;
end;

function TPasStripe.CreatePaymentIntent(AAmountPence: integer; ADesc, ACurrency: string;
  AMetaData: TStrings; AApplicationFee: integer): IpsPaymentIntent;
var
  AParams: TStrings;
  AJson: TJsonObject;
  AData: string;
  ICount: integer;
begin
  Result := TpsFactory.PaymentIntent;
  AParams := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    AParams.Values['amount'] := AAmountPence.ToString;
    AParams.Values['description'] := ADesc;
    AParams.Values['application_fee_amount'] := AApplicationFee.ToString;
    AParams.Values['currency'] := ACurrency;
    AParams.Values['payment_method_types[]'] := 'card';

    if AMetaData <> nil then
    begin
      for ICount := 0 to AMetaData.Count - 1 do
        AParams.Values['metadata[' + AMetaData.Names[ICount] + ']'] :=
          AMetaData.ValueFromIndex[ICount];
    end;

    AData := Post('payment_intents', AParams);
    //AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

function TPasStripe.DeleteAccount(AAccount: string): Boolean;
var
  AJson: TJsonObject;
  AResponse: IHTTPResponse;
begin
  Result := False;
  AResponse := HttpAction(httpDelete, C_ACCOUNTS + '/' + AAccount, '');
  if AResponse.StatusCode = 200 then
  begin
    AJson := TJsonObject.Create;
    try
      AJson.FromJSON(AResponse.ContentAsString);
      Result := AJson.B['deleted'];
    finally
      AJson.Free;
    end;
  end;
end;

function TPasStripe.HttpAction(AVerb: THttpVerb; AMethod: string;
  AParams: TStrings): IHTTPResponse;

  function ParamsToUrl(AStrings: TStrings): string;
  var
    ICount: integer;
  begin
    Result := '';
    for ICount := 0 to AStrings.Count - 1 do
    begin
      Result := Result + AStrings[ICount];
      if ICount < AStrings.Count - 1 then
        Result := Result + '&';
    end;
  end;

var
  AUrl: string;
  AHttp: THTTPClient;
begin
  AHttp := THTTPClient.Create;
  try
    AUrl := 'https://api.stripe.com/v1/' + AMethod;
    if AParams <> nil then
    begin
      if AParams.Count > 0 then
        AUrl := AUrl + '?' + ParamsToUrl(AParams);
    end;

    AHttp.CustomHeaders['Authorization'] := 'Bearer ' + FSecretKey;

    if FAccount <> '' then
      AHttp.CustomHeaders['Stripe-Account'] := FAccount;

    if AMethod.ToLower = 'account_sessions' then
      AHttp.CustomHeaders['Stripe-Version'] :=
        '2022-08-01; embedded_connect_beta=v1';

    case AVerb of
      httpGet:
        Result := AHttp.Get(AUrl);
      httpPost:
        Result := AHttp.Post(AUrl, AParams);
      httpDelete:
        Result := AHttp.Delete(AUrl);
    end;

  finally
    AHttp.Free;
  end;
end;

function TPasStripe.HttpAction(AVerb: THttpVerb; AMethod, AParams: string)
  : IHTTPResponse;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    AStrings.Text := AParams;
    Result := HttpAction(AVerb, AMethod, AStrings);
  finally
    AStrings.Free;
  end;
end;

function TPasStripe.ExpireSession(ASessionID: string): IpsCheckoutSession;
var
  AData: string;
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    AData := Post('checkout/sessions/' + ASessionID + '/expire', AStrings);
    Result.LoadFromJson(AData);
  finally
    AStrings.Free;
  end;
end;

function TPasStripe.GenerateSubscriptionCheckout(ACheckoutParams: IpsCheckoutParams): string;
var
  AParams: TStrings;
  AData: string;
  AJson: TJsonObject;
  pm: TpsPaymentMethodType;
  AIndex: integer;
begin

  AParams := TStringList.Create;
  try
    AIndex := 0;
    for pm in ACheckoutParams.PaymentMethods do
    begin
      AParams.Values['payment_method_types[' + AIndex.ToString + ']'] :=
        PaymentMethodToString(pm);
      Inc(AIndex);
    end;

    AParams.Values['mode'] := 'subscription';

    if ACheckoutParams.Customer <> '' then
      AParams.Values['customer'] := ACheckoutParams.Customer;
    if ACheckoutParams.Email <> '' then
      AParams.Values['customer_email'] := ACheckoutParams.Email;

    AParams.Values['success_url'] := ACheckoutParams.SuccessUrl;
    AParams.Values['cancel_url'] := ACheckoutParams.CancelUrl;

    AParams.Values['line_items[0][price]'] := ACheckoutParams.PriceID;
    // APriceID;
    AParams.Values['line_items[0][quantity]'] := '1';

    if ACheckoutParams.TaxID <> '' then
      AParams.Values['line_items[0][tax_rates][]'] := ACheckoutParams.TaxID;

    ACheckoutParams.MetaData.Enumerate(
      procedure(m: TpsMetaDataRecord)
      begin
        AParams.Values['metadata[' + m.Name + ']'] := m.Value;
        AParams.Values['subscription_data[[metadata[' + m.Name + ']]'] := m.Value;

      end
    );

    AData := Post('checkout/sessions', AParams);
    AJson := TJsonObject.Create;
    try
      AJson.FromJSON(AData);
      if FLastError = '' then
        Result := AJson.S['url'];
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TPasStripe.GenerateCheckoutSession(AOptions: IpsCheckoutParams): IpsCheckoutSession;
var
  AParams: TStrings;
  AData: string;
  APaymentMethod: TpsPaymentMethodType;
  AIndex: integer;
begin
  Result := TpsFactory.CheckoutSession;
  AParams := TStringList.Create;
  try
    AIndex := 0;
    for APaymentMethod in AOptions.PaymentMethods do
    begin
      if APaymentMethod = pmDirectDebit then
        AParams.Values['payment_method_types[' + AIndex.ToString + ']'] :=
          'bacs_debit';
      if APaymentMethod = pmCard then
        AParams.Values['payment_method_types[' + AIndex.ToString + ']']
          := 'card';
      Inc(AIndex);
    end;

    AParams.Values['mode'] := CheckoutModeToString(AOptions.Mode);
    AParams.Values['currency'] := AOptions.Currency;

    if AOptions.Customer <> '' then
      AParams.Values['customer'] := AOptions.Customer;
    if AOptions.Email <> '' then
      AParams.Values['customer_email'] := AOptions.Email;
    AParams.Values['success_url'] := AOptions.SuccessUrl;
    AParams.Values['cancel_url'] := AOptions.CancelUrl;
    AParams.Values['client_reference_id'] := AOptions.ClientReferenceID;

    if AOptions.PriceID <> '' then
    begin
      AParams.Values['line_items[0][price'] := AOptions.PriceID;

    end
    else
    begin
      AParams.Values['line_items[0][price_data][currency]'] :=
        AOptions.Currency;
      AParams.Values['line_items[0][price_data][product_data][name]'] :=
        AOptions.Description;
      AParams.Values['line_items[0][price_data][unit_amount]'] := Round(AOptions.Amount).ToString;
    end;
    AParams.Values['line_items[0][quantity]'] := '1';
    AParams.Values['line_items[0][tax_rates][]'] := AOptions.TaxID;

    if AOptions.Mode = cmPayment then
    begin
      if AOptions.ApplicationFee > 0 then
        AParams.Values['payment_intent_data[application_fee_amount]'] :=
          AOptions.ApplicationFee.ToString;

      if AOptions.Description <> '' then
        AParams.Values['payment_intent_data[description]'] :=
          AOptions.Description;

      AOptions.MetaData.Enumerate(
        procedure(m: TpsMetaDataRecord)
        begin
          AParams.Values['metadata[' + m.Name + ']'] := m.Value;
          AParams.Values['payment_intent_data[metadata][' + m.Name + ']'] := m.Value;
        end
      );
    end;

    AData := Post('checkout/sessions', AParams);
    if FLastError <> '' then
      Result.LoadFromJson(AData);

  finally
    AParams.Free;
  end;
end;


function TPasStripe.Get(AMethod: string; AParams: TStrings): string;
var
  AJson: TJsonObject;
begin
  FLastError := '';
  AJson := TJsonObject.Create;
  try
    Result := HttpAction(httpGet, AMethod, AParams).ContentAsString;
    AJson.FromJSON(Result);
    if AJson.Contains('error') then
      FLastError := AJson.O['error'].S['message'];
  finally
    AJson.Free;
  end;
end;

function TPasStripe.GetData(AResource: string;
  const AParams: TStrings = nil): string;
begin
  Result := Get(AResource, AParams);
end;

function TPasStripe.GetInvoice(AID: string): IpsInvoice;
var
  AData: string;
  AParams: TStrings;
begin
  Result := TpsFactory.Invoice;
  AParams := TStringList.Create;
  try
    AParams.Add('expand[]=customer');
    AParams.Add('expand[]=subscription');
    AParams.Add('expand[]=charge');
    AData := Get('invoices/' + AID, AParams);

    Result.LoadFromJson(AData);
  finally
    AParams.Free;
  end;
end;

function TPasStripe.GetLastError: string;
begin
  Result := FLastError;
end;

function TPasStripe.GetLoginLink(AAccount: string): string;
var
  AData: string;
  AJson: TJsonObject;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AData := Post('accounts/' + AAccount + '/login_links', AParams);
    AJson := TJsonObject.Create;
    try
      AJson.FromJSON(AData);
      Result := AJson.S['url'];
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TPasStripe.GetAccount: IpsAccount;
var
  AData: string;
begin
  Result := TpsFactory.Account;
  AData := Get('account', nil);
  if FLastError = '' then
    Result.LoadFromJson(AData);
end;

function TPasStripe.GetAccountID: string;
begin
  Result := FAccount;
end;

function TPasStripe.CreateAccount(AName, AEmail: string; AMetaData: TStrings): IpsAccount;
var
  AParams: TStrings;
  AData: string;
  ICount: integer;
begin
  Result := TpsFactory.Account;
  AParams := TStringList.Create;
  try
    AParams.Values['company[name]'] := AName;
    AParams.Values['business_type'] := 'company';
    AParams.Values['email'] := AEmail;
    AParams.Values['type'] := 'express';
    if AMetaData <> nil then
    begin
      for ICount := 0 to AMetaData.Count - 1 do
        AParams.Values['metadata[' + AMetaData.Names[ICount] + ']'] :=
          AMetaData.ValueFromIndex[ICount];
    end;

    AData := Post('accounts', AParams);
    Result.LoadFromJson(AData);
  finally
    AParams.Free;
  end;
end;

function TPasStripe.CreateAccountSession(AAccount: string): string;
var
  AData: string;
  AJson: TJsonObject;
  AParams: TStrings;
begin
  AParams := TStringList.Create;

  AJson := TJsonObject.Create;
  try
    AParams.Values['account'] := AAccount;
    AData := Post('account_sessions', AParams);
    AJson.FromJSON(AData);
    Result := AJson.S['client_secret'];
  finally
    AJson.Free;
    AParams.Free;
  end;
end;

function TPasStripe.Post(AMethod: string; AParams: TStrings): string;
var
  AJson: TJsonObject;
  AData: string;
begin
  Result := '';
  FLastError := '';
  AJson := TJSONObject.Create;
  try
    AData := HttpAction(httpPost, AMethod, AParams).ContentAsString;
    AJson.FromJSON(AData);
    FLastError := AJson.O['error'].S['message'];
    if FLastError = '' then
      Result := AData;
  finally
    AJson.Free;
  end;
end;

function TPasStripe.RefundCharge(AChargeID: string; var AError: string): Boolean;
var
  AParams: TStrings;
  AJson: TJsonObject;
  AResult: string;
begin
  AParams := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    AParams.Values['charge'] := AChargeID;
    AResult := Post('refunds', AParams);
    AJson.FromJson(AResult);
    Result := AJson.O['error'].S['message'] = '';
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

procedure TPasStripe.SaveCustomer(AID: string; ANameValues: TStrings);
var
  AResult: string;
begin
  AResult := Post('customers/' + AID, ANameValues);
end;

function TPasStripe.TestCredentials: Boolean;
var
  AAccount: IpsAccount;
begin
   AAccount := GetAccount;
   Result := AAccount.ID <> '';
end;

function TPasStripe.UpdateCharge(AChargeID, ADescription: string): IpsCharge;
var
  AData: string;
  AStrings: TStrings;
  AJson: TJsonObject;
begin
  AStrings := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    AStrings.Values['description'] := ADescription;
    AData := Post('charges/' + AChargeID, AStrings);
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AStrings.Free;
    AJson.Free;
  end;
end;

function TPasStripe.CreateCharge(AChargeParams: IpsChargeParams; var AError: string): IpsCharge;
var
  AParams: TStrings;
   AJson: TJsonObject;
  AResult: string;
begin
  AError := '';
  Result := TpsFactory.Charge;

  AJson := TJsonObject.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['payment_method'] := AChargeParams.PaymentMethodID;
    AParams.Values['customer'] := AChargeParams.CustomerID;
    AParams.Values['currency'] := AChargeParams.Currency;
    AParams.Values['description'] := AChargeParams.Description;
    AParams.Values['amount'] := AChargeParams.Amount.ToString;
    AParams.Values['confirm'] := 'true';
    AParams.Values['off_session'] := 'true';

    AChargeParams.Metadata.Enumerate(
      procedure(AMeta: TpsMetaDataRecord)
      begin
        AParams.Values['metadata[' + AMeta.Name + ']'] := AMeta.Value;
      end
    );

    AResult := Post('payment_intents', AParams);
    AJson.FromJSON(AResult);
//    if AJson.S['status'] = 'succeeded' then

    if FLastError = '' then
      Result.LoadFromJson(AJson.O['charges'].A['data'].Items[0] as TJSONObject);
  finally
    AParams.Free;
    AJson.Free;
  end;

end;

function TPasStripe.GetCharge(AChargeID: string; const AExpandCustomer: Boolean = False): IpsCharge;
var
  AData: string;
  AJson: TJsonObject;
  AParams: TStrings;
begin
  Result := TpsFactory.Charge;
  if AChargeID = '' then
    Exit;
  AParams := TStringList.Create;
  try
    if AExpandCustomer then AParams.Values['expand[]'] := 'customer';
    AData := Get('charges/' + AChargeID, AParams);

  finally
    AParams.Free;
  end;
  AJson := TJsonObject.Create;// Parse(AData) as TJsonObject;
  try
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TPasStripe.GetCharges(const AOptions: IpsChargeListOptions = nil): TpsChargeList;
var
  AJson: TJsonObject;
  AData: string;
  AObj: TJsonObject;
  ACharge: IpsCharge;
  AStartAfter: string;
  AParams: TStrings;
  ICount: integer;
  ALimit: integer;
begin
  Result := TpsChargeList.Create;
  AStartAfter := '';
  AJson := TJsonObject.Create;
  AParams := TStringList.Create;
  try
    ALimit := C_DEFAULT_LIMIT;

    if AOptions <> nil then
    begin
      if AOptions.Limit <> -1 then ALimit := AOptions.Limit;
      if AOptions.Query <> '' then AParams.Values['query'] := AOptions.Query;
      if AOptions.FromDate > 0 then AParams.Values['created[gte]'] := DateTimetoUnix(AOptions.FromDate).ToString;
      if AOptions.ToDate > 0 then AParams.Values['created[lte]'] := DateTimetoUnix(AOptions.ToDate).ToString;
      if AOptions.PaymentIntentID <> '' then AParams.Values['payment_intent'] := AOptions.PaymentIntentID;
    end;

    AParams.Values['limit'] := ALimit.ToString;

    AParams.Values['expand[]'] := 'data.customer';

    repeat
      if AParams.Values['query'] <> '' then
        AData := Get('charges/search', AParams)
      else
        AData := Get('charges', AParams);
      AJson.FromJSON(AData);


      for ICount := 0 to AJson.A['data'].Count - 1 do
      begin
        AObj := AJson.A['data'].Items[ICount] as TJSONObject;
        ACharge := TpsFactory.Charge;
        ACharge.LoadFromJson(AObj);
        Result.Add(ACharge);
      end;
      if ACharge <> nil then
        AParams.Values['starting_after'] := ACharge.id;

    until (AJson.B['has_more'] = False) or (Result.Count >= ALimit);

  finally
    AJson.Free;
    AParams.Free;
  end;
end;

function TPasStripe.GetCheckoutSession(ASessionID: string): IpsCheckoutSession;
var
  AData: string;
begin
  Result := TpsFactory.CheckoutSession;
  AData := Get('checkout/sessions/' + ASessionID, nil);

  Result.LoadFromJson(AData);
end;

function TPasStripe.CreateCustomer(AName, AEmail, ADescription: string; AMeta: TStrings): IpsCustomer;
var
  AParams: TStrings;
  AJson: string;
begin
  Result := TpsFactory.Customer;
  AParams := TStringList.Create;
  try
    AParams.Values['name'] := AName;
    AParams.Values['description'] := ADescription;
    AParams.Values['email'] := AEmail;
    if AMeta <> nil then
    begin
      for var ICount := 0 to AMeta.Count - 1 do
      begin
        AParams.Values['metadata[' + AMeta.Names[ICount] + ']'] :=
          AMeta.ValueFromIndex[ICount];
      end;
    end;
    AJson := Post('customers', AParams);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
  end;
end;

function TPasStripe.Getcustomer(AID: string): IpsCustomer;
var
  AData: string;
begin
  Result := TpsFactory.Customer;
  AData := Get('customers/' + AID, nil);
  Result.LoadFromJson(AData);
end;


function TPasStripe.GetPaymentIntent(AID: string): IpsPaymentIntent;
var
  AData: string;
  AJson: TJsonObject;
begin
  Result := TpsFactory.PaymentIntent;
  AData := Get('payment_intents/' + AID, nil);
  AJson := TJsonObject.Create;
  try
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TPasStripe.GetPaymentMethod(AID: string): IpsPaymentMethod;
var
  AData: string;
  AJson: TJsonObject;
begin
  Result := TpsFactory.PaymentMethod;
  AData := Get('payment_methods/' + AID, nil);
  if FLastError <> '' then
    Exit;
  AJson := TJsonObject.Create;
  try
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TPasStripe.GetSetupIntent(AID: string): IpsSetupIntent;
var
  AData: string;
  AJson: TJsonObject;
begin
  Result := TpsFactory.SetupIntent;
  AData := Get('setup_intents/' + AID, nil);
  AJson := TJsonObject.Create;
  try
    AJson.FromJSON(AData);
    if not AJson.Contains('error') then
      Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;



end.
