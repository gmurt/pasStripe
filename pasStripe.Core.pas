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

unit pasStripe.Core;

interface

uses pasStripe.Json, Classes, System.Generics.Collections, pasStripe,
  Net.HttpClient, pasStripe.Params;

type
  THttpVerb = (httpGet, httpPost, httpDelete);


  TPasStripe = class(TInterfacedObject, IPasStripe)
  private
    FSecretKey: string;
    FAccount: string;
    FLastError: string;
    function GetAccountID: string;
    function GetLastError: string;
    function HttpAction(AVerb: THttpVerb; AMethod: string; AUrlParams: TStrings): IHTTPResponse; overload;
    function HttpAction(AVerb: THttpVerb; AMethod: string; AUrlParams: string): IHTTPResponse; overload;
    function Get(AMethod: string; AParams: TStrings): string;
    function Post(AMethod: string; AUrlParams: TStrings): string; overload;
    function Post(AMethod: string; AParams: IpsBaseParams): string; overload;
  protected
    function GetAccount: IpsAccount;
    function CreateAccount(AName, AEmail: string; AMetaData: TStrings): IpsAccount; overload;
    function CreateAccount(AParams: IpsCreateAccountParams): IpsAccount; overload;
    function UpdateAccount(AId: string; AParams: IpsUpdateAccountParams): IpsAccount;

    function TestCredentials: Boolean;
    function CreateAccountSession(AAccount: string): string;
    function GetLoginLink(AAccount: string): string;

    function AddCard(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent; deprecated;

    function GetCheckoutSession(ASessionID: string): IpsCheckoutSession;

    function CreateCustomer(AName, AEmail, ADescription: string; AMeta: TStrings): IpsCustomer;
    function Getcustomer(AID: string): IpsCustomer;
    function UpdateCustomer(AID: string; AParams: IpsUpdateCustomerParams): IpsCustomer;
    procedure SaveCustomer(AID: string; ANameValues: TStrings);


    function CreatePaymentIntent(AAmountPence: integer; ADesc, ACurrency: string; AMetaData: TStrings; AApplicationFee: integer): IpsPaymentIntent;
    function CancelPaymentIntent(APaymentIntentID: string): IpsPaymentIntent;

    function GetPaymentIntent(AID: string): IpsPaymentIntent;
    function GetPaymentMethod(AID: string): IpsPaymentMethod;
    function GetPaymentMethods(ACustID: string): string;

    function CreateSetupIntent(const ACustID: string = ''): IpsSetupIntent; overload;
    function CreateSetupIntent(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent; overload;
    function GetSetupIntent(ASetupIntentID: string): IpsSetupIntent;
    function ConfirmSetupIntent(ASetupIntentID: string): IpsSetupIntent;

    function AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
    function CreateCharge(AChargeParams: IpsCreateChargeParams): IpsCharge;
    function GetCharge(AChargeID: string; const AExpandCustomer: Boolean = False): IpsCharge;
    function GetCharges(const AOptions: IpsChargeListOptions = nil): IpsChargeList;

    function GetInvoice(AID: string): IpsInvoice;


    function RefundCharge(AChargeID, AReason: string; AAmount: integer): Boolean;
    function UpdateCharge(AChargeID: string; AChargeParams: IpsUpdateChargeParams): IpsCharge;

    function ExpireSession(ASessionID: string): IpsCheckoutSession;

    function GenerateCheckoutSession(AParams: IpsCreateCheckoutParams): IpsCheckoutSession;

    function DeleteAccount(AAccount: string): Boolean;
    function GetData(AResource: string; const AParams: TStrings = nil): string;

    property AccountID: string read GetAccountID;
  public
    constructor Create(ASecretKey: string; AAccount: string);


  end;

implementation

uses SysUtils, DateUtils, Math, pasStripe.Utils, System.Json, System.NetEncoding,
  pasStripe.Constants;

const
  C_DEFAULT_LIMIT = 100;

{ TPasStripe }

function TPasStripe.AddCard(ACustID: string; ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent;
begin
  Result := CreateSetupIntent(ACustID, ANum, AMonth, AYear, ACvc);
end;

function TPasStripe.AttachPaymentMethodToCustomer(ACustID, APaymentMethodID: string): string;
var
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    AParams.Values[C_CUSTOMER] := ACustID;
    Result := Post(C_PAYMENT_METHODS + '/' + APaymentMethodID + '/attach', AParams);
  finally
    AParams.Free;
  end;

end;

function TPasStripe.CancelPaymentIntent(APaymentIntentID: string): IpsPaymentIntent;
var
  AData: string;
begin
  Result := TpsFactory.PaymentIntent;
  AData := Post(C_PAYMENT_INTENTS + '/' + APaymentIntentID+'/cancel', nil);
  if FLastError = '' then
    Result.LoadFromJson(AData);
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
  AParams: IpsCreatePaymentIntentParams;
  AData: string;
begin
  Result := TpsFactory.PaymentIntent;
  AParams := TpsFactory.CreatePaymentIntentParams(AAmountPence, ADesc, StringToCurrency(ACurrency));

  //AParams.PaymentMethods  := [TpsPaymentMethodType.pmCard];

  //AParams.FutureUsage := tfuOffSession;
  if FAccount <> '' then
  begin
    // connected account
    AParams.ApplicationFeeAmount := AApplicationFee;
  end;
  AData := Post(C_PAYMENT_INTENTS, AParams);
  Result.LoadFromJson(AData);

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
      Result := AJson.B[deleted];
    finally
      AJson.Free;
    end;
  end;
end;

function TPasStripe.HttpAction(AVerb: THttpVerb; AMethod: string;
  AUrlParams: TStrings): IHTTPResponse;

  function ParamsToUrl(AStrings: TStrings): string;
  var
    ICount: integer;
  begin
    Result := '';
    for ICount := 0 to AStrings.Count - 1 do
    begin
      Result := Result + TNetEncoding.URL.Encode(AStrings.Names[ICount])+'='+TNetEncoding.URL.Encode(AStrings.ValueFromIndex[ICount]);
      if ICount < AStrings.Count - 1 then
        Result := Result + '&';
    end;
  end;

var
  AUrl: string;
  AHttp: THTTPClient;
  APostParams: TStrings;
begin
  AHttp := THTTPClient.Create;
  APostParams := TStringList.Create; // unused, required by Post() to prevent AV
  try
    AUrl := 'https://api.stripe.com/v1/' + AMethod;
    if AUrlParams <> nil then
    begin
      if AUrlParams.Count > 0 then
        AUrl := AUrl + '?' + ParamsToUrl(AUrlParams);
    end;

    AHttp.CustomHeaders['Authorization'] := 'Bearer ' + FSecretKey;
    //AHttp.ContentType := 'application/json';

    if FAccount <> '' then
      AHttp.CustomHeaders['Stripe-Account'] := FAccount;

    if AMethod.ToLower = 'account_sessions' then
      AHttp.CustomHeaders['Stripe-Version'] := '2022-08-01; embedded_connect_beta=v1';

    

    case AVerb of
      httpGet: Result := AHttp.Get(AUrl);
      httpPost: Result := AHttp.Post(AUrl, APostParams);
      httpDelete: Result := AHttp.Delete(AUrl);
    end;

  finally
    AHttp.Free;
    APostParams.Free;
  end;
end;

function TPasStripe.HttpAction(AVerb: THttpVerb; AMethod, AUrlParams: string): IHTTPResponse;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    AStrings.Text := AUrlParams;
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

function TPasStripe.GenerateCheckoutSession(AParams: IpsCreateCheckoutParams): IpsCheckoutSession;
var
  AData: string;
begin
  Result := TpsFactory.CheckoutSession;
  AData := Post('checkout/sessions', AParams);
  if FLastError = '' then
    Result.LoadFromJson(AData);
end;


function TPasStripe.Get(AMethod: string; AParams: TStrings): string;
var
  AJson: TJsonObject;
  AResult: string;
begin
  Result := '';
  FLastError := '';
  AJson := TJSONObject.Create;
  try
    AResult := HttpAction(httpGet, AMethod, AParams).ContentAsString;
    AJson.FromJSON(AResult);
    FLastError := AJson.O[C_ERROR].S[&message];
    if FLastError = '' then
    begin
      Result := AResult;

    end
    else
      raise Exception.Create(FLastError);
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
      Result := AJson.S[url];
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

    AData := Post(C_ACCOUNTS, AParams);
    Result.LoadFromJson(AData);
  finally
    AParams.Free;
  end;
end;

function TPasStripe.CreateAccount(AParams: IpsCreateAccountParams): IpsAccount;
var
  AResponse: string;
begin
  Result := TpsFactory.Account;
  AResponse := Post(C_ACCOUNTS, AParams);
  if FLastError = '' then
    Result.LoadFromJson(AResponse);
end;

function TpasStripe.UpdateAccount(AId: string; AParams: IpsUpdateAccountParams): IpsAccount;
var
  AResponse: string;
begin
  Result := TpsFactory.Account;
  AResponse := Post(C_ACCOUNTS+'/'+AId, AParams);
  if FLastError = '' then
    Result.LoadFromJson(AResponse);
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
    AParams.Values[C_ACCOUNT] := AAccount;
    AData := Post(C_ACCOUNT_SESSIONS, AParams);
    AJson.FromJSON(AData);
    Result := AJson.S[client_secret];
  finally
    AJson.Free;
    AParams.Free;
  end;
end;

function TPasStripe.Post(AMethod: string; AUrlParams: TStrings): string;
var
  AJson: TJsonObject;
  AResult: string;
begin
  Result := '';
  FLastError := '';
  AJson := TJSONObject.Create;
  try
    AResult := HttpAction(httpPost, AMethod, AUrlParams).ContentAsString;
    AJson.FromJSON(AResult);
    FLastError := AJson.O[C_ERROR].S[&message];
    if FLastError = '' then
    begin
      Result := AResult;
    end
    else
      raise Exception.Create(FLastError);

  finally
    AJson.Free;
  end;
end;

function TPasStripe.Post(AMethod: string; AParams: IpsBaseParams): string;
var
  AUrlParams: TStrings;
begin
  AUrlParams := TStringList.Create;
  try
    AParams.PopulateStrings(AUrlParams);
    Result := Post(AMethod, AUrlParams);
  finally
    AUrlParams.Free;
  end;
end;


function TPasStripe.RefundCharge(AChargeID, AReason: string; AAmount: integer): Boolean;
var
  AParams: IpsCreateRefundParams;
begin
  AParams := TpsFactory.CreateRefundParams;
  AParams.Amount := AAmount;
  AParams.Charge := AChargeID;
  AParams.Reason := AReason;
  Post(C_REFUNDS, AParams);
  Result := FLastError = '';
end;

procedure TPasStripe.SaveCustomer(AID: string; ANameValues: TStrings);
var
  AResult: string;
begin
  AResult := Post(C_CUSTOMERS + '/' + AID, ANameValues);
end;

function TPasStripe.TestCredentials: Boolean;
var
  AAccount: IpsAccount;
begin
   AAccount := GetAccount;
   Result := AAccount.ID <> '';
end;

function TPasStripe.UpdateCharge(AChargeID: string; AChargeParams: IpsUpdateChargeParams): IpsCharge;
var
  AData: string;
begin
  Result := TpsFactory.Charge;
  AData := Post('charges/' + AChargeID, AChargeParams);
  if FLastError = '' then
    Result.LoadFromJson(AData);
end;

function TPasStripe.UpdateCustomer(AID: string; AParams: IpsUpdateCustomerParams): IpsCustomer;
var
  AData: string;
begin
  Result := TpsFactory.Customer;
  AData := Post(C_CUSTOMERS + '/' + AID, AParams);
  if FLastError = '' then
    Result.LoadFromJson(AData);
end;


function TPasStripe.CreateCharge(AChargeParams: IpsCreateChargeParams): IpsCharge;
var
  AJson: TJsonObject;
  AResult: string;
begin
  Result := TpsFactory.Charge;
  AJson := TJsonObject.Create;
  try
    AResult := Post(C_PAYMENT_INTENTS, AChargeParams);
    if FLastError = '' then
      Result.LoadFromJson(AResult);
  finally
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
    if AExpandCustomer then AParams.Values['expand[]'] := C_CUSTOMER;
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

function TPasStripe.GetCharges(const AOptions: IpsChargeListOptions = nil): IpsChargeList;
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
  Result := TpsFactory.ChargeList;
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
      if AOptions.PaymentIntentID <> '' then AParams.Values[C_PAYMENT_INTENT] := AOptions.PaymentIntentID;
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

    until (AJson.B[has_more] = False) or (Result.Count >= ALimit);

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
{var
  AParams: TpsCreateCustomerParams;
  AJson: string;   }
begin
 { Result := TpsFactory.Customer;
  AParams := TpsFactory.CreateCustomerParams;
  try
    AParams.Name := AName;
    AParams.Description := ADescription;
    AParams.Email := AEmail;
    if AMeta <> nil then
    begin
      for var ICount := 0 to AMeta.Count - 1 do
      begin
        AParams.MetaData[AMeta.Names[ICount]] := AMeta.ValueFromIndex[ICount];
      end;
    end;


    AJson := Post(C_CUSTOMERS, AParams);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
  end; }
end;

function TPasStripe.Getcustomer(AID: string): IpsCustomer;
var
  AData: string;
begin
  Result := TpsFactory.Customer;
  AData := Get(C_CUSTOMERS + '/' + AID, nil);
  Result.LoadFromJson(AData);
end;


function TPasStripe.GetPaymentIntent(AID: string): IpsPaymentIntent;
var
  AData: string;
  AJson: TJsonObject;
begin
  Result := TpsFactory.PaymentIntent;
  AData := Get(C_PAYMENT_INTENTS + '/' + AID, nil);
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
  AData := Get(C_PAYMENT_METHODS + '/' + AID, nil);
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

function TPasStripe.GetPaymentMethods(ACustID: string): string;
begin
  Result := Get(C_CUSTOMERS + '/' + ACustID+'/payment_methods', nil);
end;

function TPasStripe.CreateSetupIntent(const ACustID: string = ''): IpsSetupIntent;
var
  AParams: TStrings;
  AJson: TJsonObject;
  AData: string;

begin
  Result := TpsFactory.SetupIntent;
  AParams := TStringList.Create;
  AJson := TJsonObject.Create;
  try
    if ACustID <> '' then AParams.Values[C_CUSTOMER] := ACustID;
    AParams.Values['usage'] := 'off_session';
    //AParams.Values['confirm'] := 'true';
    AData := Post(C_SETUP_INTENTS, AParams);
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
    AJson.Free;
  end;
end;

function TPasStripe.CreateSetupIntent(ACustID, ANum: string; AMonth, AYear, ACvc: integer): IpsSetupIntent;
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
    if ACustID <> '' then AParams.Values[C_CUSTOMER] := ACustID;
    AParams.Values['payment_method_data[type]'] := 'card';
    AParams.Values['payment_method_data[card][number]'] := ANum;
    AParams.Values['payment_method_data[card][exp_month]'] := AMonth.ToString;
    AParams.Values['payment_method_data[card][exp_year]'] := AYear.ToString;
    AParams.Values['payment_method_data[card][cvc]'] := AStrCvc;
    AParams.Values['payment_method_options[card][moto]'] := 'true';
    AParams.Values['confirm'] := 'true';
    AData := Post(C_SETUP_INTENTS, AParams);
    AJson.FromJSON(AData);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
    AJson.Free;
  end;
end;


function TPasStripe.GetSetupIntent(ASetupIntentID: string): IpsSetupIntent;
var
  AData: string;
begin
  Result := TpsFactory.SetupIntent;
  AData := Get(C_SETUP_INTENTS + '/' + ASetupIntentID, nil);
  if FLastError = '' then
    Result.LoadFromJson(AData);
end;

function TpasStripe.ConfirmSetupIntent(ASetupIntentID: string): IpsSetupIntent;
var
  AData: string;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    Result := TpsFactory.SetupIntent;
    AData := Post(C_SETUP_INTENTS + '/' + ASetupIntentID + '/confirm', AParams);
    if FLastError = '' then
      Result.LoadFromJson(AData);
  finally
    AParams.Free;
  end;
end;

{ TpsBaseObjectWithMetadata }

end.
