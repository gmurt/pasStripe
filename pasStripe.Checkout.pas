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

unit pasStripe.Checkout;

interface

uses
   Classes, pasStripe, pasStripe.Base, pasStripe.Json, pasStripe.Params, System.Generics.Collections;

type
  TpsCheckoutLineItem = class(TInterfacedObject, IpsCheckoutLineItem)
    FCurrency: TpsCurrency;
    FDescription: string;
    FPriceID: string;
    FAmount: integer;
    FQty: integer;
    FTaxID: string;
    FRecurring: TpsRecurring;
    function GetAmount: integer;
    function GetCurrency: TpsCurrency;
    function GetDescription: string;
    function GetPriceID: string;
    function GetQty: integer;
    function GetRecurring: TpsRecurring;
    function GetTaxID: string;
    procedure SetTaxID(const Value: string);
    procedure SetRecurring(const Value: TpsRecurring);

    procedure SetAmount(const Value: integer);
    procedure SetCurrency(const Value: TpsCurrency);

    procedure SetDescription(const Value: string);
    procedure SetPriceID(const Value: string);
    procedure SetQty(const Value: integer);

  private
  private
  end;

  TpsCheckoutLineItems = class(TInterfacedObject, IpsCheckoutLineItems)
  private
    FItems: TList<IpsCheckoutLineItem>;
  protected

    function AddLineItem(ADescription: string; AQty: integer; APence: integer; const ATaxRate: string = ''; const ARecurring: TpsRecurring = None): IpsCheckoutLineItem; overload;
    function AddLineItem(APriceID: string; AQty: integer; const ATaxRate: string = ''): IpsCheckoutLineItem; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PopulateStrings(AStrings: TStrings);
  end;

  TpsCreateCheckoutParams = class(TpsBaseParamsWithMetaData, IpsCreateCheckoutParams)
  private
    FPaymentMethodTypes: TpsPaymentMethodsTypes;
    FLineItems: IpsCheckoutLineItems;
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
  public
    constructor Create(AParent: TpsBaseParams); override;
    procedure PopulateStrings(AStrings: TStrings); override;
    property ApplicationFeeAmount: integer read GetApplicationFeeAmount write SetApplicationFeeAmount;
  end;

  TpsCheckoutSession = class(TpsBaseObjectWithMetadata, IpsCheckoutSession)
  private
    FID: string;
    FCreated: TDateTime;
    FUrl: string;
    FPaymentIntentID: string;
    FSetupIntentID: string;
    FStatus: string;
    FPaymentStatus: string;
    FJson: string;

    function GetID: string;
    function GetCreated: TDateTime;
    function GetPaymentIntentID: string;
    function GetSetupIntentID: string;
    function GetJson: string;
    function GetPaymentStatus: string;
    function GetStatus: string;
    function GetUrl: string;

    procedure SetID(const Value: string);
    procedure SetPaymentIntentID(const Value: string);
    procedure SetSetupIntentID(const Value: string);
    procedure SetJson(const Value: string);
    procedure SetPaymentStatus(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetUrl(const Value: string);
  protected
    procedure Clear; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
  end;


implementation

uses SysUtils, DateUtils, pasStripe.Constants, pasStripe.Utils, System.JSON;

{ TpsCreateCheckoutParams }

constructor TpsCreateCheckoutParams.Create(AParent: TpsBaseParams);
begin
  inherited Create(AParent);
  FLineItems := TpsCheckoutLineItems.Create;
end;

function TpsCreateCheckoutParams.GetMode: TpsCheckoutMode;
begin
  Result := StringToCheckoutMode(GetString(TpsParamName.mode));
end;

function TpsCreateCheckoutParams.GetPaymentMethods: TpsPaymentMethodsTypes;
begin
  Result := FPaymentMethodTypes;
end;

function TpsCreateCheckoutParams.GetApplicationFeeAmount: integer;
begin
  Result := GetInteger(application_fee_amount);
end;

function TpsCreateCheckoutParams.GetCancelUrl: string;
begin
  Result := GetString(cancel_url);
end;

function TpsCreateCheckoutParams.GetSuccessUrl: string;
begin
  Result := GetString(success_url);
end;

procedure TpsCreateCheckoutParams.PopulateStrings(AStrings: TStrings);
begin
  inherited;
  FLineItems.PopulateStrings(AStrings);
end;

function TpsCreateCheckoutParams.GetCurrency: TpsCurrency;
begin
  Result := StringToCurrency(GetString(TpsParamName.currency));
end;

function TpsCreateCheckoutParams.GetCustomerEmail: string;
begin
  Result := GetString(customer_email)
end;

function TpsCreateCheckoutParams.GetLineItems: IpsCheckoutLineItems;
begin
  Result := FLineItems;
end;

procedure TpsCreateCheckoutParams.SetMode(const Value: TpsCheckoutMode);
begin
  SetString(TpsParamName.mode, CheckoutModeToString(Value));
end;

procedure TpsCreateCheckoutParams.SetPaymentMethods(
  const Value: TpsPaymentMethodsTypes);
begin
  FPaymentMethodTypes := Value;
end;

procedure TpsCreateCheckoutParams.SetApplicationFeeAmount(const Value: integer);
begin
  SetInteger(application_fee_amount, Value);
end;

procedure TpsCreateCheckoutParams.SetCancelUrl(const Value: string);
begin
  SetString(cancel_url, Value);
end;

procedure TpsCreateCheckoutParams.SetSuccessUrl(const Value: string);
begin
  SetString(success_url, Value);
end;

procedure TpsCreateCheckoutParams.SetCurrency(const Value: TpsCurrency);
begin
  SetString(TpsParamName.currency, CurrencyToString(Value))
end;

procedure TpsCreateCheckoutParams.SetCustomerEmail(const Value: string);
begin
  SetString(customer_email, Value);
end;

function TpsCheckoutSession.GetUrl: string;
begin
  Result := FUrl;
end;

{ TpsCheckoutSession }

procedure TpsCheckoutSession.Clear;
begin
  inherited;
  FID := '';
  FUrl := '';
  FCreated := 0;
  FPaymentIntentID := '';
  FSetupIntentID := '';
  FStatus := '';
  FPaymentStatus := '';
  FJson := '';
end;

function TpsCheckoutSession.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsCheckoutSession.GetID: string;
begin
  Result := FID;
end;

function TpsCheckoutSession.GetPaymentIntentID: string;
begin
  Result := FPaymentIntentID;
end;

function TpsCheckoutSession.GetJson: string;
begin
  Result := FJson;
end;

function TpsCheckoutSession.GetPaymentStatus: string;
begin
  Result := FPaymentStatus;
end;

function TpsCheckoutSession.GetSetupIntentID: string;
begin
  Result := FSetupIntentID;
end;

function TpsCheckoutSession.GetStatus: string;
begin
  Result := FStatus;
end;


procedure TpsCheckoutSession.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FID := AJson.S[id];
  FCreated := UnixToDateTime(StrToInt(AJson.S[created]));
  if not AJson.IsNull('url') then FUrl := AJson.S[url];
  if AJson.Types['payment_intent'] = jvtString then FPaymentIntentID := AJson.S[payment_intent];
  if AJson.Types['setup_intent'] = jvtString then FSetupIntentID := AJson.S[setup_intent];
  FStatus := AJson.S[status];
  FPaymentStatus := AJson.S[payment_status];
  FJson := AJson.ToJSON;
end;



procedure TpsCheckoutSession.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TpsCheckoutSession.SetPaymentIntentID(const Value: string);
begin
  FPaymentIntentID := Value;
end;

procedure TpsCheckoutSession.SetSetupIntentID(const Value: string);
begin
  FSetupIntentID := Value;
end;

procedure TpsCheckoutSession.SetJson(const Value: string);
begin
  FJson := Value;
end;

procedure TpsCheckoutSession.SetPaymentStatus(const Value: string);
begin
  FPaymentStatus := Value;
end;

procedure TpsCheckoutSession.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TpsCheckoutSession.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

{ TpsCheckoutParams }
      {
procedure TpsCheckoutParams.Clear;
begin
  FCurrency := '';
  FEmail := '';
  FCustomer := '';
  FDescription := '';
  FMode := cmSetup;
  FPriceID := '';
  FSuccessUrl := '';
  FCancelUrl := '';
  FAmount := 0;
  FPaymentMethods := [];
  FMetaData.Clear;
  FApplicationFee := 0;
  FClientReferenceID := '';
  FTaxID := '';
end;

constructor TpsCheckoutParams.Create;
begin
  FMetaData := TpsFactory.Metadata;
end;

function TpsCheckoutParams.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsCheckoutParams.GetApplicationFee: integer;
begin
  Result := FApplicationFee;
end;

function TpsCheckoutParams.GetCancelUrl: string;
begin
  Result := FCancelUrl;
end;

function TpsCheckoutParams.GetClientReferenceID: string;
begin
  Result := FClientReferenceID;
end;

function TpsCheckoutParams.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsCheckoutParams.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsCheckoutParams.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCheckoutParams.GetEmail: string;
begin
  Result := FEmail;
end;

function TpsCheckoutParams.GetMetaData: IpsMetaData;
begin
  Result := FMetaData;
end;

function TpsCheckoutParams.GetMode: TpsCheckoutMode;
begin
  Result := FMode;
end;

function TpsCheckoutParams.GetPaymentMethodsTypes: TpsPaymentMethodsTypes;
begin
  Result := FPaymentMethods;
end;

function TpsCheckoutParams.GetPriceID: string;
begin
  Result := FPriceID;
end;

function TpsCheckoutParams.GetSuccessUrl: string;
begin
  Result := FSuccessUrl;
end;

function TpsCheckoutParams.GetTaxID: string;
begin
  Result := FTaxID;
end;

procedure TpsCheckoutParams.SetAmount(const Value: integer);
begin
  FAmount := Value;
end;

procedure TpsCheckoutParams.SetApplicationFee(const Value: integer);
begin
  FApplicationFee := Value;
end;

procedure TpsCheckoutParams.SetCancelUrl(const Value: string);
begin
  FCancelUrl := Value;
end;

procedure TpsCheckoutParams.SetClientReferenceID(const Value: string);
begin
  FClientReferenceID := Value;
end;

procedure TpsCheckoutParams.SetCurrency(const Value: string);
begin
  FCurrency := Value;
end;

procedure TpsCheckoutParams.SetCustomer(const Value: string);
begin
  FCustomer := Value;
end;

procedure TpsCheckoutParams.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TpsCheckoutParams.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TpsCheckoutParams.SetMode(const Value: TpsCheckoutMode);
begin
  FMode := Value;
end;

procedure TpsCheckoutParams.SetPaymentMethodsTypes(const Value: TpsPaymentMethodsTypes);
begin
  FPaymentMethods := Value;
end;

procedure TpsCheckoutParams.SetPriceID(const Value: string);
begin
  FPriceID := Value;
end;

procedure TpsCheckoutParams.SetSuccessUrl(const Value: string);
begin
  FSuccessUrl := Value;
end;

procedure TpsCheckoutParams.SetTaxID(const Value: string);
begin
  FTaxID := Value;
end;            }

{ TpsCheckoutLineItems }

function TpsCheckoutLineItems.AddLineItem(ADescription: string; AQty,
  APence: integer; const ATaxRate: string = ''; const ARecurring: TpsRecurring = None): IpsCheckoutLineItem;
var
  AItem: IpsCheckoutLineItem;
begin
  AItem := TpsCheckoutLineItem.Create;
  AItem.Amount := APence;
  AItem.Description := ADescription;
  AItem.Qty := AQty;
  AItem.Recurring := ARecurring;
  AItem.TaxID := ATaxRate;
  FItems.add(AItem);
end;

function TpsCheckoutLineItems.AddLineItem(APriceID: string; AQty: integer; const ATaxRate: string = ''): IpsCheckoutLineItem;
var
  AItem: IpsCheckoutLineItem;
begin
  AItem := TpsCheckoutLineItem.Create;
  AItem.PriceID := APriceID;
  AItem.Qty := AQty;
  AItem.TaxID := ATaxRate;
  FItems.add(AItem);
end;

constructor TpsCheckoutLineItems.Create;
begin
  FItems := TList<IpsCheckoutLineItem>.Create;
end;

destructor TpsCheckoutLineItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TpsCheckoutLineItems.PopulateStrings(AStrings: TStrings);
var
  AItem: IpsCheckoutLineItem;
  ICount: integer;
begin
  for ICount := 0 to FItems.Count-1 do
  begin
    AItem := FItems[ICount];

    if AItem.PriceID <> '' then
    begin
      AStrings.Values['line_items['+ICount.ToString+'][price]'] := AItem.PriceID;
    end
    else
    begin
      AStrings.Values['line_items['+ICount.ToString+'][price_data][unit_amount]'] := AItem.Amount.ToString;
      AStrings.Values['line_items['+ICount.ToString+'][price_data][product_data][name]'] := AItem.Description;
      AStrings.Values['line_items['+ICount.ToString+'][price_data][currency]'] := CurrencyToString(AItem.Currency);
      if AItem.Recurring <> None then
        AStrings.Values['line_items['+ICount.ToString+'][price_data][recurring][interval]'] := IntervalToString(AItem.Recurring);

      AStrings.Values['line_items['+ICount.ToString+'][tax_rates][]'] := AItem.TaxID;

    end;
    AStrings.Values['line_items['+ICount.ToString+'][quantity]'] := AItem.Qty.ToString;



  end;
end;


{ TpsCheckoutLineItem }

function TpsCheckoutLineItem.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsCheckoutLineItem.GetCurrency: TpsCurrency;
begin
  Result := FCurrency;
end;

function TpsCheckoutLineItem.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCheckoutLineItem.GetPriceID: string;
begin
  Result := FPriceID;
end;

function TpsCheckoutLineItem.GetQty: integer;
begin
  Result := FQty;
end;

function TpsCheckoutLineItem.GetRecurring: TpsRecurring;
begin
  Result := FRecurring;
end;

function TpsCheckoutLineItem.GetTaxID: string;
begin
  Result := FTaxID;
end;

procedure TpsCheckoutLineItem.SetAmount(const Value: integer);
begin
  FAmount := Value;
end;

procedure TpsCheckoutLineItem.SetCurrency(const Value: TpsCurrency);
begin
  FCurrency := Value;
end;

procedure TpsCheckoutLineItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TpsCheckoutLineItem.SetPriceID(const Value: string);
begin
  FPriceID := Value;
end;

procedure TpsCheckoutLineItem.SetQty(const Value: integer);
begin
  FQty := Value;
end;

procedure TpsCheckoutLineItem.SetRecurring(const Value: TpsRecurring);
begin
  FRecurring := Value;
end;

procedure TpsCheckoutLineItem.SetTaxID(const Value: string);
begin
  FTaxID := Value;
end;

end.
