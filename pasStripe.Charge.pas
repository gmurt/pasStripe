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

unit pasStripe.Charge;

interface

uses pasStripe, pasStripe.Base, pasStripe.Json, pasStripe.Params, Spring;

type
  TpsUpdateChargeParams = class(TpsBaseParamsWithMetaData, IpsUpdateChargeParams)
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  end;



  TpsCreateChargeParams = class(TpsUpdateChargeParams, IpsCreateChargeParams)
  private
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
  end;



  TpsChargeListOptions = class(TInterfacedObject, IpsChargeListOptions)
  private
    FFrom: TDateTime;
    FTo: TDateTime;
    FpaymentIntentID: string;
    Flimit: integer;
    FQuery: string;
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
  public
    constructor Create; virtual;
  end;


  TpsCharge = class(TpsBaseObjectWithMetadata, IpsCharge)
  private
    FID: string;
    FCustomer: string;
    FDescription: string;
    FJson: string;
    FAmount: integer;
    FRefunded: integer;
    FCreated: TDateTime;
    FCurrency: string;
    FStatus: string;
    FCardBrand: string;
    FLast4: string;
    FPaymentIntentID: string;
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
  public
    procedure LoadFromJson(AJson: TJsonObject); override;
  end;

  TpsChargeList = class(TpsBaseList<IpsCharge>, IpsChargeList);

implementation

uses SysUtils, DateUtils, pasStripe.Constants, System.Json, pasStripe.Utils;

{ TpsChargeListOptions }

constructor TpsChargeListOptions.Create;
begin
  Flimit := 100;
  FFrom := -1;
  FTo := -1;
end;

function TpsChargeListOptions.GetFromDate: TDateTime;
begin
  Result := FFrom;
end;

function TpsChargeListOptions.GetLimit: integer;
begin
  Result := Flimit;
end;

function TpsChargeListOptions.GetPaymentIntentID: string;
begin
  Result := FpaymentIntentID;
end;

function TpsChargeListOptions.GetQuery: string;
begin
  Result := FQuery;
end;

function TpsChargeListOptions.GetToDate: TDateTime;
begin
  Result := FTo;
end;

procedure TpsChargeListOptions.SetFromDate(const Value: TDateTime);
begin
  FFrom := Value;
end;

procedure TpsChargeListOptions.SetLimit(const Value: integer);
begin
  Flimit := Value;
end;

procedure TpsChargeListOptions.SetPaymentIntentID(const Value: string);
begin
  FPaymentIntentID := Value;
end;

procedure TpsChargeListOptions.SetQuery(const Value: string);
begin
  FQuery := Value;
end;

procedure TpsChargeListOptions.SetToDate(const Value: TDateTime);
begin
  FTo := Value;
end;


{ TpsCharge }

function TpsCharge.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsCharge.GetCardBrand: string;
begin
  Result := FCardBrand;
end;

function TpsCharge.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsCharge.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsCharge.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsCharge.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCharge.GetID: string;
begin
  Result := FID;
end;

function TpsCharge.GetJson: string;
begin
  Result := FJson;
end;

function TpsCharge.GetLast4: string;
begin
  Result := FLast4;
end;

function TpsCharge.GetPaymentIntentID: string;
begin
  Result := FPaymentIntentID;
end;

function TpsCharge.GetRefunded: integer;
begin
  Result := FRefunded;
end;

function TpsCharge.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TpsCharge.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FJson := AJson.ToJSON;
  FID := AJson.S[id];
  FAmount := AJson.I[amount];
  FCurrency := AJson.S[currency];

  if AJson.IsNull('customer') = False then
  begin
    case AJson.Types['customer'] of
      jvtObject: FCustomer := AJson.O['customer'].S[id];
      jvtString: FCustomer := AJson.S[customer];
    end;
  end;

  FRefunded := AJson.I[amount_refunded];

  if AJson.IsNull('payment_method_details') = False then
  begin
    if AJson.O['payment_method_details'].IsNull('card') = False then
    begin
      FLast4 := AJson.O['payment_method_details'].O['card'].S[last4];
      FCardBrand := AJson.O['payment_method_details'].O['card'].S[brand];
    end;
  end;

  //FMetadata.LoadFromJson(AJson.O['metadata']);
  FCreated := UnixToDateTime(StrToInt(AJson.S[created]));
  if AJson.IsNull('payment_intent') = False then
    FpaymentIntentID := AJson.S[payment_intent];
  if AJson.IsNull('description') = False then
    FDescription := AJson.S[description];

  FStatus := AJson.S[status];
end;


{ TpsCreateChargeParams }

function TpsCreateChargeParams.GetAmount: integer;
begin
  Result := GetInteger(amount);
end;

function TpsCreateChargeParams.GetApplicationFeeAmount: integer;
begin
  Result := GetInteger(application_fee_amount);
end;

function TpsCreateChargeParams.GetConfirm: Boolean;
begin
  Result := GetBoolean(confirm);
end;

function TpsCreateChargeParams.GetCurrency: TpsCurrency;
begin
  Result := StringToCurrency(GetString(currency));
end;

function TpsCreateChargeParams.GetCustomer: string;
begin
  Result := GetString(customer);
end;

function TpsCreateChargeParams.GetPaymentMethod: string;
begin
  Result := GetString(payment_method);
end;

procedure TpsCreateChargeParams.SetAmount(const Value: integer);
begin
  SetInteger(amount, Value);
end;

procedure TpsCreateChargeParams.SetApplicationFeeAmount(const Value: integer);
begin
  SetInteger(application_fee_amount, Value);
end;

procedure TpsCreateChargeParams.SetConfirm(const Value: Boolean);
begin
  SetBoolean(confirm, Value);
end;

procedure TpsCreateChargeParams.SetCurrency(const Value: TpsCurrency);
begin
  SetString(currency, CurrencyToString(Value));
end;

procedure TpsCreateChargeParams.SetCustomer(const Value: string);
begin
  SetString(customer, Value);
end;

procedure TpsCreateChargeParams.SetPaymentMethod(const Value: string);
begin
  SetString(payment_method, Value);
end;

{ TpsUpdateChargeParams }

function TpsUpdateChargeParams.GetDescription: string;
begin
  Result := GetString(description);
end;



procedure TpsUpdateChargeParams.SetDescription(const Value: string);
begin
  SetString(description, Value);
end;



end.
