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

unit pasStripe.Intents;

interface

uses pasStripe, pasStripe.Base, pasStripe.Json, pasStripe.Params;

type
  TpsCreatePaymentIntentParams = class(TpsBaseParamsWithMetaData, IpsCreatePaymentIntentParams)
  private
    function GetAmount: integer;
    function GetApplicationFeeAmount: integer;
    function GetConfirm: Boolean;
    function GetCurrency: TpsCurrency;
    function GetCustomer: string;
    function GetDescription: string;
    function GetFutureUsage: TpsFutureUsage;
    function GetPaymentMethod: string;
    procedure SetAmount(const Value: integer);
    procedure SetApplicationFeeAmount(const Value: integer);
    procedure SetConfirm(const Value: Boolean);
    procedure SetCurrency(const Value: TpsCurrency);
    procedure SetCustomer(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetFutureUsage(const Value: TpsFutureUsage);
    procedure SetPaymentMethod(const Value: string);
  end;


  TpsPaymentIntent = class(TpsBaseObjectWithMetadata, IpsPaymentIntent)
  private
    FJson: string;
    Fid: string;
    FAmount: integer;
    FApplicationFee: integer;
    FPaid: Boolean;
    FCreated: TDateTime;
    FClientSecret: string;
    FPaymentMethod: string;
    FStatus: string;
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCreated: TDateTime;
    function GetId: string;
    function GetPaid: Boolean;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
    function GetStatus: string;
    function GetJson: string;
  protected
    procedure LoadFromJson(AJson: TpsJsonObject); override;
  end;

  TpsSetupIntent = class(TpsBaseObjectWithMetadata, IpsSetupIntent)
  private
    FID: string;
    FStatus: string;
    FClientSecret: string;
    FPaymentMethod: string;
    function GetID: string;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
    function GetStatus: string;
  protected

    procedure LoadFromJson(AJson: TpsJsonObject); override;
  end;

implementation

uses SysUtils, DateUtils, pasStripe.Constants, pasStripe.Utils, pasStripe.ParamTypes;

{ TpsPaymentIntent }

function TpsPaymentIntent.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsPaymentIntent.GetApplicationFee: integer;
begin
  Result := FApplicationFee;
end;

function TpsPaymentIntent.GetClientSecret: string;
begin
  Result := FClientSecret;
end;

function TpsPaymentIntent.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsPaymentIntent.GetId: string;
begin
  Result := Fid;
end;

function TpsPaymentIntent.GetJson: string;
begin
  Result := FJson;
end;

function TpsPaymentIntent.GetPaid: Boolean;
begin
  Result := FPaid;
end;

function TpsPaymentIntent.GetPaymentMethod: string;
begin
  Result := FPaymentMethod;
end;

function TpsPaymentIntent.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TpsPaymentIntent.LoadFromJson(AJson: TpsJsonObject);
begin
  inherited;
  FJson := AJson.ToJSON;
  Fid := AJson.S[id];
  FAmount := AJson.I[amount];
  if AJson.IsNull('application_fee_amount') = False then
    FApplicationFee := AJson.I[application_fee_amount];

  FPaid := AJson.I[amount_received] >= AJson.I[amount];
  FCreated := UnixToDateTime(StrToInt(AJson.S[created]));
  FClientSecret := AJson.S[client_secret];
  FStatus := AJson.S[status];
  if not AJson.IsNull('payment_method') then FPaymentMethod := AJson.S[payment_method];
  
end;

{ TpsSetupIntent }


function TpsSetupIntent.GetClientSecret: string;
begin
  Result := FClientSecret;
end;

function TpsSetupIntent.GetID: string;
begin
  Result := FID;
end;

function TpsSetupIntent.GetPaymentMethod: string;
begin
  Result := FPaymentMethod;
end;

function TpsSetupIntent.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TpsSetupIntent.LoadFromJson(AJson: TpsJsonObject);
begin
  inherited;
  FID := AJson.S[id];
  FStatus := AJson.S[status];
  if not AJson.IsNull('payment_method') then FPaymentMethod := AJson.S[payment_method];
  if not AJson.IsNull('client_secret') then FClientSecret := AJson.S[client_secret];
end;


{ TpsCreatePaymentIntentParams }

function TpsCreatePaymentIntentParams.GetAmount: integer;
begin
  Result := GetInteger(amount);
end;

function TpsCreatePaymentIntentParams.GetApplicationFeeAmount: integer;
begin
  Result := GetInteger(application_fee_amount);
end;

function TpsCreatePaymentIntentParams.GetConfirm: Boolean;
begin
  Result := GetBoolean(confirm);
end;

function TpsCreatePaymentIntentParams.GetCurrency: TpsCurrency;
begin
  Result := StringToCurrency(GetString(currency));
end;

function TpsCreatePaymentIntentParams.GetCustomer: string;
begin
  Result := GetString(customer);
end;

function TpsCreatePaymentIntentParams.GetDescription: string;
begin
  Result := GetString(description);
end;

function TpsCreatePaymentIntentParams.GetFutureUsage: TpsFutureUsage;
begin
  if GetString(setup_future_usage) = 'off_session' then
    Result := psOffSession
  else
    Result := psOnSession;
end;

function TpsCreatePaymentIntentParams.GetPaymentMethod: string;
begin
  Result := GetString(payment_method);
end;

procedure TpsCreatePaymentIntentParams.SetAmount(const Value: integer);
begin
  SetInteger(amount, Value);
end;

procedure TpsCreatePaymentIntentParams.SetApplicationFeeAmount(const Value: integer);
begin
  SetInteger(application_fee_amount, Value);
end;

procedure TpsCreatePaymentIntentParams.SetConfirm(const Value: Boolean);
begin
  SetBoolean(confirm, Value);
end;

procedure TpsCreatePaymentIntentParams.SetCurrency(const Value: TpsCurrency);
begin
  SetString(currency, CurrencyToString(Value));
end;

procedure TpsCreatePaymentIntentParams.SetCustomer(const Value: string);
begin
  SetString(customer, Value);
end;

procedure TpsCreatePaymentIntentParams.SetDescription(const Value: string);
begin
  SetString(description, Value);
end;

procedure TpsCreatePaymentIntentParams.SetFutureUsage(const Value: TpsFutureUsage);
begin
  case Value of
    psOnSession: SetString(setup_future_usage, 'on_session');
    psOffSession: SetString(setup_future_usage, 'off_session');
  end;
end;

procedure TpsCreatePaymentIntentParams.SetPaymentMethod(const Value: string);
begin
  SetString(payment_method, Value);
end;

end.
