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

unit pasStripe.PaymentMethod;

interface

uses pasStripe, pasStripe.Base, pasStripe.Json;

type
  TpsPaymentMethod = class(TpsBaseObjectWithMetadata, IpsPaymentMethod)
  private
    FID: string;
    FCustomer: string;
    FExpiryMonth: integer;
    FExpiryYear: integer;
    FBrand: string;
    FLast4: string;
    FJson: string;
    function GetID: string;
    function GetCustomer: string;
    function GetBrand: string;
    function GetExpiryMonth: integer;
    function GetExpiryYear: integer;
    function GetExpiryStr: string;
    function GetLast4: string;
    function GetJson: string;
  protected
    procedure LoadFromJson(AJson: TpsJsonObject); override;
  end;

implementation

uses SysUtils, pasStripe.Constants, pasStripe.ParamTypes;

{ TpsPaymentMethod }


function TpsPaymentMethod.GetBrand: string;
begin
  Result := FBrand;
end;

function TpsPaymentMethod.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsPaymentMethod.GetExpiryMonth: integer;
begin
  Result := FExpiryMonth;
end;

function TpsPaymentMethod.GetExpiryStr: string;
begin
  Result := FormatFloat('00', FExpiryMonth)+' / '+ FormatFloat('0000', FExpiryYear);
end;

function TpsPaymentMethod.GetExpiryYear: integer;
begin
  Result := FExpiryYear;
end;

function TpsPaymentMethod.GetID: string;
begin
  Result := FID;
end;

function TpsPaymentMethod.GetJson: string;
begin
  Result := FJson;
end;

function TpsPaymentMethod.GetLast4: string;
begin
  Result := FLast4;
end;

procedure TpsPaymentMethod.LoadFromJson(AJson: TpsJsonObject);
var
  ACard: TpsJsonObject;
begin
  inherited;
  FID := AJson.S[id];
  if AJson.Types['customer'] = jvtString then FCustomer := AJson.S[customer];

  if AJson.Contains('card') then
  begin
    ACard := AJson.O['card'];
    FExpiryMonth := ACard.I[exp_month];
    FExpiryYear := ACard.I[exp_year];
    FBrand := ACard.S[brand];
    FLast4 := ACard.S[last4];
  end;
  FJson := AJson.ToJSON;
end;

end.
