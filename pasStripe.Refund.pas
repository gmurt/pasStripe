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

unit pasStripe.Refund;

interface

uses pasStripe, pasStripe.Params, pasStripe.ParamTypes;

type
  TpsCreateRefundParams = class(TpsBaseParamsWithMetaData, IpsCreateRefundParams)
  protected
    function GetAmount: integer;
    function GetCharge: string;
    function GetReason: string;

    procedure SetAmount(const Value: integer);
    procedure SetCharge(const Value: string);
    procedure SetReason(const Value: string);
  end;

implementation

{ TpsCreateRefundParams }

function TpsCreateRefundParams.GetAmount: integer;
begin
  Result := GetInteger(amount);
end;

function TpsCreateRefundParams.GetCharge: string;
begin
  Result := GetString(charge);
end;

function TpsCreateRefundParams.GetReason: string;
begin
  Result := GetString(reason);
end;

procedure TpsCreateRefundParams.SetAmount(const Value: integer);
begin
  SetInteger(amount, Value);
end;

procedure TpsCreateRefundParams.SetCharge(const Value: string);
begin
  SetString(charge, Value);
end;

procedure TpsCreateRefundParams.SetReason(const Value: string);
begin
  SetString(reason, Value);
end;

end.
