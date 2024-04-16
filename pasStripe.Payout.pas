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

unit pasStripe.Payout;

interface

uses pasStripe, pasStripe.Base, pasStripe.Json, pasStripe.Params, Spring;

type
  TpsPayoutListOptions = class(TInterfacedObject, IpsPayoutListOptions)
  private
    FFrom: TDateTime;
    FTo: TDateTime;
    Flimit: integer;

    function GetFromDate: TDateTime;
    function GetToDate: TDateTime;
    function GetLimit: integer;
    procedure SetFromDate(const Value: TDateTime);
    procedure SetLimit(const Value: integer);
    procedure SetToDate(const Value: TDateTime);
   public
    constructor Create; virtual;
  end;


  TpsPayout = class(TpsBaseObjectWithMetadata, IpsPayout)
  private
    FID: string;
    FAMount: integer;
    FCurrency: string;
    FStatus: string;
    FDescription: string;
    FArriveBy: TDateTime;
  protected
    function GetID: string;
    function GetAmount: integer;
    function Getcurrency: string;
    function GetDescription: string;
    function GetStatus: string;
    function GetArriveBy: TDateTime;
  public
    procedure LoadFromJson(AJson: TpsJsonObject); override;
  end;

  TpsPayoutList = class(TpsBaseList<IpsPayout>, IpsPayoutList);

implementation

uses SysUtils, DateUtils, pasStripe.Constants, pasStripe.Utils, pasStripe.ParamTypes;

{ TpsPayoutListOptions }

constructor TpsPayoutListOptions.Create;
begin
  Flimit := -1;
  FFrom := -1;
  FTo := -1;
end;

function TpsPayoutListOptions.GetFromDate: TDateTime;
begin
  Result := FFrom;
end;

function TpsPayoutListOptions.GetLimit: integer;
begin
  Result := Flimit;
end;

function TpsPayoutListOptions.GetToDate: TDateTime;
begin
  Result := FTo;
end;

procedure TpsPayoutListOptions.SetFromDate(const Value: TDateTime);
begin
  FFrom := Value;
end;

procedure TpsPayoutListOptions.SetLimit(const Value: integer);
begin
  Flimit := Value;
end;

procedure TpsPayoutListOptions.SetToDate(const Value: TDateTime);
begin
  FTo := Value;
end;


{ TpsPayout }


function TpsPayout.GetArriveBy: TDateTime;
begin
  Result := FArriveBy;
end;

function TpsPayout.GetAmount: integer;
begin
  Result := FAMount;
end;

function TpsPayout.Getcurrency: string;
begin
  Result := FCurrency;
end;

function TpsPayout.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsPayout.GetID: string;
begin
  Result := FID;
end;

function TpsPayout.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TpsPayout.LoadFromJson(AJson: TpsJsonObject);
begin
  inherited;
  //FJson := AJson.ToJSON;
  FID := AJson.S[id];
  FAMount := AJson.I[amount];
  FCurrency := AJson.S[currency];
  FStatus := AJson.S[status];
  FDescription := AJson.S[description];
  FArriveBy := UnixToDateTime(AJson.I[arrival_date]);
  //FAmount := AJson.I[amount];

{  if AJson.IsNull('customer') = False then
  begin
    case AJson.Types['customer'] of
      jvtObject: FCustomer := AJson.O['customer'].S[id];
      jvtString: FCustomer := AJson.S[customer];
    end;
  end;    }




  //FMetadata.LoadFromJson(AJson.O['metadata']);
//  FCreated := UnixToDateTime(StrToInt(AJson.S[created]));
//  if AJson.IsNull('payment_intent') = False then
//    FpaymentIntentID := AJson.S[payment_intent];
//  if AJson.IsNull('description') = False then
//   FDescription := AJson.S[description];

//  FStatus := AJson.S[status];
end;




end.
