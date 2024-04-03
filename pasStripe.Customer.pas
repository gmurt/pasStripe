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

unit pasStripe.Customer;

interface

uses
  Classes, pasStripe, pasStripe.Base, pasStripe.Json, System.Generics.Collections, pasStripe.Params;

type
  TpsCustomer = class(TpsBaseObjectWithMetadata, IpsCustomer)
  private
    FId: string;
    FName: string;
    FDescription: string;
    FEmail: string;
    FJson: string;
    function GetDescription: string;
    function GetEmail: string;
    function GetID: string;
    function GetJson: string;
    function GetName: string;
  protected
    procedure LoadFromJson(AJson: TJsonObject); override;
  end;

  TpsUpdateCustomerParams = class(TpsBaseParamsWithMetaData, IpsUpdateCustomerParams)
  protected
    function GetDescription: string;
    function GetName: string;
    function GetEmail: string;
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure SetEmail(const Value: string);

  end;

implementation

uses pasStripe.Constants;

{ TpsCustomer }

function TpsCustomer.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCustomer.GetEmail: string;
begin
  Result := FEmail;
end;

function TpsCustomer.GetID: string;
begin
  Result := FId;
end;

function TpsCustomer.GetJson: string;
begin
  Result := FJson;
end;

function TpsCustomer.GetName: string;
begin
  Result := FName;
end;

procedure TpsCustomer.LoadFromJson(AJson: TJsonObject);
begin
  FId := AJson.S[id];
  if not AJson.IsNull('name') then Fname := AJson.S[name];
  if not AJson.IsNull('description') then FDescription := AJson.S[description];
  if not AJson.IsNull('email') then FEmail := AJson.S[email];
  //FMetaData.LoadFromJson(AJson.O['metadata']);
end;

{ TpsUpdateCustomerParams }

function TpsUpdateCustomerParams.GetDescription: string;
begin
  Result := GetString(TpsParamName.description);
end;

function TpsUpdateCustomerParams.GetEmail: string;
begin
  Result := GetString(email);
end;

function TpsUpdateCustomerParams.GetName: string;
begin
  Result := GetString(name);
end;

procedure TpsUpdateCustomerParams.SetDescription(const Value: string);
begin
  SetString(TpsParamName.description, Value);
end;

procedure TpsUpdateCustomerParams.SetEmail(const Value: string);
begin
  SetString(email, Value);
end;

procedure TpsUpdateCustomerParams.SetName(const Value: string);
begin
  SetString(name, Value);
end;

end.
