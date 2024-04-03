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

unit pasStripe.Account;

interface

uses Classes, pasStripe, pasStripe.Base, pasStripe.Params, pasStripe.Json;

type
  TpsUpdateAccountParams = class(TpsBaseParamsWithMetaData, IpsUpdateAccountParams)
  private
    FCompany: IpsCompanyParams;
    function GetBusinessType: string;
    function GetEmail: string;
    function GetCompany: IpsCompanyParams;
    procedure SetBusinessType(const Value: string);
    procedure SetEmail(const Value: string);
  protected
    procedure PopulateStrings(AStrings: TStrings); override;
  public
    constructor Create(AParent: TpsBaseParams); override;
  end;

  TpsCreateAccountParams = class(TpsUpdateAccountParams);



  TpsAccount = class(TpsBaseObjectWithMetadata, IpsAccount)
  private
    Fid: string;
    Fcharges_enabled: Boolean;
    FName: string;
    Fjson: string;
    function GetChargesEnabled: Boolean;
    function GetID: string;
    function GetJson: string;
    function GetName: string;
  protected
    procedure LoadFromJson(AJson: TJsonObject); override;
  end;


implementation

uses pasStripe.Constants;

{ TpsAccount }

function TpsAccount.GetName: string;
begin
  Result := FName;
end;

procedure TpsAccount.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Fjson := AJson.ToJson;
  Fid := AJson.S[id];
  Fcharges_enabled := AJson.B[charges_enabled];
  if not AJson.O['business_profile'].IsNull('name') then
    Fname := AJson.O['business_profile'].S[name];


end;


function TpsAccount.GetChargesEnabled: Boolean;
begin
  Result := Fcharges_enabled;
end;

function TpsAccount.GetID: string;
begin
  Result := Fid;
end;

function TpsAccount.GetJson: string;
begin
  Result := Fjson;
end;

{ TpsUpdateAccountParams }

constructor TpsUpdateAccountParams.Create(AParent: TpsBaseParams);
begin
  inherited;
  FCompany := TpsCompanyParams.Create(Self);
end;

function TpsUpdateAccountParams.GetBusinessType: string;
begin
  Result := GetString(business_type);
end;

function TpsUpdateAccountParams.GetCompany: IpsCompanyParams;
begin
  Result := FCompany;
end;

function TpsUpdateAccountParams.GetEmail: string;
begin
  Result := GetString(email);
end;

procedure TpsUpdateAccountParams.PopulateStrings(AStrings: TStrings);
begin
  inherited;
  FCompany.PopulateStrings(AStrings);
end;

procedure TpsUpdateAccountParams.SetBusinessType(const Value: string);
begin
  SetString(business_type, Value);
end;

procedure TpsUpdateAccountParams.SetEmail(const Value: string);
begin
  SetString(email, Value);
end;


end.
