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

unit pasStripe.Json;

interface

uses System.Json, pasStripe.Constants, pasStripe.ParamTypes;

type



  TpsJsonObject = System.Json.TJSONObject;
  TpsJsonArray = System.Json.TJSONArray;

  TJsonValueType = (jvtString, jvtObject);

  TJsonHelper = class helper for TpsJsonObject
  private
    function GetBool(AParam: TpsParamName): Boolean;
    procedure SetBool(AParam: TpsParamName; const Value: Boolean);
    function GetString(AParam: TpsParamName): string;
    procedure SetString(AParam: TpsParamName; const Value: string);
    function GetObject(AName: string): TpsJsonObject;
    procedure SetObject(AName: string; const Value: TpsJsonObject);
    function GetArray(AName: string): TJSONArray;
    procedure SetArray(AName: string; const Value: TJSONArray);
    function GetNames(AIndex: integer): string;
    function GetTypes(AName: string): TJsonValueType;
    function GetInteger(AParam: TpsParamName): integer;
    procedure SetInteger(AParam: TpsParamName; const Value: integer);
  public
    function Contains(AName: string): Boolean;
    function IsNull(AName: string): Boolean;
    //procedure Assign(ASource: TJSONValue);
   // procedure FromJSON(AJson: string);

    property Types[AName: string]: TJsonValueType read GetTypes;
    property Names[AIndex: integer]: string read GetNames;
    property B[AParam: TpsParamName]: Boolean read GetBool write SetBool;
    property S[AParam: TpsParamName]: string read GetString write SetString;
    property I[AParam: TpsParamName]: Integer read GetInteger write SetInteger;
    property O[AName: string]: TpsJsonObject read GetObject write SetObject;
    property A[AName: string]: TJSONArray read GetArray write SetArray;
  end;

implementation

uses SysUtils, System.Generics.Collections, pasStripe.Utils;

{ TJsonHelper }

function TJsonHelper.Contains(AName: string): Boolean;
begin
  Result := FindValue(AName) <> nil;
end;

function TJsonHelper.GetArray(AName: string): TJSONArray;
begin
  Result := FindValue(AName) as TJsonArray;
  if Result = nil then
  begin
    Result := TJSONArray.Create;
    AddPair(AName, Result);
  end;
end;

function TJsonHelper.GetBool(AParam: TpsParamName): Boolean;
var
  AValue: TJSONValue;
begin
  Result := False;
  AValue := FindValue(ParamToString(AParam));
  if AValue <> nil then
    Result := AValue.AsType<Boolean> = True;
end;

function TJsonHelper.GetInteger(AParam: TpsParamName): integer;
begin
  Result := StrToIntDef(GetString(AParam), 0);
end;

function TJsonHelper.GetNames(AIndex: integer): string;
begin
  Result := Pairs[AIndex].JsonString.Value;
end;

function TJsonHelper.GetObject(AName: string): TpsJsonObject;
begin
  Result := Values[AName] as TpsJsonObject;
  if Result = nil then
  begin
    Result := TpsJsonObject.Create;
    AddPair(AName, Result);
  end;
end;

function TJsonHelper.GetString(AParam: TpsParamName): string;
var
  AValue: TJSONValue;
begin
  Result := '';
  AValue := FindValue(ParamToString(AParam));
  if AValue <> nil then
    Result := AValue.AsType<string>;


end;

function TJsonHelper.IsNull(AName: string): Boolean;
begin
  Result := Values[AName] is TJSONNull;

end;

procedure TJsonHelper.SetArray(AName: string; const Value: TJSONArray);
begin
  AddPair(AName, Value);
end;

procedure TJsonHelper.SetBool(AParam: TpsParamName; const Value: Boolean);
begin
  AddPair(ParamToString(AParam), TJSONBool.Create(Value));
end;

procedure TJsonHelper.SetInteger(AParam: TpsParamName; const Value: integer);
begin
  AddPair(ParamToString(AParam), TJSONNumber.Create(Value));
end;

procedure TJsonHelper.SetObject(AName: string; const Value: TpsJsonObject);
begin
  AddPair(AName, Value)
end;

procedure TJsonHelper.SetString(AParam: TpsParamName; const Value: string);
begin
  AddPair(ParamToString(AParam), TJSONString.Create(Value));
end;

function TJsonHelper.GetTypes(AName: string): TJsonValueType;
var
  APair: TJSONValue;
begin
  Result := jvtObject;
  APair := GetValue(AName);
  if APair is TpsJsonObject then Result := jvtObject;
  if APair is TJSONString then Result := jvtString;

end;

end.
