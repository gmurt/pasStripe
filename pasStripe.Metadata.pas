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

unit pasStripe.Metadata;

interface

uses Classes, SysUtils, System.Generics.Collections, pasStripe.Json, pasStripe;

type
  TpsMetaDataRecord = class(TInterfacedObject, IpsMetaDataRecord)
  private
    FName: string;
    FValue: string;
  protected
    function GetName: string;
    function GetValue: string;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: string);

  end;

  TpsMetaData = class(TInterfacedObject, IpsMetadata)
  private
    FList: TList<IpsMetaDataRecord>;
    function GetValue(AName: string): string;
    procedure SetValue(AName: string; const Value: string);
  protected
    function FindMetaData(AName: string): IpsMetaDataRecord;
    function AddMetaData(AName, AValue: string): IpsMetaDataRecord;
    procedure LoadFromJson(AJson: TpsJsonObject);
    procedure LoadFromStrings(AStrings: TStrings);
    procedure Enumerate(ACallback: TProc<IpsMetaDataRecord>);
    procedure Clear;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses System.Json;

{ TpsMetaData }

function TpsMetaData.AddMetaData(AName, AValue: string): IpsMetaDataRecord;
begin
  Result := TpsMetaDataRecord.Create;
  Result.Name := AName;
  Result.Value := AValue;
  FList.Add(Result);
end;

procedure TpsMetaData.Clear;
begin
  FList.Clear;
end;

constructor TpsMetaData.Create;
begin
  inherited Create;
  FList := TList<IpsMetaDataRecord>.Create;
end;

destructor TpsMetaData.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TpsMetaData.Enumerate(ACallback: TProc<IpsMetaDataRecord>);
var
  m: IpsMetaDataRecord;
begin
  for m in FList do
    ACallback(m);
end;

function TpsMetaData.FindMetaData(AName: string): IpsMetaDataRecord;
var
  ARec: IpsMetaDataRecord;
begin
  Result := nil;
  for ARec in FList do
  begin
    if ARec.Name = AName then
    begin
      Result := ARec;
      Exit;
    end;
  end;
end;

function TpsMetaData.GetValue(AName: string): string;
var
  ARec: IpsMetaDataRecord;
begin
  Result := '';
  for ARec in FList do
  begin
    if ARec.Name = AName then
    begin
      Result := ARec.Value;
      Exit;
    end;
  end;
end;

procedure TpsMetaData.LoadFromJson(AJson: TpsJsonObject);
var
  ICount: integer;
begin
  FList.Clear;
  if AJson = nil then
    Exit;
  for ICount := 0 to AJson.Count-1 do
  begin
    //AMeta := AJson.Pairs[ICount];
    AddMetaData(AJson.Pairs[ICount].JsonString.Value,
                AJson.Pairs[ICount].JsonValue.Value);
  end;

end;

procedure TpsMetaData.LoadFromStrings(AStrings: TStrings);
var
  ICount: integer;
begin
  Clear;
  if AStrings <> nil then
  begin
    for ICount := 0 to AStrings.Count-1 do
    begin
      AddMetaData(AStrings.Names[ICount], AStrings.ValueFromIndex[ICount]);
    end;
  end;
end;

procedure TpsMetaData.SetValue(AName: string; const Value: string);
var
  ARec: IpsMetaDataRecord;
begin
  ARec := FindMetaData(AName);
  if ARec <> nil then
  begin
    ARec.Value := Value;
    Exit;
  end;
  AddMetaData(AName, Value);
end;


{ TpsMetaDataRecord }

function TpsMetaDataRecord.GetName: string;
begin
  Result := FName;
end;

function TpsMetaDataRecord.GetValue: string;
begin
  Result := FValue;
end;

procedure TpsMetaDataRecord.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TpsMetaDataRecord.SetValue(const Value: string);
begin
  FValue := Value;
end;

end.
