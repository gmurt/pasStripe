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

unit pasStripe.Base;

interface

uses pasStripe, SysUtils, Classes, System.Generics.Collections, pasStripe.Json;

type
  TpsBaseObject = class(TInterfacedObject, IpsBaseObject)
  protected
    procedure Clear; virtual; abstract;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TpsJsonObject); overload; virtual;

  end;

  TpsBaseObjectWithMetadata = class(TpsBaseObject, IpsBaseObjectWithMetaData)
  private
    FMetaData: IpsMetadata;
  protected
    function GetMetadata: IpsMetadata;
    procedure Clear; override;
    procedure LoadFromJson(AJson: TpsJsonObject); overload; override;
  public
    constructor Create; virtual;
  end;

  TpsBaseList<T> = class(TInterfacedObject, IpsBaseList<T>)
  strict private
    FList: TList<T>;
    function GetEnumerator: TList<T>.TEnumerator;
  strict protected
    function Add(const Value: T): Integer;
    function GetCount: integer;
    function GetItem(AIndex: integer): T;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses pasStripe.MetaData;


constructor TpsBaseList<T>.Create;
begin
  inherited;
  FList := TList<T>.Create;
end;

destructor TpsBaseList<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TpsBaseList<T>.GetCount: integer;
begin
  Result := FList.Count;
end;

function TpsBaseList<T>.GetEnumerator: TList<T>.TEnumerator;
begin
  Result := FList.GetEnumerator;
end;

function TpsBaseList<T>.GetItem(AIndex: integer): T;
begin
  Result := FList[AIndex];
end;

function TpsBaseList<T>.Add(const Value: T): Integer;
begin
  Result := FList.Add(Value);
end;

{ TpsBaseObjectWithMetadata }

procedure TpsBaseObjectWithMetadata.Clear;
begin
  inherited;
  FMetaData.Clear;
end;

constructor TpsBaseObjectWithMetadata.Create;
begin
  inherited;
  FMetaData := TpsMetaData.Create;
end;

function TpsBaseObjectWithMetadata.GetMetadata: IpsMetadata;
begin
  Result := FMetaData;
end;

procedure TpsBaseObjectWithMetadata.LoadFromJson(AJson: TpsJsonObject);
begin
  inherited;
  FMetaData.LoadFromJson(AJson.O['metadata']);
end;

{ TpsBaseObject }

procedure TpsBaseObject.LoadFromJson(AJson: TpsJsonObject);
begin
  Clear;
end;

procedure TpsBaseObject.LoadFromJson(AJson: string);
var
  AObj: TpsJsonObject;
begin
  AObj := TpsJsonObject.ParseJSONValue(AJson) as TpsJsonObject;
  try
    LoadFromJson(AObj);
  finally
    AObj.Free;
  end;
end;
                 
end.
