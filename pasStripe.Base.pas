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

uses pasStripe, Classes, System.Generics.Collections, SysUtils, pasStripe.Json;

type
  TpsBaseObject = class(TInterfacedObject, IpsBaseObject)
  protected
    procedure Clear; virtual; abstract;
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload; virtual;

  end;

  TpsBaseObjectWithMetadata = class(TpsBaseObject, IpsBaseObjectWithMetaData)
  private
    FMetaData: IpsMetadata;
  protected
    function GetMetadata: IpsMetadata;
    procedure Clear; override;
    procedure LoadFromJson(AJson: TJsonObject); overload; override;
  public
    constructor Create; virtual;
  end;

  TpsBaseList = class(TInterfacedObject)
  private
  protected
    FList: TList;
    function GetCount: integer;


  public

  end;





implementation

uses pasStripe.MetaData;

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




procedure TpsBaseObjectWithMetadata.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FMetaData.LoadFromJson(AJson.O['metadata']);
end;


{ TpsBaseList<T> }


function TpsBaseList.GetCount: integer;
begin
  Result := FList.Count;
end;




{ TpsBaseObject }

procedure TpsBaseObject.LoadFromJson(AJson: TJsonObject);
begin
  Clear;
end;

procedure TpsBaseObject.LoadFromJson(AJson: string);
var
  AObj: TJsonObject;
begin
  AObj := TJsonObject.Create;
  try
    AObj.FromJSON(AJson);
    LoadFromJson(AObj);
  finally
    AObj.Free;
  end;
end;

end.
