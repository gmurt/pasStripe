unit pasStripe.Json;

interface

uses System.Json;

type
  TJsonObject = System.Json.TJSONObject;
  TJsonArray = System.Json.TJSONArray;

  TJsonValueType = (jvtString, jvtObject);

  TJsonHelper = class helper for TJsonObject
  private
    function GetBool(AName: string): Boolean;
    procedure SetBool(AName: string; const Value: Boolean);
    function GetString(AName: string): string;
    procedure SetString(AName: string; const Value: string);
    function GetObject(AName: string): TJsonObject;
    procedure SetObject(AName: string; const Value: TJsonObject);
    function GetArray(AName: string): TJSONArray;
    procedure SetArray(AName: string; const Value: TJSONArray);
    function GetNames(AIndex: integer): string;
    function GetTypes(AName: string): TJsonValueType;
    function GetInteger(AName: string): integer;
    procedure SetInteger(AName: string; const Value: integer);

  public
    function Contains(AName: string): Boolean;
    function IsNull(AName: string): Boolean;
    procedure Assign(ASource: TJSONValue);
    procedure FromJSON(AJson: string);

    property Types[AName: string]: TJsonValueType read GetTypes;
    property Names[AIndex: integer]: string read GetNames;
    property B[AName: string]: Boolean read GetBool write SetBool;
    property S[AName: string]: string read GetString write SetString;
    property I[AName: string]: Integer read GetInteger write SetInteger;
    property O[AName: string]: TJsonObject read GetObject write SetObject;
    property A[AName: string]: TJSONArray read GetArray write SetArray;
  end;

implementation

uses SysUtils, System.Generics.Collections;

{ TJsonHelper }

procedure TJsonHelper.Assign(ASource: TJSONValue);
begin
  FromJSON(ASource.ToJSON);
end;

function TJsonHelper.Contains(AName: string): Boolean;
begin
  Result := FindValue(AName) <> nil;
end;

procedure TJsonHelper.FromJSON(AJson: string);
begin
  Parse(BytesOf(AJson), 0);
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

function TJsonHelper.GetBool(AName: string): Boolean;
var
  AValue: TJSONValue;
begin
  Result := False;
  AValue := FindValue(AName);
  if AValue <> nil then
    Result := AValue.AsType<Boolean> = True;


end;

function TJsonHelper.GetInteger(AName: string): integer;
begin
  Result := StrToIntDef(GetString(AName), 0);
end;

function TJsonHelper.GetNames(AIndex: integer): string;
begin
  Result := Pairs[AIndex].JsonString.Value;
end;

function TJsonHelper.GetObject(AName: string): TJsonObject;
begin
  Result := Values[AName] as TJSONObject;
  if Result = nil then
  begin
    Result := TJsonObject.Create;
    AddPair(AName, TJsonObject.Create);
  end;
end;

function TJsonHelper.GetString(AName: string): string;
var
  AValue: TJSONValue;
begin
  Result := '';
  AValue := FindValue(AName);
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

procedure TJsonHelper.SetBool(AName: string; const Value: Boolean);
begin
  AddPair(AName, TJSONBool.Create(Value));
end;

procedure TJsonHelper.SetInteger(AName: string; const Value: integer);
begin
  AddPair(ANAme, TJSONNumber.Create(Value));
end;

procedure TJsonHelper.SetObject(AName: string; const Value: TJsonObject);
begin
  AddPair(AName, Value)
end;

procedure TJsonHelper.SetString(AName: string; const Value: string);
begin
  AddPair(AName, TJSONString.Create(Value));
end;

function TJsonHelper.GetTypes(AName: string): TJsonValueType;
var
  APair: TJSONValue;
begin
  Result := jvtObject;
  APair := GetValue(AName);
  if APair is TJsonObject then Result := jvtObject;
  if APair is TJSONString then Result := jvtString;

end;

end.
