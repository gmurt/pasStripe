unit pasStripe.Metadata;

interface

uses SysUtils, System.Generics.Collections, pasStripe.Json, pasStripe;

type
  TpsMetaData = class(TInterfacedObject, IpsMetadata)
  private
    FList: TObjectList<TpsMetadataRecord>;
    function GetValue(AName: string): string;
    procedure SetValue(AName: string; const Value: string);
  protected
    function GetAsJson: string;
    function FindMetaData(AName: string): TpsMetaDataRecord;
    function AddMetaData(AName, AValue: string): TpsMetaDataRecord;
    procedure LoadFromJson(AJson: TJsonObject);
    procedure Enumerate(ACallback: TProc<TpsMetaDataRecord>);
    procedure Clear;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses System.Json;

{ TpsMetaData }

function TpsMetaData.AddMetaData(AName, AValue: string): TpsMetaDataRecord;
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
  FList := TObjectList<TpsMetaDataRecord>.Create(True);
end;

destructor TpsMetaData.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TpsMetaData.Enumerate(ACallback: TProc<TpsMetaDataRecord>);
var
  m: TpsMetaDataRecord;
begin
  for m in FList do
    ACallback(m);
end;

function TpsMetaData.FindMetaData(AName: string): TpsMetaDataRecord;
var
  ARec: TpsMetaDataRecord;
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

function TpsMetaData.GetAsJson: string;
var
  AJson: TJsonObject;
  m: TpsMetaDataRecord;
begin
  AJson := TJsonObject.Create;
  try
    for m in FList do
    begin
      AJson.S[m.Name] := m.Value;
    end;
  finally
    AJson.Free;
  end;
end;

function TpsMetaData.GetValue(AName: string): string;
var
  ARec: TpsMetaDataRecord;
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

procedure TpsMetaData.LoadFromJson(AJson: TJsonObject);
var
  ICount: integer;
begin
  FList.Clear;
  if AJson = nil then
    Exit;
  for ICount := 0 to AJson.Count-1 do
  begin
    AddMetaData(AJson.Names[ICount], AJson.S[AJson.Names[ICount]]);
  end;

end;

procedure TpsMetaData.SetValue(AName: string; const Value: string);
var
  ARec: TpsMetaDataRecord;
begin
  ARec := FindMetaData(AName);
  if ARec <> nil then
  begin
    ARec.Value := Value;
    Exit;
  end;
  AddMetaData(AName, Value);
end;


end.
