unit uObject;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  // El objeto base para Raccoon
  TRObject = class(TObject)
  private
    FValue: Variant;
  public
    constructor Create(v: Variant);
    destructor Destroy; override;
    property Value: Variant read FValue;
  end;

implementation
constructor TRObject.Create(v: Variant);
begin
  FValue := v;
end;

destructor TRObject.Destroy;
begin
  inherited Destroy;
end;

end.

