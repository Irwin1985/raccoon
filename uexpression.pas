unit uExpression;

{$mode delphi}{$H+}
{$intefaces corba}

interface

uses
  Classes, SysUtils, uToken;

type
  IExpressionVisitor = interface;

  TExpression = class
    abstract
  public
    function Accept(ev: IExpressionVisitor): TObject; virtual; abstract;
  end;

  { TLiteralExpression }
  TLiteralExpression = class(TExpression)
  private
    FValue: Variant;
  public
    constructor Create(v: Variant);
    function Accept(ev: IExpressionVisitor): TObject; override;
    property value: Variant read FValue;
  end;


  { IExpressionVisitor }
  IExpressionVisitor = interface
    ['{F728B90A-9D44-48EE-87B9-D2759024B67F}']
    function VisitLiteralExpr(expr: TLiteralExpression): TObject;
  end;

implementation
constructor TLiteralExpression.Create(v: Variant);
begin
  FValue := v;
end;

function TLiteralExpression.Accept(ev: IExpressionVisitor): TObject;
begin
  Result := ev.VisitLiteralExpr(Self);
end;

end.

