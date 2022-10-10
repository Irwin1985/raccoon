unit uRaccoon;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uInterpreter;

type
  { TRaccoon }
  TRaccoon = record
  public
    class var Interpreter: TInterpreter;
    class procedure Run(const Script: string); static;
    class procedure ExecuteScript(const Script: string); static;
  end;

implementation
uses Generics.Collections, Dialogs,
  uToken,
  uScanner,
  uParser,
  uAst,
  uError;

class procedure TRaccoon.Run(const Script: string);
var
  scanner: TScanner;
  parser: TParser;
  tokens: TObjectList<TToken>;
  stmts: TObjectList<TStatement>;
  tok: TToken;
  strList: TStringList;
begin
  try
    scanner := TScanner.Create(Script);
    tokens := scanner.scanTokens;
    strList := TStringList.Create();

    for tok in tokens do
        strList.Add(tok.toString);

    // ShowMessage(strList.Text);

    parser := TParser.Create(tokens);
    stmts := parser.Parse;

    // Check for Lexical or Parsing errors.
    if not Errors.isEmpty then
      ShowMessage(Errors.toString)
    else
      Interpreter.Interpret(stmts);
  finally
    if Assigned(stmts) then stmts.Free;
    tokens.Free;
    scanner.Free;
    parser.Free;
  end;
end;

class procedure TRaccoon.ExecuteScript(const Script: string);
var
  s: TScanner;
  p: TParser;
  i: TInterpreter;
  stmts: TObjectList<TStatement>;
  tokens: TObjectList<TToken>;
  // DEBUG
  printTokens: Boolean = True;
  t: TToken;
  strList: TStringList;
  // DEBUG
begin
  try
    s := TScanner.Create(Script);
    tokens := s.ScanTokens();

    // DEBUG
    if printTokens then
    begin
      strList := TStringList.Create();
      for t in tokens do
        strList.Add(t.toString);
      ShowMessage(strList.Text);
    end;
    // DEBUG

    p := TParser.Create(tokens);
    stmts := p.Parse();
    i := TInterpreter.Create();
    i.Interpret(stmts);
  finally
    s.Free;
    p.Free;
    i.Free;
    if Assigned(stmts) then stmts.Free;
    // DEBUG
    if printTokens and Assigned(strList) then strList.Free;
    // DEBUG
  end;
end;

initialization
  DefaultFormatSettings.DecimalSeparator := '.';
  TRaccoon.Interpreter := TInterpreter.Create();

finalization
  TRaccoon.Interpreter.Free;

end.

