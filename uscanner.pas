unit uScanner;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Generics.Collections, uToken, uError;

type
  TDotChecker = record
    len: integer;
    value: string;
    token: TKind;
    literal: Variant;
  end;

  { TScanner }
  TScanner = class(TObject)
    FSource: string;
    FTokens: TObjectList<TToken>;
    FStart: integer;
    FCurrent: integer;
    FLine: integer;
    FCol: integer;

    procedure ScanToken;
    procedure AddToken(tokenKind: TKind; literal: Variant);

    function Advance: char;
    function Peek: char;
    function PeekNext: char;
    function Match(expect: char): boolean;
    procedure CheckDot;
    procedure ReadString(delim: char);
    procedure ReadNumber;
    procedure ReadIdentifier;

    function IsDigit(c: char): boolean;
    function IsAlpha(c: char): boolean;
    function IsAlphaNumeric(c: char): boolean;
    function IsAtEnd: boolean;
  public
    constructor Create(source: string);
    destructor Destroy; override;
    function scanTokens: TObjectList<TToken>;
  end;
var
  aChecker: array [1..7] of TDotChecker;

implementation
(************************************************************************
* ScanToken
************************************************************************)
  procedure TScanner.ScanToken;
  var
    c: char;
  begin
    c := advance();
    case c of
    '(': addToken(TKind.tkLeftParen, c);
    ')': addToken(TKind.tkRightParen, c);
    '{': addToken(TKind.tkLeftBrace, c);
    '}': addToken(TKind.tkRightBrace, c);
    '[': addToken(TKind.tkLeftBracket, c);
    ']': addToken(TKind.tkRightBracket, c);
    ',': addToken(TKind.tkComma, c);
    '.': checkDot();
    '-': if match('=') then addToken(TKind.tkMinusAssign, '-=') else addToken(TKind.tkMinus, '-');
    '+': if match('=') then addToken(TKind.tkPlusAssign, '+=') else addToken(TKind.tkPlus, '+');
    ';':
    begin
      while Peek() in [#9, #13, #32] do
          Advance();
      if not Match(#10) then
        Errors.Append(FLine, FCol, 'Expected New Line after ";"')
    end;
    '*': if match('=') then addToken(TKind.tkMultAssign, '*=') else addToken(TKind.tkMult, '*');
    '!': if match('=') then addToken(TKind.tkNotEqual, '!=') else addToken(TKind.tkNot, '!');
    '=': if match('=') then addToken(TKind.tkEqual, '==') else addToken(TKind.tkAssign, '=');
    '<': if match('=') then addToken(TKind.tkLessEq, '<=') else addToken(TKind.tkLess, '<');
    '>': if match('=') then addToken(TKind.tkGreaterEq, '>=') else addToken(TKind.tkGreater, '>');
    '?': addToken(TKind.tkAsk, c);
    '/':
      if match('=') then
        addToken(TKind.tkDivAssign, '/=')
      else
        if match('*') then
        begin
          repeat
            advance();
          until (peek() = '*') and (peekNext() = '/');

          if not isAtEnd() then
          begin
            advance(); // '*'
            advance(); // '/'
          end
        end
        else
          if match('/') then
            while (Peek() <> #10) and (not IsAtEnd()) do
              advance()
          else
            addToken(TKind.tkDiv, '/');
    #9, #13, #32: {skip blanks} ;
    #10:
    begin
      Inc(FLine);
      FCol := 1;
      if (FTokens.Count > 0) and (FTokens.Last().Kind <> TKind.tkNewLine) then
         addToken(TKind.tkNewLine, 0);

    end;
    '"', '''': readString(c);
    '0'..'9': readNumber();
    'a'..'z','A'..'Z','_': readIdentifier();
    else
      Errors.Append(FLine, FCol, 'Unexpected character ' + c);
    end;
  end;
(************************************************************************
* AddToken
************************************************************************)
  procedure TScanner.AddToken(tokenKind: TKind; literal: Variant);
  begin
    FTokens.Add(TToken.Create(tokenKind, literal, FLine, FCol));
  end;
(************************************************************************
* Advance
************************************************************************)
  function TScanner.Advance: char;
  begin
    Inc(FCurrent);
    Inc(FCol);
    Result := FSource[FCurrent-1];
  end;
(************************************************************************
* Peek
************************************************************************)
  function TScanner.Peek: char;
  begin
    if isAtEnd() then
      Result := #0
    else
      Result := FSource[FCurrent];
  end;
(************************************************************************
* PeekNext
************************************************************************)
  function TScanner.PeekNext: char;
  begin
    if (FCurrent+1) >= High(FSource) then
      Result := #0
    else
      Result := FSource[FCurrent+1];

  end;
(************************************************************************
* Match
************************************************************************)
  function TScanner.Match(expect: char): boolean;
  begin
    if isAtEnd() then
      Exit(False);

    if FSource[FCurrent] <> expect then
      Exit(False);

    Inc(FCurrent); // es lo mismo que advance()
    Result := True;
  end;
(************************************************************************
* CheckDot
************************************************************************)
  procedure TScanner.CheckDot;
  var
    peekStr: string;
    checker: TDotChecker;
    found: Boolean;
    sourceLen: integer;
    i: integer;
  begin
    found := False;
    sourceLen := High(FSource)-FCurrent;
    for checker in aChecker do
    begin
      if sourceLen >= checker.len then
      begin
        peekStr := LowerCase(System.Copy(FSource, FCurrent, checker.len));
        if peekStr = checker.value then
        begin
          //.false.
          addToken(checker.token, checker.literal);
          i := 1;
          repeat
            Advance();
            Inc(i);
          until i > checker.len;
          found := True;
          break;
        end;
      end;
    end;
    if not found then
      addToken(TKind.tkDot, '.');
  end;
(************************************************************************
* ReadString
************************************************************************)
  procedure TScanner.ReadString(delim: char);
  var
    ch: char;
    pch: char;
    str: string;
  begin
    while not isAtEnd() do
    begin
      ch := peek();
      if ch = '\' then
      begin
        pch := peekNext();
        if (pch = '\') or (pch = '/') or (pch = 'n') or (pch = 'r') or (pch = 't') or (pch = '"') or (pch = '''') then
          advance()
      end
      else if ch = delim then
      begin
        advance();
        break;
      end;
      advance();
    end;
    if isAtEnd() then
      Errors.Append(FLine, FCol, 'Unexpected character ' + ch);

    str := System.Copy(FSource, FStart+1, FCurrent-FStart-2);
    str := StringReplace(str, '\\', '\', [rfReplaceAll]);
    str := StringReplace(str, '\/', '/', [rfReplaceAll]);
    str := StringReplace(str, '\n', LineEnding, [rfReplaceAll]);
    str := StringReplace(str, '\r', #13, [rfReplaceAll]);
    str := StringReplace(str, '\t', #9, [rfReplaceAll]);
    str := StringReplace(str, '\"', #9, [rfReplaceAll]);
    str := StringReplace(str, '\''', '''', [rfReplaceAll]);

    addToken(TKind.tkString, str);
  end;
(************************************************************************
* ReadNumber
************************************************************************)
  procedure TScanner.ReadNumber;
  var
    floatNumber: double;
  begin
    while isDigit(Peek()) do
      advance();
    if (peek() = '.') and isDigit(peekNext()) then
    begin
      advance(); // skip '.'
      while isDigit(peek()) do
        advance();
    end;
    floatNumber := System.Copy(FSource, FStart, FCurrent-FStart).ToExtended;
    addToken(tkNumber, floatNumber);
  end;
(************************************************************************
* ReadIdentifier
************************************************************************)
  procedure TScanner.ReadIdentifier;
  var
    ident: string;
    tk: TKind;
  begin
    while isAlphaNumeric(peek()) do
      advance();
    ident := lowerCase(System.Copy(FSource, FStart, FCurrent-FStart));
    if not keywords.TryGetValue(ident, tk) then
      tk := TKind.tkIdentifier;
    if (tk = TKind.tkTrue) or (tk = TKind.tkFalse) then
      addToken(tk, tk = TKind.tkTrue)
    else if tk = TKind.tkNull then
      addToken(tk, null)
    else
      addToken(tk, ident);
  end;
(************************************************************************
* IsDigit
************************************************************************)
  function TScanner.IsDigit(c: char): boolean;
  begin
    Result := (c >= '0') and (c <= '9');
  end;
(************************************************************************
* IsAlpha
************************************************************************)
  function TScanner.IsAlpha(c: char): boolean;
  begin
    Result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')) or (c = '_');
  end;
(************************************************************************
* IsAlphaNumeric
************************************************************************)
  function TScanner.IsAlphaNumeric(c: char): boolean;
  begin
    Result := isAlpha(c) or isDigit(c);
  end;
(************************************************************************
* IsAtEnd
************************************************************************)
  function TScanner.IsAtEnd: boolean;
  begin
    Result := (FCurrent = High(FSource));
  end;
(************************************************************************
* Create
************************************************************************)
  constructor TScanner.Create(source: string);
  begin
    FSource := source;
    FTokens := TObjectList<TToken>.Create(true); (* Owns tokens *)
    FStart := Low(FSource);
    FCurrent := FStart;
    FLine := 1;
    FCol := 1;
  end;
(************************************************************************
* Destroy
************************************************************************)
  destructor TScanner.Destroy;
  begin
    inherited;
  end;
(************************************************************************
* ScanTokens
************************************************************************)
  function TScanner.ScanTokens: TObjectList<TToken>;
  begin
    while not isAtEnd() do begin
      FStart := FCurrent;
      scanToken();
    end;
    if (FTokens.count > 0) and (FTokens.Last().Kind <> TKind.tkNewLine) then
      FTokens.Add(TToken.Create(TKind.tkNewLine, 0, FLine, FCol));
    FTokens.Add(TToken.Create(TKind.tkEof, 0, FLine, FCol));
    Result := FTokens;
  end;

initialization
  aChecker[1].len   := 6;
  aChecker[1].value := 'false.';
  aChecker[1].token := TKind.tkFalse;
  aChecker[1].literal := False;

  aChecker[2].len   := 5;
  aChecker[2].value := 'null.';
  aChecker[2].token := TKind.tkNull;
  aChecker[2].literal := nil;

  aChecker[3].len   := 5;
  aChecker[3].value := 'true.';
  aChecker[3].token := TKind.tkTrue;
  aChecker[3].literal := True;

  aChecker[4].len   := 4;
  aChecker[4].value := 'and.';
  aChecker[4].token := TKind.tkAnd;
  aChecker[4].literal := 'And';

  aChecker[5].len   := 3;
  aChecker[5].value := 'or.';
  aChecker[5].token := TKind.tkOr;
  aChecker[5].literal := 'Or';

  aChecker[6].len   := 2;
  aChecker[6].value := 't.';
  aChecker[6].token := TKind.tkTrue;
  aChecker[6].literal := True;

  aChecker[7].len   := 2;
  aChecker[7].value := 'f.';
  aChecker[7].token := TKind.tkFalse;
  aChecker[7].literal := False;

end.

