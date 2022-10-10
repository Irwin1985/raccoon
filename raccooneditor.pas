unit RaccoonEditor;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  SynEdit, SynHighlighterAny,
  uRaccoon;


type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    SynAnySyn1: TSynAnySyn;
    RaccoonSynEdit: TSynEdit;
    ToolBar1: TToolBar;
    ToolButtonExecute: TToolButton;
    procedure ToolButtonExecuteClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButtonExecuteClick(Sender: TObject);
var
  source: string;
begin
  source := RaccoonSynEdit.Text;

  if length(source) = 0 then
    Exit;

  TRaccoon.ExecuteScript(source);
end;

end.

