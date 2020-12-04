unit UFWait;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvExControls,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TFWait = class(TForm)
    LNowShrinking: TLabel;
    LTotalClasses: TLabel;
    LUsedClasses: TLabel;
    LMessage: TLabel;
    ASPBJNI: TProgressBar;
    ASPB: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FWait: TFWait;

implementation

{$R *.dfm}

{ TFWait }

end.
