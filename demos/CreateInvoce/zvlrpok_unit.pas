unit zvlrpok_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, IniFiles;

type

  { TZvlrpokFrame }

  TZvlrpokFrame = class(TFrame)
    Button1: TButton;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public
    procedure LoadData(AIniFile:TIniFile);
    procedure SaveData(AIniFile:TIniFile);
  end;

implementation
uses ImportGoodsAndIndirectTaxesExchangeFile;

{$R *.lfm}

{ TZvlrpokFrame }

procedure TZvlrpokFrame.Button1Click(Sender: TObject);
var
  R: TImportGoodsAndIndirectTaxesExchangeFile;
begin
  R:=TImportGoodsAndIndirectTaxesExchangeFile.Create;
  R.LoadFromXML(FileNameEdit1.FileName);
  R.Free;
end;

procedure TZvlrpokFrame.LoadData(AIniFile: TIniFile);
begin
  FileNameEdit1.Text:=AIniFile.ReadString('ZvlrpokFrame', 'FileName1', '');
end;

procedure TZvlrpokFrame.SaveData(AIniFile: TIniFile);
begin
  AIniFile.WriteString('ZvlrpokFrame', 'FileName1', FileNameEdit1.Text);
end;

end.

