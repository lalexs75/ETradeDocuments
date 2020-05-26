unit lrSBRF_QR;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, lrQRCode, Graphics, LR_Class;

type
  TlrSBRF_QR = class(TComponent)
  private

  protected

  public

  published

  end;

  { TlrSBRF_QRCodeView }

  TlrSBRF_QRCodeView = class(TlrQRCodeView)
  private
    FPayerINN: string;
    FPayRecipientAccount: string;
    FPayRecipientBank: string;
    FPayRecipientBIC: string;
    FPayRecipientCorrAccount: string;
    FPayRecipientName: string;
    FPurpose: string;
    FSum: string;
  protected
    function GetText:string; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Restrictions;

    property PayRecipientName:string read FPayRecipientName write FPayRecipientName;
    property PayRecipientAccount:string read FPayRecipientAccount write FPayRecipientAccount;
    property PayRecipientBank:string read FPayRecipientBank write FPayRecipientBank;
    property PayRecipientBIC:string read FPayRecipientBIC write FPayRecipientBIC;
    property PayRecipientCorrAccount:string read FPayRecipientCorrAccount write FPayRecipientCorrAccount;

    property Sum:string read FSum write FSum;
    property Purpose:string read FPurpose write FPurpose;
    property PayerINN:string read FPayerINN write FPayerINN;
  end;

procedure Register;

implementation
uses LR_Utils, lrSBRF_QR_Editor;

procedure Register;
begin
  RegisterComponents('LazReport',[TlrSBRF_QR]);
end;

var
  lrBMP_QRCodeView : TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_QRCodeView) then
  begin
    lrBMP_QRCodeView := TBitmap.Create;
    lrBMP_QRCodeView.LoadFromResourceName(HInstance, TlrQRCodeView.ClassName);
    frRegisterObject(TlrSBRF_QRCodeView, lrBMP_QRCodeView, TlrSBRF_QRCodeView.ClassName, nil, otlReportView, nil, @lrSBRF_QREditorProc);
  end;
end;

{ TlrSBRF_QRCodeView }

function TlrSBRF_QRCodeView.GetText: string;

function DoParse(AText:string):string;
begin
  if (Length(AText) > 0) and (Pos('[',AText)<>0) then
    Result := frParser.Calc(AText)
  else
    Result:=AText;
end;

function DoParseSum(AText:string):string;
var
  C:Currency;
begin
  if (Length(AText) > 0) and (Pos('[',AText)<>0) then
  begin
    C := frParser.Calc(AText)
  end
  else
  begin
    if not TryStrToCurr(AText, C) then
      C:=0;
  end;
  Result:=IntToStr(Trunc(C * 100));
end;

begin
  Result:='ST00011|'+
  'Name=' + DoParse(FPayRecipientName) + '|'+
  'PersonalAcc=' + DoParse(FPayRecipientAccount) + '|'+
  'BankName=' + DoParse(FPayRecipientBank) + '|'+
  'BIC=' + DoParse(FPayRecipientBIC) + '|'+
  'CorrespAcc=' + DoParse(FPayRecipientCorrAccount) + '|'+
  'Sum='+DoParseSum(FSum) + '|'+
  'Purpose=' + DoParse(FPurpose) + '|'+
  'PayerINN=' + DoParse(FPayerINN)

end;

procedure TlrSBRF_QRCodeView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);

  FPayRecipientAccount:=frReadString(Stream);
  FPayRecipientBank:=frReadString(Stream);
  FPayRecipientBIC:=frReadString(Stream);
  FPayRecipientCorrAccount:=frReadString(Stream);
  FPayRecipientName:=frReadString(Stream);

  FSum:=frReadString(Stream);
  FPurpose:=frReadString(Stream);
  FPayerINN:=frReadString(Stream);
end;

procedure TlrSBRF_QRCodeView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  frWriteString(Stream, FPayRecipientAccount);
  frWriteString(Stream, FPayRecipientBank);
  frWriteString(Stream, FPayRecipientBIC);
  frWriteString(Stream, FPayRecipientCorrAccount);
  frWriteString(Stream, FPayRecipientName);

  frWriteString(Stream, FSum);
  frWriteString(Stream, FPurpose);
  frWriteString(Stream, FPayerINN);
end;

procedure TlrSBRF_QRCodeView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FPayRecipientAccount:=XML.GetValue(Path+'PayRecipient/Value', '');
  FPayRecipientBank:=XML.GetValue(Path+'PayRecipientBank/Value', '');
  FPayRecipientBIC:=XML.GetValue(Path+'PayRecipientBIC/Value', '');
  FPayRecipientCorrAccount:=XML.GetValue(Path+'PayRecipientCorrAccount/Value', '');
  FPayRecipientName:=XML.GetValue(Path+'PayRecipientName/Value', '');
  FSum:=XML.GetValue(Path+'Sum/Value', '');
  FPurpose:=XML.GetValue(Path+'Purpose/Value', '');
  FPayerINN:=XML.GetValue(Path+'PayerINN/Value', '');
end;

procedure TlrSBRF_QRCodeView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'PayRecipient/Value', FPayRecipientAccount);
  XML.SetValue(Path+'PayRecipientBank/Value', FPayRecipientBank);
  XML.SetValue(Path+'PayRecipientBIC/Value', FPayRecipientBIC);
  XML.SetValue(Path+'PayRecipientCorrAccount/Value', FPayRecipientCorrAccount);
  XML.SetValue(Path+'PayRecipientName/Value', FPayRecipientName);

  XML.SetValue(Path+'Sum/Value', FSum);
  XML.SetValue(Path+'Purpose/Value', FPurpose);
  XML.SetValue(Path+'PayerINN/Value', FPayerINN);
end;

procedure TlrSBRF_QRCodeView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrSBRF_QRCodeView then
  begin
    FPayRecipientAccount:=TlrSBRF_QRCodeView(Source).FPayRecipientAccount;
    FPayRecipientBank:=TlrSBRF_QRCodeView(Source).FPayRecipientBank;
    FPayRecipientBIC:=TlrSBRF_QRCodeView(Source).FPayRecipientBIC;
    FPayRecipientCorrAccount:=TlrSBRF_QRCodeView(Source).FPayRecipientCorrAccount;
    FPayRecipientName:=TlrSBRF_QRCodeView(Source).FPayRecipientName;

    FSum:=TlrSBRF_QRCodeView(Source).FSum;
    FPurpose:=TlrSBRF_QRCodeView(Source).FPurpose;
    FPayerINN:=TlrSBRF_QRCodeView(Source).FPayerINN;
  end;
end;

initialization
  InitLRComp;

finalization
  if Assigned(lrBMP_QRCodeView) then
    FreeAndNil(lrBMP_QRCodeView);
end.
