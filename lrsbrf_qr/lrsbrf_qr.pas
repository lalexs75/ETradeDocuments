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
    FContract: string;
    FFirstName: string;
    FLastName: string;
    FMiddleName: string;
    FPayerINN: string;
    FPayRecipientAccount: string;
    FPayRecipientBank: string;
    FPayRecipientBIC: string;
    FPayRecipientCorrAccount: string;
    FPayRecipientName: string;
    FPurpose: string;
    FQRFormat: string;
    FSum: string;
  protected
    function GetText:string; override;
  public
    constructor Create(AOwnerPage:TfrPage);override;
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

    property QRFormat:string read FQRFormat write FQRFormat; //ST00012
    property Contract:string read FContract write FContract; //Номер договора (Номер счета на оплату)
    property LastName:string read FLastName write FLastName; //Фамилия Плательщика
    property FirstName:string read FFirstName write FFirstName; //Имя Плательщика
    property MiddleName:string read FMiddleName write FMiddleName; //Отчество Плательщика

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
  Result:=FQRFormat+'|'+
  'Name=' + DoParse(FPayRecipientName) + '|'+
  'PersonalAcc=' + DoParse(FPayRecipientAccount) + '|'+
  'BankName=' + DoParse(FPayRecipientBank) + '|'+
  'BIC=' + DoParse(FPayRecipientBIC) + '|'+
  'CorrespAcc=' + DoParse(FPayRecipientCorrAccount) + '|'+
  'PayeeINN=' + DoParse(FPayerINN) + '|'+
  'Contract=' + DoParse(FContract) + '|'+
  'Purpose=' + DoParse(FPurpose) + '|'+
  'LastName=' + DoParse(FLastName) + '|'+
  'FirstName=' + DoParse(FFirstName) + '|'+
  'MiddleName=' + DoParse(FMiddleName)+ '|'+
  'Sum='+DoParseSum(FSum)
{
  if FLastName<>'' then
    Result:=Result + '|'+ 'LastName=' + DoParse(FLastName);
  if FirstName<>'' then
    Result:=Result + '|'+ 'FirstName=' + DoParse(FFirstName);
  if MiddleName<>'' then
    Result:=Result + '|'+ 'MiddleName=' + DoParse(FMiddleName);
}

{
  ST00012 - Идентификатор формата
  Name= Наименование получателя
  PersonalAcc= Номер счета получателя платежа
  BankName= Наименование банка получателя платежа
  BIC= БИК
  CorrespAcc= Номер кор./сч. банка получателя платежа
  PayeeINN= ИНН получателя
  Contract=Номер договора (Номер счета на оплату)
  Purpose= Назначение платежа
  LastName=Фамилия Плательщика
  FirstName=Имя Плательщика
  MiddleName=Отчество Плательщика
  Sum=Сумма платежа, в копейках Макс. 18 знаков
}
end;

constructor TlrSBRF_QRCodeView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FQRFormat:='ST00012';
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
  FQRFormat:=frReadString(Stream);//ST00012
  if FQRFormat = '' then
    FQRFormat:='ST00012';
  FContract:=frReadString(Stream);//Номер договора (Номер счета на оплату)
  FLastName:=frReadString(Stream);//Фамилия Плательщика
  FFirstName:=frReadString(Stream);//Имя Плательщика
  FMiddleName:=frReadString(Stream);//Отчество Плательщика
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
  frWriteString(Stream, FQRFormat);//ST00012
  frWriteString(Stream, FContract);//Номер договора (Номер счета на оплату)
  frWriteString(Stream, FLastName);//Фамилия Плательщика
  frWriteString(Stream, FFirstName);//Имя Плательщика
  frWriteString(Stream, FMiddleName);//Отчество Плательщика
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
  FQRFormat:=XML.GetValue(Path+'QRFormat/Value', ''); //ST00012
  FContract:=XML.GetValue(Path+'Contract/Value', ''); //Номер договора (Номер счета на оплату)
  FLastName:=XML.GetValue(Path+'LastName/Value', ''); //Фамилия Плательщика
  FFirstName:=XML.GetValue(Path+'FirstName/Value', ''); //Имя Плательщика
  FMiddleName:=XML.GetValue(Path+'MiddleName/Value', ''); //Отчество Плательщика

  if FQRFormat = '' then
    FQRFormat:='ST00012';
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
  XML.SetValue(Path+'QRFormat/Value', FQRFormat);//ST00012
  XML.SetValue(Path+'Contract/Value', FContract);//Номер договора (Номер счета на оплату)
  XML.SetValue(Path+'LastName/Value', FLastName);//Фамилия Плательщика
  XML.SetValue(Path+'FirstName/Value', FFirstName);//Имя Плательщика
  XML.SetValue(Path+'MiddleName/Value', FMiddleName);//Отчество Плательщика
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
    FQRFormat:=TlrSBRF_QRCodeView(Source).FQRFormat; //ST00012
    FContract:=TlrSBRF_QRCodeView(Source).FContract; //Номер договора (Номер счета на оплату)
    FLastName:=TlrSBRF_QRCodeView(Source).FLastName; //Фамилия Плательщика
    FFirstName:=TlrSBRF_QRCodeView(Source).FFirstName; //Имя Плательщика
    FMiddleName:=TlrSBRF_QRCodeView(Source).FMiddleName; //Отчество Плательщика

    if FQRFormat = '' then
      FQRFormat:='ST00012';
  end;
end;

initialization
  InitLRComp;

finalization
  if Assigned(lrBMP_QRCodeView) then
    FreeAndNil(lrBMP_QRCodeView);
end.
